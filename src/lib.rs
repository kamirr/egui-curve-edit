use std::ops::RangeInclusive;

use cgmath::{InnerSpace, Point2, Vector2, Zero};
use egui::epaint::{Color32, Pos2, Shape, Stroke, Vec2};
use egui::{CornerRadius, Painter, Rect, Sense, StrokeKind, Widget};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct HalfCurve {
    start: Point2<f32>,
    start_ctrl: Vector2<f32>,
    end_ctrl: Vector2<f32>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Curve {
    halfs: Vec<HalfCurve>,
    end: Point2<f32>,
}

impl Curve {
    pub fn new(start: [f32; 2], end: [f32; 2]) -> Self {
        Curve {
            halfs: vec![HalfCurve {
                start: start.into(),
                start_ctrl: Vector2::new((end[0] - start[0]) / 10.0, (end[1] - start[1]) / 10.0),
                end_ctrl: Vector2::new((start[0] - end[0]) / 10.0, (start[1] - end[1]) / 10.0),
            }],
            end: end.into(),
        }
    }

    pub fn sample(
        &self,
        pts_per_segment: usize,
        clamp_y: RangeInclusive<f32>,
    ) -> Vec<(usize, [f32; 2])> {
        let mut points = Vec::new();

        let mut curve_it = self.halfs.iter().enumerate().peekable();
        let mut prev_x = self.halfs[0].start.x;

        while let Some((
            i,
            HalfCurve {
                start,
                start_ctrl,
                end_ctrl,
            },
        )) = curve_it.next()
        {
            let end = curve_it
                .peek()
                .map(|(_, next)| next.start)
                .unwrap_or(self.end);

            let start_ctrl_len = start_ctrl.magnitude();
            let start_ctrl_scaled = start_ctrl * 3f32.powf(start_ctrl_len / 10.0 - 1.0);

            let end_ctrl_len = end_ctrl.magnitude();
            let end_ctrl_scaled = end_ctrl * 3f32.powf(end_ctrl_len / 10.0 - 1.0);

            for k in 0..pts_per_segment {
                let t = (k as f32) / (pts_per_segment - 1) as f32;

                let ord1_a = start + start_ctrl_scaled * t;
                let ord1_b = start
                    + start_ctrl_scaled
                    + (end + end_ctrl_scaled - start - start_ctrl_scaled) * t;
                let ord1_c = end + end_ctrl_scaled * (1.0 - t);

                let ord2_a = ord1_a + (ord1_b - ord1_a) * t;
                let ord2_b = ord1_b + (ord1_c - ord1_b) * t;

                let ord3 = ord2_a + (ord2_b - ord2_a) * t;

                let x = ord3
                    .x
                    .clamp(start.x, end.x)
                    .clamp(prev_x, std::f32::INFINITY);
                let y = ord3.y.clamp(*clamp_y.start(), *clamp_y.end());

                points.push((i, [x, y]));
                prev_x = x;
            }
        }

        points
    }

    pub fn sample_along_x(&self, pts_total: usize, clamp_y: RangeInclusive<f32>) -> Vec<f32> {
        let points_lengthwise = self.sample(pts_total, clamp_y);
        let mut result = Vec::new();

        let mut pts_idx = 0;
        for k in 0..pts_total {
            let t = k as f32 / (pts_total - 1) as f32;
            let x = self.halfs[0].start.x + t * (self.end.x - self.halfs[0].start.x);

            while pts_idx < points_lengthwise.len() - 2
                && !(points_lengthwise[pts_idx].1[0] <= x
                    && points_lengthwise[pts_idx + 1].1[0] > x)
            {
                pts_idx += 1;
            }

            let curr = points_lengthwise[pts_idx].1;
            let next = points_lengthwise[pts_idx + 1].1;

            let interpolation_t = if curr[0] == next[0] {
                0.0
            } else {
                (x - curr[0]) / (next[0] - curr[0])
            };

            let y = curr[1] * (1.0 - interpolation_t) + next[1] * interpolation_t;

            result.push(y);
        }

        result
    }
}

#[derive(Debug)]
pub struct CurveEdit<'a> {
    y_axis: RangeInclusive<f32>,
    curve: &'a mut Curve,
}

impl<'a> CurveEdit<'a> {
    pub fn new(curve: &'a mut Curve, y_axis: RangeInclusive<f32>) -> Self {
        Self { curve, y_axis }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
struct Hover {
    curve: Option<usize>,
    curve_pt: Option<[f32; 2]>,
    start: Option<usize>,
    ctrl_start: Option<usize>,
    ctrl_end: Option<usize>,
    end: bool,
}

#[derive(Clone, Copy, Debug)]
enum PtKind {
    Terminal,
    Control,
}

impl<'a> CurveEdit<'a> {
    fn draw_grabbable_pt(
        painter: &Painter,
        pos: Pos2,
        kind: PtKind,
        selected: bool,
        selected_curve: bool,
    ) {
        let stroke = if selected_curve {
            Stroke::new(3.0, Color32::YELLOW)
        } else {
            Stroke::new(3.0, Color32::WHITE)
        };
        match kind {
            PtKind::Terminal => {
                painter.circle_stroke(pos, 8.0, stroke);
                if selected {
                    painter.circle_filled(pos, 4.0, stroke.color);
                }
            }
            PtKind::Control => {
                let sz = 6.0;
                let rect = Rect::from_x_y_ranges(-sz..=sz, -sz..=sz).translate(pos.to_vec2());
                painter.rect_stroke(rect, CornerRadius::ZERO, stroke, StrokeKind::Inside);
                if selected {
                    painter.rect_filled(rect.shrink(3.0), CornerRadius::ZERO, stroke.color);
                }
            }
        }
    }
}

impl<'a> Widget for CurveEdit<'a> {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let (resp, painter) = ui.allocate_painter(Vec2::new(400.0, 400.0), Sense::click_and_drag());

        let rect = resp.rect.shrink(8.0);

        painter.rect_filled(resp.rect, CornerRadius::ZERO, Color32::BLACK);

        let curve_x_start = self.curve.halfs[0].start.x;
        let curve_x_end = self.curve.end.x;
        let rect_width = rect.width();
        let map_x_to_px = move |x: f32| {
            let zero_to_one = (x - curve_x_start) / (curve_x_end - curve_x_start);

            rect_width * zero_to_one
        };

        let y_axis_start = *self.y_axis.start();
        let y_axis_end = *self.y_axis.end();
        let rect_height = rect.height();
        let map_y_to_px = |y: f32| {
            let zero_to_one = (y - y_axis_start) / (y_axis_end - y_axis_start);

            rect_height * zero_to_one
        };

        let rect = rect.clone();
        let xy_to_pos =
            |xy: [f32; 2]| rect.left_bottom() + Vec2::new(map_x_to_px(xy[0]), -map_y_to_px(xy[1]));
        let vec_to_xy = |vec: Vec2| [vec.x / map_x_to_px(1.0), vec.y / map_y_to_px(1.0)];

        let points = self.curve.sample(84, self.y_axis.clone());

        let hover: Hover = ui
            .memory_mut(|mem| mem.data.get_temp(ui.id()))
            .unwrap_or_default();

        if resp.dragged() {
            let [dx, dy] = vec_to_xy(resp.drag_delta());

            if let Some(i) = hover.ctrl_start {
                self.curve.halfs[i].start_ctrl += Vector2::new(dx, -dy);
            } else if let Some(i) = hover.ctrl_end {
                self.curve.halfs[i].end_ctrl += Vector2::new(dx, -dy);
            } else if let Some(i) = hover.start {
                if i == 0 {
                    self.curve.halfs[i].start += Vector2::new(0.0, -dy);
                } else {
                    self.curve.halfs[i].start += Vector2::new(dx, -dy);
                }

                let next_terminal = self
                    .curve
                    .halfs
                    .get(i + 1)
                    .map(|curve| curve.start)
                    .unwrap_or(self.curve.end);

                if self.curve.halfs[i].start.x > next_terminal.x {
                    self.curve.halfs[i].start.x = next_terminal.x
                }
            } else if hover.end {
                self.curve.end += Vector2::new(0.0, -dy);
            }
        }

        let mut hover = Hover::default();
        if let Some(hover_pos) = resp.hover_pos() {
            let mut min = (
                points[0].0,
                points[0].1,
                xy_to_pos(points[0].1).distance(hover_pos),
            );

            for &(idx, pt) in &points[1..] {
                let curve_pos = xy_to_pos(pt);
                let dist = hover_pos.distance(curve_pos);
                if dist < min.2 {
                    min = (idx, pt, dist);
                }
            }

            if min.2 < 16.0 {
                hover.curve = Some(min.0);
                hover.curve_pt = Some(min.1);
            }

            let mut min_start = (0, std::f32::INFINITY);
            let mut min_ctrl_start = (0, std::f32::INFINITY);
            let mut min_ctrl_end = (0, std::f32::INFINITY);
            let end_dist = xy_to_pos([self.curve.end.x, self.curve.end.y]).distance(hover_pos);

            let mut curve_it = self.curve.halfs.iter_mut().enumerate().peekable();
            while let Some((
                i,
                HalfCurve {
                    start,
                    start_ctrl,
                    end_ctrl,
                },
            )) = curve_it.next()
            {
                let end = curve_it
                    .peek()
                    .map(|(_, next)| next.start)
                    .unwrap_or(self.curve.end);

                let start_xy = xy_to_pos([start.x, start.y]);
                let dist = start_xy.distance(hover_pos);
                if dist < min_start.1 {
                    min_start = (i, dist);
                }

                let start_ctrl_xy = xy_to_pos([start_ctrl.x + start.x, start_ctrl.y + start.y]);
                let dist = start_ctrl_xy.distance(hover_pos);
                if dist < min_ctrl_start.1 {
                    min_ctrl_start = (i, dist);
                }

                let end_ctrl_xy = xy_to_pos([end_ctrl.x + end.x, end_ctrl.y + end.y]);
                let dist = end_ctrl_xy.distance(hover_pos);
                if dist < min_ctrl_end.1 {
                    min_ctrl_end = (i, dist);
                }
            }

            if min_start.1 < 8.0 {
                hover.start = Some(min_start.0);
            }
            if min_ctrl_start.1 < 8.0 {
                hover.ctrl_start = Some(min_ctrl_start.0);
            }
            if min_ctrl_end.1 <= 8.0 {
                hover.ctrl_end = Some(min_ctrl_end.0);
            }
            if end_dist <= 8.0 {
                hover.end = true;
            }
        };

        if resp.double_clicked() {
            if let (Some(curve_i), Some([split_x, split_y])) = (hover.curve, hover.curve_pt) {
                let end_ctrl = self.curve.halfs[curve_i].end_ctrl;
                self.curve.halfs.insert(
                    curve_i + 1,
                    HalfCurve {
                        start: Point2::new(split_x, split_y),
                        start_ctrl: Vector2::zero(),
                        end_ctrl,
                    },
                );

                self.curve.halfs[curve_i].end_ctrl = Vector2::zero();
            }
        }

        if resp.clicked_by(egui::PointerButton::Secondary) {
            if let Some(curve_i @ 1..) = hover.start {
                let removed = self.curve.halfs.remove(curve_i);
                self.curve.halfs[curve_i - 1].end_ctrl = removed.end_ctrl;
            }
        }

        if !resp.dragged() {
            ui.memory_mut(|mem| mem.data.insert_temp(ui.id(), hover.clone()));
        }

        for k in 0..points.len() - 1 {
            let (i, p1) = points[k];
            let (_, p2) = points[k + 1];

            let curve_by_point =
                hover.ctrl_start == Some(i) || hover.ctrl_end == Some(i) || hover.start == Some(i);
            let curve_directly = hover.curve == Some(i);

            let stroke = if curve_by_point {
                Stroke::new(3.0, Color32::YELLOW)
            } else if curve_directly {
                Stroke::new(3.0, Color32::GREEN)
            } else {
                Stroke::new(2.0, Color32::LIGHT_GREEN)
            };
            painter.line_segment([xy_to_pos(p1), xy_to_pos(p2)], stroke);
        }

        if let Some(hover_pt) = hover.curve_pt {
            let pos = xy_to_pos(hover_pt);

            painter.circle_filled(pos, 3.0, Color32::GREEN);
        }

        let mut curve_it = self.curve.halfs.iter().enumerate().peekable();
        while let Some((
            i,
            HalfCurve {
                start,
                start_ctrl,
                end_ctrl,
            },
        )) = curve_it.next()
        {
            let end = curve_it
                .peek()
                .map(|(_, next)| next.start)
                .unwrap_or(self.curve.end);
            let end_pos = xy_to_pos([end.x, end.y]);

            let selected_curve =
                hover.ctrl_start == Some(i) || hover.ctrl_end == Some(i) || hover.start == Some(i);
            let selected_curve_prev = hover.ctrl_start == Some(i.saturating_sub(1))
                || hover.ctrl_end == Some(i.saturating_sub(1))
                || hover.start == Some(i.saturating_sub(1));

            let start_pos = xy_to_pos([start.x, start.y]);
            Self::draw_grabbable_pt(
                &painter,
                start_pos,
                PtKind::Terminal,
                hover.start == Some(i),
                selected_curve || selected_curve_prev,
            );

            let start_ctrl_pos = xy_to_pos([start_ctrl.x + start.x, start_ctrl.y + start.y]);
            Self::draw_grabbable_pt(
                &painter,
                start_ctrl_pos,
                PtKind::Control,
                hover.ctrl_start == Some(i),
                selected_curve,
            );

            let end_ctrl_pos = xy_to_pos([end_ctrl.x + end.x, end_ctrl.y + end.y]);
            Self::draw_grabbable_pt(
                &painter,
                end_ctrl_pos,
                PtKind::Control,
                hover.ctrl_end == Some(i),
                selected_curve,
            );

            if selected_curve {
                painter.add(Shape::dashed_line(
                    &[start_ctrl_pos, start_pos],
                    Stroke::new(1.0, Color32::YELLOW),
                    3.0,
                    3.0,
                ));

                painter.add(Shape::dashed_line(
                    &[end_ctrl_pos, end_pos],
                    Stroke::new(1.0, Color32::YELLOW),
                    3.0,
                    3.0,
                ));
            }
        }

        let selected_curve_last = hover.ctrl_start == Some(self.curve.halfs.len() - 1)
            || hover.ctrl_end == Some(self.curve.halfs.len() - 1)
            || hover.start == Some(self.curve.halfs.len() - 1);
        Self::draw_grabbable_pt(
            &painter,
            xy_to_pos([self.curve.end.x, self.curve.end.y]),
            PtKind::Terminal,
            false,
            selected_curve_last,
        );

        resp
    }
}
