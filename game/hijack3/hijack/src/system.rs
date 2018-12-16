use super::*;

pub fn window(x: i32, y: i32, width: i32, height: i32, arrow_x: i32, arrow_y: i32) -> Vec<View> {
    let views = (1..width - 1).flat_map(|i| {
        (1..height - 1).map(move |j| {
            View::Image(
                "pixelart/system/window/center.png".to_string(),
                x + i * 8,
                y + j * 8,
            )
        })
    });
    let views = views.chain((1..width - 1).map(|i| {
        if i == arrow_x && arrow_y == 0 {
            View::Image(
                "pixelart/system/window/top-arrow.png".to_string(),
                x + i * 8,
                y,
            )
        } else {
            View::Image("pixelart/system/window/top.png".to_string(), x + i * 8, y)
        }
    }));
    let views = views.chain((1..height - 1).map(|j| {
        if j == arrow_y && arrow_x == 0 {
            View::Image(
                "pixelart/system/window/left-arrow.png".to_string(),
                x,
                y + j * 8,
            )
        } else {
            View::Image("pixelart/system/window/left.png".to_string(), x, y + j * 8)
        }
    }));
    let views = views.chain((1..width - 1).map(|i| {
        if i == arrow_x && arrow_y == height - 1 {
            View::Image(
                "pixelart/system/window/bottom-arrow.png".to_string(),
                x + i * 8,
                y + (height - 1) * 8,
            )
        } else {
            View::Image(
                "pixelart/system/window/bottom.png".to_string(),
                x + i * 8,
                y + (height - 1) * 8,
            )
        }
    }));
    let views = views.chain((1..height - 1).map(|j| {
        if j == arrow_y && arrow_x == width - 1 {
            View::Image(
                "pixelart/system/window/right-arrow.png".to_string(),
                x + (width - 1) * 8,
                y + j * 8,
            )
        } else {
            View::Image(
                "pixelart/system/window/right.png".to_string(),
                x + (width - 1) * 8,
                y + j * 8,
            )
        }
    }));
    let views = views.chain(vec![
        View::Image("pixelart/system/window/top-left.png".to_string(), x, y),
        View::Image(
            "pixelart/system/window/bottom-left.png".to_string(),
            x,
            y + (height - 1) * 8,
        ),
        View::Image(
            "pixelart/system/window/top-right.png".to_string(),
            x + (width - 1) * 8,
            y,
        ),
        View::Image(
            "pixelart/system/window/bottom-right.png".to_string(),
            x + (width - 1) * 8,
            y + (height - 1) * 8,
        ),
    ]);
    views.collect()
}
