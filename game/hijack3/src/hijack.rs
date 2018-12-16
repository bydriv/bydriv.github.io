#![no_main]
#![feature(extern_types)]

extern crate brownfox;
extern crate hijack;

use brownfox::Moore;
use std::collections::HashMap;

// src/hijack/platform/windows.c
extern "C" {
    type session;
    type image;
    fn hijack_platform_windows_session_alloc(
        name: *const i8,
        canFullScreen: bool,
        isFullScreen: bool,
        clientWidth: u64,
        clientHeight: u64,
        fps: u64,
    ) -> *mut session;
    fn hijack_platform_windows_session_free(session: *mut session);
    fn hijack_platform_windows_step(session: *mut session) -> bool;
    fn hijack_platform_windows_window_show(session: *mut session);
    fn hijack_platform_windows_window_flush(session: *mut session);

    fn hijack_platform_windows_image_load(
        session: *mut session,
        len: usize,
        data: *const i8,
    ) -> *mut image;

    fn hijack_platform_windows_image_width(session: *mut session, image: *mut image) -> u64;

    fn hijack_platform_windows_image_height(session: *mut session, image: *mut image) -> u64;

    fn hijack_platform_windows_image_scale(
        session: *mut session,
        scale: u64,
        image: *mut image,
    ) -> *mut image;

    fn hijack_platform_windows_image_crop(
        session: *mut session,
        x: u64,
        y: u64,
        width: u64,
        height: u64,
        image: *mut image,
    ) -> *mut image;

    fn hijack_platform_windows_image_draw(session: *mut session, x: i64, y: i64, image: *mut image);

    fn hijack_platform_windows_joypad_exists(session: *mut session, i: u64) -> bool;
    fn hijack_platform_windows_joypad_x(session: *mut session, i: u64) -> i64;
    fn hijack_platform_windows_joypad_y(session: *mut session, i: u64) -> i64;
    fn hijack_platform_windows_joypad_buttons(session: *mut session, i: u64) -> u64;
}

#[repr(C)]
struct AssetDefn {
    path: *const i8,
    name: *const i8,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

#[repr(C)]
struct Asset {
    path: *const i8,
    len: usize,
    data: *const i8,
}

// src/hijack/config.c
extern "C" {
    static SCALE: u32;
    static WIDTH: u32;
    static HEIGHT: u32;
    static ASSET_DEFNS_LEN: usize;
    static ASSET_DEFNS: [AssetDefn; 199];
}

// src/hijack/assets.c
extern "C" {
    static assets_len: usize;
    static assets: [Asset; 7];
}

#[no_mangle]
pub extern "C" fn WinMain() -> i32 {
    let name = std::ffi::CString::new("Hijack").unwrap();
    unsafe {
        let mut session =
            hijack_platform_windows_session_alloc(name.as_ptr(), false, false, 640, 480, 60);

        let mut hijack = hijack::Hijack::new();

        hijack_platform_windows_window_show(session);

        let mut ASSETS = HashMap::new();
        let mut defns = HashMap::new();

        for i in 0..assets_len {
            let asset = &assets[i];
            let path = std::ffi::CStr::from_ptr(asset.path)
                .to_string_lossy()
                .into_owned();
            let mut image = hijack_platform_windows_image_load(session, asset.len, asset.data);
            image = hijack_platform_windows_image_scale(session, SCALE.into(), image);
            ASSETS.insert(path, image);
        }

        for i in 0..ASSET_DEFNS_LEN {
            let defn = &ASSET_DEFNS[i];
            let path = std::ffi::CStr::from_ptr(defn.path)
                .to_string_lossy()
                .into_owned();
            match ASSETS.get(&path) {
                None => (),
                Some(image) => {
                    let name = std::ffi::CStr::from_ptr(defn.name)
                        .to_string_lossy()
                        .into_owned();
                    let image = hijack_platform_windows_image_crop(
                        session,
                        (SCALE * defn.x).into(),
                        (SCALE * defn.y).into(),
                        (SCALE * defn.width).into(),
                        (SCALE * defn.height).into(),
                        *image,
                    );
                    defns.insert(name, image);
                }
            }
        }

        while hijack_platform_windows_step(session) {
            let (_, views) = hijack.output();

            for view in views {
                match view {
                    hijack::View::Image(name, x, y) => match defns.get(&name) {
                        None => (),
                        Some(image) => {
                            hijack_platform_windows_image_draw(
                                session,
                                (SCALE as i32 * x).into(),
                                (SCALE as i32 * y).into(),
                                *image,
                            );
                        }
                    },
                    hijack::View::Pattern(name, width, height, x, y) => match defns.get(&name) {
                        None => (),
                        Some(image) => {
                            for i in 0..width {
                                for j in 0..height {
                                    hijack_platform_windows_image_draw(
                                        session,
                                        (SCALE as i32 * x
                                            + (i as i32
                                                * (hijack_platform_windows_image_width(
                                                    session, *image,
                                                )
                                                    as i32)))
                                            .into(),
                                        (SCALE as i32 * y
                                            + (j as i32
                                                * (hijack_platform_windows_image_height(
                                                    session, *image,
                                                )
                                                    as i32)))
                                            .into(),
                                        *image,
                                    );
                                }
                            }
                        }
                    },
                }
            }

            hijack_platform_windows_window_flush(session);

            let inputs = if hijack_platform_windows_joypad_exists(session, 0) {
                let x = hijack_platform_windows_joypad_x(session, 0) as f64 / (0x7FFF as f64);
                let y = hijack_platform_windows_joypad_y(session, 0) as f64 / (0x7FFF as f64);
                let buttons = hijack_platform_windows_joypad_buttons(session, 0);
                vec![brownfox::Input::new(
                    x,
                    y,
                    &(0..32).map(|i| (buttons >> i) & 1 == 1).collect(),
                )]
            } else {
                vec![brownfox::Input::empty()]
            };

            hijack = hijack.transit(&inputs);
        }

        hijack_platform_windows_session_free(session);
    }
    0
}
