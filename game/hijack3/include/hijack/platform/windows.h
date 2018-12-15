#ifndef HIJACK_PLATFORM_WINDOWS_H__
#define HIJACK_PLATFORM_WINDOWS_H__

#include <stdbool.h>
#include <stdint.h>
#include <windows.h>

typedef struct hijack_platform_windows_session hijack_platform_windows_session_t;
typedef struct hijack_platform_windows_image hijack_platform_windows_image_t;
typedef struct hijack_platform_windows_directory hijack_platform_windows_directory_t;

hijack_platform_windows_session_t *
hijack_platform_windows_session_alloc(
  const char *,
  bool,
  bool,
  uint64_t,
  uint64_t,
  uint64_t);

void
hijack_platform_windows_session_free(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_step(
  hijack_platform_windows_session_t *);

void
hijack_platform_windows_window_show(
  hijack_platform_windows_session_t *);

void
hijack_platform_windows_window_hide(
  hijack_platform_windows_session_t *);

uint64_t
hijack_platform_windows_window_width(
  hijack_platform_windows_session_t *);

uint64_t
hijack_platform_windows_window_height(
  hijack_platform_windows_session_t *);

double
hijack_platform_windows_window_fps(
  hijack_platform_windows_session_t *);

void
hijack_platform_windows_window_flush(
  hijack_platform_windows_session_t *);

void
hijack_platform_windows_window_fail(
  hijack_platform_windows_session_t *,
  const char *);

hijack_platform_windows_image_t *
hijack_platform_windows_image_alloc(
  hijack_platform_windows_session_t *,
  uint64_t,
  uint64_t,
  const uint8_t *);

void
hijack_platform_windows_image_free(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_image_t *);

hijack_platform_windows_image_t *
hijack_platform_windows_image_load(
  hijack_platform_windows_session_t *,
  uint64_t,
  const uint8_t *);

uint64_t
hijack_platform_windows_image_width(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_image_t *);

uint64_t
hijack_platform_windows_image_height(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_image_t *);

hijack_platform_windows_image_t *
hijack_platform_windows_image_scale(
  hijack_platform_windows_session_t *,
  uint64_t,
  hijack_platform_windows_image_t *);

hijack_platform_windows_image_t *
hijack_platform_windows_image_crop(
  hijack_platform_windows_session_t *,
  uint64_t,
  uint64_t,
  uint64_t,
  uint64_t,
  hijack_platform_windows_image_t *);

void
hijack_platform_windows_image_draw(
  hijack_platform_windows_session_t *,
  int64_t,
  int64_t,
  hijack_platform_windows_image_t *);

bool
hijack_platform_windows_keyboard_is_pressed(
  hijack_platform_windows_session_t *,
  char);

bool
hijack_platform_windows_keyboard_enter_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_lshift_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_rshift_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_lctrl_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_rctrl_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_lalt_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_ralt_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_left_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_up_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_right_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_keyboard_down_is_pressed(
  hijack_platform_windows_session_t *);

bool
hijack_platform_windows_numpad_is_pressed(
  hijack_platform_windows_session_t *,
  uint8_t);

bool
hijack_platform_winodws_joypad_exists(
  hijack_platform_windows_session_t *,
  uint64_t);

int64_t
hijack_platform_windows_jopyad_x(
  hijack_platform_windows_session_t *,
  uint64_t);

int64_t
hijack_platform_windows_jopyad_y(
  hijack_platform_windows_session_t *,
  uint64_t);

int64_t
hijack_platform_windows_jopyad_z(
  hijack_platform_windows_session_t *,
  uint64_t);

int64_t
hijack_platform_windows_jopyad_r(
  hijack_platform_windows_session_t *,
  uint64_t);

int64_t
hijack_platform_windows_jopyad_u(
  hijack_platform_windows_session_t *,
  uint64_t);

int64_t
hijack_platform_windows_jopyad_v(
  hijack_platform_windows_session_t *,
  uint64_t);

uint64_t
hijack_platform_windows_jopyad_buttons(
  hijack_platform_windows_session_t *,
  uint64_t);

uint64_t
hijack_platform_windows_jopyad_button_number(
  hijack_platform_windows_session_t *,
  uint64_t);

uint64_t
hijack_platform_windows_jopyad_pov(
  hijack_platform_windows_session_t *,
  uint64_t);

hijack_platform_windows_directory_t *
hijack_platform_windows_directory_root(
  hijack_platform_windows_session_t *);

hijack_platform_windows_directory_t *
hijack_platform_windows_directory_mkdir(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_directory_t *,
  const char *);

void
hijack_platform_windows_directory_write(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_directory_t *,
  const char *,
  uint64_t,
  const uint8_t *);

void
hijack_platform_windows_directory_read(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_directory_t *,
  const char *,
  uint64_t *,
  uint8_t **);

void
hijack_platform_windows_directory_free(
  hijack_platform_windows_session_t *,
  hijack_platform_windows_directory_t *);

#endif
