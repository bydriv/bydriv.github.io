#include <GL/gl.h>
#include <gdiplus.h>
#include <hijack/platform/windows.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

#define WM_SET_SESSION (WM_USER)

struct hijack_platform_windows_session
{
  char *name;
  bool canFullScreen;
  bool isFullScreen;
  uint64_t clientWidth;
  uint64_t clientHeight;
  uint64_t windowWidth;
  uint64_t windowHeight;
  uint64_t windowLeft;
  uint64_t windowTop;
  uint64_t fps;
  DWORD sec;
  uint64_t frames;
  double realFPS;
  DWORD ms;
  bool failed;
  DWORD lastControl;
  uint8_t *pixels;

  HINSTANCE hInstance;
  WNDCLASS wndClass;
  HWND hwnd;
  HDC hdc;
  MSG msg;

  PIXELFORMATDESCRIPTOR pfd;
  HGLRC hglrc;

  GdiplusStartupInput gdiplusStartupInput;
  ULONG_PTR gdiplusToken;

  struct
  {
    uint64_t length;
    JOYINFOEX *neutrals;
    JOYINFOEX *array;
  } joypads;
};

struct hijack_platform_windows_image
{
  uint64_t width;
  uint64_t height;
  uint8_t *pixels;
};

LRESULT CALLBACK
hijack_platform_windows_wndproc(
  HWND hwnd,
  UINT  uMsg,
  WPARAM wParam,
  LPARAM lParam)
{
  static hijack_platform_windows_session_t *session = NULL;

  switch (uMsg)
    {
    case WM_DESTROY:
      PostQuitMessage(0);
      return 0;
    case WM_SET_SESSION:
      session = (hijack_platform_windows_session_t *) wParam;
      return 0;
    case WM_MENUCHAR:
      if ((wParam & 0xFFFF) == VK_RETURN)
        return MNC_CLOSE << 16;
      else
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    case WM_MOVE:
      if (!session || !session->pixels || session->isFullScreen)
        return DefWindowProc(hwnd, uMsg, wParam, lParam);

      glDrawBuffer(GL_FRONT);
      glDrawPixels(session->clientWidth, session->clientHeight, GL_RGBA, GL_UNSIGNED_BYTE, session->pixels);
      glFlush();

      return DefWindowProc(hwnd, uMsg, wParam, lParam);
    default:
      return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
}

hijack_platform_windows_session_t *
hijack_platform_windows_session_alloc(
  const char *name,
  bool canFullScreen,
  bool isFullScreen,
  uint64_t clientWidth,
  uint64_t clientHeight,
  uint64_t fps)
{
  hijack_platform_windows_session_t *session =
    calloc(1, sizeof(hijack_platform_windows_session_t));

  session->name = calloc(strlen(name) + 1, sizeof(char));
  strcpy(session->name, name);

  session->canFullScreen = canFullScreen;
  session->isFullScreen = canFullScreen && isFullScreen;

  RECT desktopRect;
  SystemParametersInfo(SPI_GETWORKAREA, 0, &desktopRect, 0);

  RECT windowRect = {0, 0, clientWidth, clientHeight};
  AdjustWindowRect(&windowRect, WS_OVERLAPPED | WS_SYSMENU | WS_CAPTION | WS_MINIMIZEBOX, FALSE);

  session->clientWidth = clientWidth;
  session->clientHeight = clientHeight;
  session->windowWidth = windowRect.right - windowRect.left;
  session->windowHeight = windowRect.bottom - windowRect.top;
  session->windowLeft = ((desktopRect.right - desktopRect.left) - session->windowWidth) / 2;
  session->windowTop = ((desktopRect.bottom - desktopRect.top) - session->windowHeight) / 2;

  if (session->isFullScreen)
    session->pixels = calloc(GetSystemMetrics(SM_CXSCREEN) * GetSystemMetrics(SM_CYSCREEN) * 4, sizeof(uint8_t));
  else
    session->pixels = calloc(session->clientWidth * session->clientHeight * 4, sizeof(uint8_t));

  session->fps = fps;
  session->realFPS = fps;

  session->hInstance = GetModuleHandle(NULL);

  session->wndClass.style = CS_HREDRAW | CS_VREDRAW;
  session->wndClass.lpfnWndProc = hijack_platform_windows_wndproc;
  session->wndClass.cbClsExtra = 0;
  session->wndClass.cbWndExtra = 0;
  session->wndClass.hInstance = session->hInstance;
  session->wndClass.hIcon = LoadIcon(NULL, IDI_APPLICATION);
  session->wndClass.hCursor = LoadCursor(NULL, IDC_ARROW);
  session->wndClass.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
  session->wndClass.lpszMenuName = NULL;
  session->wndClass.lpszClassName = TEXT(session->name);

  if(!RegisterClass(&session->wndClass))
    return NULL;

  if (session->isFullScreen)
    session->hwnd =
      CreateWindow(
        TEXT(session->name),
        TEXT(session->name),
        WS_POPUP,
        0,
        0,
        GetSystemMetrics(SM_CXSCREEN),
        GetSystemMetrics(SM_CYSCREEN),
        NULL,
        NULL,
        session->hInstance,
        NULL);
  else
    session->hwnd =
      CreateWindow(
        TEXT(session->name),
        TEXT(session->name),
        WS_OVERLAPPED | WS_SYSMENU | WS_CAPTION | WS_MINIMIZEBOX,
        session->windowLeft,
        session->windowTop,
        session->windowWidth,
        session->windowHeight,
        NULL,
        NULL,
        session->hInstance,
        NULL);

  if (!session->hwnd)
    return NULL;

  session->hdc = GetDC(session->hwnd);

  session->pfd.nSize = sizeof(PIXELFORMATDESCRIPTOR);
  session->pfd.nVersion = 1;
  session->pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
  session->pfd.iPixelType = PFD_TYPE_RGBA;
  session->pfd.cColorBits = 24;
  session->pfd.cDepthBits = 24;
  session->pfd.cStencilBits = 8;
  session->pfd.iLayerType = PFD_MAIN_PLANE;

  int format = ChoosePixelFormat(session->hdc, &session->pfd);

  if (format == 0)
    return NULL;

  if (!SetPixelFormat(session->hdc, format, &session->pfd))
    return NULL;

  session->hglrc = wglCreateContext(session->hdc);

  wglMakeCurrent(session->hdc, session->hglrc);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  if (session->isFullScreen)
    {
      glViewport(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glOrtho(0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN), 0, -1, 1);
      glMatrixMode(GL_MODELVIEW);
    }
  else
    {
      glViewport(0, 0, session->clientWidth, session->clientHeight);
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glOrtho(0, session->clientWidth, session->clientHeight, 0, -1, 1);
      glMatrixMode(GL_MODELVIEW);
    }

  session->gdiplusStartupInput.GdiplusVersion = 1;
  session->gdiplusStartupInput.DebugEventCallback = NULL;
  session->gdiplusStartupInput.SuppressBackgroundThread = FALSE;
  session->gdiplusStartupInput.SuppressExternalCodecs = FALSE;

  GdiplusStartup(&session->gdiplusToken, &session->gdiplusStartupInput, NULL);

  session->joypads.length = joyGetNumDevs();
  session->joypads.neutrals = calloc(session->joypads.length, sizeof(JOYINFOEX));
  session->joypads.array = calloc(session->joypads.length, sizeof(JOYINFOEX));
  timeBeginPeriod(1);
  session->ms = timeGetTime();
  session->sec = session->ms;

  PostMessage(session->hwnd, WM_SET_SESSION, (WPARAM) session, 0);
  return session;
}

void
hijack_platform_windows_session_free(
  hijack_platform_windows_session_t *session)
{
  timeEndPeriod(1);

  GdiplusShutdown(session->gdiplusToken);
  wglMakeCurrent(NULL, NULL);
  wglDeleteContext(session->hglrc);
  ReleaseDC(session->hwnd, session->hdc);
  DestroyWindow(session->hwnd);
  UnregisterClass(TEXT(session->name), session->hInstance);
  free(session->name);
  free(session);
}

bool
hijack_platform_windows_step(
  hijack_platform_windows_session_t *session)
{
  DWORD now = timeGetTime();

  while (PeekMessage(&session->msg, NULL, 0, 0, PM_REMOVE))
    {
      if (session->msg.message == WM_QUIT)
        return false;

      TranslateMessage(&session->msg);
      DispatchMessage(&session->msg);
    }

  if (session->canFullScreen
        && (now - session->lastControl) > 200
        && GetKeyState(VK_RETURN) < 0 && GetKeyState(VK_MENU) < 0)
    {
      uint8_t *pixels = session->pixels;
      session->pixels = NULL;
      free(pixels);

      if (session->isFullScreen)
        {
          session->pixels = calloc(session->clientWidth * session->clientHeight * 4, sizeof(uint8_t));

          SetWindowLong(session->hwnd, GWL_STYLE, WS_OVERLAPPED | WS_SYSMENU | WS_CAPTION | WS_MINIMIZEBOX);

          SetWindowPos(
            session->hwnd,
            NULL,
            session->windowLeft,
            session->windowTop,
            session->windowWidth,
            session->windowHeight,
            SWP_NOZORDER | SWP_NOOWNERZORDER);

          glViewport(0, 0, session->clientWidth, session->clientHeight);
          glMatrixMode(GL_PROJECTION);
          glLoadIdentity();
          glOrtho(0, session->clientWidth, session->clientHeight, 0, -1, 1);
          glMatrixMode(GL_MODELVIEW);

          ShowWindow(session->hwnd, SW_SHOW);
        }
      else
        {
          session->pixels = calloc(GetSystemMetrics(SM_CXSCREEN) * GetSystemMetrics(SM_CYSCREEN) * 4, sizeof(uint8_t));

          RECT windowRect;
          GetWindowRect(session->hwnd, &windowRect);
          session->windowLeft = windowRect.left;
          session->windowTop = windowRect.top;

          SetWindowLong(session->hwnd, GWL_STYLE, WS_POPUP);

          SetWindowPos(
            session->hwnd,
            NULL,
            0,
            0,
            GetSystemMetrics(SM_CXSCREEN),
            GetSystemMetrics(SM_CYSCREEN),
            SWP_NOZORDER | SWP_NOOWNERZORDER);

          glViewport(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
          glMatrixMode(GL_PROJECTION);
          glLoadIdentity();
          glOrtho(0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN), 0, -1, 1);
          glMatrixMode(GL_MODELVIEW);

          ShowWindow(session->hwnd, SW_SHOW);
        }

      session->isFullScreen = !session->isFullScreen;
      session->lastControl = now;
    }

  for (uint64_t i = 0; i < session->joypads.length; ++i)
    session->joypads.array[i].dwSize = 0;

  DWORD elapsed = now - session->ms;

  double sec = 1000.0 / session->fps;

  double loss =
    session->frames == 0
    ? 0.0
    : ((((double) (now - session->sec)) / session->frames) - sec) * session->frames;

  if (((double) elapsed) + loss < sec) {
    DWORD ms = sec - elapsed - loss;

    if (ms > 0)
      Sleep(ms);
  }

  ++session->frames;

  if (now - session->sec >= 1000) {
    session->sec = now;
    session->realFPS = session->frames;
    session->frames = 0;
  }

  session->ms = now;

  return !session->failed;
}

void
hijack_platform_windows_window_show(
  hijack_platform_windows_session_t *session)
{
  ShowWindow(session->hwnd, SW_SHOW);
}

void
hijack_platform_windows_window_hide(
  hijack_platform_windows_session_t *session)
{
  ShowWindow(session->hwnd, SW_HIDE);
}

uint64_t
hijack_platform_windows_window_width(
  hijack_platform_windows_session_t *session)
{
  if (session->isFullScreen)
    return GetSystemMetrics(SM_CXSCREEN);
  else
    return session->clientWidth;
}

uint64_t
hijack_platform_windows_window_height(
  hijack_platform_windows_session_t *session)
{
  if (session->isFullScreen)
    return GetSystemMetrics(SM_CYSCREEN);
  else
    return session->clientHeight;
}

double
hijack_platform_windows_window_fps(
  hijack_platform_windows_session_t *session)
{
  return session->realFPS;
}

void
hijack_platform_windows_window_flush(
  hijack_platform_windows_session_t *session)
{
  SwapBuffers(session->hdc);

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);
}

void
hijack_platform_windows_window_fail(
  hijack_platform_windows_session_t *session,
  const char *msg)
{
  session->failed = true;
  MessageBeep(MB_ICONHAND);
  MessageBox(session->hwnd, msg, session->name, MB_ICONHAND);
}

hijack_platform_windows_image_t *
hijack_platform_windows_image_alloc(
  hijack_platform_windows_session_t *session,
  uint64_t width,
  uint64_t height,
  const uint8_t *pixels)
{
  hijack_platform_windows_image_t *image =
    calloc(1U, sizeof(hijack_platform_windows_image_t));

  image->width = width;
  image->height = height;
  image->pixels = calloc(width * height * 4, sizeof(uint8_t));
  memcpy(image->pixels, pixels, width * height * 4);

  return image;
}

hijack_platform_windows_image_t *
hijack_platform_windows_image_load(
  hijack_platform_windows_session_t *session,
  uint64_t len,
  const uint8_t *data)
{
  HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE, len);

  if (!hGlobal)
    return NULL;

  HGLOBAL tmpData = GlobalLock(hGlobal);

  if (!tmpData)
    {
      GlobalFree(hGlobal);
      return NULL;
    }

  CopyMemory(tmpData, data, len);
  GlobalUnlock(hGlobal);

  LPSTREAM stream;

  if (FAILED(CreateStreamOnHGlobal(hGlobal, TRUE, &stream)))
    {
      GlobalFree(hGlobal);
      return NULL;
    }

  GpBitmap *bitmap;

  GpStatus status = GdipCreateBitmapFromStream(stream, &bitmap);
  stream->lpVtbl->Release(stream);
  GlobalFree(hGlobal);

  if (status)
    {
      GdipDisposeImage(bitmap);
      return NULL;
    }

  UINT width, height;
  GdipGetImageWidth(bitmap, &width);
  GdipGetImageHeight(bitmap, &height);

  hijack_platform_windows_image_t *image =
    calloc(1U, sizeof(hijack_platform_windows_image_t));

  image->width = width;
  image->height = height;
  image->pixels = calloc(width * height * 4, sizeof(uint8_t));

  for (UINT i = 0; i < width; ++i)
    for (UINT j = 0; j < height; ++j)
      {
        ARGB argb;
        GdipBitmapGetPixel(bitmap, i, j, &argb);
        image->pixels[(i + (height - j - 1) * width) * 4] = argb >> 16;
        image->pixels[(i + (height - j - 1) * width) * 4 + 1] = argb >> 8;
        image->pixels[(i + (height - j - 1) * width) * 4 + 2] = argb;
        image->pixels[(i + (height - j - 1) * width) * 4 + 3] = argb >> 24;
      }

  GdipDisposeImage(bitmap);

  return image;
}

void
hijack_platform_windows_image_free(
  hijack_platform_windows_session_t *session,
  hijack_platform_windows_image_t *image)
{
  free(image->pixels);
  free(image);
}

uint64_t
hijack_platform_windows_image_width(
  hijack_platform_windows_session_t *session,
  hijack_platform_windows_image_t *image)
{
  return image->width;
}

uint64_t
hijack_platform_windows_image_height(
  hijack_platform_windows_session_t *session,
  hijack_platform_windows_image_t *image)
{
  return image->height;
}

hijack_platform_windows_image_t *
hijack_platform_windows_image_scale(
  hijack_platform_windows_session_t *session,
  uint64_t scale,
  hijack_platform_windows_image_t *image)
{
  if (scale == 0)
    return NULL;

  hijack_platform_windows_image_t *newimage =
    calloc(1U, sizeof(hijack_platform_windows_image_t));

  newimage->width = image->width * scale;
  newimage->height = image->height * scale;
  newimage->pixels = calloc(newimage->width * newimage->height * 4, sizeof(uint8_t));

  for (uint64_t i = 0; i < image->width; ++i)
    for (uint64_t j = 0; j < image->height; ++j)
      for (uint64_t k = 0; k < scale; ++k)
        for (uint64_t l = 0; l < scale; ++l)
          {
            newimage->pixels[(i * scale + k + (newimage->height - j * scale - l - 1) * newimage->width) * 4] =
              image->pixels[(i + (image->height - j - 1) * image->width) * 4];
            newimage->pixels[(i * scale + k + (newimage->height - j * scale - l - 1) * newimage->width) * 4 + 1] =
              image->pixels[(i + (image->height - j - 1) * image->width) * 4 + 1];
            newimage->pixels[(i * scale + k + (newimage->height - j * scale - l - 1) * newimage->width) * 4 + 2] =
              image->pixels[(i + (image->height - j - 1) * image->width) * 4 + 2];
            newimage->pixels[(i * scale + k + (newimage->height - j * scale - l - 1) * newimage->width) * 4 + 3] =
              image->pixels[(i + (image->height - j - 1) * image->width) * 4 + 3];
          }

  return newimage;
}

hijack_platform_windows_image_t *
hijack_platform_windows_image_crop(
  hijack_platform_windows_session_t *session,
  uint64_t x,
  uint64_t y,
  uint64_t width,
  uint64_t height,
  hijack_platform_windows_image_t *image)
{
  if (image->width < x + width || image->height < y + height)
    return NULL;

  hijack_platform_windows_image_t *newimage =
    calloc(1U, sizeof(hijack_platform_windows_image_t));

  newimage->width = width;
  newimage->height = height;
  newimage->pixels = calloc(width * height * 4, sizeof(uint8_t));

  for (uint64_t i = 0; i < width; ++i)
    for (uint64_t j = 0; j < height; ++j)
      {
        newimage->pixels[(i + (height - j - 1) * width) * 4] =
          image->pixels[((x + i) + (image->height - (y + j) - 1) * image->width) * 4];
        newimage->pixels[(i + (height - j - 1) * width) * 4 + 1] =
          image->pixels[((x + i) + (image->height - (y + j) - 1) * image->width) * 4 + 1];
        newimage->pixels[(i + (height - j - 1) * width) * 4 + 2] =
          image->pixels[((x + i) + (image->height - (y + j) - 1) * image->width) * 4 + 2];
        newimage->pixels[(i + (height - j - 1) * width) * 4 + 3] =
          image->pixels[((x + i) + (image->height - (y + j) - 1) * image->width) * 4 + 3];
      }

  return newimage;
}

void
hijack_platform_windows_image_draw(
  hijack_platform_windows_session_t *session,
  int64_t x,
  int64_t y,
  hijack_platform_windows_image_t *image)
{
  if (x < 0 && image->width <= -x)
    return;

  uint64_t height = hijack_platform_windows_window_height(session);

  if (y >= 0 && y >= height)
    return;

  bool isNeg = x < 0 || (y >= 0 && image->height + y > height);

  if (x < 0 && (y >= 0 && image->height + y > height))
    {
      image = hijack_platform_windows_image_crop(session, -x, 0, image->width - (-x), image->height - (image->height + y - height), image);
      x = 0;
    }
  else if (x < 0)
    {
      image = hijack_platform_windows_image_crop(session, -x, 0, image->width - (-x), image->height, image);
      x = 0;
    }
  else if (y >= 0 && image->height + y > height)
    image = hijack_platform_windows_image_crop(session, 0, 0, image->width, image->height - (image->height + y - height), image);

  glDrawBuffer(GL_BACK);

  glRasterPos2i(
    x,
    image->height + y);

  glDrawPixels(image->width, image->height, GL_RGBA, GL_UNSIGNED_BYTE, image->pixels);

  if (isNeg)
    hijack_platform_windows_image_free(session, image);
}

bool
hijack_platform_windows_keyboard_is_pressed(
  hijack_platform_windows_session_t *session,
  char c)
{
  return GetKeyState(c) < 0;
}

bool
hijack_platform_windows_keyboard_enter_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_RETURN) < 0;
}

bool
hijack_platform_windows_keyboard_lshift_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_LSHIFT) < 0;
}

bool
hijack_platform_windows_keyboard_rshift_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_RSHIFT) < 0;
}

bool
hijack_platform_windows_keyboard_lctrl_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_LCONTROL) < 0;
}

bool
hijack_platform_windows_keyboard_rctrl_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_RCONTROL) < 0;
}

bool
hijack_platform_windows_keyboard_lalt_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_LMENU) < 0;
}

bool
hijack_platform_windows_keyboard_ralt_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_RMENU) < 0;
}

bool
hijack_platform_windows_keyboard_left_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_LEFT) < 0;
}

bool
hijack_platform_windows_keyboard_up_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_UP) < 0;
}

bool
hijack_platform_windows_keyboard_right_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_RIGHT) < 0;
}

bool
hijack_platform_windows_keyboard_down_is_pressed(
  hijack_platform_windows_session_t *session)
{
  return GetKeyState(VK_DOWN) < 0;
}

bool
hijack_platform_windows_numpad_is_pressed(
  hijack_platform_windows_session_t *session,
  uint8_t i)
{
  switch (i)
    {
    case 0:
      return GetKeyState(VK_NUMPAD0) < 0;
    case 1:
      return GetKeyState(VK_NUMPAD1) < 0;
    case 2:
      return GetKeyState(VK_NUMPAD2) < 0;
    case 3:
      return GetKeyState(VK_NUMPAD3) < 0;
    case 4:
      return GetKeyState(VK_NUMPAD4) < 0;
    case 5:
      return GetKeyState(VK_NUMPAD5) < 0;
    case 6:
      return GetKeyState(VK_NUMPAD6) < 0;
    case 7:
      return GetKeyState(VK_NUMPAD7) < 0;
    case 8:
      return GetKeyState(VK_NUMPAD8) < 0;
    case 9:
      return GetKeyState(VK_NUMPAD9) < 0;
    default:
      return false;
    }
}

bool
hijack_platform_windows_joypad_exists(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i >= session->joypads.length)
    return false;

  JOYINFOEX joypad;
  joypad.dwSize = sizeof(JOYINFOEX);
  joypad.dwFlags = JOY_RETURNALL;

  MMRESULT result = joyGetPosEx(i, &joypad);

  if (result != JOYERR_NOERROR)
    {
      session->joypads.neutrals[i].dwSize = 0;
      return false;
    }

  if (session->joypads.neutrals[i].dwSize == 0)
    session->joypads.neutrals[i] = joypad;

  if (session->joypads.array[i].dwSize == 0)
    session->joypads.array[i] = joypad;

  return true;
}

int64_t
hijack_platform_windows_joypad_x(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return ((int64_t) session->joypads.array[i].dwXpos) - ((int64_t) session->joypads.neutrals[i].dwXpos);
  else
    return 0;
}

int64_t
hijack_platform_windows_joypad_y(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return ((int64_t) session->joypads.array[i].dwYpos) - ((int64_t) session->joypads.neutrals[i].dwYpos);
  else
    return 0;
}

int64_t
hijack_platform_windows_joypad_z(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return ((int64_t) session->joypads.array[i].dwZpos) - ((int64_t) session->joypads.neutrals[i].dwZpos);
  else
    return 0;
}

int64_t
hijack_platform_windows_joypad_r(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return ((int64_t) session->joypads.array[i].dwUpos) - ((int64_t) session->joypads.neutrals[i].dwUpos);
  else
    return 0;
}

int64_t
hijack_platform_windows_joypad_u(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return ((int64_t) session->joypads.array[i].dwRpos) - ((int64_t) session->joypads.neutrals[i].dwRpos);
  else
    return 0;
}

uint64_t
hijack_platform_windows_joypad_v(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return ((int64_t) session->joypads.array[i].dwVpos) - ((int64_t) session->joypads.neutrals[i].dwVpos);
  else
    return 0;
}

uint64_t
hijack_platform_windows_joypad_buttons(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return session->joypads.array[i].dwButtons;
  else
    return 0;
}

uint64_t
hijack_platform_windows_joypad_button_number(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return session->joypads.array[i].dwButtonNumber;
  else
    return 0;
}

uint64_t
hijack_platform_windows_joypad_pov(
  hijack_platform_windows_session_t *session,
  uint64_t i)
{
  if (i < session->joypads.length && session->joypads.neutrals[i].dwSize > 0)
    return session->joypads.array[i].dwPOV;
  else
    return 0;
}
