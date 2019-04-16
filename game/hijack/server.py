# -*- coding: utf-8 -*-

import http.server
import socketserver

http.server.SimpleHTTPRequestHandler.extensions_map[".wasm"] = "application/wasm"

socketserver.TCPServer(("", 8000), http.server.SimpleHTTPRequestHandler).serve_forever()
