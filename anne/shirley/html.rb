#!/usr/bin/ruby -Ianne/lib

require "shirley"
require "open3"

cfg = Shirley.config([
    ["first-paragraph", /\A@cat\s+(.*)\z/m, proc {|m| File.read(m[1].strip)}],
    ["first-paragraph", /\A#(\S+?)(?:\((.*?)\))?(?:\s+(.*))\z/m, "<\\1 \\2>\\3</\\1>"],
    ["first-paragraph", /\A(.*)\z/m, "<p>\\1</p>"],
    ["first-list", /\A@cat\s+(.*)\z/m, proc {|m| File.read(m[1].strip)}],
    ["first-list", /\A#(\S+?)(?:\((.*?)\))?(?:\s+(.*))?\z/m, "<\\1 \\2>\\3</\\1>"],
    ["first-$\n", /\A(.*?)\z/m, proc {|m| stdout, stderr, status = Open3.capture3("latexmlmath --pmml - - | tail -n +2", stdin_data: m[1]); STDERR.puts(stderr); stdout }],
    ["first-$", /\A(.*?)\z/m, proc {|m| stdout, stderr, status = Open3.capture3("latexmlmath --pmml - - | tail -n +2 | sed 's/display=\"block\"/display=\"inline\"/'", stdin_data: m[1]); STDERR.puts(stderr); stdout }],
    ["all-atom", /&/m, "&amp;"],
    ["all-atom", /</m, "&lt;"],
    ["all-atom", />/m, "&gt;"]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
