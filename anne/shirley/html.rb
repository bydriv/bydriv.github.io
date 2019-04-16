#!/usr/bin/ruby -Ianne/lib

require "shirley"

cfg = Shirley.config([
    ["first-paragraph", /\A#(\S+)\s*(.*)\z/m, "<\\1>\\2</\\1>"],
    ["first-paragraph", /\A(.*)\z/m, "<p>\\1</p>"],
    ["first-list", /\A#(\S+)\s*(.*)\z/m, "<\\1>\\2</\\1>"],
    ["all-atom", /&/m, "&amp;"],
    ["all-atom", /\\/m, "&bsol;"],
    ["all-atom", /</m, "&lt;"],
    ["all-atom", />/m, "&gt;"]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
