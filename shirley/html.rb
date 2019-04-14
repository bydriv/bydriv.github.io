#!/usr/bin/ruby -Isrc
# coding: utf-8

require "shirley"

cfg = Shirley.config([
    ["paragraph", /\A#(\S+)\s*(.*)\z/m, "<\\1>\\2</\\1>"],
    ["paragraph", /\A(.*)\z/m, "<p>\\1</p>"],
    ["inline", /\A#(\S+)\s*(.*)\z/m, "<\\1>\\2</\\1>"],
    ["atom", /\A\\#/m, "&#x23;"],
    ["atom", /&/m, "&amp;"],
    ["atom", /\\/m, "&bsol;"],
    ["atom", /</m, "&lt;"],
    ["atom", />/m, "&gt;"]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
