#!/usr/bin/ruby -Isrc
# coding: utf-8

require "shirley"

cfg = Shirley.config([
    ["paragraph", /\A\$(\d+):(.*)\z/m, "<h2>\\2</h2>"],
    ["paragraph", /\A\$(\d+)\z/m, "<h2>第\\1章</h2>"],
    ["paragraph", /\A\+\z/m, "<div class=\"blank-line\"></div>"],
    ["paragraph", /\A\*\z/m, "<div class=\"blank-line\"></div>"],
    ["paragraph", /\A(.*)\z/m, "<p>\\1</p>"],
    ["inline", /\A(\d+)\z/m, "<sup>\\1</sup>"],
    ["inline", /\A#ruby:([^:]+):([^:]+)\z/m, "<ruby>\\1<rp>(</rp><rt>\\2</rt><rp>)</rp></ruby>"]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
