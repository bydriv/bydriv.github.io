#!/usr/bin/ruby -Ianne/lib
# coding: utf-8

require "shirley"

$chap = 0

cfg = Shirley.config([
    ["first-paragraph", /\A\$(\d+):(.*?)\n\z/m, proc {|m| $chap = m[1]; "<h2 id=\"#{m[1]}\">#{m[2]}</h2>" }],
    ["first-paragraph", /\A\$(\d+)\n\z/m, proc {|m| $chap = m[1]; "<h2>第#{m[1]}章</h2>" }],
    ["first-paragraph", /\A\+\n\z/m, "<div class=\"blank-line\"></div>"],
    ["first-paragraph", /\A\*\n\z/m, "<div class=\"blank-line\"></div>"],
    ["first-paragraph", /\A(.*)\z/m, "<p>\\1</p>"],
    ["first-list", /\A(\d+)\z/m, proc {|m| "<sup id=\"#$chap:#{m[1]}\">#{m[1]}</sup>"}],
    ["first-list", /\A#ruby:([^:]+):([^:]+)\z/m, "<ruby>\\1<rp>(</rp><rt>\\2</rt><rp>)</rp></ruby>"]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
