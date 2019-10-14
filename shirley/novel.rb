#!/usr/bin/ruby -Ianne/lib
# coding: utf-8

require "shirley"

$chap = 0

cfg = Shirley.config([
    ["first-paragraph", /\A\$(\d+):(.*?)\z/m, proc {|m| $chap = m[1]; "<h2 class=\"chap\" id=\"#{m[1]}\"><a href=\"##$chap\">#{m[2]}</a></h2>" }],
    ["first-paragraph", /\A\$(\d+)\z/m, proc {|m| $chap = m[1]; "<h2 class=\"chap\" id=\"#{m[1]}\"><a href=\"##$chap\">第#{m[1]}章</a></h2>" }],
    ["first-paragraph", /\A\+\z/m, "<div class=\"blank-line\"></div>"],
    ["first-paragraph", /\A\*\z/m, "<div class=\"blank-line\"></div>"],
    ["first-paragraph", /\A((?:<sup[^>]*?><a[^>]*?>\d+<\/a><\/sup>)?<span [^>]*>)([（「『《].*)\z/m, "<p class=\"noindent\">\\1\\2</p>"],
    ["first-paragraph", /\A(.*)\z/m, "<p class=\"indent\">\\1</p>"],
    ["first-list", /\A(\d+)\z/m, proc {|m| "<sup class=\"sect\" id=\"#$chap:#{m[1]}\"><a href=\"##$chap:#{m[1]}\">#{m[1]}</a></sup>"}],
    ["first-list", /\A#fn:([^:]+):([^:]+):([^:]+)\z/m, "<sup class=\"sect\" id=\"\\1\">\\2</sup><span class=\"text\">\\3</span>"],
    ["first-list", /\A#ln:([^:]+):([^:]+)\z/m, "<sup><a href=\"#\\1\">\\2</a></sup>"],
    ["first-list", /\A#ruby:([^:]+):([^:]+)\z/m, "<ruby>\\1<rp>(</rp><rt>\\2</rt><rp>)</rp></ruby>"],
    ["first-list", /\A#pict:([^:]+)\z/m, "<img src=\"\\1\">"],
    ["all-text", /(！|？|！？)([^！？）」』》])/m, "\\1<span class=\"space\"></span>\\2"],
    ["all-text", /\n/m, ""],
    ["all-text", /\.\.\./m, "…"],
    ["all-text", /---/m, "―"],
    ["all-text", /(……)/m, "<span class=\"dots\">\\1</span>"],
    ["all-text", /(――)/m, "<span class=\"dashes\">\\1</span>"],
    ["first-text", /\A\$(\d+):(.*?)\z/m, "\\0"],
    ["first-text", /\A\$(\d+)\z/m, "\\0"],
    ["first-text", /\A\+\z/m, "\\0"],
    ["first-text", /\A\*\z/m, "\\0"],
    ["first-text", /\A\?.*\z/m, "\\0"],
    ["first-text", /\A\^.*\z/m, "\\0"],
    ["first-text", /\A(\d+)\z/m, "\\0"],
    ["first-text", /\A#fn:([^:]+):([^:]+):([^:]+)\z/m, "\\0"],
    ["first-text", /\A#ln:([^:]+):([^:]+)\z/m, "\\0"],
    ["first-text", /\A#ruby:([^:]+):([^:]+)\z/m, "\\0"],
    ["first-text", /\A#pict:([^:]+)\z/m, "\\0"],
    ["first-text", /\A(.+)\z/m, "<span class=\"text\">\\1</span>"]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
