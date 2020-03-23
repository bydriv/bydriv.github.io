#!/usr/bin/ruby -I../../anne/lib
# coding: utf-8

require "shirley"

$chap = 0

cfg = Shirley.config([
    ["first-paragraph", /\A\\([（「『〈《])/m, "\\noindent\\\\\\1"],
    ["all-text", /((?:(?<!\d)(?:\d\d)(?!\d))|\d)/m, "\\makebox[1\\zw][l]{\\rensuji{\\1}}"],
    ["all-text", /！！(?=[）」』〉》])/m, "\\exclexcl{}"],
    ["all-text", /！？(?=[）」』〉》])/m, "\\exclques{}"],
    ["all-text", /！！(?![）」』〉》])/m, "\\exclexcl{}\\hspace{1\\zw}"],
    ["all-text", /！？(?![）」』〉》])/m, "\\exclques{}\\hspace{1\\zw}"],
    ["all-text", /！(?=[）」』〉》])/m, "\\！{}"],
    ["all-text", /？(?=[）」』〉》])/m, "\\？{}"],
    ["all-text", /(?<!\\)！(?![）」』〉》])/m, "\\！{}\\hspace{1\\zw}"],
    ["all-text", /(?<!\\)？(?![）」』〉》])/m, "\\？{}\\hspace{1\\zw}"],
    ["all-text", /([ぁぃぅぇぉっゃゅょァィゥェォッャュョ、。ー（）「」『』〈〉《》])/m, "\\\\\\1{}"],
    ["all-text", /(……)/m, "\\…{}"],
    ["all-text", /　/m, ""]
])

puts(Shirley.traverse(cfg, JSON.parse(ARGF.read)))
