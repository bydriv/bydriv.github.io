require "json"

module Shirley
  extend self

  def config(cfg)
    cfg.map do |rule|
      {
        type: rule[0],
        regexp: rule[1],
        replace: rule[2]
      }
    end
  end

  def fire(rule, s)
    return s unless rule

    case rule[:replace]
    when String
      s.gsub(rule[:regexp], rule[:replace])
    when Proc
      s.gsub(rule[:regexp], &rule[:replace])
    end
  end

  def traverse_inline(cfg, json)
    case json["type"]
    when "text", "raw1", "rawn"
      s = json["value"]

      rules = cfg.select { |rule| rule[:type] == "atom" && s =~ rule[:regexp] }

      rules.each do |rule|
        s = fire(rule, s)
      end

      s
    when "list"
      s = json["value"].map do |child|
        traverse_inline(cfg, child)
      end.join("")

      rule = cfg.find { |rule| rule[:type] == "inline" && s =~ rule[:regexp] }

      fire(rule, s)
    end
  end

  def traverse_paragraph(cfg, json)
    case json["type"]
    when "blank"
      json["value"]
    when "data"
      s = json["value"].map do |child|
        traverse_inline(cfg, child)
      end.join("")

      rule = cfg.find { |rule| rule[:type] == "paragraph" && s =~ rule[:regexp] }

      fire(rule, s)
    end
  end

  def traverse(cfg, json)
    case json["type"]
    when "anne"
      s = json["value"].map do |child|
        traverse_paragraph(cfg, child)
      end.join("")

      rule = cfg.find { |rule| rule[:type] == "document" && s =~ rule[:regexp] }

      fire(rule, s)
    end
  end
end
