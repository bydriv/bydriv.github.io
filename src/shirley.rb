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
    case json
    when String
      rules = cfg.select { |rule| rule[:type] == "atom" && json =~ rule[:regexp] }

      rules.each do |rule|
        json = fire(rule, json)
      end

      json
    when Array
      s = json.map do |child|
        traverse_inline(cfg, child)
      end.join("")

      rule = cfg.find { |rule| rule[:type] == "inline" && s =~ rule[:regexp] }

      fire(rule, s)
    end
  end

  def traverse_paragraph(cfg, json)
    s = json.map do |child|
      traverse_inline(cfg, child)
    end.join("")

    rule = cfg.find { |rule| rule[:type] == "paragraph" && s =~ rule[:regexp] }

    fire(rule, s)
  end

  def traverse(cfg, json)
    s = json.map do |child|
      traverse_paragraph(cfg, child)
    end.join("")

    rule = cfg.find { |rule| rule[:type] == "document" && s =~ rule[:regexp] }

    fire(rule, s)
  end
end
