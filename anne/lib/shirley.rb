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

  def select_rules(cfg, ts, s, &f)
    rules = cfg.select do |rule|
      ts.any? do |t|
        rule[:type] == "all-#{t}"
      end
    end

    rule = cfg.find do |rule|
      ts.any? do |t|
        rule[:type] == "first-#{t}" && s =~ rule[:regexp]
      end
    end

    rules << rule if rule

    rules
  end

  def traverse(cfg, json)
    case json["type"]
    when "blank"
      s = json["value"]
      rules = select_rules(cfg, [json["type"]], s)
    when "text", "raw1", "rawn"
      s = json["value"]
      rules = select_rules(cfg, ["atom", json["type"]], s)
    when "anne", "paragraph", "list"
      s = json["value"].map do |child|
        traverse(cfg, child)
      end.join("")

      rules = select_rules(cfg, [json["type"]], s)
    end

    rules.each do |rule|
      s = fire(rule, s)
    end

    s
  end
end
