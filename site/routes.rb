require "json"
require "./lib/datafile"

ROUTES = ENV["ROUTES"].split(/\s+/).map {|route| File.join(File.expand_path(route), "/") }

routes = ROUTES.map do |route|
  datafile = Datafile.parse(File.read(File.join(".", route, "Datafile")))

  {
    "route" => route,
    "title" => datafile["title"]&.text&.chomp || route,
    "class" => datafile["class"]&.text&.chomp || "vertical component"
  }
end

puts(JSON.dump(routes))
