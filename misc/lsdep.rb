require "./lib/datafile"

ROUTE = File.join(File.expand_path(ENV["ROUTE"]), "/")

if ROUTE.count("/").odd?
  DATAFILE = Datafile.parse(File.read(File.join(".", ROUTE, "Datafile")))

  DATAFILE["nav"]&.text&.lines do |line|
    path = line.chomp
    puts File.join(".", ROUTE, path, "Datafile")
  end
else
  DATAFILE = Datafile.parse(File.read(File.join(".", File.dirname(ROUTE), "Datafile")))

  DATAFILE["nav"]&.text&.lines do |line|
    path = line.chomp
    puts File.join(".", File.dirname(ROUTE), path, "Datafile")
  end
end

