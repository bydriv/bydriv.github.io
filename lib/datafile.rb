class Datafile
  attr_accessor :data

  def initialize(data = [])
    @data = data
  end

  def [](index)
    case index
    when Integer
      @data[index]
    when String
      @data.find do |datum|
        datum.path == index
      end
    end
  end
end

class Datafile::Datum
  attr_accessor :path
  attr_accessor :text

  def initialize(path = nil, text = nil)
    @path = path
    @text = text
  end
end

class Datafile::UnterminatedHeredocError < StandardError; end

class Datafile
  def self.parse(text)
    parse_stream(text.lines)
  end

  def self.parse_stream(lines)
    data = []
    datum = nil
    delimiter = nil
    delimiter_line = nil

    lines.each do |line|
      if delimiter.nil?
        case line
        when /^@(\S+)$/
          data << datum unless datum.nil?
          datum = Datum.new($1, nil)
        when /^@(\S+)\s+(\S*)$/
          data << datum unless datum.nil?
          datum = Datum.new($1, "")
          delimiter = $2
          delimiter_line = line.chomp
        when /^$/
          data << datum unless datum.nil?
          datum = nil
        else
          datum = Datum.new if datum.nil?
          datum.text = "" if datum.text.nil?
          datum.text << line
        end
      elsif line.chomp == delimiter
        data << datum unless datum.nil?
        datum = nil
        delimiter = nil
        delimiter_line = nil
      else
        datum = Datum.new if datum.nil?
        datum.text = "" if datum.text.nil?
        datum.text << line
      end
    end

    raise UnterminatedHeredocError.new("unterminated heredoc: #{delimiter_line}") unless delimiter.nil?

    data << datum unless datum.nil?

    Datafile.new(data)
  end
end
