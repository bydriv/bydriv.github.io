my $chap = 0;
my $par = 1;

print "<div id=\"index\">\n";
print "<div id=\"default-index\" class=\"default index\"><a href=\"#home\">HOME</a></div>\n";

foreach my $line (<>) {
    if ($line =~ s/\[(\d+)\].*/<div id="$chap:\1-index" class="index">$chap:\1<\/div>/) {
    }

    if ($line =~ /\[\$(.*?)\]/) {
        $chap = $1;
    }

    if ($line =~ s/\[\$(.*?)\].*/<div id="\1-index" class="index"><a href="#\1">\1<\/a><\/div>/g) {
        print $line;
    }

    if ($line =~ /^$/) {
        $par = 1;
    } else {
        $par = 0;
    }
}

print "</div>\n";
