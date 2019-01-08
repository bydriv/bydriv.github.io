my $chap = 0;
my $par = 1;

print "<div id=\"contents\">\n";
print "<div id=\"default-contents\" class=\"default contents\"></div><div>\n";

foreach my $line (<>) {
    if ($par && $line =~ /^(\[\d+\])「/) {
        print "<p class=\"noindent\">\n";
        print "<span>\n";
        $line =~ s/\[(\d+)\]/<\/span><span id="$chap:\1-anchor" class="sect"><sup><a href="#$chap:\1">\1<\/a><\/sup>&nbsp;/;
    } elsif ($par && $line =~ /^(\[\d+\])/) {
        print "<p class=\"noindent\">\n";
        print "<span>\n";
        $line =~ s/\[(\d+)\]/<\/span><span class="indent"><\/span><span id="$chap:\1-anchor" class="sect"><sup><a href="#$chap:\1">\1<\/a><\/sup>/;
    } elsif ($par) {
        print "<p class=\"indent\">\n";
    }
    $line =~ s/\[(\d+)\]/<\/span><span id="$chap:\1-anchor" class="sect"><sup><a href="#$chap:\1">\1<\/a><\/sup>&nbsp;/g;
    $line =~ s/\[\*\]/<div class="space"><\/div>\n<span>\n/g;
    $line =~ s/\[\+\]/<div class="space"><\/div>\n<span>\n/g;

    if ($line =~ /\[\$(.*?)\]/) {
        $chap = $1;
        print "</p></div><div id=\"$chap-contents\" class=\"contents\">";
    }

    $line =~ s/\[\$(.*?)\]/<h2 id="\1">第\1章<\/h2>\n<span>\n/g;

    $line =~ s/(……)/<span class="cdots">\1<\/span>/g;
    $line =~ s/(――)/<span class="dashes">\1<\/span>/g;

    $line =~ s/！？(?!」)/！？<span class="indent"><\/span>/g;
    $line =~ s/！(?!？|」)/！<span class="indent"><\/span>/g;
    $line =~ s/？(?!」)/？<span class="indent"><\/span>/g;

    if ($line =~ /^$/) {
        print "</span>\n";
        print "</p>\n";
        $par = 1;
    } else {
        $par = 0;
    }

    print $line;
}

print "</div></div>\n";
