my $chap = 0;
my $par = 1;

foreach my $line (<>) {
    if ($par && $line =~ /^(\[\d+\])「/) {
        print "#+HTML: <div class=\"noindent\"></div>\n";
        print "@\@html:<span>@@\n";
        $line =~ s/\[(\d+)\]/@\@html:<\/span><span id="$chap:\1" class="sect"><sup><a href="#$chap:\1">\1<\/a><\/sup>@@\\nbsp{}/;
    } elsif ($par && $line =~ /^(\[\d+\])/) {
        print "#+HTML: <div class=\"noindent\"></div>\n";
        print "@\@html:<span>@@\n";
        $line =~ s/\[(\d+)\]/@\@html:<\/span><span class="indent"><\/span><span id="$chap:\1" class="sect">@@@\@html:<sup><a href="#$chap:\1">\1<\/a><\/sup>@@\\nbsp{}/;
    }
    $line =~ s/\[(\d+)\]/@\@html:<\/span><span id="$chap:\1" class="sect"><sup><a href="#$chap:\1">\1<\/a><\/sup>@@\\nbsp{}/g;
    $line =~ s/\[\*\]/#+HTML: <div class="space"><\/div>\n@\@html:<span>@@\n/g;
    $line =~ s/\[\+\]/#+HTML: <div class="space"><\/div>\n@\@html:<span>@@\n/g;

    if ($line =~ /\[\$(.*?)\]/) {
        $chap = $1;
    }

    $line =~ s/\[\$(.*?)\]/* 第\1章\n:PROPERTIES:\n:CUSTOM_ID: \1\n:END:\n@\@html:<span>@@\n/g;

    $line =~ s/(……)/@\@html:<span class="cdots">\1<\/span>@@/g;
    $line =~ s/(――)/@\@html:<span class="dashes">\1<\/span>@@/g;

    $line =~ s/！？(?!」)/！？@\@html:<span class="indent"><\/span>@@/g;
    $line =~ s/！(?!？|」)/！@\@html:<span class="indent"><\/span>@@/g;
    $line =~ s/？(?!」)/？@\@html:<span class="indent"><\/span>@@/g;

    if ($line =~ /^$/) {
        print "@\@html:</span>@@\n";
        $par = 1;
    } else {
        $par = 0;
    }

    print $line;
    #if ($line =~ /^$/) {
    #    print "\n";
    #} else {
    #    $line =~ s/\s//g;
    #}
    #if ($line =~ /^(\[\d+\])「/) {
    #    print "#+HTML: <div class=\"noindent\"></div>\n";
    #}
    #$line =~ s/\[(\d+)\]/\\nbsp^{\1}/g;
    #$line =~ s/\[\*\]/#+HTML: <div class="space"><\/div>/g;
    #$line =~ s/\[\$\]/* /g;
    #$line =~ s/(……)/@\@html:<span class="cdots">\1<\/span>@@/g;
    #$line =~ s/(――)/@\@html:<span class="dashes">\1<\/span>@@/g;
    #print $line;
}
