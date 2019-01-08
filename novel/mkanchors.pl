my $chap = 0;
my $par = 1;

foreach my $line (<>) {
    if ($line =~ s/\[(\d+)\].*/<div id="$chap:\1"><\/div>/) {
        print $line;
    }

    if ($line =~ /\[\$(.*?)\]/) {
        $chap = $1;
    }

    if ($line =~ s/\[\$(.*?)\].*/<div id="\1"><\/div>/g) {
        print $line;
    }

    if ($line =~ /^$/) {
        $par = 1;
    } else {
        $par = 0;
    }
}