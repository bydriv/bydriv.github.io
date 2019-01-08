my $chap = 0;
my $sect = 0;
my $par = 1;

foreach my $line (<>) {
    if ($line =~ /\[(\d+)\]/) {
        $sect = $1;

        print "div[id=\"$chap:$sect\"]:target ~ div#index > div#default-index {\n";
        print "  color: #00FFFF;\n";
        print "  background-color: #000000;\n";
        print "}\n";

        print "div[id=\"$chap:$sect\"]:target ~ div#index > div[id=\"$chap-index\"] {\n";
        print "  color: #000000;\n";
        print "  background-color: #00FFFF;\n";
        print "}\n";

        print "div[id=\"$chap:$sect\"]:target ~ div#contents > div#default-contents {\n";
        print "  max-height: 1280px;\n";
        print "  top: -1280px;\n";
        print "  z-index: -2000;\n";
        print "  opacity: 0;\n";
        print "}\n";

        print "div[id=\"$chap:$sect\"]:target ~ div#contents > div[id=\"$chap-contents\"] {\n";
        print "  max-height: none;\n";

        print "  top: 0;\n";
        print "  z-index: -1000;\n";
        print "  opacity: 1;\n";
        print "}\n";

        print "div[id=\"$chap:$sect\"]:target ~ div#contents span[id=\"$chap:$sect-anchor\"] {\n";
        print "  background-color: #FFFFE0;\n";
        print "}\n";
    }

    if ($line =~ /\[\$(.*?)\]/) {
        $chap = $1;
        $sect = 0;

        print "div[id=\"$chap\"]:target ~ div#index > div#default-index {\n";
        print "  color: #00FFFF;\n";
        print "  background-color: #000000;\n";
        print "}\n";

        print "div[id=\"$chap\"]:target ~ div#index > div[id=\"$chap-index\"] {\n";
        print "  color: #000000;\n";
        print "  background-color: #00FFFF;\n";
        print "}\n";

        print "div[id=\"$chap\"]:target ~ div#contents > div#default-contents {\n";
        print "  max-height: 1280px;\n";
        print "  top: -1280px;\n";
        print "  z-index: -2000;\n";
        print "  opacity: 0;\n";
        print "}\n";

        print "div[id=\"$chap\"]:target ~ div#contents > div[id=\"$chap-contents\"] {\n";
        print "  max-height: none;\n";

        print "  top: 0;\n";
        print "  z-index: -1000;\n";
        print "  opacity: 1;\n";
        print "}\n";
    }

    if ($line =~ /^$/) {
        $par = 1;
    } else {
        $par = 0;
    }
}