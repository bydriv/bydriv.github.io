my $par = 1;

foreach my $line (<>) {
    if ($par && $line =~ /^(\[\d+\])「/) {
        print "\\noindent\n";
        $line =~ s/\[(\d+)\]/\\settowidth\\sectwidth{\\ebgaramond\\scriptsize{\1}}\\settoheight\\sectheight{\\ebgaramond\\scriptsize{\1}}\\makebox[\\sectheight]\{\\raisebox{\\zh}[0mm][0mm]{\\rotatebox[origin = c]{90}{\\ebgaramond\\scriptsize{\1}}}}\\hspace*{-\\sectheight}\\nolinebreak{}/;
    } elsif ($par && $line =~ /^(\[\d+\])/) {
        print "\\noindent\n";
        $line =~ s/\[(\d+)\]/\\settowidth\\sectwidth{\\ebgaramond\\scriptsize{\1}}\\settoheight\\sectheight{\\ebgaramond\\scriptsize{\1}}\\hspace*{1\\zw}\\makebox[\\sectheight]{\\raisebox{\\zh}[0mm][0mm]{\\rotatebox[origin = c]{90}{\\ebgaramond\\scriptsize{\1}}}}\\hspace*{-\\sectheight}\\nolinebreak{}/;
    }
    $line =~ s/\[(\d+)\]/\\settowidth\\sectwidth{\\ebgaramond\\scriptsize{\1}}\\settoheight\\sectheight{\\ebgaramond\\scriptsize{\1}}\\makebox[\\sectheight]{\\raisebox{\\zh}[0mm][0mm]{\\rotatebox[origin = c]{90}{\\ebgaramond\\scriptsize{\1}}}}\\hspace*{-\\sectheight}\\nolinebreak{}/g;
    $line =~ s/\[\*\]/\\vspace{3\\baselineskip}/g;
    $line =~ s/\[\+\]/\\newpage/g;
    $line =~ s/\[\$(.*?)\]/\\chapter{}/g;

    my $i = 0;

    while ($i < 5) {
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)0/〇/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)1/一/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)2/二/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)3/三/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)4/四/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)5/五/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)6/六/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)7/七/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)8/八/g;
        $line =~ s/(?<!\{|\[|\d|\w|\.|-|\+)9/九/g;
        $line =~ s/(?<=〇|一|二|三|四|五|六|七|八|九)\./・/g;
        ++$i;
    }

    $line =~ s/:/\\：{}/g;
    $line =~ s/\(/\\（{}/g;
    $line =~ s/\)/\\）{}/g;
    $line =~ s/A/\\makebox[1\\zw]{\\rensuji{A}}/g;
    $line =~ s/B/\\makebox[1\\zw]{\\rensuji{B}}/g;
    $line =~ s/C/\\makebox[1\\zw]{\\rensuji{C}}/g;
    $line =~ s/D/\\makebox[1\\zw]{\\rensuji{D}}/g;
    $line =~ s/E/\\makebox[1\\zw]{\\rensuji{E}}/g;
    $line =~ s/F/\\makebox[1\\zw]{\\rensuji{F}}/g;
    $line =~ s/G/\\makebox[1\\zw]{\\rensuji{G}}/g;
    $line =~ s/H/\\makebox[1\\zw]{\\rensuji{H}}/g;
    $line =~ s/I/\\makebox[1\\zw]{\\rensuji{I}}/g;
    $line =~ s/J/\\makebox[1\\zw]{\\rensuji{J}}/g;
    $line =~ s/K/\\makebox[1\\zw]{\\rensuji{K}}/g;
    $line =~ s/L/\\makebox[1\\zw]{\\rensuji{L}}/g;
    $line =~ s/M/\\makebox[1\\zw]{\\rensuji{M}}/g;
    $line =~ s/N/\\makebox[1\\zw]{\\rensuji{N}}/g;
    $line =~ s/O/\\makebox[1\\zw]{\\rensuji{O}}/g;
    $line =~ s/P/\\makebox[1\\zw]{\\rensuji{P}}/g;
    $line =~ s/Q/\\makebox[1\\zw]{\\rensuji{Q}}/g;
    $line =~ s/R/\\makebox[1\\zw]{\\rensuji{R}}/g;
    $line =~ s/S/\\makebox[1\\zw]{\\rensuji{S}}/g;
    $line =~ s/T/\\makebox[1\\zw]{\\rensuji{T}}/g;
    $line =~ s/U/\\makebox[1\\zw]{\\rensuji{U}}/g;
    $line =~ s/V/\\makebox[1\\zw]{\\rensuji{V}}/g;
    $line =~ s/W/\\makebox[1\\zw]{\\rensuji{W}}/g;
    $line =~ s/X/\\makebox[1\\zw]{\\rensuji{X}}/g;
    $line =~ s/Y/\\makebox[1\\zw]{\\rensuji{Y}}/g;
    $line =~ s/Z/\\makebox[1\\zw]{\\rensuji{Z}}/g;
    $line =~ s/――/\\nolinebreak\\raisebox{0.1\\zh}[0mm][0mm]{------}\\nolinebreak{}/g;
    $line =~ s/、/\\、{}/g;
    $line =~ s/。/\\。{}/g;
    $line =~ s/ー/\\ー{}/g;
    $line =~ s/……/\\…{}/g;
    $line =~ s/「/\\「{}/g;
    $line =~ s/！！(?!」)/\\exclexcl{}\\hspace{1\\zw}/g;
    $line =~ s/！？(?!」)/\\exclques{}\\hspace{1\\zw}/g;
    $line =~ s/(?<!！)！(?!？|」)/\\！{}\\hspace{1\\zw}/g;
    $line =~ s/(?<!！)？(?!」)/\\？{}\\hspace{1\\zw}/g;
    $line =~ s/！！」/\\exclexcl{}\\」{}/g;
    $line =~ s/！？」/\\exclques{}\\」{}/g;
    $line =~ s/(?<!！)！」/\\！{}\\」{}/g;
    $line =~ s/(?<!！)？」/\\？{}\\」{}/g;
    $line =~ s/(?<!\\)」/\\」{}/g;
    $line =~ s/ァ/\\ァ{}/g;
    $line =~ s/ィ/\\ィ{}/g;
    $line =~ s/ゥ/\\ゥ{}/g;
    $line =~ s/ェ/\\ェ{}/g;
    $line =~ s/ォ/\\ォ{}/g;
    $line =~ s/ッ/\\ッ{}/g;
    $line =~ s/ャ/\\ャ{}/g;
    $line =~ s/ュ/\\ュ{}/g;
    $line =~ s/ョ/\\ョ{}/g;
    $line =~ s/ぁ/\\ぁ{}/g;
    $line =~ s/ぃ/\\ぃ{}/g;
    $line =~ s/ぅ/\\ぅ{}/g;
    $line =~ s/ぇ/\\ぇ{}/g;
    $line =~ s/ぉ/\\ぉ{}/g;
    $line =~ s/っ/\\っ{}/g;
    $line =~ s/ゃ/\\ゃ{}/g;
    $line =~ s/ゅ/\\ゅ{}/g;
    $line =~ s/ょ/\\ょ{}/g;

    if ($line =~ /^$/) {
        $par = 1;
    } else {
        $par = 0;
    }

    print $line;
}
