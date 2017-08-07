# E-book version of the Typeclassopedia

This is a Pandoc Markdown version of the
[Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia) hosted on
the Haskell Wiki. The primary goal of the Markdown conversion was to be able to
convert the Typeclassopedia to an e-book in the EPUB format, thus making it
possible to highlight text and take notes while reading.

Running `make` will convert the Markdown file to EPUB using the excellent
[`pandoc`](http://johnmacfarlane.net/pandoc/) tool. This will also add syntax
highlighting to the code snippets and handle the footnotes.

An up-to-date version of the EPUB file can be found in
[releases](https://github.com/ehamberg/typeclassopedia-md/releases).

To convert to different formats (e.g. for reading on a Kindle), the EPUB file
can be converted by using tools such as [Calibre](http://calibre-ebook.com/).

The following list shows the *Vim* commands that did the bulk of the work
converting from wiki syntax to Markdown:

    :%s/\[\(http:\S*\) \(.\{-}\)\]/[\2](\1)/g
    :%s,<code>\(.\{-}\)</code>,`\1`,g
    :%s,''\(.\{-}\)'',*\1*,g
    :%s/^==\(.*\)==$/## \1/
    :%s/^=\(.*\)=$/# \1/
    :%s,^<haskell>,```haskell,
    :%s,^</haskell>,```,
    %s,<haskell>\(.\{-}\)</haskell>,`\1`{.haskell},g
    :%s,^{{note|\(.*\)}}$,> *\1*,
    :%s,{{=}},=,g
    :%s,<math>\(.\{-}\)</math>,$\1$,g
    :%s,<i>\(.\{-}\)</i>,*\1*,g
    :%s,^<li>\(.*\)</li>$,- \1,
    :%s,\[\[\(.\{-}\)|\(.\{-}\)\]\],[\2](http://www.haskell.org/haskellwiki/\1),g
    :%s,\[\[\(.\{-}\)\]\],[\1](http://www.haskell.org/haskellwiki/\1),g
    :%s,\[{{HackageDocs|\(.*\)|\(.*\)}}\(\S*\) \(.\{-}\)\],[\4](https://hackage.haskell.org/package/\1/docs/\2.html\3),g
    %s,\[\(http\S*\) \([^\]]*\)\],[\2](\1),g

The hard work in converting from the original article was already done for the
wiki conversion. So big thanks to Geheimdienst.
