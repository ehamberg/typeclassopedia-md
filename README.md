# E-book version of the Typeclassopedia

This is a Pandoc Markdown version of the
[Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia) hosted on
the Haskell Wiki. The primary goal of the Markdown conversion was to be able to
convert the Typeclassopedia to an e-book in the EPUB format, thus making it
possible to highlight text and take notes while reading.

Running `make` will convert the Markdown file to EPUB using the excellent
[`pandoc`](http://johnmacfarlane.net/pandoc/) tool. This will also add syntax
highlighting to the code snippets and handle the footnotes.

An up-to-date version of the EPUB file can be found at
<http://hamberg.no/erlend/files/typeclassopedia.epub>.

To convert to different formats (e.g. for reading on a Kindle), the EPUB file
can be converted by using tools such as [Calibre](http://calibre-ebook.com/).
