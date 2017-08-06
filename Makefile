.PHONY:
epub: typeclassopedia.epub

.PHONY:
pdf: typeclassopedia.pdf

typeclassopedia.epub: typeclassopedia.md
	pandoc -s -S typeclassopedia.md \
		--epub-metadata=metadata.xml \
		--epub-cover-image=cover.png \
		-o typeclassopedia.epub

typeclassopedia.pdf: typeclassopedia.md
	sed 's/^The type classes we will be discussing/\\clearpage &/' \
		typeclassopedia.md|\
		sed 's/Typeclassopedia-diagram.svg/Typeclassopedia-diagram.png/'| \
		pandoc -s -S -o typeclassopedia.pdf
