typeclassopedia.epub: typeclassopedia.md
	pandoc -s -S typeclassopedia.md \
		--epub-metadata=metadata.xml \
		--epub-cover-image=cover.png \
		-o typeclassopedia.epub
