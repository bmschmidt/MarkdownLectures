MarkdownLectures
================

Convert a single markdown file into lectures, slides, and outlines.

Some potentially useful lines from my Makefile are below.

```

Lectures/outlines/%.md: Lectures/%.md scripts/lectureToOutline
	pandoc --filter scripts/lectureToOutline -o $@ $<
#	scripts/uploadToWordpress.sh $@ Outlines

slides/%.html: Lectures/%.md
	pandoc --filter scripts/lectureToSlidedeck.hs -t revealjs --template templates/revealjs.html --variable=transition:Slide $< > $@
	cp $@ $(WordpressLocation)/slides/


```
