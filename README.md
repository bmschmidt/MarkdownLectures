MarkdownLectures
================

Convert a single markdown file into lectures, slides, and outlines
with key terms.

An explanation of the philosophy is
[here](http://benschmidt.org/2014/11/07/building-outlines-for-markdown-documents-with-pandoc/),
although the lecture templates there no longer work. I've packaged a
script in src that converts from the format there (around code blocks)
to the new one (around fenced divs).

This code does not create the slide deck directly; rather, it writes to the pandoc
AST in a format that should create slides in any of Pandoc's built-in slide
formats. I generally use it with `reveal-js` slides.

This repo is periodically updated to work with the latest build of
Pandoc, which has a frequently-changing API.  If you get errors,
probably you have the wrong version of Pandoc installed.

## Quickstart

From the main directory, the following commands will take the
demonstration lecture at lecture.md and create a website you can open
at slides.html; and a PDF outline at outline.pdf. I don't know if this
is possible without a working Haskell installation locally.

```bash
pandoc --filter src/lectureToSlidedeck.hs -t revealjs lecture.md --standalone -o slides.html -V revealjs-url=http://lab.hakim.se/reveal-js
pandoc --filter src/lectureToOutline.hs lecture.md -o outline.pdf
```

## Writing format

1. Lectures are written as Markdown.
2. Anything in a **fenced code div** classed as a 'slide' will become a slide. For example.
   ```
   These are lecturer notes.
   
   :::slide
   This will appear on the *screen*.
   :::
   
   ```
3. Any standalone images (i.e., images that take up a paragraph on
   their own) will become a fullpage slide: the image caption will be
   turned into large text on the screen.
   ```

   ![Big white label](image.png)

   ![Big, red text](image.png){.red}

   ```
   (In practice, the way this works is by turning these into level 2 headers, so styling is inherited from them.)
   Image attributes are passed to the underlying text; so the special class '.red' on the bottom image makes the text be read.
   Colors supported are '.red', '.black', and '.blue'. (No color leads the text to be white.) Leave the image description
   blank for no label.

4. Any level 1 headers (`# Introduction`) are kept in the outline, and also used as the title for a slide. I use
   reveal's two-level setting, so these act as the column heads.


## Compiling

If you do this a lot, you may want to compile the pandoc filters
rather than run the scripts, which greatly increases the speed of the
conversions. The makefile will install them to a source on disk (by
default, /usr/local/bin/lectureToSlidedeck and
/usr/local/bin/lectureToOutline). Then you just use those locations
instead of the ones above.

## Making more elaborate slides.

Pandoc now allows cool things like two column slides. Those will work
with this: just insert more fenced divs inside your slide iv.



