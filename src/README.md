The main filters here are 

1. lectureToSlidedeck -- which extracts 'slide' divs and converts into a format good for reveal.js slides
2. lectureToOutline  -- which parses out the headers and any bolded terms to create a single-page outline of a lecture suitable for printing.

There's also some cruft... er, bonuses:

lectureToPrintable just removes images from slides if you want to print a copy of the lecture to read in class (or export as epub, which is what I
usually read from) without losing pages at a time to images.

convertOldFormat and convertVeryOldFormat transition from older versions of this library. I still have some old lecture courses using them.

shuffleAllLists is from a time that I made tests using these templates and wanted each students to have their ids in a different order.
