#!/bin/bash

#Read in the configurations
#source syllabus/course.cnf


FILE=$1;
BASENAME=$(echo "$FILE" | sed "s/\.md//;s/.*\///;s/_/ /g")

echo "Using basename of $BASENAME"
PARENT=$2;
: ${PARENT:=""}

#Create if not exists
parentVersion=$(wp post list --post_type=page --format=csv |  awk "{FS=\",\"}{if(\$2 ~ \"$PARENT\") {print \$1}}")
: ${parentVersion:=$(wp post create --porcelain --post_type=page --post_status=publish --post_title="$PARENT")}


#Create if not exists
EXISTINGVERSION=$(wp post list --post_type=page --format=csv | awk "{FS=\",\"}{if(\$2 ~ /^\"?$BASENAME\"?$/) {print \$1}}")
: ${EXISTINGVERSION:=$(wp post create --porcelain --post_type=page --post_status=publish --post_title="$BASENAME" --post_parent=$parentVersion)}

CONTENT=$(pandoc -t html --base-header-level=2 --bibliography=../../MyLibrary.bib --smart --csl=citationstyles/inline.csl "$FILE")

#Now that it's created, we just need to paste in the content as piped through Pandoc.
wp post update $EXISTINGVERSION --post_content="${CONTENT}"



