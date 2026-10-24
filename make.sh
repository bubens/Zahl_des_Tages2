#!/bin/bash

NAME="Zahl-des-Tages"
AUTHOR="Henning P. Beyer <bubens@unpunk.de>"
LICENSE="Licensed under the GPL-3.0-or-later"

PREAMBLE="/* $NAME\n * (c) 2026 $AUTHOR\n * $LICENSE\n */"

ID=$(date +%s)
SRC_ELM="./src/Main.elm"
TMP_OUT_ELM="/tmp/$(date +%s)_$NAME.elm.js"
TMP_OUT_UJS="/tmp/$(date +%s)_$NAME.ujs.js"
DEST_SCRIPT="./www/app.js"



echo "Making $NAME" &&
echo "Step 1: Compile elm-code" &&
npx elm make --optimize --output=$TMP_OUT_ELM $SRC_ELM &&
# npx elm make --output=$TMP_OUT_ELM $SRC_ELM &&
echo "Step 2: Compress compiled code" &&
npx terser $TMP_OUT_ELM --output=$TMP_OUT_UJS --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' &&
echo "Step 3: Mangle compressed code" &&
npx terser $TMP_OUT_UJS --format "preamble=\"$PREAMBLE\"" --output=$DEST_SCRIPT --mangle &&
echo "Results:"
echo "   Orginal size: $(cat $SRC_ELM | wc -c) bytes ($SRC_ELM)"
echo "   Compiled size: $(cat $TMP_OUT_ELM | wc -c) bytes ($TMP_OUT_ELM)"
echo "   Compressed size: $(cat $TMP_OUT_UJS | wc -c) bytes ($TMP_OUT_UJS)"
echo "   Mangeled size: $(cat $DEST_SCRIPT | wc -c) bytes ($DEST_SCRIPT)"
echo "Done"