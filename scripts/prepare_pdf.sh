#!/usr/bin/env bash

# This script takes a single input PDF and prepares three derivative files:
# 1. A ".student.pdf" version for double-sided printing.
# 2. A ".template-sheet.pdf" version for automated grading.
# 3. An ".answers-only.pdf" version containing just the answer key.
#
# It requires qpdf, pdfunite, and magick (from ImageMagick).

set -e # Exit immediately if a command fails

INPUT_PDF="$1"
BASE_NAME="${INPUT_PDF%.pdf}"
OUTPUT_STUDENT="${BASE_NAME}.student.pdf"
OUTPUT_TEMPLATESHEET="${BASE_NAME}.template-sheet.pdf"
OUTPUT_ANSWERKEY="${BASE_NAME}.answers-only.pdf"

TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

BLANK_PDF="$TEMP_DIR/blank.pdf"
magick -size 595x842 xc:white "$BLANK_PDF" > /dev/null 2>&1

echo " -> Processing: $INPUT_PDF"

# --- PDF Analysis ---
TOTAL_PAGES=$(qpdf --show-npages "$INPUT_PDF")
if [ "$TOTAL_PAGES" -lt 2 ]; then
    echo "   [ERROR] PDF has fewer than 2 pages. Cannot process."
    exit 1
fi
PENULTIMATE_PAGE=$((TOTAL_PAGES - 1))
BODY_PAGES=$((TOTAL_PAGES - 2))

# --- File Extraction ---
BODY_PDF="$TEMP_DIR/body.pdf"
qpdf "$INPUT_PDF" --pages . 1-$BODY_PAGES -- "$BODY_PDF"
qpdf "$INPUT_PDF" --pages . $PENULTIMATE_PAGE -- "$OUTPUT_TEMPLATESHEET"
qpdf "$INPUT_PDF" --pages . $TOTAL_PAGES -- "$OUTPUT_ANSWERKEY"

# --- Pad Body if Necessary ---
if (( BODY_PAGES % 2 != 0 )); then
    PADDED_BODY_PDF="$TEMP_DIR/body_padded.pdf"
    pdfunite "$BODY_PDF" "$BLANK_PDF" "$PADDED_BODY_PDF"
    BODY_PDF="$PADDED_BODY_PDF" # Use the padded version from now on
fi

# --- Assemble Final Student PDF ---
pdfunite "$BODY_PDF" "$OUTPUT_TEMPLATESHEET" "$BLANK_PDF" "$OUTPUT_STUDENT"

echo "   [OK] Created: $OUTPUT_STUDENT"
echo "   [OK] Created: $OUTPUT_TEMPLATESHEET"
echo "   [OK] Created: $OUTPUT_ANSWERKEY"