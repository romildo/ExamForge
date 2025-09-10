#!/usr/bin/env bash

# --- Script Info ---
SCRIPT_NAME="pdf-annotator"
VERSION="1.2" # Bumped version for corrected stdin handling
AUTHOR="Senior Computer Scientist"
DESCRIPTION="Annotates each page of a PDF with a line from a text file."

# --- Default Configuration ---
DEFAULT_FONT_SIZE=12
DEFAULT_X_POS=50
DEFAULT_Y_POS=50
FONT="Courier"

# --- Usage Function ---
usage() {
    cat >&2 << EOF
$SCRIPT_NAME (v$VERSION) - $DESCRIPTION

Usage: 
  $0 [OPTIONS] -i INPUT_PDF -t TEXT_FILE
  $0 [OPTIONS] -i INPUT_PDF -t -  # Read annotations from stdin
  $0 [OPTIONS] -i - -t TEXT_FILE  # Read PDF from stdin

Required Arguments:
  -i, --input-file FILE    Path to the original input PDF file. Use '-' for stdin.
  -t, --text-file FILE     Path to the text file containing annotations, one per line. Use '-' for stdin.

Options:
  -o, --output-file FILE   Path for the resulting annotated PDF file. Omit or use '-' for stdout (default: stdout).
  -f, --font-size SIZE     Font size for the annotations (default: $DEFAULT_FONT_SIZE).
  -x, --pos-x POS          Horizontal position from left edge in points (default: $DEFAULT_X_POS).
  -y, --pos-y POS          Vertical position from bottom edge in points (default: $DEFAULT_Y_POS).
  -h, --help               Display this help message and exit.
  -v, --version            Display version information and exit.

Note: --input-file and --text-file cannot both read from stdin ('-') in the same invocation.

Example:
  # Basic file-based usage
  $0 -i document.pdf -t notes.txt -o annotated-document.pdf

  # Generate annotations on the fly and pipe them in
  ./prepare_annotation_data.py -f data.csv | $0 -i document.pdf -t - 

  # Process a PDF from a web server, use a local text file
  curl -s "http://example.com/report.pdf" | $0 -i - -t notes.txt > ./annotated-report.pdf
EOF
}

# --- Version Function ---
version() {
    echo "$SCRIPT_NAME version $VERSION" >&2
}

# --- Parse Command Line Arguments ---
PARSED_ARGUMENTS=$(getopt -o i:t:o:f:x:y:hv --long input-file:,text-file:,output-file:,font-size:,pos-x:,pos-y:,help,version -n "$SCRIPT_NAME" -- "$@")
if [ $? -ne 0 ]; then
    exit 1
fi
eval set -- "$PARSED_ARGUMENTS"

INPUT_PDF=""
TEXT_FILE=""
OUTPUT_PDF="-"
FONT_SIZE=$DEFAULT_FONT_SIZE
X_POS=$DEFAULT_X_POS
Y_POS=$DEFAULT_Y_POS

while true; do
    case "$1" in
        -i | --input-file ) INPUT_PDF="$2"; shift 2 ;;
        -t | --text-file )  TEXT_FILE="$2"; shift 2 ;;
        -o | --output-file ) OUTPUT_PDF="$2"; shift 2 ;;
        -f | --font-size )  FONT_SIZE="$2"; shift 2 ;;
        -x | --pos-x )      X_POS="$2"; shift 2 ;;
        -y | --pos-y )      Y_POS="$2"; shift 2 ;;
        -h | --help )       usage; exit 0 ;;
        -v | --version )    version; exit 0 ;;
        -- )                shift; break ;;
        * )                 break ;;
    esac
done

# --- Validation Functions ---
check_file_exists() {
    if [ "$1" != "-" ] && [ ! -f "$1" ]; then
        echo "Error: File '$1' does not exist." >&2
        exit 1
    fi
}

check_dependency() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "Error: Required command '$1' is not installed. Please install it." >&2
        exit 1
    fi
}

# --- Validate Arguments ---
if [ -z "$INPUT_PDF" ] || [ -z "$TEXT_FILE" ]; then
    echo "Error: Missing required arguments (-i, -t)." >&2
    usage
    exit 1
fi

# Check that both inputs are not stdin
if [ "$INPUT_PDF" = "-" ] && [ "$TEXT_FILE" = "-" ]; then
    echo "Error: --input-file and --text-file cannot both read from stdin ('-') in the same invocation." >&2
    usage
    exit 1
fi

check_file_exists "$INPUT_PDF"
check_file_exists "$TEXT_FILE"
check_dependency "qpdf"
check_dependency "pdfinfo"
check_dependency "enscript"
check_dependency "ps2pdf"
check_dependency "pdfjam"

# Handle input from stdin for PDF
if [ "$INPUT_PDF" = "-" ]; then
    echo "$SCRIPT_NAME: Reading PDF from standard input..." >&2
    INPUT_PDF_TMP=$(mktemp)
    cat /dev/stdin > "$INPUT_PDF_TMP"
    INPUT_PDF="$INPUT_PDF_TMP"
else
    INPUT_PDF_TMP=""
fi

# Handle input from stdin for TEXT
if [ "$TEXT_FILE" = "-" ]; then
    echo "$SCRIPT_NAME: Reading annotations from standard input..." >&2
    TEXT_FILE_TMP=$(mktemp)
    cat /dev/stdin > "$TEXT_FILE_TMP"
    TEXT_FILE="$TEXT_FILE_TMP"
else
    TEXT_FILE_TMP=""
fi

# Prevent overwriting the input file if output is a file
if [ "$OUTPUT_PDF" != "-" ] && [ -f "$OUTPUT_PDF" ]; then
    if { [ -n "$INPUT_PDF_TMP" ] && [ "$INPUT_PDF" = "$OUTPUT_PDF" ]; } || \
       { [ -z "$INPUT_PDF_TMP" ] && [ "$INPUT_PDF" = "$OUTPUT_PDF" ]; }; then
        echo "Error: Input and output files cannot be the same." >&2
        [ -n "$INPUT_PDF_TMP" ] && rm -f "$INPUT_PDF_TMP"
        [ -n "$TEXT_FILE_TMP" ] && rm -f "$TEXT_FILE_TMP"
        exit 1
    fi
fi

# --- Main Script Logic ---
echo "$SCRIPT_NAME: Starting annotation process..." >&2
WORKDIR=$(mktemp -d)
echo "Using temporary workspace: $WORKDIR" >&2

# 1. Get the page size of the original PDF
echo "Analyzing input PDF..." >&2
PAGE_SIZE=$(pdfinfo "$INPUT_PDF" | grep "Page size" | awk '{print $3, $5}' | sed 's/ /x/')
PAGE_WIDTH=$(echo "$PAGE_SIZE" | cut -d'x' -f1)
PAGE_HEIGHT=$(echo "$PAGE_SIZE" | cut -d'x' -f2)
echo "  Page size: ${PAGE_WIDTH}x${PAGE_HEIGHT} points" >&2

# 2. Calculate enscript margins
# Use awk for floating point math: margins=left:right:top:bottom
# Top margin = Page Height - Y_POS - (approx text height)
MARGINS=$(awk -v width="$PAGE_WIDTH" -v height="$PAGE_HEIGHT" -v x_pos="$X_POS" -v y_pos="$Y_POS" -v font_size="$FONT_SIZE" '
    BEGIN {
        # Calculate top margin: page height - desired Y position - text height estimate
        text_height_estimate = font_size * 1.5
        top_margin = height - y_pos - text_height_estimate
        # Ensure we dont get negative values
        if (top_margin < 0) top_margin = 0
        if (y_pos < 0) y_pos = 0
        # Format the margin string: left:right:top:bottom
        printf "%.0f:0:%.0f:0", x_pos, top_margin
    }
')
echo "  Text position: X=$X_POS, Y=$Y_POS (from bottom-left)" >&2
echo "  Using font: $FONT at ${FONT_SIZE}pt" >&2
echo "  Using enscript margins: $MARGINS" >&2

# 3. Generate one PDF per line of text
LINE_NUM=1
TOTAL_LINES=$(wc -l < "$TEXT_FILE")
echo "Generating $TOTAL_LINES annotation pages..." >&2

while IFS= read -r line; do
    OUTFILE="$WORKDIR/line_$(printf "%04d" $LINE_NUM).pdf"
    printf "\r  Processing line %04d of %04d..." $LINE_NUM $TOTAL_LINES >&2

    # Use the MARGINS variable calculated by awk
    echo "$line" | enscript -B -f "$FONT$FONT_SIZE" --margins="$MARGINS" -p - | \
    ps2pdf - "$OUTFILE"

    # Use the original floating-point values for pdfjam (it can handle them)
    pdfjam --quit --outfile "$OUTFILE.tmp" --papersize "{$PAGE_WIDTH}pt,${PAGE_HEIGHT}pt" "$OUTFILE" > /dev/null 2>&1
    mv "$OUTFILE.tmp" "$OUTFILE"

    ((LINE_NUM++))
done < "$TEXT_FILE"
printf "\n  Done.\n" >&2

# 4. Concatenate all the single-line PDFs into one overlay PDF
echo "Combining text pages into a single overlay..." >&2
pdfunite "$WORKDIR"/line_*.pdf "$WORKDIR/overlay.pdf"

# 5. Overlay with QPDF
echo "Overlaying text onto original PDF..." >&2
if [ "$OUTPUT_PDF" = "-" ]; then
    qpdf "$INPUT_PDF" --overlay "$WORKDIR/overlay.pdf" -- -
else
    qpdf "$INPUT_PDF" --overlay "$WORKDIR/overlay.pdf" -- "$OUTPUT_PDF"
    echo "Success! Annotated PDF created: $OUTPUT_PDF" >&2
fi

# 6. Cleanup
rm -rf "$WORKDIR"
[ -n "$INPUT_PDF_TMP" ] && rm -f "$INPUT_PDF_TMP"
[ -n "$TEXT_FILE_TMP" ] && rm -f "$TEXT_FILE_TMP"

exit 0