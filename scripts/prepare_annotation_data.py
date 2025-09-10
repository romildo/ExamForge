#!/usr/bin/env python3
"""
csv-to-annotation (Python Version) - Extracts, filters, and formats data from a CSV for PDF annotation.
"""

import csv
import sys
import argparse
from pathlib import Path

def main():
    parser = argparse.ArgumentParser(
        description="Extracts, filters, and formats data from a CSV for PDF annotation.",
        epilog="Example:\n  %(prog)s -f data.csv -o notes.txt\n  cat data.csv | %(prog)s -f - -1 2 -2 3 -s 8 -r 7",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    # Required arguments
    parser.add_argument('-f', '--file', required=True,
                        help='Path to the input CSV file. Use "-" for stdin.')
    # Optional arguments with defaults
    parser.add_argument('-1', '--col1', type=int, default=1,
                        help='First column to extract (default: %(default)s). Note: Columns are 1-based.')
    parser.add_argument('-2', '--col2', type=int, default=2,
                        help='Second column to extract (default: %(default)s).')
    parser.add_argument('-s', '--sort-col', type=int, default=3,
                        help='Column to sort by (must be numeric and non-empty) (default: %(default)s).')
    parser.add_argument('-r', '--start-row', type=int, default=1,
                        help='First data row to process (skip header rows) (default: %(default)s).')
    parser.add_argument('-g', '--glue', default='-',
                        help='String to place between the two columns (default: "%(default)s").')
    parser.add_argument('-d', '--delimiter', default=',',
                        help='Field delimiter in the CSV (default: "%(default)s").')
    parser.add_argument('-o', '--output',
                        help='Path for the output text file. Omit for stdout.')

    args = parser.parse_args()

    # --- Validation ---
    # Convert 1-based column numbers to 0-based for Python lists
    col1_index = args.col1 - 1
    col2_index = args.col2 - 1
    sort_col_index = args.sort_col - 1

    for col, name in zip([col1_index, col2_index, sort_col_index], ['col1', 'col2', 'sort-col']):
        if col < 0:
            print(f"Error: {name} must be a positive integer (1-based).", file=sys.stderr)
            sys.exit(1)

    if args.start_row < 1:
        print("Error: Start row must be 1 or greater.", file=sys.stderr)
        sys.exit(1)

    # --- Read Input ---
    input_file = sys.stdin if args.file == '-' else open(args.file, 'r', newline='')
    # Use a reader that handles fields with surrounding quotes and trailing delimiters
    reader = csv.reader(input_file, delimiter=args.delimiter, skipinitialspace=True)

    # --- Process Data ---
    processed_data = []  # We'll store tuples of (sort_key, output_string)

    print(f"Processing CSV data...", file=sys.stderr)
    print(f"  Extracting columns: {args.col1} and {args.col2}", file=sys.stderr)
    print(f"  Sorting by column: {args.sort_col}", file=sys.stderr)
    print(f"  Starting from row: {args.start_row}", file=sys.stderr)
    print(f"  Using glue: '{args.glue}'", file=sys.stderr)
    print(f"  Using delimiter: '{args.delimiter}'", file=sys.stderr)

    for row_num, row in enumerate(reader, start=1):
        # 1. Skip rows before the start row
        if row_num < args.start_row:
            continue

        # 2. Check if the row has enough columns for our needs
        max_col_needed = max(col1_index, col2_index, sort_col_index)
        if len(row) <= max_col_needed:
            # Row doesn't have enough columns, skip it
            continue

        # 3. Get the value of the sort column and clean it
        sort_value_str = row[sort_col_index].strip()
        # 4. Filter out rows where the sort column is empty
        if not sort_value_str:
            continue

        # 5. Try to convert the sort value to a number for proper numeric sorting
        try:
            sort_key = float(sort_value_str) # Use float to handle decimals
        except ValueError:
            # If it's not a number, use the string itself (lexical sort)
            sort_key = sort_value_str
            print(f"  Warning: Row {row_num} has non-numeric sort value '{sort_value_str}'. Using lexical order.", file=sys.stderr)

        # 6. Get the values from the two columns we want to output
        col1_value = row[col1_index]
        col2_value = row[col2_index]

        # 7. Format the output string
        output_string = f"{col1_value}{args.glue}{col2_value}"

        # 8. Store for sorting
        processed_data.append((sort_key, output_string))

    if input_file is not sys.stdin:
        input_file.close()

    # --- Check if we found any data ---
    if not processed_data:
        print("Error: No valid rows found after filtering. Check your --start-row and --sort-col values.", file=sys.stderr)
        sys.exit(1)

    # --- Sort and Output ---
    # Sort by the numeric/string key we extracted
    processed_data.sort(key=lambda x: x[0])

    # Write output
    output_file = open(args.output, 'w') if args.output else sys.stdout
    for sort_key, output_string in processed_data:
        print(output_string, file=output_file)
    if output_file is not sys.stdout:
        output_file.close()
        print(f"Success! Wrote {len(processed_data)} lines to: {args.output}", file=sys.stderr)

if __name__ == '__main__':
    main()