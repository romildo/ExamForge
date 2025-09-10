#!/usr/bin/env python3
"""
Generates a single-page PDF with text at specific coordinates.
Used by pdf-annotator.sh for precise text placement.
"""

import sys
from reportlab.pdfgen import canvas
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.pagesizes import letter
from reportlab.lib.units import pt

def main():
    if len(sys.argv) < 7:
        print("Usage: generate_annotation.py text font font_size x_pos y_pos page_width page_height [output_file]")
        sys.exit(1)
    
    text = sys.argv[1]
    font_name = sys.argv[2]
    font_size = float(sys.argv[3])
    x_pos = float(sys.argv[4])
    y_pos = float(sys.argv[5])
    page_width = float(sys.argv[6])
    page_height = float(sys.argv[7])
    output_file = sys.argv[8] if len(sys.argv) > 8 else "-"
    
    # Create PDF
    if output_file == "-":
        c = canvas.Canvas(sys.stdout.buffer, pagesize=(page_width, page_height))
    else:
        c = canvas.Canvas(output_file, pagesize=(page_width, page_height))
    
    # Set font - try to use a standard monospace font
    c.setFont(font_name, font_size)
    
    # Draw text at exact coordinates (from bottom-left)
    c.drawString(x_pos, y_pos, text)
    
    c.save()

if __name__ == "__main__":
    main()