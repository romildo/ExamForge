# === Variables ===
# Tools
EXAMFORGE   := cabal run examforge --
ASSEMBLER   := cabal run exam-assembler --
# Removed -pvc for a single, non-interactive build
LATEXMK     := latexmk -pdf -pdflatex=lualatex -interaction=nonstopmode
# Define a configurable PDF viewer
PDF_VIEWER  := evince

# Directories
YAML_DIR    := questions
EXAM_DIR    := exams
GEN_DIR     := Generated
# Using a relative path for the build directory inside the exams dir
BUILD_DIR   := _build

# Default Assembler Flags
ASSEMBLER_FLAGS := --show-id --show-tags

# Get a list of all exam bases from the questions directory (e.g., EE1, EE2)
EXAM_BASES := $(patsubst $(YAML_DIR)/%.yaml,%,$(wildcard $(YAML_DIR)/*.yaml))

# === Targets ===
.PHONY: all clean multi

# Default target: build PDFs for all YAML files found in the questions/ directory.
all: $(patsubst %,$(EXAM_DIR)/%.pdf,$(EXAM_BASES))

# --- Main Pattern Rule for Single Exam Generation ---
# This rule now includes moving the final PDF and an optional preview.
#
# Usage:
#   make exams/EE1.pdf             (Builds the PDF)
#   make exams/EE1.pdf PREVIEW=yes (Builds and then opens the PDF)
#
$(EXAM_DIR)/%.pdf: $(YAML_DIR)/%.yaml
	@echo "--> Generating Haskell module for $*..."
	@$(EXAMFORGE) $<

	@echo "--> Assembling exam for $*..."
	@$(ASSEMBLER) --versions 1 --out-dir $(EXAM_DIR) --csv-file $(EXAM_DIR)/exam-B01.csv $(ASSEMBLER_FLAGS)
	@echo "--> Renaming generated files to $*..."
	@mv $(EXAM_DIR)/exam-B01.tex $(EXAM_DIR)/$*.tex
	@mv $(EXAM_DIR)/exam-B01.csv $(EXAM_DIR)/$*.csv

	@echo "--> Compiling LaTeX PDF for $*..."
	@(cd $(EXAM_DIR) && $(LATEXMK) -output-directory=$(BUILD_DIR) $*.tex)

	@echo "--> Moving final PDF to $(EXAM_DIR)..."
	@mv $(EXAM_DIR)/$(BUILD_DIR)/$*.pdf $(EXAM_DIR)/$*.pdf

	@echo "--> Finished building $@."

	@# Optional preview step
	@if [ "$(PREVIEW)" = "yes" ]; then \
		echo "--> Opening preview for $@..."; \
		$(PDF_VIEWER) $@ & \
	fi


# --- Rule for Generating Multiple Versions from ALL Questions ---
VERSIONS := 3 # Default number of versions
multi:
	@echo "--> Generating combined Haskell module from all YAML files..."
	@$(EXAMFORGE) $(wildcard $(YAML_DIR)/*.yaml)

	@echo "--> Assembling $(VERSIONS) versions..."
	@$(ASSEMBLER) --versions $(VERSIONS) --out-dir $(EXAM_DIR) --csv-file $(EXAM_DIR)/gabarito_geral.csv $(ASSEMBLER_FLAGS)

	@echo "--> Compiling all generated .tex files..."
	@(cd $(EXAM_DIR) && $(LATEXMK) -output-directory=$(BUILD_DIR) *.tex)
	@echo "--> Finished building multiple versions."


# --- Cleanup Rule ---
clean:
	@echo "--> Cleaning up generated files..."
	@rm -rf $(GEN_DIR)
	@rm -f $(EXAM_DIR)/*.tex $(EXAM_DIR)/*.pdf $(EXAM_DIR)/*.csv
	@rm -rf $(EXAM_DIR)/$(BUILD_DIR)
	@# This cleans up latexmk's own auxiliary files if any are left in the main dir
	@latexmk -C
	@echo "--> Done."
