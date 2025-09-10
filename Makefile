# === Variables ===
EXAMFORGE   := cabal run examforge --
ASSEMBLER   := cabal run exam-assembler --
LATEXMK     := latexmk -pdf -pdflatex=lualatex -interaction=nonstopmode
PDF_VIEWER  := evince
PDFUNITE    := pdfunite
PYTHON      := python



# Directories & Scripts
CONFIG_DIR  := configs
YAML_DIR    := questions
EXAM_DIR    := exams
STUDENT_SHEET_DIR := student-sheets
GEN_DIR     := generated
BUILD_DIR   := _build
CACHE_DIR   := $(EXAM_DIR)/image-cache
SCRIPT_DIR  := scripts
PREPARE_SCRIPT := $(SCRIPT_DIR)/prepare_pdf.sh
STYLE_DIR   := .
STYLE_FILE  := $(STYLE_DIR)/provastyle.sty
MCQ_SCRIPTS_DIR := /alt/projects/mcq-grader/scripts

# Find all exam configuration files (e.g., EE1, P2)
EXAM_CONFIGS := $(patsubst $(CONFIG_DIR)/%.yml,%,$(wildcard $(CONFIG_DIR)/*.yml))

# --- Fine-tuning Parameters ---
# These can be overridden from the command line, e.g., `make PADDING=5 THRESHOLD=1500`
PADDING         ?= 10
THRESHOLD       ?= 1000
CONF_RATIO      ?= 0.7

# === Targets ===
.PHONY: all clean

# Tell 'make' not to delete our intermediate files.
.PRECIOUS: $(EXAM_DIR)/%.tex $(EXAM_DIR)/%.pdf $(EXAM_DIR)/$(BUILD_DIR)/%.pdf $(EXAM_DIR)/$(BUILD_DIR)/%.aux $(EXAM_DIR)/$(BUILD_DIR)/%.zonas $(EXAM_DIR)/%.stamp

# Default target: build the first version of all exams.
all: $(patsubst %,$(EXAM_DIR)/%-01.pdf,$(EXAM_CONFIGS))

# --- Rule to Generate .tex files (and a stamp) from a config ---
# This rule creates a stamp file (e.g., exams/EE1.stamp) to signify
# that the Haskell tools have successfully generated all .tex files for that series.
$(EXAM_DIR)/%.stamp: $(CONFIG_DIR)/%.yml
	@echo "--- Generating TeX files for exam series: $* ---"
	@$(EXAMFORGE) $<
	@$(ASSEMBLER) $<
	@touch $@

# --- Rule to Generate a .pdf from a .tex file ---
# It depends on its corresponding .tex file AND the main style file.
$(EXAM_DIR)/$(BUILD_DIR)/%.pdf $(EXAM_DIR)/$(BUILD_DIR)/%.aux $(EXAM_DIR)/$(BUILD_DIR)/%.zonas: $(EXAM_DIR)/%.tex $(STYLE_FILE)
	@echo "--- Compiling PDF: $@ ---"
	@(cd $(EXAM_DIR) && TEXINPUTS="../$(STYLE_DIR):" $(LATEXMK) -output-directory=$(BUILD_DIR) $(<F))

$(EXAM_DIR)/%.pdf: $(EXAM_DIR)/$(BUILD_DIR)/%.pdf
	@cp -a $(EXAM_DIR)/$(BUILD_DIR)/$*.pdf $@
	@# Optional preview step
	@if [ "$(PREVIEW)" = "yes" ]; then \
		echo "--> Opening preview for $@..."; \
		$(PDF_VIEWER) $@ & \
	fi

# --- Rule to generate the JSON zones map from LaTeX auxiliary files
$(EXAM_DIR)/%.zones.json: $(EXAM_DIR)/$(BUILD_DIR)/%-01.aux $(EXAM_DIR)/$(BUILD_DIR)/%-01.zonas
	@echo "--> Generating zones map from LaTeX data..."
	$(PYTHON) $(MCQ_SCRIPTS_DIR)/generate_zones.py \
		--output-file $@ \
		$(EXAM_DIR)/$(BUILD_DIR)/$*-01.aux \
		$(EXAM_DIR)/$(BUILD_DIR)/$*-01.zonas

# --- Rule to generate the visual verification PDF
$(EXAM_DIR)/%.verification.pdf: $(EXAM_DIR)/%.zones.json $(EXAM_DIR)/%-01.template-sheet.pdf $(STUDENT_SHEET_DIR)/%.student-sheets.pdf
	@echo "--> Generating visual verification PDF (Padding: $(PADDING)px)..."
	$(PYTHON) $(MCQ_SCRIPTS_DIR)/verify_zones.py \
		--zones-file $(EXAM_DIR)/$*.zones.json \
		--output $@ \
		--images-dir $(CACHE_DIR) \
		--template-pdf $(EXAM_DIR)/$*-01.template-sheet.pdf \
		--student-sheets-pdf $(STUDENT_SHEET_DIR)/$*.student-sheets.pdf \
		--padding=$(PADDING)

# Rule to extract data from student answer sheets
$(EXAM_DIR)/%.results.csv: $(EXAM_DIR)/%.zones.json $(STUDENT_SHEET_DIR)/%.student-sheets.pdf $(EXAM_DIR)/%-01.template-sheet.pdf
	@echo "--> Processing answer sheets (Padding: $(PADDING)px, Threshold: $(THRESHOLD))"
	$(PYTHON) $(MCQ_SCRIPTS_DIR)/process_sheets.py \
		--zones-file $(EXAM_DIR)/$*.zones.json \
		--output-csv $@ \
		--images-dir $(CACHE_DIR) \
		--template-pdf $(EXAM_DIR)/$*-01.template-sheet.pdf \
		--student-sheets-pdf $(STUDENT_SHEET_DIR)/$*.student-sheets.pdf \
		--padding=$(PADDING) \
		--threshold=$(THRESHOLD) \
		--confidence-ratio=$(CONF_RATIO)

# Rule to generate the final graded CSV file
$(EXAM_DIR)/%.graded-results.csv: $(EXAM_DIR)/%.results.csv $(EXAM_DIR)/%.keys.csv
	@echo "--> Grading exams..."
	$(PYTHON) $(MCQ_SCRIPTS_DIR)/grade_exams.py \
		--answers-csv $(EXAM_DIR)/$*.results.csv \
		--keys-csv $(EXAM_DIR)/$*.keys.csv \
		--output-csv $@

# --- NEW: Secondary Expansion for Dynamic Dependencies ---
# This enables advanced pattern matching for our one-to-many dependency.
.SECONDEXPANSION:

# This rule links any .tex file (e.g., exams/EE1-01.tex) back to its
# master stamp file (e.g., exams/EE1.stamp).
#$(EXAM_DIR)/%.tex $(EXAM_DIR)/%.keys.csv: $$(dir $$@)$$(firstword $$(subst -, ,$$*)).stamp
$(EXAM_DIR)/%.tex: $$(dir $$@)$$(firstword $$(subst -, ,$$*)).stamp
	@# This rule has no commands. It's only here to establish the dependency.

# Temporary rule because of a bug in the exam generation
$(EXAM_DIR)/%.keys.csv: exams2/%.keys.csv
	@echo "--- Copying answer key"
	@cp -a exams2/$*.keys.csv $@

$(EXAM_DIR)/%.student.pdf $(EXAM_DIR)/%.template-sheet.pdf $(EXAM_DIR)/%.answers-only.pdf: $(EXAM_DIR)/%.pdf
	@echo "--- Preparing Printable Version for $< ---"
	@$(PREPARE_SCRIPT) $<

$(EXAM_DIR)/%.student-all.pdf: $(patsubst %,$(EXAM_DIR)/%.student.pdf,$(wildcard $(EXAM_DIR)/$*-*.pdf))
	@echo "--- Collating all student versions for $* ---"
	@$(PDFUNITE) $^ $@
	@echo "--> Created collated file $@"

$(STUDENT_SHEET_DIR)/EE1.student-sheets.pdf: $(STUDENT_SHEET_DIR)/EE1.student-sheets-0.pdf $(STUDENT_SHEET_DIR)/bcc222.2025-1.student-list.csv
	@echo "--- Annotating student sheets with their names for $* ---"
	@$(PYTHON) $(SCRIPT_DIR)/prepare_annotation_data.py \
		--file $(STUDENT_SHEET_DIR)/bcc222.2025-1.student-list.csv \
		--col1 2 --col2 3 \
		--sort-col 8 \
		--start-row 7 \
		--glue " - " | \
	$(SCRIPT_DIR)/pdf-annotator.sh \
		--input-file $(STUDENT_SHEET_DIR)/EE1.student-sheets-0.pdf \
		--text-file - \
		--output-file $@

$(STUDENT_SHEET_DIR)/EET.student-sheets.pdf: $(STUDENT_SHEET_DIR)/EET.student-sheets-0.pdf $(STUDENT_SHEET_DIR)/bcc222.2025-1.student-list.csv
	@echo "--- Annotating student sheets with their names for $* ---"
	@$(PYTHON) $(SCRIPT_DIR)/prepare_annotation_data.py \
		--file $(STUDENT_SHEET_DIR)/bcc222.2025-1.student-list.csv \
		--col1 2 --col2 3 \
		--sort-col 9 \
		--start-row 7 \
		--glue " - " | \
	$(PYTHON) $(SCRIPT_DIR)/pdf-annotator.sh \
		--input-file $(STUDENT_SHEET_DIR)/EET.student-sheets-0.pdf \
		--text-file - \
		--output-file $@


# --- Cleanup Rule ---
clean:
	@echo "--> Cleaning up generated files..."
	@rm -rf $(GEN_DIR)
	@rm -f $(EXAM_DIR)/*.tex $(EXAM_DIR)/*.pdf $(EXAM_DIR)/*.csv $(EXAM_DIR)/*.stamp
	@rm -rf $(EXAM_DIR)/$(BUILD_DIR)
	@latexmk -C
	@echo "--> Done."