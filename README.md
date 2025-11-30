<!-- File: README.md -->

# ExamForge 🚀

ExamForge is a command-line toolchain written in Haskell for generating parameterized, LaTeX-based exams from declarative YAML specifications. A single exam configuration file controls everything from the header and instructions to question selection criteria.

It is designed for instructors who are comfortable with LaTeX, YAML, and basic command-line tools, and who want to produce multiple exam versions plus answer keys in a repeatable way.

---

## Status

ExamForge is currently **pre-1.0** and under active development.

- The YAML specification is versioned (current: **Specification v3.0**).
- CLI behavior and internal implementation may change.
- Changes to the tools and the specification will be recorded in `ChangeLog.md`.

For the normative YAML format, see [`SPECIFICATION.md`](SPECIFICATION.md).

---

## Features

- **Declarative Exam Configuration**  
  Define entire exams in YAML: metadata, question bank locations, assembly options, and selection rules.

- **Flexible Question Specification**  
  Define questions, parameters, tags, and computational logic in separate, human-readable YAML files.

- **Parameterized Questions**  
  Generate many variants of a conceptual question using parameter sets and small Haskell expressions.

- **Balanced Variant Generation**  
  Each question is expanded into an infinite stream of variants, and versions are built by cycling through these streams and shuffling answers. The selection/shuffling algorithm is deterministic *given a random seed*; the current executables may obtain their seed from the runtime system.

- **PDF & LaTeX Output**  
  Produces `.tex` sources and final `.pdf` files using `latexmk`/`lualatex`, including student-ready versions and answer-sheets-only PDFs.

- **CSV Answer Key**  
  Generates a `.csv` answer key for all versions of an exam, suitable for automated grading or downstream tooling.

---

## Quickstart

### 1. Install prerequisites

You’ll need:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)
- A TeX distribution with `lualatex` and `latexmk` (e.g. TeX Live)
- PDF utilities:
  - `qpdf`
  - `pdfunite` (usually from `poppler-utils`)

Install these using your system’s package manager.

### 2. Build the tools

From the project root:

```bash
cabal build
````

This builds two executables:

* `examforge` – compiles question bank YAML files into a Haskell module.
* `exam-assembler` – reads an exam configuration and generates `.tex` exams and a `.csv` answer key.

---

## Workflow

ExamForge is designed around a **unified, config-driven workflow** orchestrated by the provided `Makefile`.

1. **Define**
   Create a YAML exam configuration in `configs/` (for example, `configs/EE1.yml`).
   This file specifies:

   * Header metadata (`header`)
   * Question banks (`question_banks`)
   * Assembly options (`assembly_options`)
   * Selection rules (`selection`)
   * Additional content (`content`, e.g. instructions for the student)

   See [`SPECIFICATION.md`](SPECIFICATION.md) for the full schema.

2. **Generate code** (`examforge`)
   ExamForge reads the exam configuration, expands the `question_banks` glob patterns, and parses all matching question bank `.yml` files. It then generates a Haskell module:

   * Module name: `Generated.Questions`
   * Path: `generated/Generated/Questions.hs`
   * Export: `questionPool :: [Question]` (internal representation)

3. **Assemble exams** (`exam-assembler`)
   The assembler:

   * Loads the same exam configuration.
   * Applies the `selection` rules to filter questions by tags.
   * Uses the parameterized variants for each question and constructs an infinite stream of variants per question.
   * For each exam version:

     * Picks one variant per question (cycling through the streams).
     * Randomly shuffles the answer options.
   * Writes:

     * LaTeX exam files: `exams/<BaseName>-NN.tex`
     * CSV key: `exams/<BaseName>.keys.csv`

   where `<BaseName>` is the basename of your config file (e.g. `EE1` for `configs/EE1.yml`).

4. **Compile PDFs** (`latexmk`)
   Finally, `latexmk` runs `lualatex` as many times as needed to produce the final PDFs.

All of this is wrapped up into a small number of `make` targets.

---

## Using the `Makefile`

The recommended way to interact with ExamForge is via the provided `Makefile`.

### 1. Create an Exam Configuration File

Create a new `.yml` file under `configs/` (e.g. `configs/EE1.yml`) describing your exam.
See [`SPECIFICATION.md`](SPECIFICATION.md#1-exam-configuration-file) for the exact schema.

### 2. Generate exam PDFs

From the project root:

```bash
# Build all versions of the exam defined in configs/EE1.yml
make exams/EE1-01.pdf

# Build and then open the first version for preview
make exams/EE1-01.pdf PREVIEW=yes
```

The Makefile will:

1. Run `examforge` on `configs/EE1.yml`.
2. Run `exam-assembler` on `configs/EE1.yml`.
3. Use `latexmk` to compile the generated `.tex` files under `exams/`.

### 3. Student-ready PDFs and answer sheets

Additional convenience targets:

```bash
# Create a single, print-ready PDF for a specific version (e.g., EE1-01)
make exams/EE1-01-student.pdf

# Create one large PDF containing all student-ready versions of the EE1 exam
make exams/EE1-allstudents.pdf

# Create a PDF containing just the answer sheets for all versions of the EE1 exam
make exams/EE1-answersheets.pdf
```

These targets are implemented in the Makefile using `qpdf`, `pdfunite`, and helper scripts under `scripts/`.

---

## Direct CLI usage (advanced)

If you prefer not to use the `Makefile`, you can invoke the tools directly.

```bash
# 1. Generate the Haskell question pool module from a config
cabal run examforge -- configs/EE1.yml

# 2. Assemble exams for the same config (requires the generated module)
cabal run exam-assembler -- configs/EE1.yml
```

This will:

* Create `generated/Generated/Questions.hs`
* Generate `exams/EE1-01.tex`, `exams/EE1-02.tex`, … according to `assembly_options.versions`
* Generate `exams/EE1.keys.csv` with the correct options for each version

You can then run `latexmk` manually on the `.tex` files if desired.

---

## YAML Specification

ExamForge uses **two types of YAML files**:

1. **[Exam Configuration Files](SPECIFICATION.md#1-exam-configuration-file)**
   High-level files describing the exam: metadata, question banks, assembly options, selection rules, and extra content.

2. **[Question Template Files](SPECIFICATION.md#2-question-template-file)**
   Question-level definitions: question IDs, text, parameters, tags, and answers.

The YAML format is defined normatively in [`SPECIFICATION.md`](SPECIFICATION.md).
That document is the authoritative reference for configuration and question templates.
