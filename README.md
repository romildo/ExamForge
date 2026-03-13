<!-- File: README.md -->

# ExamForge 🚀

ExamForge is a command-line toolchain for generating parameterized, LaTeX-based exams from declarative YAML specifications. A single exam configuration file controls everything from the header and instructions to question selection criteria and semantic grouping.

Powered by a **Polyglot Architecture**, ExamForge allows instructors to write question parameters and computations in their language of choice (Python, C, C++, or Haskell) while still leveraging the full typesetting power of LaTeX. It is designed for instructors who want to produce multiple exam versions plus answer keys in a repeatable, dynamic way.

---

## Status

ExamForge is currently **pre-1.0** and under active development.

- The YAML specification is versioned (current: **Specification v4.0**).
- CLI behavior and internal implementation may change.
- Changes to the tools and the specification will be recorded in `ChangeLog.md`.

For the normative YAML format, see [`SPECIFICATION.md`](SPECIFICATION.md).

---

## Architecture & System Boundaries

ExamForge v4.0 operates as a **single, unified executable** (`examforge`) that manages the entire pipeline without requiring ahead-of-time Haskell compilation for your question banks:

1. **The Polyglot Orchestrator:** Reads question bank YAML files, detects the target language (e.g., Python, C++), dynamically generates the execution script, and runs it securely in a temporary directory to calculate the variants.
2. **The Assembler:** Reads the exam configuration, applies selection filters (tags and semantic groups), and securely pseudo-randomizes the generated variants to produce `.tex` documents and `.csv` answer keys.

## Features

- **Declarative Exam Configuration:** Define metadata, question bank locations, assembly options, evaluator overrides, and selection rules in a single YAML file.

- **Flexible Question Specification:** Define questions, parameters, tags, and computational logic in separate, human-readable YAML files.

- **Parameterized Questions:** Generate many variants of a conceptual question using parameter sets and small expressions in a programming language.

- **Polyglot Evaluators:** Write computational logic and format strings using native Python, C, C++, or Haskell. ExamForge automatically handles the bridging, compilation, and JSON serialization.

- **Unified CLI Interface:** A single tool to dry-run your question banks (`check`), assemble full exams (`build`), and generate synthetic datasets (`mock`).

- **Semantic Group Rotation:** Ensure comprehensive coverage without redundancy. Group related questions by tags (e.g., `grupo-hof-conceito`) and constrain the assembler to pick at most $N$ questions from that group per exam, rotating choices fairly across versions.

- **Balanced Variant Generation:** Each question is expanded into an infinite stream of variants. Versions are built by cycling through these streams and shuffling answers deterministically (based on a random seed).

- **PDF & LaTeX Output:** Produces `.tex` sources and final `.pdf` files using `latexmk`/`lualatex`, including student-ready versions and answer-sheets-only PDFs.

- **CSV Answer Key** Generates a `.csv` answer key for all versions of an exam, suitable for automated grading or downstream tooling.

---

## Quickstart

### 1. Install prerequisites

You’ll need:
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) (9.4+)
- [Cabal](https://www.haskell.org/cabal/)
- A TeX distribution with `lualatex` and `latexmk` (e.g., TeX Live)
- PDF utilities:
  - `qpdf`
  - `pdfunite` (usually from `poppler-utils`)
- **Target Languages (Optional but recommended):** `python3`, `gcc`, `g++` (depending on which languages you use in your question banks).

### 2. Build the tool

From the project root:

```bash
cabal build
```

This builds the unified `examforge` executable.

---

## Workflow

ExamForge is designed around configuration files describing exams, and question bank files, containing questions templates that can be included in an exam. Both are YAML files.

1. **Define question banks**
   Create one or more YAML question banks (for example, `questions/chapter1.yml`).
   This file specifies the details of a set of questions, possibly including:

   * question metadata, like:
     - a unique identifier
     - a title
     - a subject
     - a list of tags
   * a computation block in a given programming language
   * a list of named parameter sets with values that can be used in the computation block
   * a question template text, with support for interpolation of expression
   * a list of answer choices templates, also with suppport for interpolation

2. **Define an exam configuration**
   Create a YAML exam configuration (for example, `configs/exam1.yml`).
   This file may specify, among other things:

   * exam header metadata
   * list of question banks
   * assembly options
   * question selection rules
   * additional content (e.g. instructions for the student)

   See [`SPECIFICATION.md`](SPECIFICATION.md) for the details.

3. **Assemble exams** (`examforge`)
   ExamForge does the following to assemble exams:
   
   * Reads an exam configuration.
   * Parses the questions from the question banks.
   * Applies the selection rules to filter questions by tags.
   * Uses the parameterized variants for each question and constructs an infinite stream of variants per question.
   * For each exam version:

     * Picks one variant per question (cycling through the streams).
     * Possibly randomly shuffles the answer options.
   * Writes:

     * LaTeX exam files (for example, `exams/<BaseName>-NN.tex`)
     * CSV key (for example `exams/<BaseName>-NN.keys.csv`)

   where `<BaseName>` is the basename of your config file (e.g. `exam1` for `configs/exam1.yml`).

4. **Compile PDFs** (`latexmk`)
   Finally a LaTeX installation can be used to compile the generated LaTeX files (`.tex`). This can be accomplished with a tool like `latexmk`, or directly running a LaTeX compiler, like `lualatex`.

## Example

### Exam Configuration

``` yaml
# File: configs/test-exam.yml

header:
  institution: "My University"
  course: "ExamForge Development"
  professor: "John Joe"
  semester: "2026/1"
  title: "Testing ExamForge features"

question_banks:
  - "questions/test-python.yml"
  - "questions/test-c.yml"
  - "questions/test-haskell.yml"
  - "questions/test-cpp.yml"

default_language: "python"

evaluators:
  python:
    run: "python3 %f"

assembly_options:
  versions: 2
  show_id: true
  show_tags: false
  hide_subjects: false
  shuffle_questions: true
  seed: 42

selection:
  include_tags:
    - "^python-test$"
    - "^c-test$"
    - "^haskell-test$"
    - "^cpp-test$"
  exclude_tags:
    - "level:easy"
  semantic_groups: []

content:
  instructions: |
    Good luck!
  latex_preamble: ""
```

### Question Bank

``` yaml
# File: questions/test-python.yml

- id: py-math-01
  title: "Juros Compostos"
  format: "latex"
  language: "python"
  selection_type: "any"
  subject: "Matemática Financeira"
  tags: ["python-test", "matematica", "dificuldade-media"]
  delimiters: 
    start: "@@"
    end: "@@"
  
  parameters:
    rows:
      - { principal: "1000", rate: "0.05", years: "3" }
      - { principal: "2500", rate: "0.08", years: "5" }
      
  computations: |
    # Python native calculations
    amount = principal * ((1 + rate) ** years)
    
    # Common student mistakes for distractors
    simple_interest_amount = principal * (1 + (rate * years))
    wrong_power = principal * ((1 + rate) ** (years - 1))
    
  question: |
    Se um capital de R\\$ @@principal:,.2f@@ for investido a uma taxa de juros compostos
    de @@rate:.2f@@ ao ano, qual será o montante final após @@years@@ anos?
  
  answers:
    - correct: "R\\$ @@amount:,.2f@@"
    - incorrect: "R\\$ @@simple_interest_amount:,.2f@@"
    - incorrect: "R\\$ @@wrong_power:,.2f@@"
    - incorrect: "R\\$ @@principal:,.2f@@"

- id: py-physics-01
  title: "Cinemática Básica"
  format: "latex"
  language: "python"
  selection_type: "any"
  subject: "Física"
  tags: ["python-test", "fisica", "dificuldade-facil"]
  delimiters: 
    start: "{{"
    end: "}}"
  
  parameters:
    rows:
      - { dist: "150", time: "2" }
      - { dist: "320", time: "4" }
      
  computations: |
    speed = dist / time
    wrong_mul = dist * time
    wrong_inv = time / dist
    
  question: |
    Um carro percorre uma distância de {{dist}} km em {{time}} horas.
    Qual é a sua velocidade média?
  
  answers:
    - correct: "{{speed:.1f}} km/h"
    - incorrect: "{{wrong_mul:.1f}} km/h"
    - incorrect: "{{wrong_inv:.3f}} km/h"
    - incorrect: "{{dist:.1f}} km/h"
```

---

## Command-Line Usage

The `examforge` CLI provides three main subcommands: `check`, `build`, and `mock`.

### 1. Checking Question Banks (`check`)

Before assembling a full exam, you can dry-run a question bank to ensure your code executes correctly and formatting is applied.

```bash
# Evaluate an entire question bank
cabal run examforge -- check questions/my-bank.yml

# Evaluate a specific question by ID
cabal run examforge -- check questions/my-bank.yml --id py-math-01

# Debugging: Dump the generated source code (Python, C, Haskell, etc.) to the terminal without running it
cabal run examforge -- check questions/my-bank.yml -i py-math-01 --show-script

# Debugging: Output the evaluated JSON Intermediate Representation (IR) bridging the script and Haskell AST
cabal run examforge -- check questions/my-bank.yml -i py-math-01 --show-json
```

### 2. Assembling Exams (`build`)

To assemble a complete exam suite, pass your configuration file to the `build` command.

```bash
cabal run examforge -- build configs/EE1.yml
```

This will process all linked question banks and generate:

* LaTeX exam files: `exams/EE1-01.tex`, `exams/EE1-02.tex`, …
* CSV key: `exams/EE1.keys.csv`

You can then compile the PDFs using `make exams/EE1-01.pdf` or manually via `latexmk`.

### 3. Generating Mock Data (`mock`)

If you are modifying ExamForge's selection algorithms or developing new constraint features, you can use the `mock` command to generate large synthetic datasets to stress-test the Semantic Group Rotation algorithm.

```bash
cabal run examforge -- mock \
  --bank-out questions/mock-bank.yml \
  --config-out configs/mock-config.yml \
  --num-questions 200 \
  --num-groups 30 \
  --versions 15 \
  --max-per-group 2
```

**Mock Options:**
| Flag | Short | Description | Default |
| --- | --- | --- | --- |
| `--bank-out` |  | **(Required)** Output path for the generated question bank YAML. |  |
| `--config-out` |  | **(Required)** Output path for the generated exam config YAML. |  |
| `--num-questions` | `-q` | Total number of questions to generate in the bank. | |
| `--num-groups` | `-g` | Total number of available semantic group tags in the universe. | |
| `--versions` | `-v` | Number of exam variations to specify in the generated config. | |
| `--group-prefix` | `-p` | Prefix for semantic group tags. | `grupo-` |
| `--max-per-group` | `-m` | Maximum questions allowed per semantic group per variant. | `1` |
| `--seed` | `-s` | Random seed for deterministic generation. | `42` |

---

## YAML Specification

ExamForge uses **two types of YAML files**:

1. **Exam Configuration Files:** High-level files describing the exam (metadata, question banks, assembly options, evaluator overrides, selection rules, etc.).
2. **Question Template Files:** Question-level definitions (IDs, tags, format, language, parameters, text, and answers).

The YAML format is defined normatively in [`SPECIFICATION.md`](SPECIFICATION.md). That document is the authoritative reference for exam configuration and question templates.
