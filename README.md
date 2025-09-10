# ExamForge 🚀

ExamForge is a command-line tool written in Haskell for generating multiple, unique versions of exams from a flexible YAML-based question bank. It is driven by declarative configuration files that specify everything about the exam, from the header and instructions to the question selection criteria.

It is designed for educators who need to create exams with parameterized questions, ensuring that each version is different but fair. The system automatically generates high-quality, printable PDF exams and a CSV answer key for automated grading.

## Features

* **Declarative Exam Configuration:** Define entire exams, including metadata, question sources, and filtering rules, in clean, reusable YAML files.
* **Flexible Question Specification:** Define questions, parameters, and computational logic in a separate, human-readable YAML format.
* **Parameterized Questions:** Create countless variations of a single question template using variables and Haskell expressions.
* **Deterministic Variety:** Ensures all question variations are used over time through a unique cycling and shuffling algorithm.
* **PDF & LaTeX Output:** Generates professional, ready-to-compile `.tex` files and print-ready `.pdf` files, including a scannable answer sheet.
* **CSV Answer Key:** Automatically produces a CSV file with the correct answers for all generated exam versions.

## Workflow

The system now works in a unified, config-driven process automated by a `Makefile`:

1.  **Define:** Create a YAML file in the `configs/` directory to specify the exam you want to build. This file points to the question bank files (`.yml` files in the `questions/` directory) and sets all assembly options.
2.  **Build:** Run a single `make` command. This command orchestrates the entire workflow:
    * **`examforge`** is called to read your question banks and compile them into an efficient Haskell module.
    * **`exam-assembler`** is called with your exam configuration file. It uses the generated module to select questions, create the specified number of unique exam versions (`.tex` files), and produce the CSV answer key.
    * **`latexmk`** compiles the `.tex` files into final `.pdf` documents.

## Prerequisites

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
* [Cabal (Haskell build tool)](https://www.haskell.org/cabal/)
* A TeX distribution (e.g., TeX Live) with `lualatex` and `latexmk`.
* PDF manipulation tools: `qpdf` and `pdfunite` (from `poppler-utils`).

## Build Instructions

To build both executables (`examforge` and `exam-assembler`), run the following command from the project's root directory:

```bash
cabal build
````

## Usage with `Makefile`

The recommended way to generate exams is by using the provided `Makefile`.

### 1\. Create an Exam Configuration File

Create a new `.yml` file in the `configs/` directory (e.g., `configs/EE1.yml`). See the **[Exam Configuration Specification](https://www.google.com/search?q=SPECIFICATION.md)** for details.

### 2\. Generate the Exam PDFs

Run `make` with the target PDF you want to create. The Makefile will find the corresponding config file and run the entire build process automatically.

```bash
# Build all versions of the exam defined in configs/EE1.yml
make exams/EE1-01.pdf

# Build and then open the first version for preview
make exams/EE1-01.pdf PREVIEW=yes
```

### 3\. Generate Final Printable PDFs

The `Makefile` also provides targets for creating print-ready and collated PDFs.

```bash
# Create a single, print-ready PDF for a specific version (e.g., EE1-01)
make exams/EE1-01-student.pdf

# Create one large PDF containing all student-ready versions of the EE1 exam
make exams/EE1-allstudents.pdf

# Create one PDF containing just the answer sheets for all versions of the EE1 exam
make exams/EE1-answersheets.pdf
```

## YAML Specification

The project uses two types of YAML files:

1.  **[Exam Configuration Files](https://www.google.com/search?q=SPECIFICATION.md%23exam-configuration-file)**: High-level files that define *what* an exam is.
2.  **[Question Template Files](https://www.google.com/search?q=SPECIFICATION.md%23question-template-file)**: Low-level files that define the *content* of the questions.

Please see the **[Full Specification Document](https://www.google.com/search?q=SPECIFICATION.md)** for details on both.
