# ExamForge 🚀

ExamForge is a command-line tool written in Haskell for generating multiple, unique versions of exams from a flexible YAML-based question bank.

It is designed for educators who need to create exams with parameterized questions, ensuring that each version is different but fair. The system automatically generates LaTeX source files for the exams and a CSV answer key for automated grading.

## Features

* **Flexible YAML Specification:** Define questions, parameters, and computational logic in a clean, human-readable format.
* **Parameterized Questions:** Create countless variations of a single question template using variables and Haskell expressions.
* **Deterministic Variety:** Ensures all question variations are used over time through a unique cycling and shuffling algorithm.
* **LaTeX Output:** Generates professional, ready-to-compile `.tex` files for high-quality exam printing, including a scannable answer sheet.
* **CSV Answer Key:** Automatically produces a CSV file with the correct answers for all generated exam versions, ready for integration with auto-grading tools.

## Workflow

The system works in a two-step process:

1.  **Generate (`examforge`):** First, you run the `examforge` executable. It reads all your `.yml` question files and compiles them into a single, efficient Haskell module (`Generated/Questions.hs`). This step is done once, or whenever you update your question bank.
2.  **Assemble (`exam-assembler`):** Second, you run the `exam-assembler` executable. It uses the module generated in the first step to create the final exam documents (`.tex` files) and the CSV answer key. You can run this step anytime you need to produce a new set of exams.

## Prerequisites

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
* [Cabal (Haskell build tool)](https://www.haskell.org/cabal/)

## Build Instructions

To build both executables, run the following command from the project's root directory:

```bash
cabal build
````

## Usage

### 1\. Generating the Question Module

Run the `examforge` executable, passing it the path to your YAML files. You can use shell globs to specify multiple files.

```bash
# Process all .yml files in the 'questions' directory
cabal run examforge -- questions/*.yml

# Specify a different output directory for the generated Haskell module
cabal run examforge -- --output build questions/*.yml
```

### 2\. Assembling the Final Exams

Run the `exam-assembler` executable. It offers several options to filter questions and control the output.

```bash
# Generate 3 versions of the exam using all available questions
cabal run exam-assembler -- --versions 3

# Generate 5 versions, saving the .tex files to 'final-proofs' and creating a CSV key
cabal run exam-assembler -- --versions 5 --out-dir final-proofs --csv-file answers.csv

# Generate 2 versions, using only questions tagged with "lambda" or "haskell"
cabal run exam-assembler -- --versions 2 --tag "lambda|haskell"

# See all available options
cabal run exam-assembler -- --help
```

## YAML Specification

For a detailed guide on how to write question template files, please see the **[YAML Specification](https://www.google.com/search?q=SPECIFICATION.md)**.
