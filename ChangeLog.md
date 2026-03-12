# ChangeLog

All notable changes to ExamForge and its specification will be documented in this file.

- Tooling (Haskell executable: `examforge`) uses a Cabal version number (currently `0.1.0.0` in `ExamForge.cabal`).
- The YAML format is separately versioned as the **ExamForge Specification** (current: **v4.0**).

Until the first tagged release is cut, all changes live under the **[Unreleased]** section.

---

## [Unreleased]

### Status

Current development state:

- Tools: development build (Cabal `version: 0.1.0.0`), not yet officially released.
- Spec: **ExamForge Specification v4.0** (`SPECIFICATION.md`).

### Added (v4.0 Polyglot Architecture)

- **Polyglot Runtime Engine:** ExamForge can now evaluate parameterized questions using external scripts. Native support is built-in for Python, C, C++, and Haskell.
- **Unified CLI:** The toolchain has been consolidated into a single `examforge` executable with powerful subcommands (`build`, `check`, `mock`). 
- **AST Templating Engine:** Replaced brittle regex-based substitution with a robust AST parser (`ExamForge.Template`) that safely extracts variables and format specifiers.
- **Developer Debugging Tools:** Added a `--show-script` (`-s`) flag to the `check` command to dump the dynamically generated evaluator source code (e.g., raw C or Python) directly to the terminal for inspection.
- **Improved DX Output:** The CLI now uses `pretty-simple` to format deeply nested JSON/Haskell records into highly readable, indented output during dry-runs.
- **JSON Intermediate Representation (IR):** Established a strict JSON schema for evaluators to pass computed variants back to the Haskell assembler securely.

### Added (v3.1 features)

- **Custom LaTeX Preamble:** The LaTeX formatter now supports injecting raw LaTeX code directly into the document preamble, enabling question-specific packages (e.g., `\usepackage{menukeys}`).
- **OMR Registration Grid:** The LaTeX formatter and `provastyle.sty` now support generating a dynamic, machine-readable bubbling grid for student registration numbers (matrícula) on the answer sheet. 
- **Semantic Groups Implementation:** The assembler features a stateful constraint solver that safely rotates overlapping semantic groups across infinite variant streams without violating quotas.
- **Configurable Randomness:** The assember now respects a user-defined random seed from the configuration file for fully reproducible exam generation.

### Changed

- **No Ahead-of-Time Compilation:** The assembler no longer generates and compiles a static `Generated.Questions.hs` file. All evaluations are handled dynamically via system processes and temporary directories.
- **CLI Consolidation:** The separate `exam-assembler` and `examforge-mockgen` applications were converted into library modules and integrated as `examforge build` and `examforge mock`.
- **Native Parameters:** The `parameters` values in the generator are now parsed directly as string expressions rather than JSON `Value` ASTs, allowing for native types in the language used for computations.
- **Deterministic Assembly:** Hardcoded an initial seed for the random generator in the assembler to ensure strictly deterministic exam variants across builds when no seed is provided in the configuration.
- **Internal Refactoring:** Renamed the `ExamConfig` module to `ExamForge.ExamConfig` for better namespace hygiene.

### Removed

- Removed the `exam-assembler` and `examforge-mockgen` standalone executables from the `.cabal` file.
- Removed the ZeroMQ / Jupyter kernel communication architecture in favor of standard process execution.

### Spec (v4.0 & v3.1)

- **Polyglot Configuration:** Added `default_language` and `evaluators` to the Exam Configuration schema to define build/run commands for external scripts.
- **Language Overrides:** Added `language` to the Question Template schema to allow per-question evaluator overrides.
- **Parameter Types:** Added `parameters.types` to the Question Template schema to support static type declarations for compiled languages like C and C++.
- **Native Format Rule:** Delimiters now support native format specifiers (e.g., `{{var:%.2f}}` for C, or `@@var:.2f@@` for Python).
- **LaTeX Preamble Configuration:** Added an optional `latex_preamble` string field to the `content` block. This allows authors to define custom macros or import packages directly from the YAML configuration.
- **Registration Digits Configuration:** Added an optional `registration_digits` integer field to `assembly_options`. If provided and greater than `0`, it triggers the generation of the OMR registration grid on the answer sheet.
- **Semantic Group Constraints:** Introduced `selection.semantic_groups` to the Exam Configuration schema. This allows setting multi-dimensional, regex-based maximum quotas for specific conceptual groups across exam variants.
- **Random Seed Configuration:** Added an optional `seed` integer field to `assembly_options`. When provided, it explicitly initializes the PRNG to guarantee reproducible exam and variant selections.
- **Assembly Options Clarification:** Fixed the documentation for `shuffle_questions` to explicitly define its default value, removing the ambiguity of "implementation dependent".
- **Documentation Polish:** Cleaned up `README.md` and `SPECIFICATION.md` for clarity and removed overly long real-world examples in favor of concise structural examples.

---

## [v3.0 - Previous Baseline]

### Added

- Two Haskell CLI executables:
  - `examforge` – reads an exam configuration, expands `question_banks`, parses question templates, and generates the `Generated.Questions` Haskell module under `generated/`.
  - `exam-assembler` – reads the same configuration, filters questions via `selection`, assembles parameterized variants, and generates:
    - LaTeX exam files under `exams/`
    - a CSV answer key file `<BaseName>.keys.csv`.

- Makefile-based workflow:
  - Targets to:
    - build specific exam versions (`exams/EE1-01.pdf`, etc.),
    - generate student-ready PDFs and “all students” aggregate PDFs,
    - generate answer-sheet-only PDFs,
    - clean generated artifacts.
  - Automation of:
    - running `examforge`,
    - running `exam-assembler`,
    - invoking `latexmk` with `lualatex`,
    - basic PDF post-processing with `qpdf` and `pdfunite`.

- LaTeX formatter:
  - Renders exams with header metadata and optional instructions.
  - Controls visibility of question IDs, tags, and subject per `assembly_options`.

### Spec (v3.0)

- Defined a **single exam configuration schema** with the following top-level keys:

  - `header` (required) – metadata for the exam.
  - `question_banks` (required) – list of file paths/glob patterns to question template files.
  - `assembly_options` (optional) – number of versions, printing of IDs/tags, and subject visibility.
  - `selection` (optional) – include/exclude tag filters using POSIX regular expressions.
  - `content` (optional) – extra instructions printed in the exam.

- Defined the **question template schema**:

  - Required keys:
    - `id`, `title`, `format`, `selection_type`, `question`, `answers`.
  - Optional keys:
    - `subject`, `tags`, `parameters`, `computations`, `delimiters`.

- Clarified the semantics of:

  - `selection_type` (`"any"` vs `"all"`).
  - `include_tags` / `exclude_tags`:
    - behavior when lists are empty,
    - matching via regex on individual tags.
  - `assembly_options` defaults and behavior when omitted.
  - `content.instructions` defaulting to the empty string.

- Documented the basic variant generation and assembly algorithm:

  - Parameterized question templates.
  - Infinite variant streams via cycling/shuffling.
  - Per-version answer shuffling.
  - Output naming conventions for `.tex` and `.keys.csv`.
