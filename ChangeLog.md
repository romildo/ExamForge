# ChangeLog

All notable changes to ExamForge and its specification will be documented in this file.

- Tooling (Haskell executables: `examforge`, `exam-assembler`, `examforge-mockgen`) uses a Cabal version number (currently `0.1.0.0` in `ExamForge.cabal`).
- The YAML format is separately versioned as the **ExamForge Specification** (current: **v3.1**).

Until the first tagged release is cut, all changes live under the **[Unreleased]** section.

---

## [Unreleased]

### Status

Current development state:

- Tools: development build (Cabal `version: 0.1.0.0`), not yet officially released.
- Spec: **ExamForge Specification v3.1** (`SPECIFICATION.md`).

### Added

- **`examforge-mockgen` CLI tool:** A new application to generate random, synthetic question banks and exam configurations to stress-test constraint algorithms.
- **Semantic Groups Implementation:** The `exam-assembler` now features a stateful constraint solver that safely rotates overlapping semantic groups across infinite variant streams without violating quotas.

### Changed

- **Native Haskell Parameters:** The `parameters` values in the generator are now parsed directly as Haskell `String` expressions rather than JSON `Value` ASTs, allowing for native GHC type-inference and complex Haskell types.
- **Deterministic Assembly:** Hardcoded an initial seed for the random generator in the assembler to ensure strictly deterministic exam variants across builds.
- **Internal Refactoring:** Renamed the `ExamConfig` module to `ExamForge.ExamConfig` for better namespace hygiene.

### Spec (v3.1)

- **Semantic Group Constraints:** Introduced `selection.semantic_groups` to the Exam Configuration schema. This allows setting multi-dimensional, regex-based maximum quotas for specific conceptual groups across exam variants.
- **Parameter Types:** Updated the Question Template schema to mandate that `parameters` values must be valid Haskell strings.
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
