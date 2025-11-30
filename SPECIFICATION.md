<!-- File: SPECIFICATION.md -->

# ExamForge Specification (v3.0)

`ExamForge` uses two types of YAML files to manage the exam generation process:

1. **Exam Configuration Files** – high-level files that define what an exam is, which questions it uses, and how it should be assembled.
2. **Question Template Files** – lower-level files that define the actual content (text, parameters, answers) of individual questions.

---

## 0. Scope and versioning

This document is the **normative specification** of the ExamForge YAML format, version **v3.0**.

- All configuration (`configs/*.yml`) and question template (`questions/*.yml` / `*.yaml`) files are expected to conform to this version unless otherwise stated.
- Future changes to the YAML format will:
  - update this document to a new version (e.g. `v3.1`),
  - and be recorded in `ChangeLog.md`.

The current Haskell codebase is intended to be compatible with **Specification v3.0**.

---

## 1. Exam Configuration File

The exam configuration file is the main *recipe* for an exam. It is the single input to the `exam-assembler` executable and is also read by `examforge` to locate question banks.

### 1.1 Top-Level Structure

The root of the file is a single YAML object:

```yaml
# File: configs/EE1.yml

header: { ... }
question_banks: [ ... ]
assembly_options: { ... }  # Optional
selection: { ... }         # Optional
content: { ... }           # Optional
````

### 1.2 Key reference

#### `header` (Object, Required)

Metadata for the PDF title page and answer sheet.

* `institution` (String)
* `course` (String)
* `professor` (String)
* `semester` (String)
* `title` (String)

All of these fields are required.

#### `question_banks` (List of Strings, Required)

A list of file paths (or glob patterns) pointing to question bank YAML files.

* Paths are resolved relative to the project root (as used by the `Makefile`).
* Glob patterns are expanded using `System.FilePath.Glob.glob`.

Example:

```yaml
question_banks:
  - "questions/EE1.cap-01.yaml"
  - "questions/EE1.cap-02.yaml"
  - "questions/EE2.cap-*.yaml"
```

If the patterns match no files, `examforge` still succeeds but simply generates an empty question pool. Subsequent `exam-assembler` runs will report that no questions matched the filters.

#### `assembly_options` (Object, Optional)

Settings controlling how versions of the exam are assembled.

* `versions` (Int)
  Number of distinct exam versions to generate.
  Default: `1`. Must be a positive integer.

* `show_id` (Bool)
  If `true`, the question `id` is printed in the exam.
  Default: `false`.

* `show_tags` (Bool)
  If `true`, the question `tags` are printed in the exam.
  Default: `false`.

* `hide_subjects` (Bool)
  If `true`, the `subject` field is omitted from the exam.
  Default: `false`.

Example:

```yaml
assembly_options:
  versions: 4
  show_id: false
  show_tags: false
  hide_subjects: true
```

If `assembly_options` is omitted entirely, the defaults above are used.

#### `selection` (Object, Optional)

Rules for filtering which questions from the banks are included in the exam.

Example:

```yaml
selection:
  include_tags:
    - "^logic$"
    - "^set-theory$"
  exclude_tags:
    - "bonus"
```

Fields:

* `include_tags` (List of Strings)
  A list of **POSIX regular expressions** (as accepted by `text-regex-tdfa`).

  Semantics:

  * Each question has a list of `tags` (from the question template).
  * If `include_tags` is **empty or omitted**, all questions pass this filter.
  * If `include_tags` is **non-empty**, a question passes this filter iff **at least one** of its tags matches **at least one** of the patterns.

* `exclude_tags` (List of Strings)
  Another list of POSIX regular expressions.

  Semantics:

  * If `exclude_tags` is empty or omitted, no questions are excluded by this rule.
  * If `exclude_tags` is non-empty, a question is excluded if **any** of its tags matches **any** of the patterns.

Both filters are applied per question:

```text
isIncluded = (include_tags empty) OR (some tag matches some include pattern)
isExcluded = (exclude_tags non-empty) AND (some tag matches some exclude pattern)

Question is kept iff isIncluded && not isExcluded.
```

If, after applying all filters, no questions remain, `exam-assembler` prints a warning:

> "No questions matched the filters. No exams generated."

and does not produce `.tex` or `.csv` output.

#### `content` (Object, Optional)

Additional content to be inserted into the exam.

* `instructions` (String)
  A (possibly multi-line) string of instructions printed on the title page.
  Default: `""`.

Example:

```yaml
content:
  instructions: |
    Read the questions carefully.
    No electronic devices are allowed.
    Show all your work.
```

If `content` or `instructions` are omitted, no extra instructions are printed.

---

## 2. Question Template File

Question templates live in separate `.yml` / `.yaml` files, typically under `questions/`.

A single file contains a **list** of one or more question template objects.

### 2.1 File structure

Example:

```yaml
# File: questions/EE1.cap-01.yaml

- id: logic-001
  title: Basic propositional logic
  format: latex
  selection_type: any
  subject: Logic
  tags: ["logic", "intro"]
  parameters: [ ... ]
  computations: |
    let ...
  question: |
    ...
  answers:
    - correct: ...
    - incorrect: ...
    - incorrect: ...
  delimiters:
    start: "{{"
    end: "}}"

- id: logic-002
  ...
```

Each item in the list is a single **Question Template Object**.

### 2.2 Question Template Object

Each object describes one logical question, possibly parameterized into multiple variants.

#### Required keys

* `id` (String)
  A unique identifier for the question, such as `"logic-001"`.
  Uniqueness is expected across all question banks used in a given exam configuration.

* `title` (String)
  A Human-readable title or short description.

* `format` (String)
  The primary output format; currently the codebase assumes `"latex"` and renders questions to LaTeX.

* `selection_type` (String)
  Defines how correctness is evaluated for multiple-choice answers:

  * `"any"` – the question is considered correct if **at least one** of the correct options is marked, and no incorrect option is marked (typical single-answer multiple-choice).
  * `"all"` – the question is correct only if **all and only** the correct options are marked (useful for "select all that apply").

  Internally this is mapped to `SelectionType`:

  * `"any"` → `SelectAny`
  * `"all"` → `SelectAll`

  Any other value is treated as invalid and causes YAML parsing to fail.

* `question` (String)
  The template for the question body (usually LaTeX).
  It may contain parameter references and computed expressions (see `parameters`, `computations`, and `delimiters` below).

* `answers` (List of Objects)
  The answer choices. Each element of the list is an object with exactly one key:

  * `correct`: String – a correct answer option.
  * `incorrect`: String – an incorrect answer option.

  Example:

  ```yaml
  answers:
    - correct: "2"
    - incorrect: "1"
    - incorrect: "3"
    - incorrect: "4"
  ```

  Internally this is parsed into an `Answer` type with a Boolean flag indicating correctness.

#### Optional keys

* `subject` (String)
  Logical grouping for the question (e.g. chapter/section).
  Used for metadata and optional printing; can be hidden with `assembly_options.hide_subjects`.

* `tags` (List of Strings)
  A list of tags used for selection and filtering (e.g. `"logic"`, `"set-theory"`, `"easy"`).
  Defaults to `[]`.

* `parameters` (List of Objects)
  Parameter sets used to create multiple concrete variants of the same question.

  Each element is a key–value map.

  Example:

  ```yaml
  parameters:
    - { a: 2, b: 3 }
    - { a: 4, b: -1 }
    - { a: 5, b: 0 }
  ```

  Values may be numbers, strings, or other YAML scalars; they are exposed to computations as a heterogeneous map (`Map String Value`).

* `computations` (String)
  A block of Haskell code (typically a `let` block) used to derive secondary values from parameters.

  Example:

  ```yaml
  computations: |
    let
      x = -b / a :: Double
      absA = abs a
  ```

  This code is embedded into the generated Haskell module and must type-check in that context. Any compilation error will surface as a normal Haskell compilation error when building the generated module.

* `delimiters` (Object)
  Custom delimiters for inline expressions in `question` and `answers`.

  Structure:

  * `start` (String) – opening delimiter
  * `end` (String) – closing delimiter

  Example:

  ```yaml
  delimiters:
    start: "{{"
    end: "}}"
  ```

  If absent, ExamForge uses its default delimiters (currently `{{` and `}}`). Within the question/answer text, any occurrence of:

  ```text
  {{expression}}
  ```

  is interpreted as a Haskell expression in scope of the current parameters and computations, and its rendered value is substituted into the text.

---

## 3. End-to-end example (parameters + computations)

Below is a minimal but complete example of a parameterized question template using `parameters`, `computations`, and the default `{{ .. }}` delimiters.

```yaml
- id: algebra-001
  title: Linear equation
  format: latex
  selection_type: any
  subject: Algebra
  tags: ["algebra", "equations"]

  parameters:
    - { a: 2, b: 3 }
    - { a: 4, b: -1 }

  computations: |
    let
      x = - (fromIntegral b) / (fromIntegral a) :: Double

  question: |
    Solve the equation ${{a}}x + {{b}} = 0$ for $x$.

  answers:
    - correct:   "$x = {{x}}$"
    - incorrect: "$x = {{b}} / {{a}}$"
    - incorrect: "$x = {{a}} / {{b}}$"
    - incorrect: "$x = {{a}} + {{b}}$"
```

Semantics:

* This template produces two concrete variants, corresponding to the two parameter sets.
* For each parameter set:

  * `a` and `b` are bound.
  * `computations` defines `x` as the solution.
  * The `question` and `answers` are rendered by substituting `{{a}}`, `{{b}}`, and `{{x}}` with their evaluated values.
* During exam assembly, these variants are folded into the variant stream for this logical question and combined with variants from other questions.

---

## 4. Assembly algorithm (informal overview)

Given:

* A configuration file `configs/EE1.yml`.
* A generated question pool `[Question]` from the question templates.

The process is:

1. **Selection**
   `selection` filters the question pool using `include_tags` and `exclude_tags` (as described above).

2. **Variant streams**
   For each remaining question, ExamForge builds a (possibly finite) list of variants produced from:

   * the Cartesian product of `parameters` and `computations`, and
   * the rendered question/answer templates.

3. **Cycling & shuffling**

   * Each question’s finite list of variants is turned into an infinite stream using a `cycleShuffle` function:

     * variants are shuffled using a pseudo-random generator,
     * the shuffled list is concatenated with a recursively shuffled continuation, giving an infinite stream with “fair” reuse.
   * All questions’ streams are combined position-wise so that an exam version is a list of one variant per question.

4. **Versions and output**

   * The number of exam versions is determined by `assembly_options.versions` (default: 1).
   * For `N = versions`, the assembler takes the first `N` combinations from the combined stream.
   * For each version:

     * Each question’s answer choices are shuffled (again using pseudo-randomness).
     * A LaTeX document is generated with header, optional instructions, questions, and choices.
   * Output:

     * `exams/<BaseName>-01.tex`, `exams/<BaseName>-02.tex`, …
     * `exams/<BaseName>.keys.csv` with the sequence of correct answers for each version.

The algorithm is **deterministic for a given random seed**, but the executables currently obtain their initial seed from the runtime system (`newStdGen`). Running the same configuration multiple times may thus produce different orderings of variants and answers. If strict reproducibility is needed, you should treat the generated `.tex` files as the stable artifact (e.g. commit them to version control).

---

## 5. Validation and error handling

ExamForge enforces a number of structural and semantic invariants:

* YAML structure:

  * Missing required keys (e.g., `header`, `question_banks`, `id`, `question`, `answers`) cause parsing to fail.
  * Wrong types (e.g. `tags` not being a list) cause parsing to fail.
* Question IDs:

  * The spec requires `id` to be unique across all question templates used in an exam. Duplicate IDs may result in confusing behavior and should be considered invalid, even if not yet enforced at runtime.
* Selection type:

  * `selection_type` must be `"any"` or `"all"`; anything else is rejected.
* Computations:

  * Malformed Haskell code or type errors in `computations` cause the generated module to fail to compile. These appear as ordinary GHC errors.
* Delimiters and expressions:

  * Expressions between delimiters must be valid Haskell in the context of the generated module.

Users should treat all such errors as **hard failures** and fix their YAML / Haskell code before generating exams.

---

## 6. Informal schema summary

For convenience, here is an approximate Haskell-style summary of the YAML schema:

```haskell
-- Exam configuration
data Config = Config
  { header         :: Header
  , question_banks :: [FilePath]        -- glob patterns allowed
  , assembly_options :: AssemblyOptions -- defaulted if omitted
  , selection      :: Selection         -- defaulted if omitted
  , content        :: Content           -- defaulted if omitted
  }

data Header = Header
  { institution :: String
  , course      :: String
  , professor   :: String
  , semester    :: String
  , title       :: String
  }

data AssemblyOptions = AssemblyOptions
  { versions      :: Int    -- default 1
  , show_id       :: Bool   -- default False
  , show_tags     :: Bool   -- default False
  , hide_subjects :: Bool   -- default False
  }

data Selection = Selection
  { include_tags :: [String]  -- regex patterns, default []
  , exclude_tags :: [String]  -- regex patterns, default []
  }

data Content = Content
  { instructions :: String    -- default ""
  }

-- Question template
data QuestionTemplate = QuestionTemplate
  { id              :: String
  , title           :: String
  , format          :: String            -- "latex"
  , selection_type  :: "any" | "all"
  , question        :: String
  , answers         :: [AnswerSpec]
  , subject         :: Maybe String
  , tags            :: [String]
  , parameters      :: [Map String Value]
  , computations    :: Maybe String      -- Haskell code
  , delimiters      :: Maybe { start :: String, end :: String }
  }

data AnswerSpec
  = Correct   String
  | Incorrect String
```

This summary is **informal** and may omit internal details, but it reflects the intended shape of the YAML documents as of Specification v3.0.
