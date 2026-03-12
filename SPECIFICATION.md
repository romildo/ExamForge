<!-- File: SPECIFICATION.md -->

# ExamForge Specification (v4.0)

`ExamForge` uses two families of YAML files to control exam generation at a high-level:

1. **Exam configuration files:** recipes describing the exam metadata, question banks, execution environments, and selection constraints.
2. **Question template files:** data describing parameterized questions, including their text, answers, parameters, tags, and target-language computations.

---

## Scope and versioning

This document is the **normative specification** of the ExamForge YAML format, version **v4.0**.

- All configuration (`configs/*.yml`) and question template (`questions/*.yml` / `*.yaml`) files are expected to conform to this version unless otherwise stated.
- Version **v4.0** marks a major architectural shift to a **Polyglot Runtime Engine**. Parameter computations are no longer strictly bound to Haskell at compile-time. Instead, ExamForge orchestrates external scripts (Python, C, Haskell, etc.) to evaluate variants and generate an Intermediate Representation (JSON).

---

## 1. Architectural Overview: The Two-Phase Pipeline

ExamForge v4.0 separates exam generation into two distinct phases:

### Phase 1: Evaluation (The Polyglot Orchestrator)
When ExamForge reads a parameterized question, it uses the specified `language` to generate a temporary source file containing the user's `computations` and parameter `rows`. It executes this script using the commands defined in the `evaluators` configuration. 

The external script is responsible for executing the math/logic, interpolating the strings using native format specifiers, and outputting a flat **Expanded Variant Bank** in JSON format to `stdout`.

### Phase 2: Assembly (The Constraint Solver)
The Haskell assembler consumes the JSON output. Because the JSON contains pre-computed, fully interpolated variants, the assembler's only job is to apply `selection` filters, enforce `semantic_group` quotas, shuffle the variants, and render the final LaTeX and CSV files.

---

## 2. Exam Configuration File

The exam configuration file is the main *recipe* for an exam.

### Top-level structure

The root of the file is a single YAML object:

```yaml
# File: configs/E2.yml

header: { ... }
question_banks: [ ... ]
default_language: "python" # Optional (New in v4.0)
evaluators: { ... }        # Optional (New in v4.0)
assembly_options: { ... }  # Optional
selection: { ... }         # Optional
content: { ... }           # Optional
```

### Key reference

#### `header` (Object, Required)

Metadata for the PDF title page and answer sheet.

Fields:

* `institution` (String)
* `course` (String)
* `professor` (String)
* `semester` (String)
* `title` (String)

All of these fields are required.

#### `question_banks` (List of Strings, Required)

A list of file paths or glob patterns locating question template files. Absolute paths are supported. Relative paths are resolved relative to the project root (execution directory).

Example:

```yaml
question_banks:
  - "questions/E1.cap-01.yaml"
  - "questions/E1.cap-02.yaml"
  - "questions/E2.cap-*.yaml"
  - "/alt/projects/prog-fun-haskell/exams/questions/q17-superior.yml"
```

If the patterns match no files, `examforge` still succeeds but simply generates an empty question pool. Subsequent `exam-assembler` runs will report that no questions matched the filters.


#### `default_language` (String, Optional)

The fallback language used for evaluating question templates if a specific question omits the `language` field.  
Default: `"haskell"`.

#### `evaluators` (Object, Optional)

Defines how ExamForge should compile and run external scripts for specific languages.

Fields for each language object:

* `build` (String, Optional): The compilation command. Use `%f` for the input source file and `%e` for the output executable.
* `run` (String, Required): The execution command.
* `extension` (String, Optional): The extension used in the temporary file name of the script source code.

Example:

```yaml
evaluators:
  python:
    run: "python3 %f"
  c:
    build: "gcc -Wall -o %e %f"
    run: "./%e"
    extension: "c"
```

#### `assembly_options` (Object, Optional)

Controls how versions of the exam are assembled and rendered.

* `versions` (Integer)  
  Number of distinct exam variants to generate.  
  Must be a positive integer.  
  Default: `1`.

* `show_id` (Boolean)  
  If `true`, the question `id` is printed in the exam.  
  Default: `false`.

* `show_tags` (Boolean)  
  If `true`, the question `tags` are printed in the exam.  
  Default: `false`.

* `hide_subjects` (Boolean)  
  If `true`, the `subject` field is omitted from the exam.  
  Default: `false`.

* `shuffle_questions` (Boolean):  
  Whether to shuffle the selected questions' order within the variant.  
  Default: `true`

* `registration_digits` (Integer, Optional)  
  Specifies the number of digits in a student's registration number (e.g., `7`).  
  If provided and greater than `0`, the generated LaTeX answer sheet will include an OMR bubbling grid for the registration number and emit the `id_C_R` cell coordinates to the auxiliary `.zonas` file.  
  If omitted or `0`, the answer sheet falls back to printing a simple fill-in-the-blank line.

* `seed` (Integer, Optional)  
  A specific integer used to initialize the pseudo-random number generator for variant selection and shuffling.  
  If provided, assembling the exam with the same configuration will deterministically produce the exact same variants and choices.  
  If omitted, the assembler uses a default deterministic seed (or a system-generated random seed, depending on your implementation choice).

Example:

```yaml
assembly_options:
  versions: 4
  show_id: false
  show_tags: false
  hide_subjects: true
  shuffle_questions: false
  seed: 20261
```

If `assembly_options` is omitted entirely, the defaults above are used.

#### `selection` (Object, Optional)

Rules to filter which questions from the banks form the candidate pool for inclusion in the exam, and constrain semantic groups.

Example:

```yaml
selection:
  include_tags:
    - "^logic$"
    - "^set-theory$"
  exclude_tags:
    - "bonus"

  semantic_group:
    tag_pattern: "^grupo-.*$"
    max_per_group: 1
```

##### `include_tags` / `exclude_tags` (List of Strings, Optional)

* `include_tags` (List of Strings, Optional)
  A list of **POSIX regular expressions** (as accepted by `text-regex-tdfa`).

  Semantics:

  * Each question has a list of `tags` (from the question template).
  * If `include_tags` is **empty or omitted**, all questions pass this filter.
  * If `include_tags` is **non-empty**, a question passes this filter iff **at least one** of its tags matches **at least one** of the patterns.

* `exclude_tags` (List of Strings, Optional)
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

##### `semantic_groups` (List of Objects, Optional)

Configures **semantic group constraints** on top of the basic tag filters. This allows hierarchical or multi-dimensional limits (e.g., limiting the number of questions from a broad chapter, and separately limiting questions about a specific concept).

If this block is **absent** or **empty**, semantic groups are **ignored**.

If this block is **present**, the assembler evaluates the rules from top to bottom. Questions are partitioned into semantic groups based on their tags, and per-variant constraints and rotation are enforced.

Example:

```yaml
selection:
  semantic_groups:
    # 1. Specific concept constraint
    - pattern: "^group-referential-transparency$"
      maximum: 2
    # 2. General chapter constraint
    - pattern: "^chapter-01.*$"
      maximum: 6

```

Fields for each rule object:

* `pattern` (String, **Required**)
  
  A POSIX regular expression. Any question tag whose *name* matches this pattern is considered a **semantic group tag**.

  If a question has **no tags** matching `tag_pattern`, it does **not** belong to any semantic group and is **unconstrained** by this feature.

  Example:

    * `^grupo-.*$`

      Tags like `grupo-param-def`, `grupo-guarded-equations` define distinct semantic groups.

* `maximum` (Integer, Optional, default: `1`)
  
  For each exam variant, at most this many questions from the corresponding semantic group may be selected.
  
  Must be `>= 1`. If the value is non-positive, configuration parsing must fail with an error.

**Resolving Overlapping Tags (First-Match Rule):**

Because the rules are evaluated as a list, order matters. If a single tag matches multiple `pattern` rules, the **first matching rule** dictates the `maximum` quota for that tag.

**Semantic group membership:**

* A **semantic group** is identified by a specific tag string (e.g., `"chapter-01-intro"`).
* A question may belong to:
* **0 groups** – if none of its tags match any `pattern`.
* **1 group** – if exactly one tag matches a `pattern`.
* **N groups** – if multiple distinct tags match patterns; all such group quotas are tracked and enforced simultaneously.

**Behavior:**

The assembler will rotate available questions within a semantic group across the generated exam variants. It attempts to select up to the `maximum` requested, but will naturally select fewer if the pool is exhausted or if picking further questions would violate the maximum constraints of any other overlapping semantic groups they belong to.

---

#### `content` (Object, Optional)

Additional content to be inserted into the exam.

* `latex_preamble` (String, Optional)  
  A block of raw LaTeX code to be injected into the document preamble (between `\usepackage{provastyle}` and `\begin{document}`).  
  Useful for importing specific packages (e.g., `\usepackage{menukeys}`) or defining custom LaTeX macros required by specific questions in the exam.  
  Default: `""`

* `instructions` (String)  
  A (possibly multi-line) string of instructions printed on the title page.  
  Default: `""`.

Example:

```yaml
content:
  latex_preamble: |
    \usepackage{menukeys}
    \newcommand{\mycustommacro}[1]{\textbf{#1}}
  instructions: |
    Read the questions carefully.
    No electronic devices are allowed.
    Show all your work.
```

If `content` or `instructions` are omitted, no extra instructions are printed.

---

## 3. Question Template File

A single file contains a **list** of one or more question template objects.

### File structure

Question templates live in separate `.yml` / `.yaml` files, typically under `questions/`.

A single file contains a **list** of one or more question template objects.

Example:

```yaml
# File: questions/logic-questions.yaml

- id: "logic-001"
  title: "Basic propositional logic"
  format: "latex"
  selection_type: "any"
  subject: "Logic"
  tags: ["logic", "introduction", "group-prop-basics"]
  delimiters:
    start: "{{"
    end: "}}"
  language: "haskell"
  parameters:
    rows:
      - { p: "1", q: "2" }
      - { p: "3", q: "4" }
  computations: |
    -- Haskell code
    result = p + q
  question: |
    Seja p = {{p}} e q = {{q}}. Qual é p + q?
  answers:
    - correct: "{{result}}"
    - incorrect: "{{result + 1}}"
    - incorrect: "{{result - 1}}"

- id: "logic-002"
  ...
```

Each item in the list is a single **Question Template Object**.

### Question Template Object

Each object describes one logical question, possibly parameterized into multiple variants.

#### Required keys

* `id` (String)  
  A unique identifier for the question, such as `"logic-001"`.  
  Uniqueness is expected across all question banks used in a given exam configuration.

* `title` (String)  
  A human-readable title or short description.

* `format` (String)  
  The primary output format. Currently the codebase assumes `"latex"` and renders questions to LaTeX.

* `selection_type` (String)  
  Defines how correctness is evaluated for multiple-choice answers:

  * `"any"` – the question is considered correct if **at least one** of the correct options is marked, and no incorrect option is marked (typical single-answer multiple-choice).
  * `"all"` – the question is correct only if **all and only** the correct options are marked (useful for "select all that apply").

  Any other value is treated as invalid and causes YAML parsing to fail.

* `question` (String)  
  The template for the question body (usually LaTeX).  
  It may contain parameter references and computed expressions (see `parameters`, `computations`, and `delimiters` below).

* `answers` (List of Objects)  
  The answer choices. Each element of the list is an object with exactly one key:

  * `correct` (String): a correct answer option.
  * `incorrect` (String): an incorrect answer option.

  Example:

  ```yaml
  answers:
    - correct: "2"
    - incorrect: "1"
    - incorrect: "3"
    - incorrect: "4"
  ```

#### Optional keys

* `subject` (String)  
  Logical grouping for the question (a free-form category, e.g. chapter/section).  
  Used for metadata and optional printing; can be hidden with `assembly_options.hide_subjects`.

* `tags` (List of Strings)  
  A list of tags used for selection, filtering (e.g. `"logic"`, `"set-theory"`, `"easy"`), and semantic grouping (e.g. `"grupo-hof-conceito"`).  
  Defaults to `[]`.  
  
  **Semantic group tags** are **ordinary tags** whose *name* matches the configured `selection.semantic_group.tag_pattern`. There is nothing special encoded in the question file itself; all semantic-group meaning is driven by the exam configuration.

* `language` (String)  
  The target evaluator language (e.g., `"python"`, `"c"`). If omitted, falls back to `default_language` in the exam configuration.

* `parameters` (Object)  
  Parameter sets used to create multiple concrete variants of the same logical question.

  * **`types`** (Dictionary, Optional):  
    Maps parameter names to their native types. Strictly required for statically typed languages (like `"c"`), but safely ignored by dynamically typed languages (like `"python"`).

  * **`rows`** (List of Dictionaries, Required if parameters exist):  
    - The explicitly defined sets of values for each generated variant.
    - All rows must define exactly the same keys.  
    - Values **must be strings** representing valid expressions in the computations language (e.g., `"173"`, `"[1, 2, 3]"`, `"\"Patrick\""`, for Haskell).  
    - These keys are bound as variables in the generated code. They are exposed directly to the `computations` block, and can be interpolated into the `question` and `answers` strings using the configured delimiters.

  Example:
  ```yaml
  language: "c"
  parameters:
    types: { height: "int", weight: "float", name: "char *" }
    rows:
      - { height: "173", weight: "67.5", name: "\"Patrick\"" }
      - { height: "180", weight: "86.0", name: "\"Mary\"" }

* `computations` (String)  
  A block of code written in the target `language`. Useful to derive secondary values from parameters. This code is injected directly into the evaluation loop of the generated script. It has access to the variables defined in the current parameter row.

  Example:

  ```yaml
  language: "haskell"
  computations: |
    let
      x = -b / a :: Double
      absA = abs a
  ```

* `delimiters` (Object)  
  Custom delimiters for inline expressions in `question` and `answers`.

  Defaults to `["{{", "}}"]`.

  Structure:

  * `start` (String): opening delimiter
  * `end` (String): closing delimiter

  Example:

  ```yaml
  delimiters:
    start: "|"
    end: "|"
  ```

  If absent, ExamForge uses its default delimiters (currently `{{` and `}}`). Within the question/answer text, any occurrence of:

  ```text
  {{expression}}
  ```

  is interpreted as a Haskell expression in scope of the current parameters and computations, and its rendered value is substituted into the text.

  **The Native Format Rule (New in v4.0):** Variables inside delimiters can optionally include a format specifier separated by a colon. **The specifier must be written in the native syntax of the target language.** ExamForge extracts this hint to generate native interpolation code (e.g., `f"{var:.2f}"` for Python, or `printf("%.2f", var)` for C).  
  Example:
  ```yaml
  # For Python:
  question: "Patient BMI is @@imc:.2f@@"
  # For C:
  question: "Patient BMI is @@imc:%.2f@@"
  ```

---

## 4. End-to-end Examples

### Example 1: Statically Typed with Type Inference (Haskell)

```yaml
- id: "algebra-001"
  title: "Linear equation"
  language: "haskell"
  format: "latex"
  selection_type: "any"
  subject: "Algebra"
  tags: ["algebra", "equations"]

  parameters:
    rows:
      - { a: "2", b: "3" }
      - { a: "4", b: "-1" }

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


### Example 2: Haskell (List Manipulation)

```yaml
- id: "cap15-hof-flip-map-concreto"
  title: "HOF - Combinação de map e flip"
  subject: "15: Funções de Ordem Superior"
  format: "latex"
  selection_type: "any"
  tags: ["capítulo-15", "hof", "grupo-hof-aplicacao"]
  delimiters: { start: "|", end: "|" }
  language: "haskell"
  
  parameters:
    - { exp': "2", list: "[1, 2, 3]", wrongOp: "[2, 4, 8]" }
    - { exp': "3", list: "[2, 3]", wrongOp: "[9, 27]" }
    
  computations: |
    correct = map (flip (^) exp) list
    
  question: |
    Qual será o resultado da expressão abaixo?
    \begin{minted}{haskell}
    map (flip (^) |show exp|) |show list|
    \end{minted}
    
  answers:
    - correct: |
        \mintinline{haskell}{|show correct|}
    - incorrect: |
        \mintinline{haskell}{|show wrongOp|}
```

For each parameter set, `examforge` binds `exp'` and `list`, evaluates the `computations` block to find `correct`, and injects the results into the `|...|` delimiters in the question and answers.


### Example 3: Dynamically Typed (Python)

```yaml
- id: "imc-calc-01"
  title: "Cálculo de IMC"
  language: "python"
  format: "latex"
  selection_type: "any"
  delimiters: { start: "@@", end: "@@" }
  parameters:
    rows:
      - { altura: "1.73", peso: "67.5" }
      - { altura: "1.80", peso: "86.0" }
      
  computations: |
    # Native Python logic
    result = peso / (altura ** 2)
    wrong1 = peso / altura
    
  question: |
    Um paciente de altura @@altura:.2f@@m e peso @@peso:.1f@@kg tem qual IMC?
  
  answers:
    - correct: "@@result:.2f@@"
    - incorrect: "@@wrong1:.2f@@"
```

### Example 4: Statically Typed (C)

```yaml
- id: "pointer-arithmetic-01"
  title: "Aritmética de Ponteiros Básica"
  language: "c"
  format: "latex"
  selection_type: "any"
  delimiters: { start: "{{", end: "}}" }
  
  parameters:
    types: { offset: "int", base: "int" }
    rows:
      - { offset: "3", base: "1000" }
      - { offset: "4", base: "2000" }
      
  computations: |
    /* Native C logic */
    int result = base + (offset * 4);
    int wrong1 = base + offset;
    
  question: |
    Se um ponteiro de 32 bits armazena o endereço {{base:%d}},
    qual o valor de \\texttt{p + {{offset:%d}}}?
  
  answers:
    - correct: "{{result:%d}}"
    - incorrect: "{{wrong1:%d}}"
```

---

## 5. The Intermediate Representation (JSON IR)

For developers implementing new Language Evaluator modules, your generated script must output a JSON object to `stdout` matching this schema. This is the bridge between the evaluator and the assembler.

```json
{
  "id": "pointer-arithmetic-01",
  "title": "Aritmética de Ponteiros Básica",
  "subject": "3: Pointers",
  "tags": ["dificuldade-media"],
  "selection_type": "any",
  "variants": [
    {
      "question": "Se um ponteiro de 32 bits armazena o endereço 1000...",
      "answers": [
        { "correct": true,  "text": "1012" },
        { "correct": false, "text": "1003" }
      ]
    }
  ]
}
```

---

## 6. Assembly algorithm (informal overview)

1. **Selection:** Filter the pool using `include_tags` and `exclude_tags`. Identify semantic groups via `tag_pattern`.
2. **Variant streams:** For each remaining question, expand `parameters` and `computations` into a (finite) list of variants of the question. If the question is not parameterized, the list of variants will have only the original question.
3. **Cycling & Shuffling:** Convert the finite variant list of each question into an infinite, pseudo-random stream using a cycle-and-shuffle mechanism.
   * The list of variants are shuffled using a pseudo-random generator.
   * The shuffled list is concatenated with a recursively shuffled continuation, giving an infinite stream with *fair* reuse.

4. **Semantic Group Rotation:** Ensure that no more than `max_per_group` questions sharing a semantic group tag (e.g., `grupo-hof-aplicacao`) are selected per exam variation, rotating selections evenly across versions.
5. **Output Generation:** For each requested exam variation, combine the selected variant streams, shuffle the answer choices, and render to LaTeX and CSV.

---

## 7. Validation and error handling

ExamForge enforces a number of structural and semantic invariants:

* YAML structure:

  * Missing required keys (e.g., `header`, `question_banks`, `id`, `question`, `answers`) cause parsing to fail.
  * Wrong types (e.g. `tags` not being a list) cause parsing to fail.
  * Invalid regular expressions cause assembling to fail.
  * With impossible semantic constraints (e.g., requesting 10 questions but only 5 groups exist with `max_per_group: 1`) the assembler will hard-fail.
* Question IDs:

  * The spec requires `id` to be unique across all question templates used in an exam. Duplicate IDs may result in confusing behavior and should be considered invalid, even if not yet enforced at runtime.
* Selection type:

  * `selection_type` must be `"any"` or `"all"`; anything else is rejected.
* Computations:

  * Malformed Haskell code, or type errors in `computations` cause the generated module to fail to compile. These appear as ordinary GHC errors.
* Delimiters and expressions:

  * Expressions between delimiters must be valid Haskell in the context of the generated module.

Users should treat all such errors as **hard failures** and fix their YAML / Haskell code before generating exams.

---

## 8. Informal Schema Summary

For convenience, here is an approximate Haskell-style summary of the YAML schema:

```haskell
-- Exam configuration (v4.0)
data Config = Config
  { header           :: Header
  , question_banks   :: [FilePath]
  , default_language :: Maybe String
  , evaluators       :: Map String EvaluatorConfig
  , assembly_options :: AssemblyOptions
  , selection        :: Selection
  , content          :: Content
  }

data Header = Header
  { institution :: String
  , course      :: String
  , professor   :: String
  , semester    :: String
  , title       :: String
  }

data AssemblyOptions = AssemblyOptions
  { versions          :: Int    -- default 1
  , show_id           :: Bool   -- default False
  , show_tags         :: Bool   -- default False
  , hide_subjects     :: Bool   -- default False
  , shuffle_questions :: Bool
  }

data Selection = Selection
  { include_tags    :: [String]              -- regex patterns, default []
  , exclude_tags    :: [String]              -- regex patterns, default []
  , semantic_groups :: [SemanticGroupRule]   -- default []
  }

data SemanticGroupRule = SemanticGroupRule
  { pattern :: String   -- e.g. "^chapter-01.*$"
  , maximum :: Int      -- default 1
  }

data Content = Content
  { instructions :: String    -- default ""
  }

data EvaluatorConfig = EvaluatorConfig
  { build     :: Maybe String
  , run       :: String
  , extension :: Maybe String
  }

-- Question template (v4.0)
data QuestionTemplate = QuestionTemplate
  { id             :: String
  , title          :: String
  , format         :: String
  , language       :: Maybe String
  , selection_type :: String            -- "any" | "all"
  , question       :: String
  , answers        :: [AnswerSpec]
  , subject        :: Maybe String
  , tags           :: [String]
  , parameters     :: Maybe ParameterBlock
  , computations   :: Maybe String
  , delimiters     :: Maybe { start :: String, end :: String }
  }

data ParameterBlock = ParameterBlock
  { types :: Maybe (Map String String)
  , rows  :: [Map String String]
  }

data AnswerSpec
  = Correct   String
  | Incorrect String
```
