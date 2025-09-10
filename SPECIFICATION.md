# ExamForge Specification (v3.0)

`ExamForge` uses two types of YAML files to manage the exam generation process:
1.  **Exam Configuration Files:** High-level files that define *what* an exam is, which questions it uses, and how it should be assembled.
2.  **Question Template Files:** Low-level files that define the actual *content* of the questions.

---
## 1. Exam Configuration File

This file acts as the main "recipe" for an exam. It is the single input to the `exam-assembler` executable.

### Top-Level Structure

The root of the file is a single object with the following keys.

```yaml
# File: configs/EE1.yml

header: { ... }
question_banks: [ ... ]
assembly_options: { ... }  # Optional
selection: { ... }         # Optional
content: { ... }           # Optional
````

### Key Reference

#### `header` (Object, Required)

Metadata for the PDF title page and answer sheet.

  * `institution` (String)
  * `course` (String)
  * `professor` (String)
  * `semester` (String)
  * `title` (String)

#### `question_banks` (List of Strings, Required)

A list of file paths to the question bank `.yml` files. Supports glob patterns.
*Example:* `["questions/cap01.yml", "questions/cap02-*.yml"]`

#### `assembly_options` (Object, Optional)

Settings that control how the exam is assembled.

  * `versions` (Int): Number of unique exam versions to generate. (Default: `1`)
  * `show_id` (Bool): If `true`, prints the question `id` in the exam. (Default: `false`)
  * `show_tags` (Bool): If `true`, prints the question `tags` in the exam. (Default: `false`)
  * `hide_subjects` (Bool): If `true`, hides the `subject` field in the exam. (Default: `false`)

#### `selection` (Object, Optional)

Rules for filtering which questions from the banks are included.

  * `include_tags` (List of Strings): A list of regex patterns. A question is included if **any** of its tags match **any** of the patterns. If this list is omitted or empty, all questions pass this filter.
  * `exclude_tags` (List of Strings): A list of regex patterns. A question is excluded if **any** of its tags match **any** of the patterns.

#### `content` (Object, Optional)

Additional content to be inserted into the exam.

  * `instructions` (String): A multi-line string of instructions to be printed on the title page. (Default: `""`)

-----

## 2\. Question Template File

A single `.yml` file can contain a **list** of one or more question template objects.

### Question Template Object

Each object in the list represents a single question template and is defined by a set of key-value pairs.

#### Required Keys

  * `id` (String): A unique identifier for the question (e.g., `"logic-001"`).
  * `title` (String): A human-readable title for the question.
  * `format` (String): The primary output format (e.g., `"latex"`).
  * `selection_type` (String): Defines how correct answers are evaluated (`"any"` or `"all"`).
  * `question` (String): The template string for the question body.
  * `answers` (List of Objects): A list defining the answer choices. Each object has a single key: `correct` or `incorrect`.

#### Optional Keys

  * `subject` (String): The subject or chapter the question relates to.
  * `tags` (List of Strings): Keywords for filtering. Defaults to `[]`.
  * `parameters` (List of Objects): Parameter sets to generate question variants. Defaults to `[]`.
  * `computations` (String): A block of Haskell `let` bindings.
  * `delimiters` (Object): Custom delimiters for expressions (e.g., `{ start: "|", end: "|" }`). Defaults to `{{` and `}}`.
