# ExamForge Question Specification v2.0

## 1\. Introduction

This document outlines the `v2.0` specification for creating question template files for the ExamForge system. ExamForge is a tool designed to generate multiple unique variations of exam questions from a single declarative template file.

The core philosophy is to separate a question's content, data, and logic into a clean, human-readable YAML format. This allows educators to create rich, algorithmic questions without embedding presentation logic inside a programming language.

Each question is defined in its own `.yml` file.

## 2\. Example

Here is a complete example of a parameterized question file. This demonstrates most of the features of the specification.

```yaml
# (Required) Unique identifier for the question.
id: "cap16-lambdaAninhada"

# (Required) Question title for cataloging.
title: "Lambda Aninhada"

# (Required) The markup language used in the templates.
# Supported values: "latex", "markdown".
format: "latex"

# (Optional) Subject/chapter for filtering.
subject: "16: Expressão Lambda"

# (Optional) List of tags for filtering.
tags:
  - "lambda"
  - "haskell"
  - "função anônima"

# (Required) Defines the answer correction criteria.
# "any": Only one of the correct answers needs to be marked.
# "all": All correct answers must be marked.
selection_type: "any"

# (Optional) List of parameter sets to generate question variations.
parameters:
  - { x: 5,  y: 3  }
  - { x: 6,  y: 2  }
  - { x: 10, y: 5 }

# (Optional) Code block for calculations.
computations: |
  correct = x * 2 + y
  distractor1 = "Erro de tipo"
  distractor2 = x * y + 2
  distractor3 = correct + 5

# (Required) The question's statement template.
question: |
  Analise a expressão Haskell a seguir. Qual será o resultado de sua avaliação?
  
  \begin{minted}{haskell}
  let f = (\x -> \y -> x * 2 + y) {{x}} in f {{y}}
  \end{minted}

# (Required) List of answer choice templates.
answers:
  - correct:   "\\mintinline{haskell}{{{show correct}}}"
  - incorrect: "{{distractor1}}"
  - incorrect: "\\mintinline{haskell}{{{show distractor2}}}"
  - incorrect: "\\mintinline{haskell}{{{show distributor3}}}"
```

## 3\. Top-Level Keys

This section details every valid key at the top level of the question YAML file.

### `id`

  - **Type:** `String`
  - **Required:** Yes
  - **Description:** A unique identifier for the question across the entire question bank. It is recommended to use a consistent naming scheme, such as `<subject>-<descriptionInCamelCase>`.

### `title`

  - **Type:** `String`
  - **Required:** Yes
  - **Description:** A human-readable title for the question. Used for display and cataloging purposes.

### `format`

  - **Type:** `String`
  - **Required:** Yes
  - **Description:** Specifies the markup language used in the `question` and `answers` templates. The generator will use this information to produce the correct output file type.
  - **Valid Values:** `latex`, `markdown`.

### `subject`

  - **Type:** `String`
  - **Required:** No
  - **Description:** The subject, chapter, or topic the question belongs to. This can be used by the exam orchestrator to filter questions.

### `tags`

  - **Type:** `List of Strings`
  - **Required:** No
  - **Description:** A list of keywords or tags associated with the question. This allows for more granular filtering when assembling an exam.

### `selection_type`

  - **Type:** `String`
  - **Required:** Yes
  - **Description:** Defines the scoring logic for questions with multiple correct answers.
  - **Valid Values:**
      - `any`: The student needs to select only one of the available correct options.
      - `all`: The student must select all of the correct options.

### `parameters`

  - **Type:** `List of Maps`
  - **Required:** No
  - **Description:** A list where each item is a map (dictionary) of key-value pairs. Each item in the list will be used to generate one unique variation of the question. The keys defined here become available as variables inside the `computations` block and the templates. If this key is omitted, the question will have only one static variation.

### `computations`

  - **Type:** `String` (Multiline block)
  - **Required:** No
  - **Description:** A block of code in the target programming language (e.g., Haskell, Python). This code is executed for each variation. It has access to the variables defined in the `parameters` for the current variation. Any new variables defined in this block become available for use in the `question` and `answers` templates.

### `question`

  - **Type:** `String` (Multiline block)
  - **Required:** Yes
  - **Description:** The template for the question's statement. It can contain placeholders for variable substitution.

### `answers`

  - **Type:** `List of Maps`
  - **Required:** Yes
  - **Description:** A list defining the answer choices. Each item in the list is a map with a single key, which must be either `correct` or `incorrect`. The value is the string template for that answer choice. The order in this list is preserved before the final shuffling by the exam orchestrator.

## 4\. Templating System

The `question` and `answers` fields are templates that are processed for each variation.

  - **Variable Substitution:** Variables are injected into the templates using double curly braces: `{{variable_name}}`.
  - **Scope:** The template engine has access to all variables from the `parameters` block for the current variation, as well as any variables defined in the `computations` block.
  - **Expressions:** For languages like Haskell, you can embed simple expressions that return a string, such as `{{show correct}}`. The `computations` block is the preferred place for complex logic.