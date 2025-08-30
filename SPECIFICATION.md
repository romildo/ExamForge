# ExamForge Question Template Specification (v2.1)

`ExamForge` uses a YAML-based format to define question templates. A single `.yml` file can contain a list of one or more question template objects.

## Top-Level Structure

The root of a `.yml` file must be a **list** of question template objects.

```yaml
- id: "question-001"
  # ... fields for the first question
- id: "question-002"
  # ... fields for the second question
```

-----

## Question Template Object

Each object in the list represents a single question template and is defined by a set of key-value pairs.

### Required Keys

  * `id` (String): A unique identifier for the question (e.g., `"logic-001"`, `"chapter5-recursion"`). It's used to generate the Haskell module name.
  * `title` (String): A human-readable title for the question.
  * `format` (String): The primary output format for the question's content (e.g., `"latex"`, `"markdown"`).
  * `selection_type` (String): Defines how correct answers are evaluated.
      * `"any"`: At least one of the correct answers must be chosen (for multiple-choice, single-answer questions).
      * `"all"`: All of the correct answers (and no incorrect ones) must be chosen.
  * `question` (String): The template string for the question body. This can be a multi-line string.
  * `answers` (List of Objects): A list defining the answer choices. Each object must have a single key:
      * `correct`: The value is the template string for a correct answer.
      * `incorrect`: The value is the template string for an incorrect answer.

### Optional Keys

  * `subject` (String): The subject or chapter the question relates to.
  * `tags` (List of Strings): A list of keywords for filtering and organization. If omitted, it defaults to an empty list `[]`.
  * `parameters` (List of Objects): A list of parameter sets to generate question variants. Each object is a map of variable names to their values (String, Number, or Bool). If omitted, it defaults to an empty list `[]`, producing one static variant.
  * `computations` (String): A block of Haskell code containing `let` bindings. The variables defined here, along with those from `parameters`, are available for use within the `question` and `answer` templates.
  * `delimiters` (Object): An object specifying the start and end delimiters for expressions within templates. If omitted, defaults to `{{` and `}}`.
      * `start`: The starting delimiter string.
      * `end`: The ending delimiter string.

-----

## Full Example

This example demonstrates all features, including multiple questions in one file, custom delimiters, parameters, and computations.

```yaml
# File: chapter16.yml
-
  id: "cap16-lambdaAninhada"
  title: "Lambda Aninhada"
  format: "latex"
  subject: "16: Expressão Lambda"
  tags: ["lambda", "haskell", "função anônima"]
  selection_type: "any"
  delimiters: { start: "|", end: "|" }

  parameters:
    - { x: 5,  y: 3 }
    - { x: 6,  y: 2 }
    - { y: 5,  x: 10 } # Order does not matter

  computations: |
    correct = x * 2 + y
    distractor1 = "Erro de tipo"
    distractor2 = x * y + 2
    distractor3 = correct + 5

  question: |
    Analise a expressão Haskell a seguir. Qual será o resultado de sua avaliação?
    
    \begin{minted}{haskell}
    let f = (\x -> \y -> x * 2 + y) |show x| in f |show y|
    \end{minted}

  answers:
    - correct:   "\\mintinline{haskell}{|show correct|}"
    - incorrect: "|distractor1|"
    - incorrect: "\\mintinline{haskell}{|show distractor2|}"
    - incorrect: "\\mintinline{haskell}{|show distractor3|}"

-
  id: "static-test-001"
  title: "Simple Static Test"
  format: "markdown"
  selection_type: "all"
  tags: ["testing"]

  question: "Which of the following are prime numbers?"

  answers:
    - correct: "2"
    - incorrect: "4"
    - correct: "7"
    - incorrect: "9"

```
