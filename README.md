# Lisp semantic diff
It is a project of lisp semantic diff. It was started as the main part of my bachelor's diploma project.
# Features
- supports semantic diff of a small subset of Common Lisp by analyzing ***s-exprs***
- compares 2 lisp files and detects added, deleted, and modified def-s-exprs (like `defun`, `defvar` etc) 
- can highlight added, deleted, and moved s-exprs inside particular modified def-s-expr.
- also detects and highlights lexical, syntactic, and some semantic erros

# Dependencies
- QT5 libs and qmake
- sbcl and asdf

# Screenshots
## Start window
![Start window](/screenshots/first-window.png?raw=true)
## Standard diff results
![Standard diff results](/screenshots/normal-result-window.png?raw=true)
## Errors detecting
![Errors detecting](/screenshots/errors-highlighting.png?raw=true)
## Moved s-expr selecting
![Moved s-expr selecting](/screenshots/moved-s-expr-selection.png?raw=true)
## Def to def diff
![Def to def diff](/screenshots/def-to-def-result.png?raw=true)

