# krr-question-answering

This project solves the tasks proposed by "Towards ai-complete question answering : a set of prerequisite toy tasks" 
by Jase et al from Facebook AI Research. Data set is under data/ folder.

The approach used is to parse the text using a lisp file (main.lsp) and to use Companions reasoning system to answer queries.

## Requirements

The following software is required to run this project:

1 - Common Lisp
2 - Northwestern University Companions software

## How to run

1. - First open companions and start a a new session.

2. - The following must be loaded in order in Companions:
2.1 - base.meld -Defines global micro-theory and define base predicate and relations
2.2 - rules.meld - Defines the rules for reasoning about time, space and basic logic
3. - In main.lsp change the variable "file-root" to the root directory of this project. 
Then go to Companions console and load main.lsp using the command (load "YOUR_FOLDER_DIRECTORY\\main.lsp")
4. - Check the output files generated in the data\ folder.