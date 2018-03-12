;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

;; (load "C:\\Users\\danil\\Documents\\Northwestern\\Winter 2018\\EECS 371 - NRR\\Project\\krr-question-answering\\main.lsp")

;; Not sure how to user relative paths in lisp. Work with absolute paths for now.
;; NOTE: Remember to change that before running.
(setq file-root "C:\\Users\\Han\\Desktop\\371-KRR\\krr-question-answering\\")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and parsing functions

;; Reads a text file and returns list of string with the file's lines.
;; If file doesn't exist, returns an empty list.
(defun read-text-file (file-name) 
  (let ((file (open file-name :if-does-not-exist nil))
        (return-str (list)))
    (when file
        (do ((line (read-line file nil) (read-line file nil)))
            ((null line))
            (setq return-str (append return-str (list line))))
        (close file))
    return-str
  )
)

;; Writes the input string to the specified file.
(defun write-text-file (file-name output-string) 
  (with-open-file (stream file-name :direction :output :if-exists :new-version)
   (format stream output-string)
  )
)

;; Splits a string, the delimiters are defined in delimiterp.
(defun string-split (chars str &optional (lst nil) (accm ""))
  (cond
    ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
    (string-split chars (subseq str 1) (cons accm lst) "")
    (string-split chars (subseq str 1) 
                        lst 
                        (concatenate 'string
           accm
         (string c))))
   ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task related functions

;; Adds a task prefix to the string, we should do that for name of people, places
;; and objects since they might collide with existing data in the KB.
(defun add-task-prefix (str)
  (concatenate 'string "Task" str)
)

;; Creates an event name for an event number.
(defun event-name-from-number (event-number)
  (concatenate 'string "Event" event-number "Mt")
)

;; Changes name of direction to match KB, e.g. "north" to "northOf".
(defun add-direction-suffix (direction)
  (concatenate 'string direction "Of")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task 1 related functions

(defun execute-task1 (lines)
  (let ((output-response-list '())
        (previous-event nil))
  (dolist (line lines)
    (let ((tokens (string-split (list #\Space #\tab) line)))
    (let ((event-number (nth 0 tokens)))
      
      ;; If first token is "1" clean the KB.
      (if (string= event-number "1")
        (progn
          (setq previous-event nil)
          (clean-local-mt)
        )
      )
      (if (string= (nth 1 tokens) "Where")
        ;; The line is of the form "3 Where is John? hallway 1" 
        ;; perform a query.
        (let ((person (add-task-prefix (string-right-trim "?" (nth 3 tokens))))
              (current-place nil))
          
          ;; Clear working memory to prevent using old facts.
          (clear-wm)
          ;; For some weird reason facts in GlobalMt are being deleted.
          ;; Loading this file again fixes the problem for now.
          (fire::meld-file->kb (concatenate 'string file-root "rules.meld"))
          
          (setq current-place (ask-q (list 'isCurrentlyIn (intern person) '?x)))
          (write current-place) (terpri) (terpri) ;; TODO - Delete.
          (setq current-place (cdr (car (car current-place))))
          (setq output-response-list (append output-response-list (list current-place)))
        )
        
        ;; Otherwise the line is of the form "1 John travelled to the hallway."
        ;; Add information to the KB.
        (let ((event-mt (intern (event-name-from-number event-number)))
              (person  (intern (add-task-prefix (nth 1 tokens))))
              (place (intern (add-task-prefix (string-right-trim "." (car (last tokens)))))))
          
          ;; Store data in KB.
          (kb-store (list 'isa event-mt 'Microtheory) 'TaskLocalMt)
          (kb-store (list 'genlMt event-mt 'TaskLocalMt) 'TaskLocalMt)
          (kb-store (list 'MovesTo person place) event-mt)
          (if previous-event
            (kb-store (list 'happensAfter event-mt previous-event) 'TaskLocalMt)
          )
          
          ;; Update previous event.
          (setq previous-event event-mt)
        )
      )
    ))
  )
  ;; return the list of places got from the "Where is" queries.  
  output-response-list)  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task 4 related functions

(defun execute-task4 (lines)
  (let ((output-response-list '())
        (previous-event nil))
  (dolist (line lines)
    (let ((tokens (string-split (list #\Space #\tab) line)))
    (let ((event-number (nth 0 tokens)))
      
      ;; If first token is "1" clean the KB.
      (if (string= event-number "1")
        (progn
          (setq previous-event nil)
          (clean-local-mt)
        )
      )
      (if (string= (nth 1 tokens) "What")
        ;; The line has two forms: 
        ;;      1 - "3 What is the bathroom east of?	bedroom	2" 
        ;;      2 - "3 What is west of the kitchen?	bedroom	1" 
        ;; perform a query.
        (let ((place nil)
              (direction nil)
              (answer nil))
          
          ;; Clear working memory to prevent using old facts.
          (clear-wm)
          ;; For some weird reason facts in GlobalMt are being deleted.
          ;; Loading this file again fixes the problem for now.
          (fire::meld-file->kb (concatenate 'string file-root "rules.meld"))
          
          (if (string= (nth 3 tokens) "the")
            ;; First question format
            (progn
              (setq place (intern (add-task-prefix (nth 4 tokens))))
              (setq direction (intern (add-direction-suffix (nth 5 tokens))))
              (setq answer (ask-q (list direction place '?x)))
            )
            ;; else, second format
            (progn
              (setq place (intern (add-task-prefix (string-right-trim "?" (nth 6 tokens)))))
              (setq direction (intern (add-direction-suffix (nth 3 tokens))))
              (setq answer (ask-q (list direction '?x place)))
            )
          )
          
          (write answer) (terpri) (terpri) ;; TODO - Delete.
          (setq answer (cdr (car (car answer))))
          (setq output-response-list (append output-response-list (list answer)))
        )
        
        ;; Otherwise the line is of the form "1 The hallway is east of the bathroom."
        ;; Add information to the KB.
        (let ((event-mt (intern (event-name-from-number event-number)))
              (place1  (intern (add-task-prefix (nth 2 tokens))))
              (direction  (intern (add-direction-suffix (nth 4 tokens))))
              (place2 (intern (add-task-prefix (string-right-trim "." (car (last tokens)))))))
          
          ;; Store data in KB.
          (kb-store (list 'isa event-mt 'Microtheory) 'TaskLocalMt)
          (kb-store (list 'genlMt event-mt 'TaskLocalMt) 'TaskLocalMt)
          (kb-store (list direction place1 place2) event-mt)
          (if previous-event
            (kb-store (list 'happensAfter event-mt previous-event) 'TaskLocalMt)
          )
          
          ;; Update previous event.
          (setq previous-event event-mt)
        )
      )
    ))
  )
  ;; return the list of places got from the "Where is" queries.  
  output-response-list)  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task 6 related functions

(defun execute-task6 (lines)
  (let ((output-response-list '())
        (previous-event nil))
  (dolist (line lines)
    (let ((tokens (string-split (list #\Space #\tab) line)))
    (let ((event-number (nth 0 tokens)))
      
      ;; If first token is "1" clean the KB.
      (if (string= event-number "1")
        (progn
          (setq previous-event nil)
          (clean-local-mt)
        )
      )
      (if (string= (nth 1 tokens) "Is")
        ;; The line is of the form "3 Is John in the kitchen?   no  2" 
        ;; perform a query.
        (let ((person (add-task-prefix (nth 2 tokens)))
              (judge-place (add-task-prefix (string-right-trim "?" (nth 5 tokens))))
              (ans nil))
          
          ;; Clear working memory to prevent using old facts.
          (clear-wm)
          ;; For some weird reason facts in GlobalMt are being deleted.
          ;; Loading this file again fixes the problem for now.
          (fire::meld-file->kb (concatenate 'string file-root "rules.meld"))
          
          (setq current-place (ask-q (list 'isCurrentlyIn (intern person) '?x)))
          (write current-place) (terpri) (terpri) ;; TODO - Delete.
          (setq current-place (cdr (car (car current-place))))
          (write current-place) (terpri) (terpri)
          (write (intern judge-place)) (terpri) (terpri)
          (setq answer (if (eql (intern judge-place) current-place) "yes" "no"))
          (setq output-response-list (append output-response-list (list answer)))
        )
        
        ;; Otherwise the line is of the form "2 John moved to the bedroom." or "1 Daniel went back to the kitchen."
        ;; Skip the line with the information about "picked up" or "discarded".
        ;; Add information to the KB.
        (if (or (string= (nth 3 tokens) "to") (string= (nth 4 tokens) "to"))
          (let ((event-mt (intern (event-name-from-number event-number)))
                (person  (intern (add-task-prefix (nth 1 tokens))))
                (place (intern (add-task-prefix (string-right-trim "." (car (last tokens)))))))
            
            ;; Store data in KB.
            (kb-store (list 'isa event-mt 'Microtheory) 'TaskLocalMt)
            (kb-store (list 'genlMt event-mt 'TaskLocalMt) 'TaskLocalMt)
            (kb-store (list 'MovesTo person place) event-mt)
            (if previous-event
              (kb-store (list 'happensAfter event-mt previous-event) 'TaskLocalMt)
            )
          
          ;; Update previous event.
            (setq previous-event event-mt)
          )
        )
      )
    ))
  )
  ;; return the list of answer got from the "isCurrentlyIn" queries.  
  output-response-list)  
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRE related functions 

;; NOTE: following FIRE functions only works on companions shell - 
;; uncomment/comment them when needed

;; Ask fire from companions command line.
(defun ask-f (query)
  (write query) (terpri) ;; TODO: delete this.
  (fire::ask-it query :context :all :response :bindings)
)
    
;; Query fire from companions command line. 
;; We should normally use this one since it supports inference.
(defun ask-q (query)
  (write query) (terpri) (list (list (list 'x 'place))) ;; TODO: delete this.
  (fire::q query :context :all :response :bindings)
)

(defun kb-store (fact microtheory)
  (write fact) (write '-) ;; TODO: delete this.
  (write microtheory) (terpri) ;; TODO: delete this.
  (fire::kb-store fact :mt microtheory)
)

(defun nuke-kb-item ()
  (write query) (terpri) ;; TODO: delete this.
  (fire::nuke-kb-item 'TaskLocalMt)
)

(defun clean-local-mt ()
  (dolist (mt (fire::tabulate-mt-sizes))
    (if (and 
         (eql 'symbol (type-of (car mt)))
         (string= "Event" (subseq (string (car mt)) 0 5))  
        )
      (progn
        (write "nuking: ") (write (car mt)) (terpri) ;; TODO: delete this.
        (fire::nuke-kb-item (car mt))
      )
    )
  )
  (write "nuking: ") (write 'TaskLocalMt) (terpri)
  (fire::nuke-kb-item 'TaskLocalMt)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; Main function, reads input file with tasks. So far it only splits the input
;; string into tokens and outputs the tokens to a file.
;; TODO: parse the input and output the a .meld file with the event microtheories
;; TODO: get result of queries from companions/FIRE and write results on output
;; file.
(defun main ()
  (let ((lines (read-text-file (concatenate 'string file-root "data\\qa1_single-supporting-fact_test.txt"))))
    (let ((output (execute-task1 lines))
          (output-str ""))
      (dolist (element output)
        (setq output-str (concatenate 'string output-str 
                                     (format nil "~s~%" element)))
      )
      ;; writes the output-str to output file.
      (write-text-file (concatenate 'string file-root "data\\qa1_single-supporting-fact_test.out") output-str)
      (write "output saved to data\\qa1_single-supporting-fact_test.out")
    )
  )

  (let ((lines (read-text-file (concatenate 'string file-root "data\\qa4_two-arg-relations_test.txt"))))
    (let ((output (execute-task4 lines))
          (output-str ""))
      (dolist (element output)
        (setq output-str (concatenate 'string output-str 
                                     (format nil "~s~%" element)))
      )
      ;; writes the output-str to output file.
      (write-text-file (concatenate 'string file-root "data\\qa4_two-arg-relations_test.out") output-str)
      (write "output saved to data\\qa4_two-arg-relations_test.out")
    )
  )

  (let ((lines (read-text-file (concatenate 'string file-root "data\\qa6_yes-no-questions_test.txt"))))
    (let ((output (execute-task6 lines))
          (output-str ""))
      (dolist (element output)
        (setq output-str (concatenate 'string output-str 
                                     (format nil "~s~%" element)))
      )
      ;; writes the output-str to output file.
      (write-text-file (concatenate 'string file-root "data\\qa6_yes-no-questions_test.out") output-str)
      (write "output saved to data\\qa6_yes-no-questions_test.out")
    )
  ) 
)

;; Executes main function.
(main)
