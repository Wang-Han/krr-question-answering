;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

;; (load "C:\\Users\\danil\\Documents\\Northwestern\\Winter 2018\\EECS 371 - NRR\\Project\\krr-question-answering\\main.lsp")

;; Not sure how to user relative paths in lisp. Work with absolute paths for now.
;; NOTE: Remember to change that before running.
(setq file-root "C:\\Users\\danil\\Documents\\Northwestern\\Winter 2018\\EECS 371 - NRR\\Project\\krr-question-answering\\")

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
;; Task 1 related functions

;; Adds a task prefix to the string, we should do that for name of people, places
;; and objects since they might collide with existing data in the KB.
(defun add-task-prefix (str)
  (concatenate 'string "Task" str)
)

;; Creates an event name for an event number.
(defun event-name-from-number (event-number)
  (concatenate 'string "Event" event-number "Mt")
)

;; Add entites (Person and Places) into KB
(defun add-entities (lines)  
  ;; TODO: When cleaning the 'TaskLocalMt we were also removing entities.
  ;; Now we are adding them to 'TaskGlobalMt but that's not the best solution
  ;; If we are running multiple tasks.
  (let ((persons-seen-list '())
          (places-seen-list '()))
    (dolist (line lines)
      (let ((tokens (string-split (list #\Space) line)))
        (if (string/= (cadr tokens) "Where")
          (let ((person (intern (add-task-prefix (cadr tokens))))
                (place (intern (add-task-prefix (string-right-trim "." (car (last tokens)))))))
            (if (not (member person persons-seen-list))
              (progn
                (kb-store (list 'isa person 'Person) 'TaskGlobalMt)
                (setq persons-seen-list (append persons-seen-list (list person)))
              )
            )
            (if (not (member place places-seen-list))
              (progn
                (kb-store (list 'isa place 'Place) 'TaskGlobalMt)
                (setq places-seen-list (append places-seen-list (list place)))
              )
            )
          )
        )
      )
    )
  )
)

(defun execute-task1 (lines)
  (add-entities lines)
  (let ((output-response-list '())
        (previous-event nil))
  (dolist (line lines)
    (let ((tokens (string-split (list #\Space) line)))
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
          ;; TODO: get result and store in local variable.
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
  (let ((lines (read-text-file (concatenate 'string file-root "qa1_single-supporting-fact_test.txt"))))
    (write (execute-task1 lines))
    
    ;; loops though the lines in the input file.
    ;; (for line in lines 
    ;;    do (loop for token in (string-split (list #\Space) line) 
    ;;           do (setq output-str (concatenate 'string output-str 
    ;;                                 (format nil "~s~%" token)))))
    ;; writes the output-str to output file.
    ;; (write-text-file "out.txt" output-str)
  )
)

;; Executes main function.
(main)