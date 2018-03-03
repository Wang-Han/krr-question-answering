;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and parsing functions

;; Reads a text file and returns list of string with the file's lines.
;; If file doesn't exist, returns an empty list.
(defun read-text-file (file-name) 
  (let ((file (open file-name :if-does-not-exist nil))
        (return-str (list)))
    (when file
      (loop for it from 1 to 5
          do (setq return-str 
                   (append return-str 
                     (list (read-line file)))))
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

;; Helper for the (string-split) function.
(defun string-split-delimiter (c) (or (char= c #\Space) (char= c #\,)))

;; Splits a string, the delimiters are defined in delimiterp
(defun string-split (string &key (delimiterp #'string-split-delimiter))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRE functions (only works on companions shell)

;; That's how we can query fire from companions command line.
;; (defun ask-f (q)
;;     (fire::ask-it q 
;;     :context :all :response :bindings))
    
;; TODO find what function in FIRE does query instead of ask-it
;; (defun ask-q  (q)
;;    (fire::query q 
;;    :context :all :response :bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; Main function, reads input file with tasks. So far it only splits the input
;; string into tokens and outputs the tokens to a file.
;; TODO: parse the input and output the a .meld file with the event microtheories
;; TODO: get result of queries from companions/FIRE and write results on output
;; file.
(defun main ()
  (let ((lines (read-text-file "qa1_single-supporting-fact_test.txt"))
        (output-str ""))
    ;; loops though the lines in the input file.
    (loop for line in lines 
        do (loop for token in (string-split line) 
               do (setq output-str (concatenate 'string output-str 
                                     (format nil "~s~%" token)))))
    ;; writes the output-str to output file.
    (write-text-file "out.txt" output-str)
  )
)

;; Executes main function.
(main)