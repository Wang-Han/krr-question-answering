;; That's how we can query fire from companions command line.
(defun ask-f (q)
    (fire::ask-it q 
    :context :all :response :bindings))
    
;; TODO find what function in FIRE does query instead of ask-it
; (defun ask-q  (q)
;    (fire::query q 
;    :context :all :response :bindings))

