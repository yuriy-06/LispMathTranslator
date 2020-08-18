(in-package :LispMathTranslator)
;; This is simple translator lisp formula's to LibreOffice formula's  module Math.
 
;; Simple examples:
;; (lpt '(setq mu (+ 1 (* (/ 1 h) (+ (* m1 l1  ) (* m2 l2) ) )))) => 
;;   => "MU = (1 + {1 over H} times (M1 times L1 + M2 times L2))"
;;(let ((h 8))    
;;	(lpt `(setq mu (+ 1 (* (/ 1 ,h) (+ (* m1 l1 ) (* m2 l2) ) ))))) =>
;;  => "MU = (1 + {1 over 8} times (M1 times L1 + M2 times L2))"
 (defmacro ret ()
	`(return-from lpt string ))
 
 (defun lpt (expr)
    (declare (optimize (debug 3)))
	(let (f r string) ; nil connects perfectly to strings
		(if (equal (type-of expr) 'cons) ;; if argument is list
			(progn
				(setq f (first expr))  ;; based on the first argument, select the parsing rule
				(setq r (rest expr))   ;; the "digital" part of an expression
				(cond  ;; here are the rules for converting tokens in constructions of another language
					((equal f 'setq) 
						(progn 
							(setq string (concatenate 'string (lpt (second expr)) " = " (lpt (third expr))))
							(ret)))
                    ((equal f '<=) 
						(progn
							(if (> (list-length r) 1) ;  more than 1 parameters of the function "*"
                                    (setq string (concatenate 'string  (lpt (first r)) " leslant " (lpt (append (list '<=) (rest r)))))
                                    ;(progn (break "lpt * breal")(setq string (lpt r))))
                                    (setq string (lpt (first r))))
							(ret)))
					((equal f '/) 
						(progn	
							(if (> (list-length r) 1) ;  more than 1 parameters of the function "/"
                                    (setq string (concatenate 'string "{" (lpt (first r)) " over {" (lpt (append (list '*) (rest r))) "}}"))
                                    (setq string (lpt (first r))))
							(ret)))
					
					((equal f '*) 
						(progn
							(if (> (list-length r) 1) ;  more than 1 parameters of the function "*"
                                    (setq string (concatenate 'string  (lpt (first r)) " times " (lpt (append (list '*) (rest r)))))
                                    ;(progn (break "lpt * breal")(setq string (lpt r))))
                                    (setq string (lpt (first r))))
							(ret)))
					((equal f '+) 
						(progn	
							;; brackets are missing - if the expression is complex (LispMathTranslator is called recursively again ) -- parentheses are added
							;;; decoding - if a parenthesis with summation is multiplied (the multiplication operator is higher in the tree) - parentheses are added
							;;; so far, they are added anyway during the summation operation - and unnecessary ones are manually removed in the report
							(if (> (list-length r) 1) 
								(setq string (concatenate 'string "(" (lpt (first r)) " + " (lpt (append (list '+) (rest r))) ")"))
								(setq string (lpt (first r))))
							(ret)))
                    ((equal f '-) 
						(progn	
							(if (> (list-length r) 1)  
								(setq string (concatenate 'string "(" (lpt (first r)) " - " (lpt (append (list '-) (rest r))) ")"))
								(setq string (lpt (first r))))
							(ret)))
                    ((equal f 'sqrt)
						(progn 
							(setq string (concatenate 'string "SQRT{" (lpt(second expr)) "}"))
							(ret)))
					((equal f 'expt)
						(progn 
							(setq string (concatenate 'string "{" (lpt(second expr)) "}^{" (lpt (third expr )) "}"))
							(ret)))
					)))
		;; if no one rule works, then I output the atom - expr
		(values (write-to-string expr))))  ;; if the expression is not a list, I just output it
			
(defun LispMathTranslator (expr)
    (lpt expr))
