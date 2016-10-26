# LispMathTranslator
This is simple translator lisp formula's to LibreOffice formula's  module Math.

;; Simple examples:
;; (LispMathTranslator '(setq mu (+ 1 (* (/ 1 h) (+ (* m1 l1  ) (* m2 l2) ) )))) => 
;;   => "MU = (1 + {1 over H} times (M1 times L1 + M2 times L2))"
;;(let ((h 8))    
;;	(LispMathTranslator `(setq mu (+ 1 (* (/ 1 ,h) (+ (* m1 l1 ) (* m2 l2) ) ))))) =>
;;  => "MU = (1 + {1 over 8} times (M1 times L1 + M2 times L2))"
