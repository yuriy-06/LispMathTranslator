;; This is simple translator lisp formula's to LibreOffice formula's  module Math.
 
;; Simple examples:
;; (LispMathTranslator '(setq mu (+ 1 (* (/ 1 h) (+ (* m1 l1  ) (* m2 l2) ) )))) => 
;;   => "MU = (1 + {1 over H} times (M1 times L1 + M2 times L2))"
;;(let ((h 8))    
;;	(LispMathTranslator `(setq mu (+ 1 (* (/ 1 ,h) (+ (* m1 l1 ) (* m2 l2) ) ))))) =>
;;  => "MU = (1 + {1 over 8} times (M1 times L1 + M2 times L2))"
 (defmacro ret ()
	`(return-from LispMathTranslator string ))
 
 (defun LispMathTranslator (expr &optional )
	(let (f r string)
		(if (equal (type-of expr) 'cons) ;; если аргумент список
			(progn
				(setq f (first expr))  ;; на основании первого аргумента выбираем правило разбора
				(setq r (rest expr))   ;; используется для рекурсивного вызова функции для дальнейшего разбора выражения
		
				(cond  ;; здесь описываются правила преобразования лексем в конструкции другого языка
					((equal f 'setq) 
						(progn 
							(setq string (concatenate 'string (LispMathTranslator (second expr)) " = " (LispMathTranslator (third expr))))
							(ret)))
					((equal f '/) 
						(progn 
							(setq string (concatenate 'string "{" (LispMathTranslator(second expr)) " over " (LispMathTranslator (third expr )) "}" ))
							(ret)))
					((equal f 'sqrt)
						(progn 
							(setq string (concatenate 'string "SQRT{" (LispMathTranslator(second expr)) "}"))
							(ret)))
					((equal f '*) 
						(progn
							(if (not (equal (list-length r) 1)) 
								(setq string (concatenate 'string  (LispMathTranslator (second expr)) " times " (LispMathTranslator (append (list '*) (rest r)))))
								(setq string (LispMathTranslator (first r))))
							(ret)))
					((equal f '+) 
						(progn	
							;;  скобок не хватает  -- если выражение сложное (LispMathTranslator снова вызывается рекурсивно ) -- добавляются скобки
							;;; расшифровка - если скобка с суммированием (результат операции с суммированием) умножается (выше по дереву стоит оператор умножения) -- добавляются скобки
							;;; пока что они добавляются в любом случае при операции суммирования -- и удаляются лишние в отчете вручную
							(if (not (equal (list-length r) 1))  
								(setq string (concatenate 'string "(" (LispMathTranslator (second expr)) " + " (LispMathTranslator (append (list '+) (rest r))) ")"))
								(setq string (LispMathTranslator (first r))))
							(ret)))
					((equal f 'expt)  ;; возведение в степень
						(progn 
							(setq string (concatenate 'string "{" (LispMathTranslator(second expr)) "}^{" (LispMathTranslator (third expr )) "}"))
							(ret)))
					)))
			;; если ни одно правило не срабатывает - то вывожу атом - expr
		(values (write-to-string expr))))  ;; вывожу символьное представление атома
			

