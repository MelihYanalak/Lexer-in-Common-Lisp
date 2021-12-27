


; 							EXPLANATION OF DFA

;If the input is alphabetic -> identifier -> if the next input is alphanumeric -> identifier (until else input) -> before accept check if it is KW
;
;					 -> if the next input is something else -> error
;
;If the input is numeric -> value -> if the input is 0 -> value -> next input -> error
;				 -> if the input is 1-9 -> the next input is numeric -> value
;
;if the input is ; -> isComment? -> next input is ; too -> comment
;		                -> next input is something else -> error
;If the input is operator -> for +,-,/ -> next input should not be alphanumeric
;			-> for * -> mult -> next input is * too -> doublemult
;					 -> else -> error
;			-> for paranthesis there is no rule put it directly




(defun gppinterpreter (&optional fileName)
	
	(if (not (equal fileName Nil))
		(lexFile fileName)
	)
	(princ #\newline)
	(princ ">")
	(loop for line = (read-line nil)
		while (not (equal line "exit")) do
			(lexLine line)
			(princ #\newline)
			(princ ">")
	)
	(print "Program Terminated")	


)
(defun lexFile (fileName)
	(with-open-file (stream fileName)
    (loop for line = (read-line stream nil)
          while line do
          (lexLine line)))


)
(defun start() 
	(princ "$")
    (setq startStr (read-line nil))
	(if (equal (length startStr) 3) 
		(gppinterpreter )
		(progn
			(setq fileName (subseq startStr 4))
			(gppinterpreter fileName)
		)
	)
	


)
(defun lexLine (inputLine)
	(setq c Nil)
	(setq openOrClose Nil)
	(setq startIndex Nil)
	(setq finishIndex Nil)
	(loop for i from 0 to (- (length inputLine) 1) do
   		(setq c (char inputLine i))
		(setq c1 Nil)
		(setq c2 Nil)
		
		(if (< i (- (length inputLine) 2))
			(progn
				(setq c1  (char inputLine (+ i 1)))
				(setq c2  (char inputLine (+ i 2)))

			)
		)
		(if (equal i (- (length inputLine) 2))
			(progn
				(setq c1  (char inputLine (+ i 1)))

			)
		)



		(if (isOperator c)
			(progn
			

				(if (equal c #\+)
					(progn
					(if (or (or (equal c1 #\space) (equal c1 Nil)) (or (equal c1 #\( ) (equal c1 #\) )))
						(print "OP_PLUS")
						(format t "~a~b can not be tokenized" c c1)
					)
					)
					
					
				)
				(if (equal c #\-)
					(progn
					(if (or (or (equal c1 #\space) (equal c1 Nil)) (or (equal c1 #\( ) (equal c1 #\) )))
						(print "OP_MINUS")
						(format t "~a~b can not be tokenized" c c1)
					))
				)
				(if (equal c #\/)
					(progn
					(if (or (or (equal c1 #\space) (equal c1 Nil)) (or (equal c1 #\( ) (equal c1 #\) )))
						(print "OP_DIV")
						(format t "~a~b can not be tokenized" c c1)
					))
				)
				(if (equal c #\,)
					(progn
					(if (or (or (equal c1 #\space) (equal c1 Nil)) (or (equal c1 #\( ) (equal c1 #\) )))
						(print "OP_COMMA")
						(format t "~a~b can not be tokenized" c c1)
					))
				)
				(if (equal c #\*)
					(progn
					(if (or (or (equal c1 #\space) (equal c1 Nil)) (or (equal c1 #\( ) (equal c1 #\) )))
						(print "OP_MULT")
						(progn 
							(if (not (equal c1 #\*) ) 
								(format t "~a~b can not be tokenized" c c1)
								(progn 
									(if (or (or (equal c2 #\space) (equal c2 Nil)) (or (equal c2 #\( ) (equal c2 #\) )))
										(progn
										(print "OP_DBLMULT")
										(setq i (+ i 1))
										)
										(format t "~a~b~c can not be tokenized" c c1 c2)
									)

								)	
							)

						)
					))
				)
				(if (equal c #\) )
					(print "OP_CP")
				)
				(if (equal c #\( )
					(print "OP_OP")
				)
				(if (equal c #\" )
					(progn
						(if (equal openOrClose Nil)
							(progn
								(print "OP_OC")
								(setq openOrClose t)
							)
							(progn
								(print "OP_CC")
								(setq openOrClose Nil)
							)
						)

					)
				)


		))



		(if (isValue c)
			(progn

				(if (and (not (equal Nil (digit-char-p c) ) ) (not (equal 0 (digit-char-p c) ) ))
					(progn
					(loop for tempC = (char inputLine i)
						while (not (equal Nil (digit-char-p tempC) ) ) do
							(setq i (+ i 1))					
					)
					
					(if (or (or (equal (char inputLine i) #\space) (equal (char inputLine i) Nil)) (or (equal (char inputLine i) #\( ) (equal (char inputLine i) #\) )))
						(print "VALUE")
						(print "This is not a valid value")
					)
					
						(setq i (- i 1))	
					)
						
				)
				(if (equal c #\0)
					(progn
					(if (or (or (equal c1 #\space) (equal c1 Nil)) (or (equal c1 #\( ) (equal c1 #\) )))
						(print "VALUE")			
						(format t "~a~b can not be tokenized" c c1)
					)
					)
				)

			)
		)

		(if (isIdentifier c)
			(progn	
				(setq startIndex i)
				(loop for tempc = (char inputLine i)						
				while (or (not (equal Nil (digit-char-p tempc))) (equal t (alpha-char-p tempc)) ) do
						(setq i (+ i 1))
				)
				(setq finishIndex i)
				(if (or (or (equal (char inputLine i) #\space) (equal (char inputLine i) Nil)) (or (equal (char inputLine i) #\( ) 					(equal (char inputLine i) #\) )))
					(progn				
						(if (isKeyword (subseq inputLine startIndex 		finishIndex))						(print (getMappedKeyword (subseq inputLine startIndex finishIndex)))
							(print "IDENTIFIER")
						)
					)
					(print "This is not a valid identifier name")
				)
				(setq i (- i 1))

		))		
		
		(if (equal c #\;)
			(progn
			(if (equal c1 #\;)
				(progn
					(setq i (length inputLine))
					(print "COMMENT")
				)
				(format t "~a~b can not be tokenized" c c1)		
			)
			)	
		)
	)

)

(defun isOperator(c)
	(setq returnV Nil)
	(if (equal c #\+ )
		(setq returnV t)
	)
	(if (equal c #\- )
		(setq returnV t)
	)
	(if (equal c #\* )
		(setq returnV t)
	)
	(if (equal c #\/ )
		(setq returnV t)
	)
	(if (equal c #\, )
		(setq returnV t)
	)
	(if (equal c #\) )
		(setq returnV t)
	)
	(if (equal c #\( )
		(setq returnV t)
	)
	(if (equal c #\" )
		(setq returnV t)
	)
	returnV
)
(defun isIdentifier(c)
	(if (equal t (alpha-char-p c))
		t
		Nil
	)

)
(defun isValue(c)

	
	(if (not (equal Nil (digit-char-p c) ) )
		t
		Nil
	)



)

(defun isKeyword (identifier)
	(setq control Nil)
	(setq list '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
	(setq list2 '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))

	
	(loop for keyword in list do 
		(if (equal keyword identifier)
			(setq control t)	
		)
	
	)

control

)
(defun getMappedKeyword (keyword)
		(setq i 0)
		(setq returnWord Nil)
		(setq list '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
	(setq list2 '("KW_AND" "KW_OR" "KW_ NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
	(loop for kw = (nth i list)
		while (not (equal kw keyword)) do
			(setq i (+ i 1))
		)

	(setq returnWord (nth i list2))
	returnWord

)


(start)

























