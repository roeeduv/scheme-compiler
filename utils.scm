(define newline "\n\t\t")
(define init (list "ERROR"))

(define sob_Void (string-append "sob_Void:
			dq MAKE_LITERAL(T_VOID,0)" newline ))
(define sob_Null (string-append "sob_Null:
			dq MAKE_LITERAL(T_NIL,0)" newline))
(define sob_False (string-append "sob_False:
			dq MAKE_LITERAL(T_BOOL,0)" newline))
(define sob_True (string-append "sob_True:
			dq MAKE_LITERAL(T_BOOL,1)" newline))
(define sob_One (string-append "sob_One:
			dq MAKE_LITERAL(T_INTEGER,1)" newline))	



(define lookup-const
  (lambda (val table)
    (if (null? table) 'errorConst
        (let ((curr (car table)))
          (if (equal? (car curr) val)
              (cadr curr)
              (lookup-const val (cdr table)))))))

(define lookup-fvar
  (lambda (var ftable)
    (cond 
      ((null? ftable) 'ErrorFvar)
      ((eq? var (caar ftable)) (cadr (car ftable)))
      (else (lookup-fvar var (cdr ftable)))))
  )  




(define get-all-cdr
    (lambda (lst)
      (cond ((null? lst) lst)
            (else (cons (cdr lst) (get-all-cdr (cdr lst)))))))
			
(define make_list_from_not_proper_list
    (lambda (almost_lst)
        (cons (car almost_lst) (cons (cdr almost_lst) '()))
    ))			

(define list->set 
  (lambda (s) 
    (fold-right
      (lambda (a s)
        (if (ormap (lambda (si) (equal? a si)) s)
            s
            (cons a s))) 
      '()
      s)))


(define fraction? 
  (lambda (n)
    (and (not (integer? n)) (rational? n))))

(define last-bucket "bucket_0")

(define get-bucket
  (lambda ()
    (set! last-bucket (bucketL))
    last-bucket))
    
(define string-join
  (lambda (lst delimeter)
    (if (null? lst) ""
    (fold-left string-append (format "~A" (car lst)) (map (lambda (e) (format "~A~A" delimeter e) ) (cdr lst)))  
  )))

(define string-join-end
  (lambda (lst delimeter)
    (if (null? lst) ""
    (fold-left string-append "" (map (lambda (e) (format "~A~A" e delimeter) )  lst))  
  )))
  
  

(define label-factory
  (lambda (name)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append name "_"(number->string n))))))

(define fvarL
    (label-factory "globalsFvarL"))

(define constL
  (label-factory "const"))
  
(define elseL
  (label-factory "elseL"))

(define exitL
  (label-factory "exitL"))

(define bucketL
  (label-factory "bucketL"))
  
(define errorL
  (label-factory "errorL"))

(define loopL
    (label-factory "loopL"))
    
(define loopExitL
    (label-factory "loopExitL"))
   
(define lambdaL
    (label-factory "lambdaL"))
    
(define lambdaExitL
    (label-factory "lambdaExitL"))
    
(define closureL
    (label-factory "closureL")) 

(define contL
  (label-factory "contL"))	
	
(define falseL
  (label-factory "falseL"))

(define trueL
  (label-factory "trueL"))

(define finishL
  (label-factory "finishL"))

(define typeL
  (label-factory "typeL"))

(define fractL
  (label-factory "fractionL"))

(define posL
  (label-factory "posL"))

(define eqL
  (label-factory "eqL"))

(define regL
  (label-factory "regL"))

(define makeStrL
  (label-factory "makeStrL"))

(define makeVecL
  (label-factory "makeVecL"))
  
(define makeStrL
  (label-factory "makeStrL"))  
 
(define intL
  (label-factory "intL"))
  
(define multL
  (label-factory "multL"))

(define compL
  (label-factory "compL"))

(define divL
  (label-factory "divideL"))
 
(define sheverL
  (label-factory "sheverL"))

(define negL
  (label-factory "negL"))

(define testL
  (label-factory "testL"))  


(define global-fvars
	'(append apply < > = + / * - char? string? boolean? integer? 
	number? null? eq? pair? symbol? vector? zero? car cdr char->integer 
	procedure? rational? cons denominator integer->char list make-string 
	make-vector map not remainder numerator string-length 
	string-ref string-set! string->symbol symbol->string vector vector-length
	vector-ref vector-set! list-length plus minus mul div)) 
					 
					 
(define reset_reg (string-append "xor r8, r8 " newline
								"xor r9,r9 " newline 
								"xor r10,r10 " newline 
								"xor r11,r11 " newline 
								"xor r12,r12 " newline))

								
(define print_rax (string-append newline "push rax " newline
								 "call write_sob_if_not_void " newline
								 "add RSP,8" newline ))
								 
(define prolog (string-append  newline "jmp END
                ERROR_NOT_CHAR:
                   add rsp, 8 
                   jmp END
                ERROR_NOT_CLOSURE:
                   add rsp, 8
                   jmp END
                ERROR_NOT_PAIR:
                   add rsp, 8 
                   jmp END
                ERROR_NOT_NUMBER:
                   add rsp, 8
                   jmp END
                ERROR:
                   push const_1
                   call write_sob_if_not_void
                   add rsp, 16 
                   jmp END
                END:
                add rsp, 5*8
                ret\n
				;this *@#$#&* project is done!!!!
				; *****************************************
				;*************Duvdevani*****************
				;*****************************************
				" ))
(define enter
   (lambda (B L addr)
   (string-append "test_malloc 16 " newline
                      "mov rbx,0 " newline
                      "MAKE_LITERAL_CLOSURE rax, rbx ," B newline
                      "mov qword[" addr "], rax " newline
                      "jmp " L newline
                       B ":" newline
                      "push rbp" newline
                      "mov rbp, rsp" newline
					  "xor rdx, rdx" newline )))
								