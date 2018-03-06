(load "project/comp.scm")
(load "project/runtimefuncs.scm")

;mayer funcs 
 (define pipeline
  (lambda (s)
    ((star <sexpr>) s
      (lambda (m r)
        (map (lambda (e)
          (annotate-tc
          (pe->lex-pe
          (box-set
          (remove-applic-lambda-nil (parse e))))))
        m))
      (lambda (f) 'fail))))

(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
        (lambda ()
          (let ((ch (read-char in-port)))
            (if (eof-object? ch)
              (begin
                (close-input-port in-port)
                '())
              (cons ch (run)))))))
      (run)))))

(define string->file
  (lambda (str out)
     (begin 
     (delete-file out)
        (let* ((out-port (open-output-file out)))
            (begin (for-each (lambda(ch) (write-char ch out-port)) (string->list str)) 
        (close-output-port out-port))))))

		;;;;;;

(define ast-search-const
  (lambda (exp)
    (cond
      ((or (null? exp) (not (list? exp)))	 '())
      ((eq? 'const (car exp))	(cdr exp))
      (else (append (ast-search-const (car exp)) (ast-search-const (cdr exp)))))))
	  
(define ast-search-fvar
  (lambda (exp)
    (cond
      ((or (null? exp) (not (list? exp)))	 '())
      ((eq? 'fvar (car exp))	(cdr exp))
      (else (append (ast-search-fvar (car exp)) (ast-search-fvar (cdr exp)))))))	
        
    
(define get-all-fvar-expr
    (lambda (list-exprs)
         (let ((fvars `( ,@(map ast-search-fvar list-exprs))))
            (fold-left append '() fvars))))

(define get-all-const-expr
  (lambda (list-exprs)
    (let* ((init-consts-lsts `( ,@(map ast-search-const list-exprs)))
           (init-consts (fold-left append '() init-consts-lsts))
        (no_duplicates_list  
            (reverse (list->set  (reverse (append init (split-consts init-consts '())))))))
      no_duplicates_list)))			
			
(define build-ftable
    (lambda (list-fvars)
        (map (lambda (var) `(,var ,(fvarL))) (list->set (append global-fvars list-fvars)))))
    
(define build-ctable
  (lambda (const-lst)
    (letrec ((iter
      (lambda (table lst addr)
       (if (null? lst) `(,table ,addr)
        (let ((type (get-const-type (car lst) table)))
            (iter `(,@table (,(car lst) ,(if (symbol? (car lst)) (caddr type) addr) ,type)) (cdr lst) (constL)))))))
      (iter '() const-lst (constL)))))
	
	
; handle all type of consts and typology sort them inside a list 
; todo add symbol
(define split-consts
  (lambda (consts-lst acc)
    (cond
      ((null? consts-lst) acc)
      ((fraction? (car consts-lst)) 
          (split-consts (cdr consts-lst) (append acc `(,(numerator (car consts-lst))) `(,(denominator (car consts-lst))) `(,(car consts-lst)))))
      ((vector? (car consts-lst))
          (split-consts (cdr consts-lst) (append acc (split-consts (vector->list (car consts-lst)) '()) `( ,(car consts-lst)))))
      ((list? (car consts-lst)) 
          (split-consts (cdr consts-lst) (append acc (split-consts (car consts-lst) '()) (reverse (get-all-cdr (car consts-lst))) `(,(car consts-lst)))))
      ((pair? (car consts-lst))
                (split-consts (cdr consts-lst) (append acc (split-consts (make_list_from_not_proper_list (car consts-lst)) '()) `(, (car consts-lst)))))
      ((symbol? (car consts-lst))
            (split-consts (cdr consts-lst) (append acc `(,(symbol->string (car consts-lst)) ,(car consts-lst)))))
      (else 
          (split-consts (cdr consts-lst) (append acc `(,(car consts-lst))))))))


(define get-const-type 
  (lambda (val table)
    (cond 
       ((integer? val)  `("T_INTEGER" ,val))
       ((equal? val (void)) `("T_VOID" ,0))
       ((null? val) `("T_NIL" ,0)) 
       ((boolean? val) `("T_BOOL" ,(if val 1 0)))
       ((char? val) `("T_CHAR" ,(char->integer val)))
       ((string? val) `("T_STRING" ,val))
       ((vector? val ) `("T_VECTOR" ,@(map (lambda (ele) (lookup-const ele table)) (vector->list val))))
       ((fraction? val) `("T_FRACTION" ,(lookup-const (numerator val) table) ,(lookup-const (denominator val) table)))
       ((pair? val) `("T_PAIR" ,(lookup-const (car val) table) ,(lookup-const (cdr val) table)))
       ((symbol? val)
        (let ((next-bucket last-bucket))
        `("T_SYMBOL" ,(lookup-const (symbol->string val) table) ,(get-bucket) ,next-bucket))))
    ))


(define get-asm-const-line
  (lambda (table-line)
    (let* ((value (caddr table-line))
           (addr (cadr table-line))
           (type (car value)))
      (cond 
        ((or (equal? type "T_INTEGER") (equal? type "T_NIL") (equal? type "T_VOID") (equal? type "T_BOOL") (equal? type "T_CHAR"))
            (format "\t\t~A:\n\t\t\tdq MAKE_LITERAL(~A,~S)\n" addr type (cadr value)))
        ((equal? type "T_PAIR") 
         (format "\t\t~A:\n\t\t\tdq MAKE_LITERAL_PAIR(~A,~A)\n" addr (cadr value) (caddr value)))
        ((equal? type "T_STRING") 
         (format "\t\t~A:\n\t\t\tMAKE_LITERAL_STRING ~S\n" addr (cadr value)))
        ((equal? type "T_VECTOR")
         (if (null? (vector->list (car table-line))) 
              (format "\t\t~A:\n\t\t\tdq MAKE_LITERAL(~A,0)\n" addr type)
          (format "\t\t~A:\n\t\t\tMAKE_LITERAL_VECTOR ~A\n" 
                 addr (string-join (map (lambda (e) (format "~A" e)) (cdr value)) ", "))))
        ((equal? type "T_FRACTION") 
         (format "\t\t~A:\n\t\t\tdq MAKE_LITERAL_FRACTION(~A,~A)\n" addr (cadr value) (caddr value)))
        ((equal? type "T_SYMBOL")
         (let ((bucket_label (caddr value))
               (next_bucket (cadddr value))
               (sym_string (cadr value)))
           (string-append newline
                          bucket_label ":
                            dq MAKE_SYMBOL_BUCKET("sym_string ","next_bucket ")" newline
                          )))
          
      (else "WTF"))
    )))


(define get-asm-const-table
  (lambda (table)
    (if (null? table) ""
    (string-append (get-asm-const-line (car table)) (get-asm-const-table (cdr table))))))



(define get-asm-ftable
    (lambda (scm-ftable)
        (if (null? scm-ftable) ""
              (string-append (cadr (car scm-ftable)) ":" newline "dq SOB_UNDEFINED" newline (get-asm-ftable (cdr scm-ftable))))))


			 

(define generate_asm
  (lambda (lst-expr ctable ftable code)
    (if (null? lst-expr) code 
        (generate_asm (cdr lst-expr) ctable  ftable
                  (string-append code 
                    (cgen (car lst-expr) 0 ctable ftable)
					print_rax )))))


					
;generating one expr code in assembly, depend on type
(define cgen
  (lambda (expr major ctable ftable)
    (cond 
       ((eq? 'const (car expr))
			(cgen-const expr major ctable ftable))          
       ((eq? 'if3 (car expr))
          (cgen-if expr major ctable ftable))
       ((eq? 'or (car expr))
          (cgen-or expr major ctable ftable))
       ((eq? 'seq (car expr))
          (cgen-seq expr major ctable ftable))
       ((eq? 'pvar (car expr))
            (cgen-pvar expr major ctable ftable))
       ((eq? 'bvar (car expr))
            (cgen-bvar expr major ctable ftable))
       ((eq? 'fvar (car expr))
            (cgen-fvar expr major ctable ftable))
       ((or (eq? 'set (car expr)) (eq? 'define (car expr))) 
            (cgen-set expr major ctable ftable))
       ((eq? 'box (car expr))
            (cgen-box expr major ctable ftable))
       ((eq? 'box-get (car expr))
            (cgen-box-get expr major ctable ftable))
       ((eq? 'box-set (car expr))
            (cgen-box-set expr major ctable ftable))
		((eq? 'applic (car expr))
            (cgen-applic expr major ctable ftable))
       ((eq? 'tc-applic (car expr))
            (cgen-tc-applic expr major ctable ftable))
		((eq? 'lambda-simple (car expr))
            (cgen-lambda-simple expr major ctable ftable))
		((eq? 'lambda-opt (car expr))
            (cgen-lambda-opt expr major ctable ftable))		
  )))

(define cgen-const
	(lambda (expr major ctable ftable)
	(string-append newline "mov rax,"  (lookup-const (cadr expr) ctable) newline )))  
 
(define cgen-if
	(lambda (expr major ctable ftable)
          (let* ((if_test (cadr expr))
                 (if_then (caddr expr))
                 (if_else (cadddr expr))
                 (label_else (elseL))
                 (label_exit (exitL)))
                    (format "~A
                            mov rax, [rax]
                            CMP RAX,QWORD[~A]
                            JE ~A
                            ~A
                            jmp ~A
                            ~A:
                            ~A
                            ~A:" 
                    (cgen if_test major ctable ftable) (lookup-const #f ctable) label_else (cgen if_then major ctable ftable) 
					label_exit label_else (cgen if_else major ctable ftable) label_exit))))
       
 
(define cgen-or 
	(lambda (expr major ctable ftable)
	(let ((label_exit (exitL)))
        (letrec ((iter
          (lambda (exp rest ans)
            (if (not (null? rest))
              (let ((ans (string-append ans 
                    (format "
                           ~A
                           mov rbx,[rax] 
                           CMP RBX, QWORD[~A] ; #f
                           JNE ~A
                           "
                  (cgen exp major ctable ftable) (lookup-const #f ctable) label_exit))))
                (iter (car rest) (cdr rest) ans))
              (string-append ans
                      (format 
                            "~A
                             "
                  (cgen exp major ctable ftable)))
              ))))
        (string-append (iter (car (cadr expr)) (cdr (cadr expr)) "") 
                    (format "~A:" label_exit))
        ))))
 
(define cgen-seq
	(lambda (expr major ctable ftable)	
		(fold-left string-append "" (map (lambda (exp) (cgen exp major ctable ftable)) (cadr expr)))))

(define cgen-define 
	(lambda (expr major ctable ftable)
		(string-append (cgen (caddr expr) major ctable ftable) newline
						"mov qword[" (lookup-fvar (cadr(cadr expr)) ftable) "],rax " newline
						"mov rax, sob_Void " newline )))
						
(define cgen-box-set 
    (lambda (expr major ctable ftable)
				(string-append 	(cgen (caddr expr) major ctable ftable) newline 
								"mov rbx, rax " newline
								(cgen (cadr expr) major ctable ftable) newline
								"mov qword[rax],rbx " newline 
								"mov rax,sob_Void " newline )))
  
(define cgen-box-get 
    (lambda (expr major ctable ftable)
		(string-append (cgen (cadr expr) major ctable ftable) newline
		"mov rax,qword[rax] " newline )))
              
(define cgen-box 
	(lambda (expr major ctable ftable)
		(string-append (cgen (cadr expr) major ctable ftable) newline
						"mov rbx,rax " newline
						"test_malloc 8 " newline
						"mov qword[rax],rbx " newline )))
 
(define cgen-fvar
	(lambda (expr major ctable ftable)
			(string-append "mov rax, qword[" (lookup-fvar (cadr expr) ftable)"]" newline)))

(define cgen-pvar
    (lambda (expr major ctable ftable)
			(string-append "mov rax, [rbp + (4 +" (number->string (caddr expr))")*8]" newline)))
                    
(define cgen-bvar
        (lambda (expr major ctable ftable)
				  (string-append "mov rax,qword[rbp + 2*8] " newline
								 "mov rax,qword[rax + "(number->string (caddr expr)) "*8]" newline
								 "mov rax,qword[rax + "(number->string (cadddr expr)) "*8]" newline ))) 
 
 						
(define cgen-set-pvar 
	(lambda (expr major ctable ftable)	
		(let* 	((var (cadr expr))
				(value (caddr expr))
				(mi (caddr var)))
					(string-append (cgen value major ctable ftable) newline
					"mov qword[rbp + (4+" (number->string mi) ")*8],rax " newline 
					"mov rax, sob_Void " newline ))))

(define cgen-set-bvar 
	(lambda (expr major ctable ftable)	
		(let* 	((var (cadr expr))
				(value (caddr expr))
				(mi (caddr var))
				(ma (caddr var)))
					(string-append (cgen value major ctable ftable) newline
						"mov rbx, qword[rbp + 2*8] " newline
						"mov qword[rbp + (4+" (number->string ma) ")*8],rax " newline
                        "mov qword[rbp + (4+" (number->string mi) ")*8],rax " newline
                        "mov rax, sob_Void " newline ))))			
						
(define cgen-set
	(lambda (expr major ctable ftable)
		(let ((var (cadr expr)))
			(cond
            ((eq? 'pvar (car var)) 
			(cgen-set-pvar expr major ctable ftable))
			((eq? 'bvar (car var))
               (cgen-set-bvar expr major ctable ftable))
            (else 
              (cgen-define expr major ctable ftable))))))
  

  

  
(define extend-env 
	(lambda (major lambda_label)
		(let 	((start_first_loop (loopL))
                (end_first_loop (loopExitL))
				(start_second_loop (loopL))
				(body (lambdaL))
				(end_second_loop (loopExitL)))
                (string-append "test_malloc " (number->string (* 8 (+ 1 major))) newline
						"mov rbx, rax" newline
					    "mov rax, [rbp + 8*2] " newline
                        "mov rdi, 0" newline
                        start_first_loop ": " newline
                        "cmp rdi, " (number->string major)  newline
                        "je " end_first_loop  newline
                        "mov r10, [rax + rdi*8]" newline           
                        "mov [rbx + rdi*8 + 1*8], r10" newline       
                        "inc rdi" newline
                        "jmp " start_first_loop  newline
                        end_first_loop ": " newline
                        "mov r8, [rbp+8*3] " newline
                        "add r8,1 " newline
                        "mov r9, r8" newline
                        "shl r9, 3" newline
                        "test_malloc r9" newline
                        "mov rcx , rax" newline
                        "mov rdi, 0" newline
                         start_second_loop ":" newline
                        "cmp rdi , r8" newline
                        "je " end_second_loop  newline
                        "mov r9, [rbp + (4+rdi) * 8]" newline
                        "mov [rcx + rdi*8], r9 " newline
                        "inc rdi" newline
                        "jmp " start_second_loop  newline
                         end_second_loop ":"  newline
                        "mov [rbx], rcx" newline	
						"test_malloc 16 " newline
						"MAKE_LITERAL_CLOSURE rax, rbx, " body newline
						"jmp " lambda_label newline
						 body ":" newline
						 ;reset_reg
						"push rbp" newline
						"mov rbp, rsp" newline	))))
						
(define cgen-lambda-simple 
	(lambda (expr major ctable ftable)
        (let 	((lambda_label (lambdaExitL)))
                (string-append (extend-env major lambda_label)
						 (cgen (caddr expr) (+ 1 major) ctable ftable) 
						"CLEAN_STACK" newline
						"ret" newline
						lambda_label ": " newline	))))
  
  
(define cgen-lambda-opt 
    (lambda (expr major ctable ftable)
        (let* ((lambda_label (lambdaExitL))
			(fix_params (length (cadr expr)))
            (loop_enter (loopL))
            (loop_exit (loopExitL))
            (closure_label (closureL)))
                (string-append (extend-env major lambda_label)
                    "mov r8, [rbp+3*8] " newline
                    "sub r8, " (number->string fix_params) newline
                    "mov rdi, 8* " (number->string fix_params) newline
                    "add rdi, 3*8" newline
                    "mov rsi, r8 " newline
                    "mov rax, sob_Null " newline
                     loop_enter ":" newline
                    "cmp rsi, 0" newline
                    "je " loop_exit newline
                    "push rax " newline
                    "mov rax, rsi" newline
                    "shl rax,3" newline
					"xor r10 , 10 " newline
                    "mov r10, rdi " newline
                    "add r10,rax " newline
                    "mov rax, qword[rbp + r10] " newline
                    "push rax " newline
                    "mov rax, [" (lookup-fvar 'cons ftable) "]" newline
                    "CALL_LIB_FUN rax, 1 " newline
                    "dec rsi" newline
                    "jmp " loop_enter newline
                     loop_exit ":" newline
                    "mov [rbp + "(number->string (* 8 (+ 4 fix_params)))"], rax" newline
                     (cgen (cadddr expr) (+ 1 major) ctable ftable) newline
                    "CLEAN_STACK" newline
                    "ret" newline
                    lambda_label ":" newline ))))
            

			
(define cgen-applic-setup
    (lambda (expr major ctable ftable)
                (string-append  "mov rax, sob_Null " newline
								"push rax" newline 
                    (string-join-end (map (lambda (p) (cgen p major ctable ftable)) (reverse (caddr expr))) "\n\t\tpush rax\n\t\t")
								"mov rax, " (number->string (length (caddr expr))) newline
								"push rax" newline
								(cgen (cadr expr) major ctable ftable) newline
                                "CHECK_CLOSURE" newline
                                "CLOSURE_ENV rbx" newline
                                "push rbx" newline )))
  
(define cgen-applic
    (lambda (expr major ctable ftable)
                (string-append  (cgen-applic-setup expr major ctable ftable)
                                "CLOSURE_CODE rax" newline
								;reset_reg
                                "call rax" newline)))								 

(define cgen-tc-applic
    (lambda (expr major ctable ftable)
        (let* ((loop_enter (loopL))
               (loop_exit (loopExitL)))
                (string-append (cgen-applic-setup expr major ctable ftable) 
									"mov r11, qword[rbp + 8*3] " newline
                                    "mov r12, r11 " newline                                  
                                    "mov r8,rbp " newline
                                    "mov rbp,[rbp] " newline                                                                
                                    "mov rbx, [r8+8] " newline
                                    "push rbx " newline
                                    "mov rdi,0 " newline
                                    "mov r10, r11 " newline
                                    "add r10, 5 " newline
                                    "shl r10, 3 " newline                                  
                                    "add r10,r8 " newline                                                               
                                    loop_enter ": " newline
                                    "cmp rdi, " (number->string (+ 4 (length (caddr expr)))) newline
                                    "je " loop_exit newline
                                    "sub r8, 8 " newline
                                    "sub r10,8 " newline             
                                    "mov r9, [r8] " newline
                                    "mov [r10], r9 " newline
                                    "inc rdi " newline
                                    "jmp " loop_enter  newline
                                    loop_exit ": " newline                                     
                                    "add r12, 4+1 " newline
                                    "shl r12, 3 " newline
                                    "add rsp, r12 " newline                       
                                    "CLOSURE_CODE rax " newline
                                    "jmp rax " newline
                                    )))) 
		 

(define compile-scheme-file 
  (lambda (in out)
    (let* ((lst-exprs (append scheme-functions  (pipeline (file->list in))))
        (ctable (build-ctable (get-all-const-expr lst-exprs)))
        (ftable (build-ftable (get-all-fvar-expr lst-exprs)))
        (asm-ctable (string-append 
					"const_table:" newline (get-asm-const-table (car ctable))))
		(asm-ftable (string-append
					"global_table:" newline (get-asm-ftable ftable)))
		(asm-lib-func 
			(string-append 
                             (runtime-boolean? ftable) newline
                             (runtime-car ftable)  newline
                             (runtime-cdr ftable) newline
                             (runtime-char? ftable) newline
                             (runtime-eq? ftable) newline
                             (runtime-string? ftable) newline
                             (runtime-cons ftable) newline
                             (runtime-char->integer ftable) newline
                             (runtime-null? ftable) newline
                             (runtime-integer? ftable)newline
                             (runtime-number? ftable) newline
                             (runtime-apply ftable) newline
                             (runtime-b_plus ftable) newline
                             (runtime-string-ref ftable)newline
                             (runtime-symbol? ftable) newline
                             (runtime-string-set ftable) newline
                             (runtime-make-vector ftable) newline
                             (runtime-symbol->string ftable) newline
                             (runtime-string->symbol ftable) newline
                             (runtime-list->vector ftable) newline
                             (runtime-b_div ftable)newline
                             (runtime-string-length ftable)newline
							 (runtime-zero? ftable) newline
                             (runtime-pair? ftable) newline
                             (runtime-procedure? ftable) newline
                             (runtime-b_minus ftable) newline
                             (runtime-vector? ftable) newline
                             (runtime-rational? ftable) newline
                             (runtime-numerator ftable) newline
                             (runtime-denominator ftable) newline
                             (runtime-vector-set ftable) newline
                             (runtime-make-string ftable) newline
                             (runtime-vector-length ftable)newline
                             (runtime-vector-ref ftable)newline
                             (runtime-remainder ftable) newline
                             (runtime-integer->char ftable) newline
                             (runtime-not ftable) newline
                             (runtime-b_equal ftable) newline
                             (runtime-positive? ftable) newline
                             (runtime-b_mul ftable)newline       
							 ))
           (asm-code (generate_asm lst-exprs (car ctable) ftable newline)) 
           (asm-output 
				(string-append "%include \"project/scheme.s\"\nsection .bss\nglobal main\nsection .data\n\t " newline
				asm-ctable newline
				sob_Void 
				sob_Null
				sob_False
				sob_True
				sob_One
				"bucket_0: " newline
				"dq MAKE_SYMBOL_BUCKET(sob_Null, sob_Null)" newline
				"bucket_head: " newline
				"dq 0 " newline
				asm-ftable newline
				"section .text "newline
				"main: "newline
				"mov qword[bucket_head]," last-bucket newline
				"mov rax, malloc_pointer " newline
				"mov qword [rax], start_of_data2 " newline
				asm-lib-func newline
				"push sob_Null " newline
                "mov rax, 0 " newline
                "push rax " newline
                "mov rax, [sob_Null] " newline
                "push rax " newline
                "mov rax, 0x1234 " newline
                "push rax " newline
                "push rbp " newline
                "mov rbp, rsp " newline
				asm-code
				prolog
				)))
         (string->file asm-output out))))    