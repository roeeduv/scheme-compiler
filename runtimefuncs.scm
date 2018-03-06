(load "project/utils.scm")

	
(define scheme-functions 
 (map (lambda(e) 
          (annotate-tc
          (pe->lex-pe
          (box-set
          (remove-applic-lambda-nil (parse e))))))
  (list 
     
    '(define list (lambda x x))
    
    '(define + 
       (let ((b_plus b_plus))
        (lambda x
          (if (null? x) 0
            (b_plus (car x) (apply + (cdr x)))))))    
    
    '(define - 
       (let ((b_minus b_minus))
        (lambda (x . y)
          (letrec ((iter 
       (lambda (acc x . z)
          (if (null? z) (b_minus acc x)
(apply iter (cons (b_minus acc x) z))))))
          (if (null? y) (b_minus 0 x)
         (apply iter (cons x y)))))))
    '(define =
       (let ((b_equal b_equal))
       (lambda (x . y)
         (if (null? y) #t
             (and (b_equal x (car y)) (apply = y))))))
    
    
    '(define list-length
      (lambda (x)
        (if (null? x) 
          0
          (+ 1 (list-length (cdr x))))))
    
    '(define append 
        (lambda x
          (letrec ((app 
        (lambda (lst1 lst2)
            (if (null? lst1)  lst2
  (cons (car lst1) (app (cdr lst1) lst2))))))
  (if (null? x) x
      (if (null? (cdr x)) (car x)
          (app (car x) (apply append (cdr x))))))))

    '(define >
       (let ((bigger? bigger?)
            (positive? positive?))
      (lambda (x . y)
        (if (null? y)
          #t
          (letrec ((bigger?
       (lambda (x y rest)
          (if (null? rest) 
            (positive? (- x y))
            (and (positive? (- x y))
(bigger? y (car rest) (cdr rest)))))))
          (bigger? x (car y) (cdr y)))))))

    '(define <
      (let ((bigger? bigger?)
            (positive? positive?))
      (lambda (x . y)
        (if (null? y)
          #t
          (letrec ((bigger?
    (lambda (x y rest)
      (if (null? rest) 
        (and (not (positive? (- x y))) 
(not (zero? (- x y))))
        (and (not (positive? (- x y))) 
(not (zero? (- x y)))
             (bigger? y (car rest) (cdr rest)))))))
          (bigger? x (car y) (cdr y)))))))


    '(define *
        (lambda x 
          (let ((b_mul b_mul))
          (cond ((null? x) 1)
            (else (b_mul (car x) (apply * (cdr x))))))))

    '(define / 
      (let ((b_div b_div))
        (lambda (x . y)
          (letrec ((iter 
       (lambda (acc x . z)
          (if (null? z) (b_div acc x)
(apply iter (cons (b_div acc x) z))))))
          (if (null? y) (b_div 1 x)
         (apply iter (cons x y)))))))
         
    '(define vector
       (let ((list-length list-length)) 
        (lambda x 
          (if (null? x) #()
            (list->vector x (list-length x))))))

    
    '(define b_map
      (lambda (f s)
        (if (null? s)
            s
            (cons (f (car s))
    (b_map f (cdr s))))))
    
    '(define maplist
       (let ((b_map b_map))
      (lambda (f s)
        (if (null? (car s))
            '()
            (cons (apply f (b_map car s))
    (maplist f (b_map cdr s)))))))
    
    '(define map
      (let ((maplist maplist))
        (lambda (f . s)
          (maplist f s))))
  )))



(define runtime-string-set
  (lambda (ftable)
    (let* ((address (lookup-fvar 'string-set! ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) 
       "mov rbx, qword[rbp + 4*8]
        mov rcx, qword[rbp + 5*8] 
        mov rdx, qword[rbp + 6*8] 
        mov rax, [rbx]
        STRING_ELEMENTS rax
        mov r8, [rcx]
        DATA r8
        add rax, r8
        xor rcx, rcx
        mov rcx, [rdx]
        DATA rcx
        mov byte [rax], cl
        mov rax, sob_Void
        CLEAN_STACK
        ret
        " lambda_end ":"
      ))))



(define runtime-symbol->string
  (lambda (ftable)
    (let* ((address (lookup-fvar 'symbol->string ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rbx, qword[rbp + 4*8] 
        mov rbx,[rbx]
		xor rax, rax ;
        mov rax, rbx
        TYPE rax
        cmp rax, T_SYMBOL
        JNE ERROR
        packed_car rbx
        mov rax, rbx
        CLEAN_STACK
        RET
        " lambda_end ":"
      ))))


(define runtime-string->symbol
  (lambda (ftable)
    (let* ((address (lookup-fvar 'string->symbol ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (loop_start (loopL))
      (loop_end (loopExitL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rbx, qword[rbp + 4*8] 
        mov r9,[rbx]
        mov rdx, qword[bucket_head]
        "loop_start ":
        cmp rdx, bucket_0
        je .create_new_bucket
        mov r8, rdx
        mov rdx, [rdx]
		xor rax, rax ;
        mov rax, rdx
        CAR rax
        STR_CMPR rax, r9
        je .found_bucket 
        packed_cdr rdx
        jmp " loop_start "
        .create_new_bucket:
        mov rdx, qword[bucket_head]
        MAKE_BUCKET rbx, rdx
        test_malloc 8
        mov [rax], rbx
        mov [bucket_head],rax
        mov r8,rax
        .found_bucket:
       mov rax,r8
        .finish:
        CLEAN_STACK
        RET
        " lambda_end ":"
      ))))



(define runtime-string? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'string? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        TYPE rax
        cmp rax, T_STRING
        jne " check_false "
        mov rax,sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-rational? 
  (lambda (ftable)
     (let ((address-src (lookup-fvar 'number? ftable))
          (address-dest (lookup-fvar 'rational? ftable)))
       (string-append "
        mov rax, qword["address-src "]
        mov qword["address-dest "],rax
        "))))
        
(define runtime-number? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'number? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (check_true (trueL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
		xor rax, rax ;
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_false "
        mov rax, sob_True
        jmp " finish "
        " check_false ":
        cmp rax, T_FRACTION
        je " check_true "
        mov rax,sob_False
        jmp " finish "
        " check_true ":
        mov rax, sob_True
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))	  
	  
(define runtime-make-vector
  (lambda (ftable)
    (let* ((address (lookup-fvar 'make-vector ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (loop_start (loopL))
      (loop_end (loopExitL))
      (regular_case (regL))
      (build_vector (makeVecL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov r8, qword[rbp + 3*8] 
        mov rbx, qword[rbp + 4*8] 
        cmp r8, 2
        je "regular_case "
        mov rdx, 0
        MAKE_INT rdx
        jmp "build_vector "
        "regular_case ":
        mov rcx, qword[rbp + 5*8] 
        mov rdx, [rcx]
        "build_vector ":
        mov r9, [rbx]
        DATA r9
        mov r10,r9 
        shl r9, 3 
        test_malloc r9
        mov r8, rax
        mov r11, r8 
        mov rsi, 0
        "loop_start ":
        cmp rsi, r10
        je "loop_end "
        test_malloc 8
        mov [rax], rdx
        mov [r8+rsi*8], rax
        add rsi, 1
        jmp "loop_start "
        "loop_end ":
        add r8, r9
        MAKE_VECTOR r11, r8
        test_malloc 8
        mov [rax], r8
        CLEAN_STACK
        ret
        " lambda_end ":
        "
      ))))	


(define runtime-vector-ref
  (lambda (ftable)
    (let* ((address (lookup-fvar 'vector-ref ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        mov rbx, [rbx]
        mov rdx, [rdx]
        DATA rbx
        test_malloc 8
        mov r8, rax
        VECTOR_REF r8,rdx,rbx
       ; test_malloc 8
        mov rax, r8
        CLEAN_STACK
        ret
        " lambda_end ":"
      ))))




(define runtime-null?
  (lambda (ftable)
    (let ((address (lookup-fvar 'null? ftable))
          (check_false (falseL))
          (finish (finishL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
		xor rdx, rdx ;
        mov rdx, qword[rbp + 4*8] 
        mov rdx,[rdx]
        mov rax, qword[sob_Null] 
        cmp rdx, rax
        jne " check_false "
        mov rax, sob_True 
        jmp " finish "
        " check_false ":
        mov rax, sob_False 
        " finish ":    
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))
	  
(define runtime-integer->char
  (lambda (ftable)
    (let ((address (lookup-fvar 'integer->char ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rdx,[rdx]
        mov rax, rdx
        TYPE rax
        cmp rax, T_INTEGER
        jne ERROR_NOT_CHAR    
        test_malloc 8
        DATA rdx
        shl rdx, TYPE_BITS
        or rdx, T_CHAR
        mov [rax],rdx
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))	  

(define runtime-char->integer 
  (lambda (ftable)
    (let ((address (lookup-fvar 'char->integer ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rdx,[rdx]
        mov rax, rdx
        TYPE rax
        cmp rax, T_CHAR
        jne ERROR_NOT_CHAR
        test_malloc 8
        shr rdx, TYPE_BITS
        shl rdx, TYPE_BITS
        or rdx, T_INTEGER
        mov [rax],rdx
        CLEAN_STACK
        ret
        " lambda_end ":
        "))))
	  

(define runtime-b_div
  (lambda (ftable)
    (let* ((address (lookup-fvar 'b_div ftable))
          (compare_second (compL))
          (divide (divL))
          (multiple (multL))
          (switch (divL))
          (error (errorL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
		xor rdx, rdx ;
        mov rdx, qword[rbp + 4*8]
        mov rbx, qword[rbp + 5*8] 
        mov rax, [rdx] 
        TYPE rax
        cmp rax, T_FRACTION
        je " compare_second "
        cmp rax, T_INTEGER
        jne " error "
        mov rax, sob_One   
        MAKE_FRACTION rdx, rax
        test_malloc 8
        mov [rax], rdx
        mov rdx, rax
        "compare_second ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        je "switch "
        cmp rax, T_INTEGER
        jne " error "
        mov rax, [rbx]
        DATA rax
        cmp rax, 0
        je " error "        
        mov rax, sob_One   
        MAKE_FRACTION rbx, rax
        test_malloc 8
        mov [rax], rbx
        mov rbx, rax
        "switch ":
        mov r8, [rbx]
        CAR r8 
        mov rcx, r8 
        DATA rcx
        cmp rcx, 0
        jg "divide "
        mov r10, rdx
        mov rax, rcx 
        mov rcx, -1
        mul rcx
        mov rcx, rax
        MAKE_INT rcx
        test_malloc 8
        mov [rax], rcx
        mov r9, rax 
        mov rax, [rbx]
        CDR rax 
        DATA rax
        mov rcx, -1
        mul rcx
        mov r8, rax
        MAKE_INT r8
        test_malloc 8
        mov [rax], r8
        mov rbx, rax 
        MAKE_FRACTION rbx, r9       
        test_malloc 8
        mov [rax], rbx
        mov rbx, rax 
        mov rdx, r10
        jmp "multiple "
        "divide ":
        mov r8, [rbx]
        mov rbx, [rbx]
        CDR rbx 
        CAR r8 
        test_malloc 8
        mov [rax], r8
        mov r8, rax
        test_malloc rbx
        mov [rax], rbx
        mov rbx, rax
        MAKE_FRACTION rbx, r8 
        test_malloc 8
        mov [rax], rbx
        mov rbx, rax 
        "multiple ":
        MUL_FRACTION rdx, rbx
        MAKE_FRACTION rdx, rbx
        REDUCE rdx
        REMOVE_FRACTION rdx
        test_malloc 8
        mov [rax],rdx      
        CLEAN_STACK
        ret
        " error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":
        ")))) 


(define runtime-b_mul
  (lambda (ftable)
    (let* ((address (lookup-fvar 'b_mul ftable))
          (compare_second (compL))
          (multiple (multL))
          (error (errorL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        mov rax, [rdx] 
        TYPE rax
        cmp rax, T_FRACTION
        je " compare_second "
        cmp rax, T_INTEGER
        jne " error "
        mov rax, sob_One   
        MAKE_FRACTION rdx, rax
        test_malloc 8
        mov [rax], rdx
        mov rdx, rax
        "compare_second ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        je "multiple "
        cmp rax, T_INTEGER
        jne " error "
        mov rax, sob_One   
        MAKE_FRACTION rbx, rax
        test_malloc 8
        mov [rax], rbx
        mov rbx, rax
        "multiple ":
        MUL_FRACTION rdx, rbx
        MAKE_FRACTION rdx, rbx
        REDUCE rdx
        REMOVE_FRACTION rdx
        test_malloc 8
        mov [rax],rdx      
        CLEAN_STACK
        ret
        " error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":
        "))))          

(define runtime-b_plus
  (lambda (ftable)
    (let* ((address (lookup-fvar 'b_plus ftable))
          (check_fract (fractL))
          (check_fract_2 (fractL))
          (check_fract_3 (fractL))
          (shever (sheverL))
          (both_fraction (fractL))
          (finish (finishL))
          (error (errorL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        mov rax, [rdx] 
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract_3 "
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract "
        mov rdx, [rdx]
        mov rbx, [rbx]       
        DATA rdx
        DATA rbx
        add rdx, rbx
        MAKE_INT rdx
        jmp " finish "
        " check_fract_3 ":
        mov rax, [rdx]
        TYPE rax
        cmp rax, T_FRACTION
        jne "error "
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        je " both_fraction "
        cmp rax, T_INTEGER
        jne "error "
        jmp " shever "
        " check_fract ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        jne " error "
        mov r8, rdx
        mov rdx, rbx
        mov rbx, r8
        jmp "shever "
        " check_fract_2 ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        jne "error "       
        " both_fraction ":
        ADD_FRACTION rdx, rbx 
        MAKE_FRACTION rdx, rbx
        REDUCE rdx
        REMOVE_FRACTION rdx
        jmp " finish "
        " shever ":
        mov r11,[rdx] 
        mov r9, [rbx] 
        mov rax,r11 
        CAR r11 
        DATA r11 
        CDR rax
        mov rbx,rax
        DATA rax 
        xor rdx,rdx
        DATA r9
        mul r9 
        add rax, r11 
        mov r9, rax
        MAKE_INT r9
        test_malloc 8
        mov [rax], r9
        mov rdx, rax 
        test_malloc 8
        mov [rax], rbx
        mov rbx, rax
        MAKE_FRACTION rdx, rbx
        REDUCE rdx
        REMOVE_FRACTION rdx 
        " finish ":
        test_malloc 8
        mov [rax],rdx
        CLEAN_STACK
        ret
        " error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":
        "))))

(define runtime-b_equal
  (lambda (ftable)
    (let* ((address (lookup-fvar 'b_equal ftable))
          (check_fract (fractL))
          (check_fract_2 (fractL))
          (check_fract_3 (fractL))
          (both_fraction (fractL))
          (continue (contL))
          (equal (eqL))
          (finish (finishL))
          (error (errorL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
	    xor rdx, rdx ;
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        mov rax, [rdx] 
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract_3 "
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract "
        mov rdx, [rdx]
        mov rbx, [rbx]       
        DATA rdx
        DATA rbx
        cmp rdx, rbx
        je "equal "
        mov rax, sob_False     
        jmp " finish "
        " check_fract_3 ":
        mov rax, [rdx]
        TYPE rax
        cmp rax, T_FRACTION
        jne "error "
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        je " both_fraction "
        cmp rax, T_INTEGER
        jne "error "
        mov rax, sob_False
        jmp "finish "
        " check_fract ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract_2 "
        mov rax, sob_False
        jmp " finish "
        " check_fract_2 ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        jne "error "       
        " both_fraction ":
        mov rdx, [rdx] 
        mov rbx, [rbx] 
        mov rax, rdx
        CAR rax 
        mov rcx, rbx
        CAR rcx 
        cmp rax, rcx
        je " continue "
        mov rax, sob_False
        jmp "finish "
        "continue ":
        mov rax, rdx
        CDR rax
        mov rcx, rbx
        CDR rcx
        cmp rax, rcx             
        mov rax, sob_False
        jne " finish " 
        "equal ":        
        mov rax, sob_True
        " finish ":    
        CLEAN_STACK
        ret
        " error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":
        "))))


(define runtime-b_minus
  (lambda (ftable)
    (let* ((address (lookup-fvar 'b_minus ftable))
          (check_fract (fractL))
          (check_fract_2 (fractL))
          (check_fract_3 (fractL))
          (both_fraction (fractL))
          (finish (finishL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        mov rax, [rdx] 
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract_3 "
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract "
        mov rdx, [rdx]
        mov rbx, [rbx]       
        DATA rdx
        DATA rbx
        sub rdx, rbx
        MAKE_INT rdx
        jmp " finish "
        " check_fract_3 ":
        mov rax, [rdx]
        TYPE rax
        cmp rax, T_FRACTION
        jne ERROR_NOT_NUMBER
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        je " both_fraction "
        cmp rax, T_INTEGER
        jne ERROR_NOT_NUMBER
        mov rax, sob_One   
        MAKE_FRACTION rbx, rax
        test_malloc 8
        mov [rax], rbx
        mov rbx, rax
        jmp " both_fraction "
        " check_fract ":
        mov rax, [rdx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_fract_2 "
        mov rax, sob_One   
        MAKE_FRACTION rdx, rax
        test_malloc 8
        mov [rax], rdx
        mov rdx, rax
        jmp " both_fraction "
        " check_fract_2 ":
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_FRACTION
        jne ERROR_NOT_NUMBER       
        " both_fraction ":
        NEG_FRACTION rbx
        ADD_FRACTION rdx, rbx 
        MAKE_FRACTION rdx, rbx
        REDUCE rdx
        REMOVE_FRACTION rdx
        " finish ":
        test_malloc 8
        mov [rax],rdx        
        CLEAN_STACK
        ret
        " lambda_end ":
        "))))


(define runtime-zero?
  (lambda (ftable)
    (let ((address (lookup-fvar 'zero? ftable))
          (check_false (falseL))
          (finish (finishL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        xor rdx, rdx ;
		mov rdx, qword[rbp + 4*8] 
        mov rdx, [rdx]
        cmp rdx, T_INTEGER
        jne " check_false "
        mov rax, sob_True 
        jmp " finish "
        " check_false ":
        mov rax, sob_False
        " finish ":    
        CLEAN_STACK
        ret
        " lambda_end ":
        "))))


(define runtime-not
  (lambda (ftable)
    (let ((address (lookup-fvar 'not ftable))
          (check_false (falseL))
          (finish (finishL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
		mov rdx, qword[rbp + 4*8] 
        cmp rdx, sob_False
        jne " check_false "
        mov rax, sob_True
        jmp " finish "
        " check_false ":
        mov rax, sob_False
        " finish ":    
        CLEAN_STACK
        ret
        " lambda_end ":
        "))))

(define runtime-vector? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'vector? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        TYPE rax
        cmp rax, T_VECTOR
        jne " check_false "
        mov rax,sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-pair? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'pair? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        TYPE rax
        cmp rax, T_PAIR
        jne " check_false "
        mov rax,sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))
	  
	  
(define runtime-make-string
  (lambda (ftable)
    (let* ((address (lookup-fvar 'make-string ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (loop_start (loopL))
      (loop_end (loopExitL))
      (regular_case (regL))
      (build_string (makeStrL))
      (lambda_end (lambdaExitL)))
      (string-append "
        test_malloc 16
        mov rbx,0 
        MAKE_LITERAL_CLOSURE rax, rbx ," lambda_start "
        mov qword[" address "], rax
        jmp " lambda_end "
        " lambda_start ":
        push rbp
        mov rbp, rsp
        mov r8, qword[rbp + 3*8] 
        cmp r8, 2
        jg "error " 
        cmp r8, 1
        jl "error " 
        mov rbx, qword[rbp + 4*8] 
        mov rax, [rbx]
        TYPE rax
        cmp rax, T_INTEGER
        jne "error "
        cmp r8, 2
        je "regular_case "
        mov rdx, 0
        jmp "build_string "
        "regular_case ":
        mov rcx, qword[rbp + 5*8] 
        mov r8, [rcx]
        TYPE r8
        cmp r8, T_CHAR
        jne "error "
        mov rdx, [rcx]
        DATA rdx 
        "build_string ":
        mov r9, [rbx]
        DATA r9
        test_malloc r9
        mov r8, rax 
        mov rsi, 0
        "loop_start ":
        cmp rsi, r9
        je "loop_end " 
        mov [r8+rsi], dl
        add rsi, 1
        jmp "loop_start "
        "loop_end ":
        add r8, r9
        MAKE_STRING rax, r8
        test_malloc 8
        mov [rax], r8
        CLEAN_STACK
        ret
        "error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":
        "
      ))))
	  
	  

(define runtime-apply
  (lambda (ftable)
    (let* ((address (lookup-fvar 'apply ftable))
          (address-list (lookup-fvar 'list ftable))
          (loop_enter1 (loopL))
          (loop_exit1 (loopExitL))
          (loop_enter2 (loopL))
          (loop_exit2 (loopExitL))
          (finish (finishL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 3*8]
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8]
        mov rdx,[rdx]
        mov rbx,[rbx]            
        mov rdi, 0
        push T_NIL
        " loop_enter1 ":
        mov rax, rbx
        TYPE rax
        cmp rax, T_PAIR
        jne " loop_exit1 "
        inc rdi
        mov rax, rbx
        packed_car rax
        push rax
        CDR rbx
        jmp " loop_enter1 "
        " loop_exit1 ":
        mov rax, ["address-list "]
        CALL_LIB_FUN rax, rdi
        mov rbx, [rax]
        mov rdi, 0
        push T_NIL
        " loop_enter2 ":
        mov rax, rbx
        TYPE rax
        cmp rax, T_PAIR
        jne " loop_exit2 "
        inc rdi
        mov rax, rbx
        packed_car rax
        push rax
        CDR rbx
        jmp " loop_enter2 "
        " loop_exit2 ":
        mov rdx, qword[rbp + 4*8] 
        CALL_LIB_FUN rdx, rdi
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))	  

(define runtime-vector-set
  (lambda (ftable)
    (let* ((address (lookup-fvar 'vector-set! ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rbx, qword[rbp + 4*8] 
        mov rcx, qword[rbp + 5*8] 
        mov rdx, qword[rbp + 6*8] 
        mov rax, [rbx]
        VECTOR_ELEMENTS rax
        mov r8, [rcx]
        DATA r8
        shl r8, 3
        lea rax, [rax + r8]
        mov [rax], rdx
        mov rax, sob_Void
        CLEAN_STACK
        ret
        " lambda_end ":" ))))

(define runtime-apply
  (lambda (ftable)
    (let* ((address (lookup-fvar 'apply ftable))
          (loop_enter1 (loopL))
          (loop_exit1 (loopExitL))
          (loop_enter2 (loopL))
          (loop_exit2 (loopExitL))
          (loop_enter3 (loopL))
          (loop_exit3 (loopExitL))
          (lambda_start (lambdaL))
          (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 3*8] 
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        mov rdx,[rdx]
        mov rbx,[rbx]            
        mov rdi, 0
        mov rcx, rbx
        mov rax, sob_Null
        "loop_enter1 ":
        cmp rcx,T_NIL
        je " loop_exit1 "
        mov r8, rcx
        packed_car r8
        MAKE_PAIR r8, rax
        test_malloc 8
        mov [rax], r8
        CDR rcx
        inc rdi
        jmp " loop_enter1 "
        " loop_exit1 ":
        push sob_Null
        mov rax, [rax]
        "loop_enter2 ":
        cmp rax,T_NIL
        je " loop_exit2 "
        mov rcx, rax
        packed_car rcx
        push rcx
        CDR rax
        jmp " loop_enter2 "             
        " loop_exit2 ":
        push rdi
        mov r11, rdx
        CLOSURE_ENV r11            
        push r11  
        mov r8,rbp
        mov rbp,[rbp]       
        mov rbx, [r8+8] 
        push rbx
        mov rsi,0
        mov r10, 7*8    
        add r10,r8 
        add rdi, 4        
        " loop_enter3 ":
        cmp rsi, rdi
        je " loop_exit3 "
        sub r8, 8
        sub r10,8 
        .b:          
        mov r9, [r8]
        mov [r10], r9
        inc rsi
        jmp " loop_enter3 "
        " loop_exit3 ":        
        add rsp, 7*8
        .b:
        mov rax, rdx        
        CLOSURE_CODE rax
        jmp rax           
        " lambda_end ":
        "))))
	  

(define runtime-procedure? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'procedure? ftable))
			(lambda_start (lambdaL))
			(lambda_end (lambdaExitL))
			(check_false (falseL))
			(finish (finishL)))
      (string-append "
        test_malloc 16
        mov rbx,0 
        MAKE_LITERAL_CLOSURE rax, rbx ," lambda_start "
        mov qword[" address "], rax
        jmp " lambda_end "
        " lambda_start ":
        push rbp
        mov rbp, rsp
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        TYPE rax
        cmp rax, T_CLOSURE
        jne " check_false "
        mov rax,const_4
        jmp " finish "
        " check_false ":
        mov rax,const_3
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        "))))
		


(define runtime-list->vector
  (lambda (ftable)
    (let ((address (lookup-fvar 'list->vector ftable))
         (error (errorL))
         (loop_start (loopL))
         (loop_end (loopExitL))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append "
        test_malloc 16
        xor rbx, rbx 
        MAKE_LITERAL_CLOSURE rax, rbx ," lambda_start "
        mov qword[" address "], rax
        jmp " lambda_end "
        " lambda_start ":
        push rbp
        mov rbp, rsp
        mov rcx, qword[rbp + 3*8] 
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        cmp rcx, 2
        jne "error "
        mov rax, [rdx]
        TYPE rax
        cmp rax, T_PAIR
        jne "error "
        mov r8, [rdx]
        mov rsi, 0
        mov r11, [rbx]
        DATA r11
        mov rbx, r11
        shl rbx, 3
        test_malloc rbx
        mov r10, rax 
        "loop_start ":
        cmp rsi, r11
        je "loop_end "
        mov rdx, r8
        CAR r8
        check:
        test_malloc 8
  check2:
        mov [rax], r8
        mov [r10+rsi*8], rax
        add rsi, 1
        CDR rdx
        mov r8, rdx
        jmp "loop_start "
        "loop_end ":
        mov rax, r10
        shl rsi, 3
        add r10, rsi
        MAKE_VECTOR rax, r10
        test_malloc 8
        mov [rax], r10
        CLEAN_STACK
        ret
        "error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":
        ")
      )))	  
	  

(define runtime-cons 
  (lambda (ftable)
    (let ((address (lookup-fvar 'cons ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        MAKE_PAIR rdx, rbx
        test_malloc 8
        mov [rax], rdx   
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))


(define runtime-boolean? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'boolean? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        TYPE rax
        cmp rax, T_BOOL
        jne " check_false "
        mov rax, sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-remainder 
  (lambda (ftable)
    (let ((address (lookup-fvar 'remainder ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (error (errorL))
         (positive (posL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8]
        mov rbx, qword[rbp + 5*8]    
        mov rax,[rdx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " error "
        mov rax,[rbx]
        TYPE rax
        cmp rax, T_INTEGER
        jne " error "
        mov rax, [rdx]
        DATA rax
        mov r9, rax
		xor r8, r8 ;
        mov r8, rax
        cmp r8, 0
        jg .positive
        neg r8
        .positive:
        mov rax, r8
        xor rdx, rdx
        mov rbx, [rbx]
        DATA rbx
        idiv rbx
        cmp r9, 0
        jg " positive "
        neg rdx         
        "positive ": 
        MAKE_INT rdx
        test_malloc 8
        mov [rax], rdx            
        jmp " finish "             
        " error ":
        CLEAN_STACK
        jmp ERROR
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-string-ref
  (lambda (ftable)
    (let* ((address (lookup-fvar 'string-ref ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8]
        mov rbx, [rbx]
        mov rdx, [rdx]
        DATA rbx
        test_malloc 8
        xor rcx, rcx
        mov cl, byte[rax]
        STRING_REF cl,rdx,rbx
        MAKE_CHAR rcx
        test_malloc 8
        mov [rax], rcx
        CLEAN_STACK
        ret
        " lambda_end ":"
      ))))

(define runtime-vector-length
  (lambda (ftable)
    (let* ((address (lookup-fvar 'vector-length ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8]
        mov rax, [rdx] 
        mov rcx, [rdx]
        VECTOR_LENGTH rcx
        MAKE_INT rcx
        test_malloc 8
        mov [rax], rcx
        CLEAN_STACK
        ret
        " lambda_end ":"
      ))))

(define runtime-string-length
  (lambda (ftable)
    (let* ((address (lookup-fvar 'string-length ftable))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8]
        mov rax, [rdx] 
        xor rcx, rcx ;
        mov rcx, [rdx]
        STRING_LENGTH rcx
        MAKE_INT rcx
        test_malloc 8
        mov [rax], rcx
        CLEAN_STACK
        ret
        " lambda_end ":"
      ))))

(define runtime-positive?
  (lambda (ftable)
    (let* ((address (lookup-fvar 'positive? ftable))
      (negative (negL))
      (positive (posL))
      (interger (testL))
      (fraction (testL))
      (finish (finishL))
      (error (errorL))
      (lambda_start (lambdaL))
      (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rdx, qword[rbp + 4*8] 
        mov rax, [rdx] 
        TYPE rax
        cmp rax, T_INTEGER
        je "interger "
        mov rax, [rdx]
        TYPE rax
        cmp rax, T_FRACTION
        je "fraction "
        jmp "error "
        "interger ":
        mov rax, [rdx]
        DATA rax
        cmp rax,0
        jg "positive "
        mov rax, sob_False
        jmp "finish "
        "fraction ":
        mov rax, [rdx]
        CAR rax
        DATA rax
        cmp rax,0
        jg "positive "
        mov rax,sob_False
        jmp "finish "
        "positive ":
        mov rax, sob_True
        jmp "finish "
        "finish ":
        CLEAN_STACK
        ret
        "error ":
        CLEAN_STACK
        jmp ERROR
        " lambda_end ":"
      ))))
	  

(define runtime-symbol? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'symbol? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax,[rax]
        TYPE rax
        cmp rax, T_SYMBOL
        jne " check_false "
        mov rax, sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))


(define runtime-integer? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'integer? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax,[rax]
        TYPE rax
        cmp rax, T_INTEGER
        jne " check_false "
        mov rax, sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))


(define runtime-char? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'char? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax,[rax]
        TYPE rax
        cmp rax, T_CHAR
        jne " check_false "
        mov rax, sob_True
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-eq? 
  (lambda (ftable)
    (let ((address (lookup-fvar 'eq? ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL))
         (check_false (falseL))
         (finish (finishL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8] 
        mov rbx, qword[rbp + 5*8] 
        cmp rax, rbx
        jne " check_false "
        mov rax, sob_True 
        jmp " finish "
        " check_false ":
        mov rax,sob_False
        " finish ":
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))	  
	  
(define runtime-numerator
  (lambda (ftable)
    (let ((address (lookup-fvar 'numerator ftable))
          (check-integer (intL))
          (finish (finishL))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        mov rbx,rax
        TYPE rax
        cmp rax,T_FRACTION
        jne " check-integer "
        mov rax,rbx
        packed_car rax
        jmp "finish "
        " check-integer ":
        cmp rax, T_INTEGER
        jne ERROR_NOT_NUMBER
        mov rax, qword[rbp + 4*8] 
        "finish ":     
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-denominator
  (lambda (ftable)
    (let ((address (lookup-fvar 'denominator ftable))
          (check-integer (intL))
          (finish (finishL))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        mov rbx,rax
        TYPE rax
        cmp rax,T_FRACTION
        jne " check-integer "
        mov rax,rbx
        packed_cdr rax
        jmp "finish "
        " check-integer ":
        cmp rax, T_INTEGER
        jne ERROR_NOT_NUMBER
        mov rax, sob_One 
        "finish ":     
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))


(define runtime-car
  (lambda (ftable)
    (let ((address (lookup-fvar 'car ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address)
        "mov rax, qword[rbp + 4*8]
        mov rax, [rax]
        mov rbx,rax
        TYPE rax
        cmp rax,T_PAIR
        JNE ERROR_NOT_PAIR
        mov rax,rbx
        packed_car rax
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))

(define runtime-cdr
  (lambda (ftable)
    (let ((address (lookup-fvar 'cdr ftable))
         (lambda_start (lambdaL))
         (lambda_end (lambdaExitL)))
      (string-append (enter lambda_start lambda_end address) "
        mov rax, qword[rbp + 4*8]
        mov rax,[rax]
        mov rbx,rax
        TYPE rax
        cmp rax,T_PAIR
        JNE ERROR_NOT_PAIR
        mov rax,rbx
        packed_cdr rax
        CLEAN_STACK
        ret
        " lambda_end ":
        ")
      )))	  								