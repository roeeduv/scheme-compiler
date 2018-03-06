(load "project/pc.scm")

;helpers ;
(define <Whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))
	 
(define <LineComment>
  (let ((<EndOfLineComment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))	 
	 (*parser <any-char>)
	 (*parser <EndOfLineComment>)
	 *diff *star
	 (*parser <EndOfLineComment>)
	 (*caten 3)
	 done)))
	 
(define <SexprComment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr2>))
       (*caten 2)
       done))
	   
(define <Comment>
	(new (*parser <LineComment>)
		(*parser <SexprComment>)
		(*disj 2)
		done))
	
(define <Skip>
	(new (*parser <Comment>)
		(*parser <Whitespace>)
		(*disj 2)
		done))
		
(define <SkipInfix>; <Skip>)
  (new
   (*parser <Whitespace>)
   (*parser <LineComment>)
   (*parser (word "#;"))
   (*delayed (lambda () <SkipInfix>)) *star
   (*delayed (lambda () <l1>)) (*delayed (lambda () <sexpr2>)) (*disj 2)
   (*delayed (lambda () <SkipInfix>)) *star
   (*caten 4)
   (*pack (lambda (x) '()))
   (*disj 3)
   done))
   
(define ^^<Wrapped>
  (lambda (<Wrapper>)
    (lambda (<p>)
      (new (*parser <Wrapper>)
	   (*parser <p>)
	   (*parser <Wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))
	   
(define ^<Skipped*> (^^<Wrapped> (star <Skip>)))
	
(define LastElement (lambda (l) (car (reverse l))))

(define RemoveLastElement (lambda (l) (reverse (cdr (reverse l)))))

(define <0-9>	(range  #\0 #\9))
    
(define <1-9>	(range  #\1 #\9))

(define <a-z>	(range  #\a #\z))
    
(define <A-Z>	(range  #\A #\Z))

(define <VisibleSimpleChar>	(range  #\! #\~))

(define char->symbol (lambda (c)
    (cond ((equal? c #\+) '+)
          ((equal? c #\-) '-)
          ((equal? c #\/) '/)
          ((equal? c #\*) '*))))
		  
		  ;;;;;
		  
	;boolean
(define ^<createBool>
  (lambda (str ch)
    (new (*parser (word-ci str))
	 (*pack (lambda (_) ch))
	 done)))
	 
(define <Boolean>
    (new (*parser (^<createBool> "#t" #t))
         (*parser (^<createBool> "#f" #f))
		 (*disj 2)
		 done))
     

    
    ;;;;;;;;char  ;;;;;;;;
	
(define <CharPrefix>
  (new (*parser (word  "#\\"))
       done))
	   
(define ^<CreateChar>
    (lambda (str ch)
		(new (*parser (word-ci str))
			(*pack (lambda (_) ch))
			done)))
	 
(define <NamedChar>
	(new (*parser (^<CreateChar> "lambda" (integer->char 955)))
		(*parser (^<CreateChar> "newline" #\newline))
		(*parser (^<CreateChar> "nul" #\nul))
		(*parser (^<CreateChar> "page" #\page))
		(*parser (^<CreateChar> "return" #\return))
		(*parser (^<CreateChar> "space" #\space))
		(*parser (^<CreateChar> "tab" #\tab))
		(*disj 7)
		done))
    
(define <HexChar>
    (new (*parser (range #\0 #\9))
         (*parser (range #\a #\f))
         (*parser (range #\A #\F))
         (*disj 3)
         done))
         
(define <HexUnicodeChar>
        (new (*parser (char #\x))
             (*parser <HexChar>) *plus
             (*caten 2)
			 (*pack-with
				(lambda (x str)
					(integer->char (string->number(list->string str)16))))
					done))
   
(define <Char> 
    (new (*parser <CharPrefix>)
        (*parser <NamedChar>)
        (*parser <HexUnicodeChar>)
        (*parser <VisibleSimpleChar>)
        (*disj 3)
        (*caten 2)
        (*pack-with (lambda(prefix char)char))        
		done))
 
 ;;;;integers ;;;;
   
(define <Natural>
    (new (*parser (char #\0 )) *star
		(*parser <1-9>)
		(*parser <0-9>) *star
		(*caten 3)
		(*pack-with
		(lambda (zero head tail)
		  (string->number
		   (list->string
			`(,head ,@tail)))))  
		(*parser (char #\0))
			(*pack (lambda (_) 0))
			(*disj 2)
			done))
       
(define <Integer>
	(new (*parser (char #\+))
        (*parser <Natural>)
		(*caten 2)
        (*pack-with (lambda (++ n) n))
	 	(*parser (char #\-))
	  	(*parser <Natural>)
		(*caten 2)
		(*pack-with	(lambda (-- n) (- n)))
        (*parser <Natural>)
        (*disj 3)
       done))
       
(define <Fraction>
	(new (*parser <Integer>)
		(*parser (char #\/))
		(*parser <Natural>)
	    (*only-if (lambda (n) (not (zero? n))))
		(*caten 3)
		(*pack-with	(lambda (num div den)
			(/ num den)))
			done))
	   
	   ;;;;;;;;;;;;;;;;;;; string

	 
(define <StringMetaChar>
  (new (*parser (^<CreateChar> "\\\\" #\\))
		(*parser (^<CreateChar> "\\\"" #\"))
		(*parser (^<CreateChar> "\\t" #\tab))
		(*parser (^<CreateChar> "\\f" #\page))
		(*parser (^<CreateChar> "\\n" #\newline))
		(*parser (^<CreateChar> "\\r" #\return))
		(*disj 6)
		done))
				
(define <StringLiteralChar>
	(new (*parser <any-char>)
		(*parser (char #\\))
		(*parser (char #\"))
		(*disj 2)
		*diff
		done))
	
(define <StringHexChar>
	(new (*parser (word "\\x"))
		(*parser <HexChar>) *star
		(*parser (char #\;))
		(*caten 3)
		(*pack-with	(lambda (x str deli)
			(integer->char (string->number (list->string str)16))))
		done))
			
(define <StringChar>
	(new (*parser <StringLiteralChar>)
		(*parser <StringMetaChar>)
		(*parser <StringHexChar>)
		(*disj 3)
		done))
				
(define <String>
	(new (*parser (char #\"))
	(*parser <StringChar>)
	(*parser (char #\"))
	*diff 
	*star
	(*parser (char #\"))
	(*caten 3)
	(*pack-with	(lambda (geresh1 str geresh2)
					(list->string  str)))
					done))

			;;;;;;;;;;symbol

(define <SymbolChar>
	(new (*parser <0-9>)
		(*parser <a-z>)
		(*parser <A-Z>)
		(*pack (lambda (a) (integer->char (+ (char->integer a) 32))))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\>))
		(*parser (char #\<))
		(*parser (char #\?))
		(*parser (char #\/)) 
    (*disj 15)  
    done))
			
			
(define <Symbol> 
	(new (*parser <SymbolChar>) *plus
	(*pack (lambda (sym)
			(string->symbol (list->string sym))))
		done))
	   
(define <Number>
	(new (*parser <Fraction>)
	(*parser <Integer>)
	(*disj 2)
	(*parser <Symbol>) *not-followed-by
	done))	   
	   
(define <ProperList>
	(new (*parser (char #\())
		(*delayed (lambda () <sexpr2>)) *star
		(*parser (char #\)))
		(*caten 3)
		(*pack-with (lambda (soger1 expression soger2)	expression))
		done))
								
(define <ImproperList>
	(new (*parser (char #\())
	(*delayed (lambda () <sexpr2>)) *plus
	(*parser (char #\.))
	(*delayed (lambda () <sexpr2>))
	(*parser (char #\) ))
	(*caten 5)
	(*pack-with (lambda (soger1 expression1 dot expression2 soger2)
	`(,@expression1 . ,expression2))) 
	done))
					
(define <Vector>
	(new (*parser (char #\#))
	(*parser (char #\()) 
	(*delayed (lambda () <sexpr2>)) *star
	(*parser (char #\)))
	(*caten 4)
	(*pack-with (lambda (del1 del2 str del3)	(list->vector str))) 
	done))
							
(define <Quoted>
	(new (*parser (char #\'))
	(*delayed (lambda() <sexpr2>)) 
	(*caten 2)
	(*pack-with	(lambda (prefix str )	`' ,str ))
	done))
							
(define <QuasiQuoted>
	(new (*parser (char #\`))
	(*delayed (lambda() <sexpr2>))
	(*caten 2)
	(*pack-with
	(lambda (del1 str )	` (,'quasiquote ,str)))
	done))

(define <Unquoted>
	(new (*parser (char #\,))
	(*delayed (lambda() <sexpr2>))  
	(*caten 2)
	(*pack-with	(lambda (del1 str)	` (,'unquote ,str)))			  
	done))

(define <UnquotedAndSpliced>
	(new (*parser (char #\,))
	(*parser (char #\@))
	(*delayed (lambda() <sexpr2>))      
	(*caten 3)
	(*pack-with	(lambda (a c b )	`,@ ,b ))
	done))	 
			   
			   
(define <CBNameSyntax1>
	(new (*parser (char #\@))
	(*delayed (lambda() <sexpr2>))     
	(*caten 2)
	(*pack-with	(lambda (del1 str)	(list 'cbname str))) 
	done))

(define <CBNameSyntax2>
	(new (*parser (char #\{))
	(*delayed (lambda () <sexpr2>)) *star
	(*parser (char #\}))
	(*caten 3)
	(*pack-with (lambda (del1 str del2)	`,(cons 'cbname str ))) 
	done))	

(define <CBName>
	(new (*parser <CBNameSyntax1>)
		(*parser <CBNameSyntax2>)
		(*disj 2)
		done))
	 
			;;;;;;;;;;;infix

(define <InfixPrefixExtensionPrefix> 
  (new (*parser (word "##"))
       (*parser (word "#%"))
       (*disj 2)
       done))
	   
(define <PowerSymbol>
       (new (*parser (word "**"))
	   (*parser (char #\^))
       (*disj 2)
       done))
	   
 (define <InfixParen>
        (new (*parser (char #\())
		(*delayed (lambda () <InfixExpression>))
		(*parser (char #\)))
		(*caten 3)
		(*pack-with (lambda (soger1 expr soger2) expr))
        done)) 
        
(define <InfixSexprEscape>
        (new    (*delayed (lambda () <InfixPrefixExtensionPrefix>))
                (*delayed (lambda () <sexpr>))
                (*caten 2)
                (*pack-with (lambda (infixPrefix expr) expr))
        done))
        
(define <InfixNeg>
        (new (*parser (char #\-))
		(*delayed (lambda () <InfixHelper>))
		(*caten 2)
		(*pack-with (lambda (minus expr)	`(- ,expr)))
        done))
        
(define <L0>
        (new (*parser (^<Skipped*> <InfixSexprEscape>))
		(*parser (^<Skipped*> <InfixParen>))
		(*delayed (lambda() (^<Skipped*> <InfixNumber>)))
		(*parser (^<Skipped*> <InfixNeg>))
		(*delayed (lambda() (^<Skipped*> <InfixSymbol>)))
		(*disj 5)
		done))
	   
(define <L1> 
       (new (*delayed (lambda() <L2>))
	   (*parser (char #\+))
	   (*parser (char #\-))
	   (*disj 2)
	   (*delayed (lambda() <L2>))
	   (*caten 2) *star
	   (*caten 2)
	   (*pack-with (lambda (sign expr)
			(if (null? expr) sign
				(fold-left (lambda (x y) `(,(char->symbol (car y)) ,x ,(cadr y))) sign expr)))) 
				done))
        
(define <L2> 
       (new (*delayed (lambda() <L3>))
	   (*parser (char #\*))
	   (*parser (char #\/))
	   (*disj 2)
	   (*delayed (lambda() <L3>))
	   (*caten 2) *star
	   (*caten 2)
	   (*pack-with (lambda (sign expr)
			(if (null? expr) sign
			   (fold-left (lambda (x y) `(,(char->symbol (car y)) ,x ,(cadr y))) sign expr)))) 
				done))

(define <L3> 
    (new (*delayed (lambda() (^<Skipped*> <InfixHelper>)))
        (*parser <PowerSymbol>)
        (*delayed (lambda() (^<Skipped*> <InfixHelper>)))
        (*caten 2) *star
        (*caten 2)
        (*pack-with (lambda (sym expr)
				(if (null? expr) sym
				   (let* ((expr-values (map cadr expr))
					   (LastElement (LastElement expr-values))
					   (expr-reduced (RemoveLastElement expr-values))
					   (FinalExp (cons sym expr-reduced)))
						(fold-right (lambda (x y) `(,'expt ,x ,y)) LastElement FinalExp)))))
				done))
        
(define <InfixHelper>
       (new (*delayed (lambda() <L0>))
            (*parser (char #\[))
            (*delayed (lambda() <InfixExpression>))
            (*parser (char #\]))
            (*caten 3) 
            (*pack-with (lambda (a b c) (cons 'arr b)))
            (*parser (char #\())
            (*delayed (lambda() <InfixArgList>))
            (*parser (char #\)))
            (*caten 3) 
            (*pack-with (lambda (a b c) (cons 'func b)))
            (*disj 2)
             *star
             (*caten 2) 
            (*pack-with (lambda (a b)
                     (fold-left (lambda (x y) 
                        (let ((y-op (car y))
                              (y (cdr y)))
                        (if (eq? y-op 'arr) 
                           `(vector-ref ,x ,y)	`(,x ,@y))))	a b)))
			done))
        
(define <InfixArrayGet> 
       (new  (*delayed (lambda() (^<Skipped*> <InfixFuncall>)))
            (*parser (char #\[))
            (*delayed (lambda() <InfixExpression>))
            (*parser (char #\]))
            (*caten 3) 
            *star
            (*parser <epsilon>)
            (*disj 2)
            (*caten 2) 
            (*pack-with (lambda (s1 s2)
				(lambda (a b)
					(if (null? b) a
					   (fold-left (lambda (x y) `(,'vector-ref ,x ,(cadr y))) a b)))))    
        done))
        
 (define <InfixFuncall>
       (new (*delayed (lambda() <L0>))
            (*parser (char #\())
            (*delayed (lambda() <InfixArgList>))
            (*parser (char #\)))
            (*caten 3)
            *star
            (*parser <epsilon>)
            (*disj 2)
            (*caten 2)
            (*pack-with (lambda (a b)
							(if (null? b) a
							(fold-left (lambda (x y) `(,x ,@(cadr y))) a b))))      
			done))
        
(define <InfixArgList>
        (new (*delayed (lambda() <InfixExpression>))
            (*parser (char #\,))
            (*delayed (lambda() <InfixExpression>))
            (*caten 2) *star
            (*pack (lambda(a)	(map cadr a)))
            (*caten 2)
            (*pack-with (lambda(a b)	(cons a b)))
            (*parser (^<Skipped*> <epsilon>))
            (*disj 2)
			done))
   
(define <InfixExpression> <L1>)
        
(define <InfixSymbol>
        (new (*parser <SymbolChar>) 		
            (*parser (char #\+))
            (*parser (char #\-))
            (*parser (char #\*))
            (*parser (char #\^))
            (*parser (char #\/))
            (*parser (word "**"))
            (*disj 6) *diff *plus
            (*pack (lambda (a)	(string->symbol (list->string a))))
			done))
        
(define <InfixNumber>
	(new (*parser <Fraction>) 
	    (*parser <Integer>)
	    (*disj 2) 
	    (*parser <InfixSymbol>)
	    (*parser <Natural>)
	    *diff *not-followed-by
	    done))
        
(define <InfixExtension>
        (new (*parser <InfixPrefixExtensionPrefix>)
            (*parser <InfixExpression>)
            (*caten 2)
            (*pack-with (lambda (a b)	b))
			done))
				 
(define <sexpr2>
	(^<Skipped*>
	(disj <Boolean>  <Char>	<Number>	<String>	<Symbol>	<ProperList>
				  <ImproperList>	<Vector>	<Quoted>	<QuasiQuoted>
				  <UnquotedAndSpliced>	<Unquoted>	<CBName>	<InfixExtension>
				 )))
				 
(define <sexpr> <sexpr2>)	


;;assignment2

(load "project/qq.scm")


(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))
		
		
;helpers
		
(define reserved-word?
  (lambda (expr)
    (if
      (not (list? expr)) (member expr *reserved-words*)
      (member (car expr) *reserved-words*))))

(define Flatten
  (lambda (expr)
    (cond ((null? expr) '())
          ((pair? expr) (append (Flatten (car expr)) (Flatten (cdr expr))))
          (else (list expr)))))
		  	  
		  
(define getVals
  (lambda (lst)
    (map (lambda(x) (cadr x)) lst)))
    
(define getVars
  (lambda (lst)
    (map (lambda(x) (car x)) lst)))	 
	
(define removeSeq
	(lambda (lst)
		(letrec ((helper (lambda (x l)
					 (cond ((= (length x) 0) l)
							((and (list? (car x)) (eq? (caar x) 'seq)) (helper (append (cadar x) (cdr x)) l))
							(else (helper (cdr x) (append l (list (car x)))))))))
				(helper lst '()))))	

(define delete-dup
  (lambda (lst)
    (let ((lst2 (getVars lst)))
      (letrec ((helper (lambda (lst3)
                         (cond ((null? lst3) #f)
                               ((member (car lst3) (cdr lst3)) #t)
                               (else (helper (cdr lst3)))))))
        (helper lst2)))))
		
(define checkConds
  (lambda (x)
    (let ((removeLast (reverse (cdr (reverse x)))))
      (not (member 'else removeLast))))) 		
		
(define fix
  (lambda (lst)
    (map (lambda (x) (list (car x) '#f)) lst))) 

(define endLet*
  (lambda (expr)
    (if (eq? (cdadr expr) '())
        `(begin ,@(cddr expr))
        `(let*  ,(cdadr expr) ,@(cddr expr))))) 	

(define let*empty?
  (lambda (x)
    (and (list? x) (eq? (car x) 'let*) (= (length (cadr x)) 0))))
	
(define or-helper
  (lambda (expr)
    (if (eq? expr '())
    (parse '#f)
    `(or ,(map (lambda (x) (parse x)) expr)))))
	
(define or1?
  (lambda (expr)
    (and (list? expr)
      (eq? (car expr) 'or)
      (= (length expr) 1)
   )))

(define or2?
  (lambda (expr)
    (and (list? expr)
      (eq? (car expr) 'or)
      (= (length expr) 2)
   )))


(define or?
  (lambda (expr)
    (and (list? expr) (eq? (car expr) 'or) (> (length expr) 2))))	  
	
(define cond-if
  (lambda (expr)
    (cond ((= (length expr) 1) (if (>= (length (car expr)) 2)
                                (if (eq? (caar expr) 'else)
                                    `(begin ,@(cdar expr))
                                    `(if ,(caar expr) (begin ,@(cdar expr))))
                            (caar expr)))
          ((= (length (car expr)) 1)
              `(if ,(caar expr) ,(caar expr) (cond ,@(cdr expr))))
          (else `(if ,(caar expr) (begin ,@(cdar expr)) (cond ,@(cdr expr)))))))	
	
(define nested-if
  (lambda (expr)
    (cond ((= (length expr) 0) '#t)
          ((= (length expr) 1) (car expr))
          (else `(if ,(car expr) (and ,@(cdr expr)) '#f)))))
	
;;			

	
(define Const? 
  (lambda (expr)
    (or
        (null? expr)
        (vector? expr)
        (boolean? expr)
        (number? expr)
        (char? expr)
        (string? expr)
        (eq? (void) expr))))  		
		
(define seq
	(lambda (expr)(if (> (length (cdr expr)) 1)
            `(seq ,(removeSeq (map (lambda (x) (parse x)) (cdr expr))))
             (if (= (length (cdr expr)) 1)  
             (parse (cadr expr))
            `(const ,@(list (if #f #f)) )))))


(define var? 
  (lambda (expr)
   (and
    (symbol? expr)
    (not (reserved-word? expr )))))

(define application?
  (lambda (expr)
    (and (list? expr)
      (not (eq? 'seq (car expr)))
      (not (reserved-word? expr )))))
	

(define seq?
  (lambda (expr)
    (and (list? expr) (eq? (car expr) 'begin))))    


(define set?
  (lambda (expr)
    (and (list? expr)  (eq? (car expr) 'set!) (var? (cadr expr)))))	

(define define?
  (lambda (expr)
    (and (list? expr)  (eq? (car expr) 'define) (var? (cadr expr)))))
	
(define define-MIT?
  (lambda (expr)
    (and (list? expr)  (eq? (car expr) 'define) (pair? (cadr expr)) (var? (caadr expr)))))  
	
(define if?
  (lambda (expr)
    (and (list? expr) (eq? (car expr) 'if) (= (length expr) 4))))

(define if2?
  (lambda (expr)
     (and (list? expr) (eq? (car expr) 'if) (= (length expr) 3))))
	 
(define let?
  (lambda (x)
    (and (list? x) (eq? (car x) 'let) (not (delete-dup (cadr x))))))

(define let*?
  (lambda (x)
     (and (list? x) (eq? (car x) 'let*))))
	 	
(define letrec?
  (lambda (x)
    (and (list? x) (eq? (car x) 'letrec))))
	
(define lambda?
  (lambda (expr)
    (and (list? expr)  (eq? (car expr) 'lambda) (list? (cadr expr)))))
	

(define lambda-opt?
  (lambda (expr)
    (and (list? expr)  (eq? (car expr) 'lambda) (pair? (cadr expr)))))

(define lambda-args?
  (lambda (expr)
    (and (list? expr)  (eq? (car expr) 'lambda) (var? (cadr expr)))))
 
				
(define cond?
  (lambda (x)
    (and (list? x) (eq? (car x) 'cond) (checkConds (getVars (cdr x))))))
              
(define and?
  (lambda (x)
     (and (list? x) (eq? (car x) 'and))))


(define parse
  (lambda (sexp)
    (cond
      ((or (vector? sexp) (const? sexp)) `(const ,(unquotify sexp)))
      ((if? sexp) `(if3 ,(parse (cadr sexp)) ,(parse (caddr sexp)) ,(parse (cadddr sexp))))
      ((if2? sexp) `(if3 ,(parse (cadr sexp)) ,(parse (caddr sexp)) (const ,@(list (if #f #f)) ) ))     
	  ((or1? sexp) (parse #f))
      ((or2? sexp)  (parse (cadr sexp)))
      ((or? sexp) `(or (,@(map parse (cdr sexp)))))	  
	  ((lambda? sexp) `(lambda-simple ,(cadr sexp) ,(parse `(begin ,@(cddr sexp)))))
      ((lambda-opt? sexp) `(lambda-opt ,(reverse (cdr(reverse (Flatten (cadr sexp))))) ,(car(reverse (Flatten (cadr sexp))))  ,(parse `(begin ,@(cddr sexp)))))
      ((lambda-args? sexp) (parse `(lambda (,'() . ,(cadr sexp)) ,@(cddr sexp))))
      ((define-MIT? sexp) `(define ,(parse (caadr sexp)) ,(parse `(lambda ,(cdadr sexp) ,@(cddr sexp)))))
      ((define? sexp) `(define ,(parse (cadr sexp)) ,(parse (caddr sexp))))
      ((set? sexp) `(set ,(parse (cadr sexp)) ,(parse (caddr sexp))))
      ((application? sexp) `(applic ,(parse (car sexp)) ,(map (lambda (x) (parse x)) (cdr sexp))))
      ((var? sexp) `(var ,sexp))
      ((seq? sexp) (seq sexp))
      ((let? sexp) (parse `((lambda ,(getVars (cadr sexp)) ,@(cddr sexp)) ,@(getVals (cadr sexp)))))
      ((let*empty? sexp) (parse `(let ()  (begin ,@(cddr sexp)))))
      ((let*? sexp) (parse `(let (,(caadr sexp)) ,(endLet* sexp))))
      ((letrec? sexp) (parse `(let ,(fix (cadr sexp)) ,@(map (lambda (x) `(set! ,(car x) ,(cadr x))) (cadr sexp)) ((lambda (),@(cddr sexp))))))
      ((and? sexp) (parse `,(nested-if (cdr sexp))))
      ((cond? sexp) (parse `,(cond-if (cdr sexp))))
      ((eq? 'quasiquote (car sexp)) (parse (expand-qq (cadr sexp))))
      )))		



;;;assignment 3

			


(define check-null-or-not-list
	(lambda (expr)
		(or	(null? expr) (not (list? expr)))))

  
(define check-simple-var-lambda
	(lambda (expr)
		(or (equal? (car expr) 'lambda-simple) (equal? (car expr) 'lambda-var))))

(define check-opt-lambda
	(lambda (expr)
		(equal? (car expr) 'lambda-opt)))
		
(define lambda-exp?
    (lambda(exp)
        (if (or (null? exp) (not (list? exp)))
            #f
            (or (equal? (car exp) 'lambda-simple)
                (equal? (car exp) 'lambda-opt)
                (equal? (car exp) 'lambda-var)))))		

(define check-seq-or
	(lambda (expr)
		(or (equal? (car expr) 'seq) (equal? (car expr) 'or))))	

(define check-def-set-box
	(lambda (expr)
		(or (equal? (car expr) 'def) (equal? (car expr) 'box-set) (equal? (car expr) 'set))))

(define member-not-equal
		(lambda (expr origin param)
			(and (not (equal? expr origin)) (member param (get-params expr)))))		
 
(define create-vars-list
    (lambda (var-list)
        (if (not (list? var-list))
            (list (list 'var var-list))
            (map (lambda(var) (list 'var var)) (car var-list)))))
                    
(define annotate
    (lambda (expr)
		(cond 	((check-null-or-not-list expr) expr)
                ((check-seq-or expr)(append (list (car expr))
										(list (reverse (append (list (annotate (car (reverse (cadr expr)))))
                                           (reverse (map annotate-tc (reverse (cdr (reverse (cadr expr)))))))))))
				((equal? (car expr) 'if3)	(append (list 'if3) (list (annotate-tc (cadr expr))) (annotate (cddr expr))))
                ((equal? (car expr) 'applic) (append (list 'tc-applic) (annotate-tc (cdr expr))))
				((check-def-set-box expr) (map annotate-tc expr))
                (else	(map annotate expr)))))



(define lex-helper
    (lambda (expr params)
        (cond   ((check-null-or-not-list expr) expr)
                ((equal? (car expr) 'var)	(tag expr params))
                ((equal? (car expr) 'lambda-simple)
					(list (car expr) (cadr expr)
					(lex-helper (caddr expr) (append (list (create-vars-list  (cdr expr))) params))))
                ((equal? (car expr) 'lambda-var)
                    (list (car expr) (cadr expr)
					(lex-helper (caddr expr) (append (list (create-vars-list  (cadr expr))) params))))
                ((equal? (car expr) 'lambda-opt)
                    (list	(car expr)	(cadr expr)	(caddr expr)
					(lex-helper	(car (cdddr expr))	(append (list (create-vars-list 
					(list (append (cadr expr) (list (caddr expr)))))) params))))
                (else (map (lambda(x) (lex-helper x params)) expr)))))
                                    
(define find-var
    (lambda (var param-list)
        (let ((checker
			(if (null? param-list)	'()	(find-var var (cdr param-list)))))
             (cond	((null? param-list)	#f)
					((equal? var (car param-list)) 0)
                    ((equal? #f checker) #f)
                     (else   (+ 1 checker))))))
                    
(define find-var2
    (lambda (var param-list lvl)
        (let ((checker
			(if (null? param-list)	'()	(find-var var (car param-list)))))
			 (cond	((null? param-list)	#f)
                ((not (equal? checker #f)) (list lvl checker))
                   (else (find-var2 var (cdr param-list ) (+ 1 lvl)))))))

(define tag
    (lambda(var param-list)
        (let ((pvars
                (if (null? param-list)	'()	(find-var var (car param-list))))
              (bvars
				(if (null? param-list)	'()	(find-var2 var (cdr param-list) 0))))         
            (cond   ((null? param-list)	(list 'fvar (cadr var)))
					((not (equal? pvars #f))	(list 'pvar (cadr var) pvars)) 
                    ((not (equal? bvars #f))	(append (list 'bvar (cadr var))   bvars))
                       (else (list 'fvar (cadr var)))))))


			
			
;;;;		

(define box-set?
    (lambda (var bound-params-list boxed-param-list)
        (and
            (not (member var bound-params-list))
			(member var boxed-param-list))))





                
(define is-bound-somewhere?
    (lambda (param expr origin)
        (ormap 
            (lambda (inner-lambda)
                (appers-as-bound-in-lambda? param inner-lambda))
            (get-inner-lambdas origin param expr))))

(define get-inner-lambdas
    (lambda (origin param body ) 
        (if (check-null-or-not-list body) '()
            (if (member-not-equal body origin param)
                '()
                (if (lambda-exp? body)
                    (append (list body) (map (lambda (x) (get-inner-lambdas origin param x)) body))
                    (map (lambda (x) (get-inner-lambdas origin param x)) body))))))
    
(define appers-as-bound-in-lambda?
    (lambda (param inner-lambda)
        (if (member param (get-params inner-lambda))
            #f
            (ormap (lambda(x) 
                        (if (check-null-or-not-list x)
						#f
						(if (equal? x (list 'var param))
						#t
						(appers-as-bound-in-lambda? param x))))
                    inner-lambda))))
					


(define is-set-somewhere?
    (lambda (param expr origin)
        (if (check-null-or-not-list expr)	#f
            (if (member-not-equal expr origin param) #f
                (if (and (equal? (car expr) 'set) (equal? (cadr expr) (list 'var param)))
                    #t
                    (ormap (lambda(x) (is-set-somewhere? param x  origin)) expr))))))

(define is-get-somewhere?
    (lambda (param expr origin)
        (if (check-null-or-not-list expr)	#f
            (if (member-not-equal expr origin param) #f
                (if (equal? expr (list 'var param))	#t
                    (if (equal? (car expr) 'set)
                        (is-get-somewhere? param (caddr expr)  origin)
                        (ormap (lambda(x) (is-get-somewhere? param x  origin)) expr)))))))

(define make-box-set-lambda
    (lambda(expr box all-boxed-param-list)
        (let* ((opt-last-param
                    (if (check-opt-lambda expr)	(list (caddr expr))	'()))
              (body 
                    (if (check-opt-lambda expr)	(cdddr expr)	(cddr expr)))
              (last-exp
                    (if (check-opt-lambda expr)
                        (box-helper body all-boxed-param-list '() )
                        (list (box-helper (caddr expr) all-boxed-param-list '() )))))
            (append (list (car expr))	
				(list	(car (cdr expr))) opt-last-param  
                    (list (list 'seq	(append
                                (create-set-list box) 
                                (if (equal? (caar body) 'seq) ; rest-body
                                    (box-helper (cadar body) all-boxed-param-list '() ) 
                                    last-exp))))))))

    
(define add-box-get
    (lambda (expr boxed-param-list bound-and-param-list  ) 
        (let*((var (car (cdr expr))))
            (if (box-set? var  bound-and-param-list boxed-param-list)
                (list 'box-get expr)
                expr))))
                 
(define add-box-set
    (lambda (expr boxed-params bound-params-list)
        (let*	((var (car (cdadr expr)))
                (prefix 
                    (if (box-set? var  bound-params-list boxed-params)
                        (list 'box-set)
                        (list (car expr)))))
             (append
                prefix
                (list (car (cdr expr)))
                (box-helper (cdr (cdr expr)) boxed-params bound-params-list   )))))



(define get-params
    (lambda(expr)
        (if (check-null-or-not-list expr) '()
            (cond   ((equal? (car expr) 'lambda-simple)
                        (cadr expr))
                    ((equal? (car expr) 'lambda-opt)
                        (append (cadr expr) (list (caddr expr))))
                    ((equal? (car expr) 'lambda-var)
                        (list (cadr expr)))
                    (else '())))))		

					

			
(define create-set-list
    (lambda (box)
        (if (null? box)
            '()
            (append (list (list 'set
                                (list 'var (car box))
                                (list 'box (list 'var (car box)))))
                    (create-set-list (cdr box))))))			
		
(define box
    (lambda (expr)
        (filter
            (lambda (param)
                (and
                    (is-bound-somewhere? param expr expr)
                    (is-set-somewhere? param expr expr)
                    (is-get-somewhere? param expr expr)))
            (get-params expr))))		

(define box-helper
    (lambda (expr boxed-param-list bound-param-list-without-boxed)
        (let*   ((box
                        (if (lambda-exp? expr)	(box expr) '()))
                 (bound-and-param-list
                        (if (lambda-exp? expr) (append bound-param-list-without-boxed (get-params expr))
												bound-param-list-without-boxed)))
                (cond  
                    ((check-null-or-not-list expr) expr)
                    ((equal? (car expr) 'set)	(add-box-set expr boxed-param-list bound-and-param-list))
                    ((equal? (car expr) 'var)	(add-box-get expr boxed-param-list bound-and-param-list))
                    ((and (lambda-exp? expr) (not (null? box)))
							(make-box-set-lambda expr box (append boxed-param-list box)))
                    (else (map (lambda(x) (box-helper x boxed-param-list bound-and-param-list)) expr))))))

			
		
		
(define remove-applic-lambda-nil
    (lambda (expr)
		(cond 	((check-null-or-not-list expr) expr)
				((and 
				(equal? (car expr) 'applic)
				(equal? (caadr expr) 'lambda-simple)
				(null? (car (cdadr expr)))
				(null? (caddr expr)))	(remove-applic-lambda-nil (cadr (cdadr expr))))
                (else	(map remove-applic-lambda-nil expr)))))		
				
(define box-set
    (lambda (expr)
        (box-helper expr '() '() )))		

(define annotate-tc
	(lambda (expr)
		(cond 	((check-null-or-not-list expr) expr)
                ((check-simple-var-lambda expr) (append (list (car expr)) (list(cadr expr))  (annotate (cddr expr))))
                ((check-opt-lambda expr)(append  (list (car expr)) (list (cadr expr)) (list (caddr expr)) (annotate (cdddr expr))))
                (else
                    (map annotate-tc expr)))))		
					
(define pe->lex-pe
    (lambda(expr)
        (lex-helper expr '())))					



