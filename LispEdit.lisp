;; Common Lisp Structure Editor

;; The editor code was taken from the structure editor in LISPF4 and is:
;;     Copyright (c) 1984 Dr. Mats Nordstrom, Hans Eriksson, Kristina Johansson, Dr. Tore Risch,
;;                        Mats Carlsson, and Jaan Koort
;;     All rights reserved.

;; It was subsequently enhanced and converted to Common Lisp by:
;;     Copyright (c) 2015 Blake McBride (blake@mcbridemail.com)

;; It is released under the license that accompanies this file.


(CL:DEFPACKAGE "LISPEDIT"
  (:USE "COMMON-LISP")
  (:SHADOW "DEFMACRO"
	   "DEFUN"
	   "LOAD")
  (:EXPORT "DEFUN"
	   "DEFMACRO"
	   "EDITF"
	   "EDITV"
	   "EDITP"
	   "EDITS"
	   "LOAD"
	   "MAKEFILE"
	   "SAVE-IMAGE"))
(IN-PACKAGE "LISPEDIT")

(DEFPARAMETER *PACKAGE-SPECIAL* '(
				  (SHADOW 'DEFMACRO)
				  (SHADOW 'DEFUN)
				  (SHADOW 'LOAD)
				  (SETF (GET 'DEFMACRO 'FNCELL)
				   '(MACRO (&REST ARGS)
				     `(PROGN (COMMON-LISP:DEFMACRO ,(CAR ARGS) ,(CADR ARGS) ,@(CDDR ARGS))
					     (SETF (GET ',(CAR ARGS) 'FNCELL) '(MACRO ,@(CDR ARGS)))
					     ',(CAR ARGS))))
				  (LET ((FUN (GET 'DEFMACRO 'FNCELL)))
				    (EVAL `(COMMON-LISP:DEFMACRO DEFMACRO ,@(CDR FUN))))
				  ))

(SHADOW 'DEFMACRO)
(SETF (GET 'DEFMACRO 'FNCELL)
      '(MACRO (&REST ARGS)
	`(PROGN (COMMON-LISP:DEFMACRO ,(CAR ARGS) ,(CADR ARGS) ,@(CDDR ARGS))
		(SETF (GET ',(CAR ARGS) 'FNCELL) '(MACRO ,@(CDR ARGS)))
		',(CAR ARGS))))
(LET ((FUN (GET 'DEFMACRO 'FNCELL)))
  (EVAL `(COMMON-LISP:DEFMACRO DEFMACRO ,@(CDR FUN))))

;; (COMMON-LISP:DEFMACRO DEFMACRO (&REST ARGS)
;;   `(PROGN
;;      (COMMON-LISP:DEFMACRO ,(CAR ARGS)
;; 	 ,(CADR ARGS)
;;        ,@(CDDR ARGS))
;;      (SETF (GET ',(CAR ARGS) 'FNCELL)
;; 	   '(MACRO ,@(CDR ARGS)))
;;      ',(CAR ARGS)))

(SHADOW 'DEFUN)
(DEFMACRO DEFUN (&REST ARGS)
  `(PROGN
     (COMMON-LISP:DEFUN ,(CAR ARGS)
	 ,(CADR ARGS)
       ,@(CDDR ARGS))
     (SETF (GET ',(CAR ARGS) 'FNCELL)
	   '(LAMBDA ,@(CDR ARGS)))
     ',(CAR ARGS)))

(SHADOW 'LOAD)
(DEFUN LOAD (FNAME)
  (WITH-OPEN-FILE (S FNAME :DIRECTION :INPUT :IF-DOES-NOT-EXIST :ERROR)
    (PROG (EXP)
     LOOP
     (SETQ EXP (READ S NIL :EOF))
     (AND (EQ EXP :EOF)
	  (RETURN NIL))
     (EVAL EXP)
     (GO LOOP))))

(DEFUN IS-FUNCTION (SYMBOL)
   (AND (FBOUNDP SYMBOL)
            (NOT (MACRO-FUNCTION SYMBOL))
            (NOT (SPECIAL-OPERATOR-P SYMBOL))))

(DEFUN IS-MACRO (SYMBOL)
   (AND (FBOUNDP SYMBOL)
            (MACRO-FUNCTION SYMBOL)
            (NOT (SPECIAL-OPERATOR-P SYMBOL))))

(DEFUN MAKEFILE (FNAME)
  (WITH-OPEN-FILE (S FNAME :DIRECTION :OUTPUT :IF-EXISTS :OVERWRITE :IF-DOES-NOT-EXIST :CREATE)
    (MAPC (LAMBDA (X) (PRINT X S)(TERPRI S)) *PACKAGE-SPECIAL*)
    (DO-ALL-SYMBOLS (SYM)
      (WHEN (AND (EQ (FIND-PACKAGE 'COMMON-LISP-USER)
		     (SYMBOL-PACKAGE SYM))
		 (BOUNDP SYM)
		 (NOT (EQ SYM '*PACKAGE-SPECIAL*)))
	(PPRINT (CONS 'DEFPARAMETER (LIST SYM `',(SYMBOL-VALUE SYM))) S)
	(TERPRI S)))
    (DO-ALL-SYMBOLS (SYM)
      (WHEN (AND (EQ (FIND-PACKAGE 'COMMON-LISP-USER)
		     (SYMBOL-PACKAGE SYM))
		 (IS-MACRO SYM))
	(PPRINT (CONS 'DEFMACRO (CONS SYM (CDR (GET SYM 'FNCELL)))) S)
	(TERPRI S)))
    (DO-ALL-SYMBOLS (SYM)    
      (WHEN (AND (EQ (FIND-PACKAGE 'COMMON-LISP-USER)
		     (SYMBOL-PACKAGE SYM))
		 (IS-FUNCTION SYM))
	(PPRINT (CONS 'DEFUN (CONS SYM (CDR (GET SYM 'FNCELL)))) S)
	(TERPRI S)))
    (AND (BOUNDP '*PACKAGE-SPECIAL*)
	 *PACKAGE-SPECIAL*
	 (PPRINT `(DEFPARAMETER *PACKAGE-SPECIAL* ',*PACKAGE-SPECIAL*) S)
	 (TERPRI S)))
  FNAME)
			 
(DEFMACRO INTH (L N)
  `(NTHCDR (1- ,N) ,L))

(DEFMACRO NLISTP (X)
  `(NOT (CONSP ,X)))

(DEFMACRO COM-READ (CMD)
  `(PROG (RET)
     (COND ((NULL ,CMD)
	    (PRINC "EDIT> " *QUERY-IO*)
	    (FINISH-OUTPUT *QUERY-IO*)
	    (RETURN (READ *QUERY-IO*)))
	   (T (SETQ RET (CAR ,CMD))
	      (SETQ ,CMD (CDR ,CMD))
	      (RETURN RET)))))

(DEFMACRO EDMSG (MSG)
  `(PROGN
     (PRINC ,MSG *QUERY-IO*)
     (TERPRI *QUERY-IO*)
     (SETQ EDCOM NIL)
     (GO NEXT)))
  
(DEFMACRO EDITF (FN &REST L)
  `(PROG ((ESF (GET ',FN 'EDIT-SAVE))
	  (VF (GET ',FN 'FNCELL))
	  RESULT EXIT-TYPE)
      (MULTIPLE-VALUE-SETQ (RESULT EXIT-TYPE) (EDITS (OR ESF VF) ,@L))
      (CASE EXIT-TYPE
	(OK (REMPROP ',FN 'EDIT-SAVE)
	    (SETF (GET ',FN 'FNCELL) RESULT)
	    (CASE (CAR RESULT)
	      (LAMBDA
;		  (SETF (SYMBOL-FUNCTION ',FN) (COERCE RESULT 'FUNCTION)))
		  (SETF (SYMBOL-FUNCTION ',FN) (COMPILE NIL RESULT)))
	      (MACRO
	       (EVAL (CONS 'DEFMACRO (CONS ',FN (CDR RESULT)))))))
	(SAVE (SETF (GET ',FN 'EDIT-SAVE) RESULT)))
      (RETURN ',FN)))
  
(DEFMACRO EDITP (A &REST L)
  `(PROG (EXIT-TYPE VAL)
      (MULTIPLE-VALUE-SETQ (VAL EXIT-TYPE) (EDITS (SYMBOL-PLIST ',A) ,@L))
      (AND (EQ EXIT-TYPE 'OK)
	   (SETF (SYMBOL-PLIST ',A) VAL))
      (RETURN ',A)))

(DEFMACRO EDITV (A &REST L)
  `(PROG (EXIT-TYPE VAL)
      (MULTIPLE-VALUE-SETQ (VAL EXIT-TYPE) (EDITS ,A ,@L))
      (AND (EQ EXIT-TYPE 'OK)
	   (SETQ ,A VAL))
      (RETURN ',A)))

(DEFUN EDFIND1ST2 (A S TRC)
  (PROG (RES)
   LOOP (COND ((NLISTP S)
	       (RETURN))
	      ((EQUAL A (CAR S))
	       (RETURN (CONS (CAR S) TRC)))
	      ((SETQ RES
		     (EDFIND1ST2 A
				 (CAR S)
				 (CONS (CAR S) TRC)))
	       (RETURN RES))
	      (T (SETQ S (CDR S))
		 (GO LOOP)))))

(DEFUN EDFIND1ST (A S TRC)
  (PROG (TEMP)
     (COND ((SETQ TEMP (MEMBER A (CDR S)))
	    (RETURN (CONS TEMP TRC)))
	   ((SETQ TEMP
		  (EDFIND1ST2 A
			      (COND ((EQUAL (CAR S) A)
				     (CDR S))
				    (T S))
			      TRC))
	    (RETURN TEMP)))
     LOOP
     (SETQ S (CAR TRC))
     (SETQ TRC (CDR TRC))
     (COND ((NULL TRC)
	    (RETURN))
	   ((SETQ TEMP
		  (EDFIND1ST2 A
			      (CDR (MEMBER S (CAR TRC)))
			      TRC))
	    (RETURN TEMP))
	   (T (GO LOOP)))))
  
(DEFUN EDITPR (X PP DEPTH CTLS)
  (LET ((*PRINT-LEVEL* DEPTH)
	(*PRINT-PRETTY* PP))
    (COND ((TAILP X (CADR CTLS))
	   (PRINC "--- " *QUERY-IO*)
	   (MAPC #'(LAMBDA (Y)
		     (COND (PP (PRIN1 Y *QUERY-IO*)
			       (PRINC "    " *QUERY-IO*))
			   (T (PRINC Y *QUERY-IO*)
			      (PRINC " " *QUERY-IO*))))
		 X)
	   (PRINC ")" *QUERY-IO*)
	   (TERPRI *QUERY-IO*))
	  (T (COND (PP (PRIN1 X *QUERY-IO*)
		       (TERPRI *QUERY-IO*))
		   (T (PRIN1 X *QUERY-IO*)
		      (TERPRI *QUERY-IO*)))))))

(DEFUN EDSMASH (X A B)
  (RPLACA X A)
  (RPLACD X B))
  
(DEFUN EDITS (S-ORIG &REST EDCOM)
  (PROG (CL CTLS TEMP X A B L (S (COPY-TREE S-ORIG)))
     (AND (NLISTP S)
	  (PRINT "NOT EDITABLE")
	  (RETURN (VALUES NIL 'CANT-EDIT)))
     (AND EDCOM (SETQ EDCOM (APPEND EDCOM '(OK))))
     START
     (SETQ CL S)
     (SETQ CTLS (LIST CL))
     NEXT
     (SETQ L (COM-READ EDCOM))
     (COND ((ATOM L)
	    (GO ATOML)))
     (SETQ X (CAR L))
     (SETQ L (CDR L))
     (COND ((INTEGERP X)
	    (GO NUMCARX))
	   ((GET X 'DEEDITL)
	    (SETQ EDCOM
		  (APPEND
		   (APPLY (GET X 'DEEDITL)
			  (LIST L))
		   EDCOM))
	    (GO NEXT))
	   (T (CASE X
		(R (NSUBST (CADR L)
			   (CAR L) CL))
		(N (NCONC CL L))
		(US (COND ((SETQ TEMP
				 (COPY-TREE (GET (CAR L) 'EDITVALUE)))
			   (SETQ EDCOM
				 (APPEND (SUBST TEMP (CAR L) (CDR L))
					 EDCOM)))
			  (T (GO ILLG))))
		(MARK (SETF (GET (CAR L) 'EDITCHAIN) CTLS))
		(/ (COND ((AND (SYMBOLP (CAR L))
			       (SETQ TEMP
				     (GET (CAR L) 'EDITCHAIN)))
			  (SETQ CL (CAR TEMP))
			  (SETQ CTLS TEMP))
			 (T (GO ILLG))))
		(= (SETQ EDCOM
			 (CONS 'UP
			       (CONS (CONS 1 L) EDCOM))))
		(MBD (SETQ EDCOM
			   (CONS (CONS '= (SUBST CL '* L))
				 (CONS 1 EDCOM))))
		(XTR (SETQ EDCOM
			   (APPEND
			    (CONS
			     '(MARK LISPF4-XTR)
			     (APPEND L
				     '(S LISPF4-XTR
				       (/ LISPF4-XTR)
				       (US LISPF4-XTR
					(= LISPF4-XTR)))))
			    EDCOM)))
		(B (SETQ EDCOM
			 (APPEND
			  (LIST 'UP (CONS -1 L))
			  EDCOM)))
		(A (SETQ EDCOM
			 (APPEND
			  (LIST 'UP (CONS -2 L))
			  EDCOM)))
		(ESET (COND (L (COND
				 ((SYMBOLP (CAR L))
				  (COND
				    ((CADR L)
				     (SETF (GET (CAR L) 'DEEDITA) (LIST 'QUOTE (CDR L))))
				    (T (REMPROP (CAR L) 'DEEDITA))))))))
		(OTHERWISE (GO CONT)))))
     (GO NEXT)
     CONT (COND ((OR (NULL (INTEGERP (CAR L)))
		     (< (CAR L) 1))
		 (GO ILLG)))
     (OR (SETQ TEMP (INTH CL (CAR L)))
	 (GO EMPTY))
     (CASE X
       (LO (EDSMASH TEMP (CAAR TEMP)
		    (CDAR TEMP)))
       (LI (EDSMASH TEMP
		    (CONS (CAR TEMP)
			  (CDR TEMP))
		    NIL))
       (RO (NCONC (CAR TEMP)
		  (CDR TEMP))
	   (RPLACD TEMP NIL))
       (RI (OR (INTEGERP (CADR L))
	       (GO ILLG))
	   (SETQ A (INTH (CAR TEMP)
			 (CADR L)))
	   (OR (CDR A)
	       (GO EMPTY))
	   (RPLACD TEMP (NCONC (CDR A)
			       (CDR TEMP)))
	   (RPLACD A NIL))
       (BO (EDSMASH TEMP
		    (CAAR TEMP)
		    (NCONC (CDAR TEMP)
			   (CDR TEMP))))
       (BI (SETQ B
		 (CDR (SETQ A
			    (COND ((NULL
				    (INTEGERP (CADR L)))
				   TEMP)
				  (T (INTH CL (CADR L)))))))
	   (RPLACD A NIL)
	   (EDSMASH TEMP (CONS (CAR TEMP)
			       (CDR TEMP)) B))
       (OTHERWISE (GO ILLG)))
     (GO NEXT)
     NUMCARX
     (COND ((ZEROP X)
	    (GO ILLG))
	   ((< X 0)
	    (COND ((EQL X -1)
		   (SETQ L
			 (NCONC L
				(CONS (CAR CL)
				      (CDR CL))))
		   (EDSMASH CL (CAR L)
			    (CDR L)))
		  ((NLISTP
		    (SETQ A (INTH CL (- (1+ X)))))
		   (GO EMPTY))
		  (T (RPLACD A (NCONC L (CDR A))))))
	   ((EQL X 1)
	    (COND (L (EDSMASH CL
			      (CAR L)
			      (NCONC (CDR L)
				     (CDR CL))))
		  ((NLISTP CL)
		   (GO EMPTY))
		  ((NLISTP (CDR CL))
		   (SETQ TEMP (LENGTH (CADR CTLS)))
		   (SETQ EDCOM
			 (NCONC
			  (LIST 0
				(CASE TEMP
				  (1 '(1 NIL))
				  (OTHERWISE (CONS TEMP NIL))))
			  EDCOM)))
		  (T (EDSMASH CL (CADR CL)
			      (CDDR CL)))))
	   ((NLISTP (SETQ A (INTH CL (1- X))))
	    (GO EMPTY))
	   (T (RPLACD A
		      (COND ((CDR A)
			     (NCONC L (CDDR A)))
			    (T L)))))
     (GO NEXT)
     ATOML
     (SETQ X L)
     (COND ((INTEGERP X)
	    (COND ((ZEROP X)
		   (OR (CDR CTLS)
		       (GO TOP))
		   (SETQ CTLS (CDR CTLS))
		   (SETQ CL (CAR CTLS)))
		  (T (AND (< X 0)
			  (SETQ X
				(+ (LENGTH CL) 1 X)))
		     (SETQ X (INTH CL X))
		     (OR (LISTP X)
			 (GO EMPTY))
		     (SETQ CL (CAR X))
		     (SETQ CTLS (CONS CL CTLS)))))
	   (T (CASE X
		(P (EDITPR CL NIL 2 CTLS))
		(PP (EDITPR CL T 2 CTLS))
		(OK (RETURN (VALUES S 'OK)))
		(STOP (RETURN (VALUES S-ORIG 'STOP)))
		(SAVE (RETURN (VALUES S 'SAVE)))
		(UP (COND ((TAILP CL (CADR CTLS)))
			  ((NULL (CDR CTLS))
			   (GO TOP))
			  (T (SETQ CTLS (CDR CTLS))
			     (SETQ CL
				   (MEMBER CL (CAR CTLS)))
			     (OR (EQ CL (CAR CTLS))
				 (SETQ CTLS (CONS CL CTLS))))))
		(E (SETQ EDCOM (CONS (READ *QUERY-IO*) EDCOM))
		   (SETQ X (EVAL (COM-READ EDCOM)))
		   (COND ((NULL EDCOM)
			  (PRIN1 X *QUERY-IO*)
			  (TERPRI *QUERY-IO*))))
		(F (SETQ EDCOM (CONS (READ *QUERY-IO*) EDCOM))
		   (SETQ TEMP
			 (EDFIND1ST
			  (COM-READ EDCOM)
			  CL CTLS))
		   (COND ((NULL TEMP)
			  (EDMSG "NOT FOUND"))
			 (T (SETQ CTLS TEMP)
			    (SETQ CL (CAR TEMP))))
		   (COND ((ATOM CL)
			  (SETQ EDCOM (CONS 'UP EDCOM)))))
		(! (GO START))
		(NX (COND ((TAILP CL (CADR CTLS))
			   (SETQ CL (CDR CL))
			   (RPLACA CTLS CL))
			  (T (SETQ CTLS (CDR CTLS))
			     (SETQ CL
				   (CADR (MEMBER CL (CAR CTLS))))
			     (SETQ CTLS (CONS CL CTLS)))))
		(? (EDITPR CL NIL 100 CTLS))
		(?? (EDITPR CL T 100 CTLS))
		(S (SETQ EDCOM (CONS (READ *QUERY-IO*) EDCOM))
		   (SETQ A (COM-READ EDCOM))
		   (COND ((SYMBOLP A)
			  (SETF (GET A 'EDITVALUE) CL))
			 (T (GO ILLG))))
		(OTHERWISE (COND ((AND (SYMBOLP X) (GET X 'DEEDITA))
				  (SETQ EDCOM
					(APPEND
					 (EVAL (GET X 'DEEDITA))
					 EDCOM)))
				 (T (GO ILLG)))))))
     (GO NEXT)
     ILLG (EDMSG "ILLEGAL COMMAND")
     TOP  (EDMSG "ON TOP LEVEL")
     EMPTY
     (EDMSG "LIST EMPTY")))
  
#+:sbcl (defmacro save-image (file) `(sb-ext:save-lisp-and-die ,file))
#+:clisp (defmacro save-image (file) `(saveinitmem ,file))
#+:ccl (defmacro save-image (file) `(ccl:save-application ,file))

