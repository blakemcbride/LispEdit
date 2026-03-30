;;; Exhaustive tests for LispEdit structure editor

(cl:load "LispEdit.lisp")
(in-package "LISPEDIT")

(cl:defparameter *test-count* 0)
(cl:defparameter *pass-count* 0)
(cl:defparameter *fail-count* 0)

(cl:defun test-equal (name expected actual)
  (cl:incf *test-count*)
  (cond ((equal expected actual)
	 (cl:incf *pass-count*)
	 (format t "  PASS: ~A~%" name))
	(t
	 (cl:incf *fail-count*)
	 (let ((*print-circle* t) (*print-level* 5) (*print-length* 10))
	   (format t "  FAIL: ~A~%    Expected: ~S~%    Got:      ~S~%" name expected actual)))))

(cl:defun test-edits (name input commands expected)
  "Run EDITS with commands (OK is appended automatically by EDITS) and check result."
  (let ((result (apply #'edits input commands)))
    (test-equal name expected result)))

(format t "~%========================================~%")
(format t "LispEdit Structure Editor Tests~%")
(format t "========================================~%")

;;; ============================================================
;;; 1. Basic exit commands
;;; ============================================================
(format t "~%--- Basic exit commands ---~%")

(test-edits "OK returns edited copy"
	    '(A B C) '(OK) '(A B C))

(multiple-value-bind (val exit-type)
    (edits '(A B C) 'STOP)
  (test-equal "STOP returns STOP exit-type" 'STOP exit-type))

(multiple-value-bind (val exit-type)
    (edits '(A B C) 'SAVE)
  (test-equal "SAVE returns SAVE exit-type" 'SAVE exit-type))

;;; ============================================================
;;; 2. Positioning commands
;;; ============================================================
(format t "~%--- Positioning commands ---~%")

(test-edits "Descend into sublist and modify"
	    '(A (B C) D) '(2 (1 X) 0 OK) '(A (X C) D))

(test-edits "Negative index descent"
	    '(A (B C) (D E)) '(-1 (1 X) 0 OK) '(A (B C) (X E)))

(test-edits "! resets to top level"
	    '(A (B C) D) '(2 ! (1 X) OK) '(X (B C) D))

(test-edits "0 ascends one level"
	    '(A (B C) D) '(2 1 0 (1 X) 0 OK) '(A (X C) D))

(test-edits "Descend into 1st element"
	    '((A B) C D) '(1 (1 X) 0 OK) '((X B) C D))

(test-edits "Descend into 3rd element"
	    '(A B (C D)) '(3 (2 X) 0 OK) '(A B (C X)))

;;; ============================================================
;;; 3. Replacement commands
;;; ============================================================
(format t "~%--- Replacement commands ---~%")

(test-edits "Replace 1st element"
	    '(A B C) '((1 X) OK) '(X B C))

(test-edits "Replace 2nd element"
	    '(A B C) '((2 X) OK) '(A X C))

(test-edits "Replace 3rd element"
	    '(A B C) '((3 X) OK) '(A B X))

(test-edits "Replace with multiple elements"
	    '(A B C) '((2 X Y) OK) '(A X Y C))

;; (= e1 ...) goes UP then replaces, so e1 ... replace the position in parent
(test-edits "(= e1 e2) replaces cexpr position"
	    '(A (B C) D) '(2 (= X Y) 0 OK) '(A X Y D))

(test-edits "(= e1) replaces cexpr with single"
	    '(A (B C) D) '(2 (= (P Q)) 0 OK) '(A (P Q) D))

(test-edits "(R x y) replaces all occurrences"
	    '(A B A C A) '((R A Z) OK) '(Z B Z C Z))

(test-edits "(R x y) in nested structure"
	    '(A (B A) C) '((R A Z) OK) '(Z (B Z) C))

;;; ============================================================
;;; 4. Insertion commands
;;; ============================================================
(format t "~%--- Insertion commands ---~%")

(test-edits "Insert before 1st element"
	    '(A B C) '((-1 X) OK) '(X A B C))

(test-edits "Insert before 2nd element"
	    '(A B C) '((-2 X) OK) '(A X B C))

(test-edits "Insert before 3rd element"
	    '(A B C) '((-3 X) OK) '(A B X C))

(test-edits "Insert multiple before 2nd"
	    '(A B C) '((-2 X Y) OK) '(A X Y B C))

(test-edits "(N e1) appends to end"
	    '(A B C) '((N D) OK) '(A B C D))

(test-edits "(N e1 e2) appends multiple"
	    '(A B C) '((N D E) OK) '(A B C D E))

(test-edits "(A e1) adds after cexpr"
	    '(A (B C) D) '(2 (A X) 0 OK) '(A (B C) X D))

(test-edits "(A e1 e2) adds multiple after cexpr"
	    '(A (B C) D) '(2 (A X Y) 0 OK) '(A (B C) X Y D))

(test-edits "(B e1) adds before cexpr"
	    '(A (B C) D) '(2 (B X) 0 OK) '(A X (B C) D))

(test-edits "(B e1 e2) adds multiple before cexpr"
	    '(A (B C) D) '(2 (B X Y) 0 OK) '(A X Y (B C) D))

;;; ============================================================
;;; 5. Deletion commands
;;; ============================================================
(format t "~%--- Deletion commands ---~%")

(test-edits "Delete 1st element"
	    '(A B C) '((1) OK) '(B C))

(test-edits "Delete 2nd element"
	    '(A B C) '((2) OK) '(A C))

(test-edits "Delete 3rd element"
	    '(A B C) '((3) OK) '(A B))

(test-edits "Delete 1st of 2 elements"
	    '(A B) '((1) OK) '(B))

(test-edits "Delete last element of multi-element list"
	    '(A B C) '((3) OK) '(A B))

;; (1) when only one element in sublist - should remove sublist from parent
(test-edits "Delete last element of sublist removes sublist"
	    '(A (B) C) '(2 (1) OK) '(A C))

;; (=) deletes cexpr via UP then (1) -- works from descended position
(test-edits "(=) deletes cexpr after descent"
	    '(A (B C) D) '(2 (=) OK) '(A D))

;; (=) delete first element
(test-edits "(=) deletes first element"
	    '((A B) C D) '(1 (=) OK) '(C D))

;; (=) delete last element
(test-edits "(=) deletes last element"
	    '(A B (C D)) '(3 (=) OK) '(A B))

;; (=) delete middle element after descent
(test-edits "(=) delete middle element after descent"
	    '(A (B C) D E) '(2 (=) OK) '(A D E))

;; (= e1 ...) replaces cexpr
(test-edits "(= X Y Z) replaces cexpr"
	    '(A (B C) D) '(2 (= X Y Z) 0 OK) '(A X Y Z D))

(test-edits "(= (T Z)) replaces cexpr with single list"
	    '(A (B C) D) '(2 (= (T Z)) 0 OK) '(A (T Z) D))

;;; ============================================================
;;; 6. Parenthesis manipulation
;;; ============================================================
(format t "~%--- Parenthesis manipulation ---~%")

(test-edits "(BI 2 3) wraps elements 2 through 3"
	    '(A B C D) '((BI 2 3) OK) '(A (B C) D))

(test-edits "(BI 1 3) wraps elements 1 through 3"
	    '(A B C D) '((BI 1 3) OK) '((A B C) D))

(test-edits "(BI 2) wraps single element"
	    '(A B C) '((BI 2) OK) '(A (B) C))

(test-edits "(BO 2) removes both parens"
	    '(A (B C) D) '((BO 2) OK) '(A B C D))

(test-edits "(LI 2) inserts left paren before 2nd"
	    '(A B C D) '((LI 2) OK) '(A (B C D)))

(test-edits "(LO 1) removes left paren from 1st"
	    '((A B C) D E) '((LO 1) OK) '(A B C))

(test-edits "(RO 2) moves right paren of 2nd to end"
	    '(A (B C) D E) '((RO 2) OK) '(A (B C D E)))

(test-edits "(RI 1 2) moves right paren in"
	    '((A B C) D) '((RI 1 2) OK) '((A B) C D))

;;; ============================================================
;;; 7. MBD (embed) command
;;; ============================================================
(format t "~%--- MBD command ---~%")

(test-edits "(MBD (IF * T)) embeds cexpr"
	    '(A (PRINT X) C) '(2 (MBD (IF * T)) 0 OK) '(A (IF (PRINT X) T) C))

;;; ============================================================
;;; 8. XTR (extract) command
;;; ============================================================
(format t "~%--- XTR command ---~%")

(test-edits "(XTR 3) extracts 3rd element"
	    '(A (COND (X Y) (T Z)) C) '(2 (XTR 3) 0 OK) '(A (T Z) C))

(test-edits "(XTR 2) extracts 2nd element"
	    '(A (COND (X Y) (T Z)) C) '(2 (XTR 2) 0 OK) '(A (X Y) C))

;;; ============================================================
;;; 9. F (find) command
;;; ============================================================
(format t "~%--- Find command ---~%")

(test-edits "F finds atom and positions there"
	    '(A B C D) '(F C (1 X) 0 OK) '(A B X D))

(test-edits "F finds atom in nested structure"
	    '(A (B C D) E) '(F D (1 X) 0 0 OK) '(A (B C X) E))

(test-edits "F finds first occurrence"
	    '(A B C B D) '(F B (1 X) 0 OK) '(A X C B D))

;;; ============================================================
;;; 10. S/US (store/use) commands
;;; ============================================================
(format t "~%--- S/US commands ---~%")

(test-edits "S stores and US retrieves expression"
	    '(A (B C) D) '(2 S MYVAR 0 (US MYVAR (3 MYVAR)) OK) '(A (B C) (B C)))

;;; ============================================================
;;; 11. MARK and / commands
;;; ============================================================
(format t "~%--- MARK and / commands ---~%")

(test-edits "MARK and / return to marked position"
	    '(A (B C) D) '(2 (MARK M1) 0 (/ M1) (1 X) 0 OK) '(A (X C) D))

;;; ============================================================
;;; 12. E (eval) command
;;; ============================================================
(format t "~%--- E (eval) command ---~%")

(test-edits "E evaluates expression"
	    '(A B C) '(E (+ 1 2) OK) '(A B C))

;;; ============================================================
;;; 13. NX command
;;; ============================================================
(format t "~%--- NX command ---~%")

(test-edits "NX from tail position (no modification)"
	    '(A (B C) D) '(2 UP NX 0 OK) '(A (B C) D))

;; After 2 UP NX, CL is the tail ((D E)). Descend into 1 to modify (D E).
(test-edits "NX after descent then modify"
	    '(A (B C) (D E)) '(2 UP NX 1 (1 X) 0 0 OK) '(A (B C) (X E)))

;;; ============================================================
;;; 14. UP command
;;; ============================================================
(format t "~%--- UP command ---~%")

(test-edits "UP from descended position"
	    '(A (B C) D) '(2 1 UP (1 X) 0 OK) '(A (X C) D))

;; After 2 2 UP, CL is the tail ((C D)) inside (B (C D)). (1 X) replaces (C D) with X.
(test-edits "UP from nested descent"
	    '(A (B (C D)) E) '(2 2 UP (1 X) 0 0 OK) '(A (B X) E))

;;; ============================================================
;;; 15. Complex/combined operations
;;; ============================================================
(format t "~%--- Complex operations ---~%")

(test-edits "Multiple replacements"
	    '(A B C D E) '((1 X) (3 Y) (5 Z) OK) '(X B Y D Z))

(test-edits "Deep nested modification"
	    '(A (B (C (D E))) F) '(2 2 2 (1 X) 0 0 0 OK) '(A (B (C (X E))) F))

(test-edits "Insert then delete"
	    '(A B C) '((-2 X) (3) OK) '(A X C))

(test-edits "Modify nested then append"
	    '(A (B C)) '(2 (1 X) 0 (N D) OK) '(A (X C) D))

(test-edits "(BI 2) then (BO 2) restores structure"
	    '(A B C D) '((BI 2 3) (BO 2) OK) '(A B C D))

(test-edits "Delete nested element preserves parent"
	    '(A (B C D) E) '(2 (2) 0 OK) '(A (B D) E))

(test-edits "Find deep and modify"
	    '(PROG (X) (SETQ X 1) (PRINT X)) '(F PRINT (2 Y) ! OK)
	    '(PROG (X) (SETQ X 1) (PRINT Y)))

(test-edits "Multiple deletions"
	    '(A B C D E) '((2) (2) OK) '(A D E))

(test-edits "Replace then insert"
	    '(A B C) '((2 X) (-3 Y) OK) '(A X Y C))

;;; ============================================================
;;; 16. Edge cases
;;; ============================================================
(format t "~%--- Edge cases ---~%")

(test-edits "Single element list - replace"
	    '(A) '((1 B) OK) '(B))

(test-edits "Deeply nested modification"
	    '((((A)))) '(1 1 1 (1 B) 0 0 0 OK) '((((B)))))

(test-edits "Replace in 2-element list"
	    '(A B) '((1 X) OK) '(X B))

(test-edits "Large list operations"
	    '(1 2 3 4 5 6 7 8 9 10)
	    '((5 X) (10 Y) OK)
	    '(1 2 3 4 X 6 7 8 9 Y))

(test-edits "(=) delete first element from top"
	    '((X Y) B C) '(1 (=) OK) '(B C))

(test-edits "(=) delete last element from top"
	    '(A B (X Y)) '(3 (=) OK) '(A B))

(test-edits "(=) delete middle sublist"
	    '(A (B) C) '(2 (=) OK) '(A C))

(test-edits "Append to nested list"
	    '(A (B C)) '(2 (N D) 0 OK) '(A (B C D)))

(test-edits "Insert at beginning of nested"
	    '(A (B C)) '(2 (-1 X) 0 OK) '(A (X B C)))

;;; ============================================================
;;; 17. EDITF/EDITV tests
;;; ============================================================
(format t "~%--- EDITF/EDITV ---~%")

;; Test EDITF - define a function then edit it
(defun test-fn (x) (+ x 1))
(editf test-fn '(R X Y) 'OK)
(let ((fncell (get 'test-fn 'fncell)))
  (test-equal "EDITF modifies function definition"
	      '(LAMBDA (Y) (+ Y 1))
	      fncell))

;; Test EDITV
(cl:defparameter *test-var* '(A B C))
(editv *test-var* '(1 X) 'OK)
(test-equal "EDITV modifies variable"
	    '(X B C) *test-var*)

;;; ============================================================
;;; 18. MAKEFILE test
;;; ============================================================
(format t "~%--- MAKEFILE ---~%")

(defun makefile-test-fn (x) (* x 2))
(let ((fname "/tmp/lispedit-test-output.lisp"))
  (makefile fname)
  (test-equal "MAKEFILE returns filename"
	      fname
	      (if (probe-file fname) fname nil)))

(ignore-errors (delete-file "/tmp/lispedit-test-output.lisp"))

;;; ============================================================
;;; 19. Error handling
;;; ============================================================
(format t "~%--- Error handling ---~%")

(multiple-value-bind (val exit-type)
    (edits 'not-a-list 'OK)
  (test-equal "Non-list returns CANT-EDIT" 'CANT-EDIT exit-type))

(multiple-value-bind (val exit-type)
    (edits 42 'OK)
  (test-equal "Number returns CANT-EDIT" 'CANT-EDIT exit-type))

;;; ============================================================
;;; 20. STOP preserves original
;;; ============================================================
(format t "~%--- STOP preserves original ---~%")

(let ((orig '(A B C)))
  (multiple-value-bind (val exit-type)
      (edits orig 'STOP)
    (test-equal "STOP returns original structure" t (equal orig val))))

;;; ============================================================
;;; Summary
;;; ============================================================
(format t "~%========================================~%")
(format t "Results: ~A/~A passed, ~A failed~%"
	*pass-count* *test-count* *fail-count*)
(format t "========================================~%")

(if (zerop *fail-count*)
    (format t "ALL TESTS PASSED~%")
    (format t "SOME TESTS FAILED~%"))

(sb-ext:exit :code *fail-count*)
