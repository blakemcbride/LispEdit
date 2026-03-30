# LispEdit Code Analysis

## Bugs / Likely Errors

### 1. `MAKEFILE` uses `:OVERWRITE` instead of `:SUPERSEDE` (line 93)

```lisp
(WITH-OPEN-FILE (S FNAME :DIRECTION :OUTPUT :IF-EXISTS :OVERWRITE ...)
```

`:OVERWRITE` opens the existing file and writes from the beginning but does **not** truncate it. If the new content is shorter than the old file, stale data remains at the end. This is almost certainly wrong — `:SUPERSEDE` (replace the file) is the correct choice for writing a fresh save.

### 2. `EDITF` doesn't handle missing function definition (lines 143–158)

If `FN` has neither an `EDIT-SAVE` nor a `FNCELL` property, `VF` and `ESF` are both `NIL`, so `(EDITS NIL)` is called. `EDITS` then prints `"NOT EDITABLE"` and returns `(VALUES NIL 'CANT-EDIT)`. The `CASE` on `EXIT-TYPE` doesn't match `CANT-EDIT`, so it falls through silently and returns `FN` as if everything is fine. Should probably warn the user.

### 3. `(= e1 ...)` replacement — `(=)` delete doesn't work correctly (line 274–275)

The `=` command is implemented as:
```lisp
(= (SETQ EDCOM (CONS 'UP (CONS (CONS 1 L) EDCOM))))
```
For `(=)`, `L` is `NIL`, so this becomes `(CONS 1 NIL)` → `(1)`, which is a delete of element 1. Then `UP` moves up. This deletes the **first element** of `cexpr`, not the entire `cexpr`. Per the EDIT.txt spec, `(=)` should "Delete the current expression." The correct implementation would need to replace `cexpr` with nothing at the parent level.

### 4. `NX` command doesn't guard against end-of-list (lines 432–438)

When `NX` is used and there's no next element, `CL` becomes `NIL` (via `CDR` past the end or `CADR` of a list where no next exists). This silently sets `CL` to `NIL`, which will cause confusing behavior on subsequent commands rather than printing an error.

### 5. `UP` command tail handling (lines 407–414)

```lisp
(UP (COND ((TAILP CL (CADR CTLS)))
          ((NULL (CDR CTLS)) (GO TOP))
          (T (SETQ CTLS (CDR CTLS))
             (SETQ CL (MEMBER CL (CAR CTLS)))
             ...)))
```

The first clause `(TAILP CL (CADR CTLS))` does nothing when true — it's a no-op. This means if `CL` is already a tail of the parent, `UP` silently does nothing without changing position. The intent seems to be to set `CL` to the parent in that case.

### 6. `(1)` delete of last remaining element (lines 361–376)

When `cexpr` has only one element and you do `(1)` (delete it), the code tries to handle it by constructing an edit command sequence involving element `(CASE TEMP ...)`. This is convoluted and may produce incorrect results when `cexpr` is nested at different depths. It also doesn't handle `(CDR CL)` being a dotted pair rather than `NIL`.

## Completeness Issues (vs. EDIT.txt spec)

### 7. No true `(=)` for deleting the current expression

As noted in bug #3, `(=)` doesn't actually delete the current expression — it deletes its first element. The InterLisp `(=)` command removes the entire current expression from its parent.

### 8. `F` command reads from `*QUERY-IO*` even in programmatic mode (line 420)

```lisp
(F (SETQ EDCOM (CONS (READ *QUERY-IO*) EDCOM))
```

`F` always reads its argument from `*QUERY-IO*` with a hard-coded `READ`, ignoring `EDCOM`. The `S` command (line 441) has the same issue. All other commands correctly use `COM-READ` to pull from the command list when running non-interactively. This means `(EDITS expr F PRINT OK)` won't work — it'll block waiting for interactive input.

### 9. `E` command has the same interactive-only issue (line 415)

Same pattern as `F` — always reads from `*QUERY-IO*` rather than `COM-READ`.

### 10. No UNDO command

The InterLisp editor has an `UNDO` command. Since `EDITS` works on a `COPY-TREE`, there's no undo within a session beyond `STOP` (which discards everything). Not necessarily a bug, but a notable omission from a "complete" InterLisp editor.

## Minor Issues

### 11. `MAKEFILE` only saves `COMMON-LISP-USER` symbols (lines 96–103)

The `DO-ALL-SYMBOLS` loop checks `(SYMBOL-PACKAGE SYM)` against `COMMON-LISP-USER`. If the user is working in a different package, nothing gets saved. This should probably use `*PACKAGE*` or accept a package argument.

### 12. `*PACKAGE-SPECIAL*` is hardcoded (lines 30–41)

The shadowing/setup forms are duplicated — once as data in `*PACKAGE-SPECIAL*` and once executed inline. Any change to the setup must be made in two places.

## Summary

| # | Severity | Issue |
|---|----------|-------|
| 1 | **Bug** | `:OVERWRITE` should be `:SUPERSEDE` in `MAKEFILE` |
| 2 | **Minor** | `EDITF` silently ignores undefined functions |
| 3 | **Bug** | `(=)` deletes first element instead of current expression |
| 4 | **Bug** | `NX` past end of list sets CL to NIL silently |
| 5 | **Bug** | `UP` is a no-op when CL is a tail of parent |
| 6 | **Fragile** | `(1)` delete of last element is unreliable |
| 7 | **Missing** | No true "delete current expression" |
| 8 | **Bug** | `F` and `S` ignore programmatic command list |
| 9 | **Bug** | `E` ignores programmatic command list |
| 10 | **Missing** | No UNDO |
| 11 | **Limitation** | `MAKEFILE` hardcoded to `COMMON-LISP-USER` |
| 12 | **Smell** | `*PACKAGE-SPECIAL*` duplicated |
