# LispEdit

A Common Lisp structure editor inspired by the InterLisp structure editor, ported from LISPF4.

## Project Overview

LispEdit provides an image-based development environment for Common Lisp. The core is a structure editor that operates on s-expressions directly (not text), enabling in-environment editing of functions, macros, variables, and property lists.

The project supports SBCL, CLISP, and CCL.

## Source Files

- `LispEdit.lisp` — The entire codebase (single file, ~460 lines)
- `EDIT.txt` — Editor command reference manual
- `ImageBasedDevelopment.txt` — Description of image-based vs disk-based development
- `LICENSE.txt` — BSD 2-clause license

## Architecture

All code lives in the `LISPEDIT` package which shadows `DEFUN`, `DEFMACRO`, and `LOAD` from `COMMON-LISP`:

- **Custom `DEFUN`/`DEFMACRO`**: Store source s-expressions on symbol property lists (`FNCELL`) so the editor can retrieve and modify them
- **Custom `LOAD`**: Uses the custom `DEFUN`/`DEFMACRO` when loading files
- **`EDITS`**: Core editor loop — a PROG-based state machine using GO for control flow. All editor commands are dispatched here
- **`EDITF`/`EDITV`/`EDITP`**: Entry points for editing functions, variables, and property lists (macros wrapping `EDITS`)
- **`MAKEFILE`**: Serializes all user-defined symbols back to a loadable text file
- **`SAVE-IMAGE`**: Platform-conditional macro for saving Lisp images (SBCL/CLISP/CCL)

## Key Conventions

- Code uses uppercase symbols throughout (traditional Lisp style)
- Destructive list operations (`RPLACA`, `RPLACD`, `NCONC`, `NSUBST`) are used intentionally for in-place editing
- The editor works on a `COPY-TREE` of the input, only committing changes on `OK`
- Editor commands are read from `*QUERY-IO*`
- `PROG`/`GO` style (not functional) is used for the main editor loop

## Loading

```lisp
(load "LispEdit.lisp")
(use-package :lispedit)
```

## Exported Symbols

`DEFUN`, `DEFMACRO`, `EDITF`, `EDITV`, `EDITP`, `EDITS`, `LOAD`, `MAKEFILE`, `SAVE-IMAGE`
