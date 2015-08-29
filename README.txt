
The included code attempts to create an image based development
environment as described in the file named ImageBasedDevelopment.txt

The included structure editor is where the focus of my effort has been
thus far.  It is a pretty complete substantial sub-set of the standard
InterLisp structure editor for Common Lisp.  I have done some minimal
testing on all of its functions.  As far as I know, it functions error
free.

Since Common Lisp does not keep source s-exp's on loaded source code,
I had to re-define the standard DEFUN & DEFMACRO to save a copy of the
source on the symbol's property list, in addition to doing what DEFUN
& DEFMACRO are supposed to do.  You must use these if you wish to edit
functions and macros.  I have also created a LOAD that will use the
defined DEFUN & DEFMACRO facilities.

The editor provides four function.  One edits functions and macros,
one for variables, one for property lists, and one for s-exp's.
So, for example, a function named MyFun may be edited as follows:

    (EDITF MyFun)

You should not quote the function name.  A complete reference manual
for the editor is contained in the file named EDIT.txt

There is also a function named MAKEFILE.  That function attempts to
write all of the functions, macros, and variables within a package
into am external text file in a for suitable for reload'ing using the
accompanying LOAD.  It is extremely rudimentary at this point and
surly needs to be enhanced.

FUTURE

The system does not yet understand anything beyond functions, macros,
and variables.  It needs work to understand CLOS and packages.

I suppose currently the real value in this package is the structure
editor.  That is pretty complete and useful.  Much of the rest of the
code represents a small effort to create an environment that would render
the structure editor useful.

Blake McBride
blake@mcbride.name

