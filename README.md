# The Design of an M6800 LISP Interpreter
S Tucker Taft
Harvard University Science Center
1 Oxford St
Cambridge MA 02138

Anyone exposed to small computer systems has used a language interpreter of some sort, and certainly may have thought about implementing their own interpreter. Unhappily, implementing an interpreter for a complete version of most computer languages is a difficult and time-consuming job, unsuitable for a part-time personal computer enthusiast. The language LISP provides a unique opportunity in this respect. The foundation for a very complete interpreter can be programmed by a single person in several months of part-time effort. As a bonus, the resulting interpreter provides the user with a high level language in which to express algorithms.

### The Language
From the user's point of view, the primary data structure in LISP is the _list_. Every element of a list is either an _atom_ or another list. An atom is a primitive named object, the name being an arbitrary string of characters:

`ABC` is an atom.
`135` is an atom.
`(ABC 135)` is a list of two elements, both atoms.
`((ABC 135) XYZ)` is a list of two elements, the first of which is a list, the second is an atom.
`( () () )` is a list of two elements, both being lists of zero elements. A list of zero elements, the null list, is identified with the atom `NIL`.

The feature of the language LISP which makes it at the same time a uniquely interesting language, and relatively easy to implement, is that all program elements are represented using these same kinds of objects: atoms and list. Constants, variables, expressions, conditionals, even function definitions are all represented using only atoms and lists.

A value is associated with each atom, allowing atoms to represent program variables and constants. A symbolic atom, like `XYZ`, would represent a variable. A numeric atom, like `237`, would represent a constant.

Operations on variables and constants, like addition, or a function call, are represented by list expressions:

`(ADD 2 5)` would represent the expression `2 + 5`.
`(SIN (MUL 2 Y)` would represent the expression `sin(2y)`.

Conditionals, loops, and function definitions are also represented by list expressions, as illustrated by this recursive function implementing Euclid's greatest common divisor algorithm:

```
(DEF GCD (LAMBDA (X Y)
  (COND
    ((GREATER X Y) (GCD (SUB X Y) Y))
    ((GREATER Y X) (GCD X (SUB Y X)))
    (T X)
  )
))
```

This would be equivalent to the Pascal program:

```
function ged(x, y: integer): integer
  begin
    if x > y then gcd := gcd(x-y, y)
  else
    if y >x then gcd := gcd(x, y-x)
  else
    gcd := x
  end.
```

An important difference to note in the above comparison is that no explicit assignment to a function return value is made in LISP, whereas in Pascal one must explicitly say `gcd :=...` to specify the return value. In Pascal, and most other _procedural_ languages, a distinction is made between program statements and expressions. In such languages some program statement must be executed to specify the return value, usually either a return statement or an assignment to the function name. In LISP, and other `applicative` languages, no such distinction is made. A function is simply a single expression, whose value is the return value of the subprogram.

This is made possible by built-in functions like `COND` used above. `COND` takes a list of two element lists as argument. It goes down the list of pairs, evaluating the first element of each pair. If the result is true (the atom `T`), the result of the entire `COND` is the value of the second element of the pair. If the value of the first element of the pair is false (the atom `NIL`), `COND` proceeds to the next pair. If `COND` reaches the end of the list, the result of the entire `COND` is simply `NIL`. In the above example this would never happen because the first element of the last pair is the atom `T` (whose value is always guaranteed to be itself, the atom `T`). This is the normal technique in LISP for using the `COND` function.

The expression:

```
(DEF GCD (LAMBDA (X Y)...
```

defines the atom `GCD` to be a function (or _lambda expression_) taking two arguments, to be called `X` and `Y` in the body of the definition. Notice that no explicit specification of the type of `X` or `Y` is provided. In LISP any arbitrary value, atom, or list may be the value associated with an atom. In this sense LISP is a typeless language. In fact the type of a _value_ (ie: whether it is an atom or a list) is always determinable at execution time. Functions must check the types of the values of atoms if only certain types are legal arguments. In the above example the calls on `GREATER` and `SUB` would fail if the values associated with `X` and `Y` were not numeric atoms.

### CARs and CDRS
Thus far we have only shown how to re-express algorithms written in a more conventional language, in the language LISP. The real power of LISP comes from its ability to directly manipulate lists, a data type not normally accessible in other languages. Three primitives, `CAR`, `CDR` (pronounced could-er), and `CONS` are provided for list manipulation. The function `CAR` takes a list as argument, and returns the first element of the list, which may either be an atom or another list. The function `CDR` takes a list as argument, and returns the tail of the list, that is, all but the first element of the orginal list, as a new list. The function CONS takes two arguments, a new first element, and the tail of a list, and reconstructs a list, now one element lunger. For example:

Assume the atom `X` is associated with the value:
```
  (A B C)
```
Assume the atom `Y` is associated with the value:
```
  (THE CAT IN THE HAT)
```

`(CAR X)` would be the atom `A`.
`(CDR Y)` would be the list `CAT IN THE HAT)`.
`(CONS (CAR X) (CDR Y))` would be the list: `(A CAT IN THE HAT)`
`(CAR (CDR X))` would be the atom `B`.

In general the `CAR` of the `CDR` of a list is its second element, and a function called `CADR` is frequently defined as a kind of shorthand for `CAR` of the `CDR`.

You might wonder what would result if you gave two atoms as arguments to `CONS`, rather than an atom and a list. In most LISP systems this is in fact legal. The result reveals the underlying representation used for lists in LISP. In virtually all LISP systems, lists are built up out of _dotted pairs_, two-address cells, the left cell pointing to the first element of a list, and the right cell pointing to the rest of the list. This can be diagrammed schematically as in figure 1.

Because dotted pairs are used this way to build up lists, it is natural to call the left cell of a dotted pair the `CAR` and the right cell the `CDR`. (In fact the genealogy of the words `CAR` and `CDR` runs the other way. Dotted pairs were used in the initial implementation of LISP, and `CAR` and `CDR` referred to the address field and the decrement field of a word on the IBM 704.) Now you can perhaps guess that when you pass two atoms as arguments to `CONS`, you simply get a dotted pair with an atom in both the `CAR` and `CDR`. For example:

| A | B |

would be printed as:

```
(A . B)
```

The notation `(A . B)` is used whenever the `CDR` of the last dotted pair forming a linked list is a non-NIL atom. In general `(D E F . NIL)` would be equivalent to `(D E F)`, whereas `(D E F . G)` could not be expressed without the _dot_ notation.

Given the three primitives `CAR`, `CDR`, and `CONS`, and understanding the underlying representation of lists using dotted pairs, it is possible to write powerful list-manipulating programs in LISP. For example, suppose it is desirable to edit a large data structure, and change all occurrences of the symbol `APPLE` to `ORANGE`. In LISP we could easily write a routine called `REPLACE` which, given the data structure (ie: list structure), the original symbol (the atom `APPLE`), and the replacement symbol (the atom `ORANGE`), would go through the structure and do the replacement, using itself recursively to do the replacement in all sublists of the list structure:

```
(DEF REPLACE (LAMBDA (STRUC OLD NEW)
  (COND
    ((EQ STRUC OLD) NEW)
    ((ATOM STRUC) STRUC)
    (T (CONS
      (REPLACE (CAR STRUC) OLD NEW)
      (REPLACE (CDR STRUC) OLD NEW)
    ))
  )
))
```

Notice how the first two lines of the `COND` allow for the possibility that the input data structure is simply an atom (which may or may not be equal to the atom to be replaced). In addition, notice that the entire body of this function definition is a single `COND`. just as it was in the GCD example given above. This is frequently true in LISP programs. Finally, notice how the function simply _passes the buck_ to recursive calls on itself if the `STRUC` argument is not an atom, `CONS`ing together the results of the two inner calls. The reader is encouraged to go through an example of the execution of this function when the argument `OLD` is the atom `APPLE`, the argument NEW is the atom `ORANGE`, and the argument `STRUC` is the list structure:

```
(AN (APPLE A DAY) KEEPS (THE (APPLE MAN) BUSY))
```

The result should be:

```
(AN (ORANGE A DAY) KEEPS (THE (ORANGE MAN) BUSY))
```

If `STRUC` were:

```
(PEAR BANANA . APPLE)
```

the result should be:
```
(PEAR BANANA . ORANGE)
```

Other kinds of list-manipulating programs which are relatively easy to write in LISP, but very difficult in more conventional languages, include formula manipulation programs which might take in the list representation for a function (eg: `(SIN (MUL 2 X))`), and return the list representation for its derivative according to the rules of the calculus (eg: `(MUL 2 (COS (MUL 2 X)))`).

The author's system is being used for the development of a compiler/interpreter system which generates the list representation for a program written in a programming language, and then either interprets it directly, Or generates the list of machine language statements to implement the program on a particular microcomputer. LISP makes such an undertaking quite straightforward (although not trivial, unfortunately!).

### LISP Interpreter
Because programs are data objects (list structures) in LISP, the same routines used to read and print data objects may be used to read and print programs. Furthermore user functions, like a general list editor, can be used also to edit programs. This uniformity vastly simplifies the task of writing an interpreter for LISP. Only three basic modules need be produced: `READ`, `EVAL`, and `PRINT`. `READ` accepts a LISP list expression from the terminal, in full parenthesized notation, and builds the internal representation of the list, sometimes called a _form_. `EVAL` takes a form as its single argument, and evaluates the form according to the LISP convention that the first element of such a list specifies the function, with the rest of the list as arguments.

The result of `EVAL` is another form. (The term _form_ is sometimes reserved for LISP expressions which are legal input to `EVAL`. The term _S-expression_ covers all types of lists, whether or not the first element is a legal function name. Within this paper, form will be used to refer to the internal representation of any type of LISP expression.)

`PRINT` takes a form as its argument, and types it on the terminal in fully parenthesized form. The top level loop of the LISP interpreter simply prompts the user for input (`- >` is the LISP prompt), `READ`s in the users input, `EVAL`s the resulting forin, and `PRINT`s the result of `EVAL`. In a conventional high level language syntax, this would be:

```
while true do begin 
  patom("->"):
  form := read();
  form := eval(form);
  print(form)
end.
```

or in M6800 assembly language:

```
BIGLUP LDX PRMPAT get prompt atom
       JSR PATOM  print the atom
       JSR READ   read the form typed in
* result now in M6800 x-register
       JSR EVAL   eval the form
* result of EVAL back in x-register
       JSR PRINT  print the form
       BRA BIGLUP and loop around
```

`PATOM` is a subroutine, also called by `PRINT`, when a form is known to be an atom. In an assembly language implementation, it would be very convenient to pass forms in the M6800 index (`X`) register. This register is 16 bits long, so it requires that forms be only 16 bits. Some representation must be chosen for all LISP objects so that a single 16 bit number may uniquely specify any arbitrary object. Dotted pairs are used to represent lists. Dotted pairs hold two forms, a `CAR` and a `CDR`, so they must be 32 bit objects. A natural choice is to allocate 4 consecutive M6800 memory bytes for dotted pairs, and specify dotted pairs by the address of their first byte. This means that any two different dotted pairs will be easily differentiated by the forms that specity them.

This still leaves the problem of deciding on an internal representation for atoms, including symbolic atoms, numeric atoms, and NIL. In the author's LISP system only two items of information are needed for each symbolic atom, the string of characters which are the print name of the atom, and the value currently associated with the atom (which is an arbitrary form). Again a 4 byte representation is chosen, with the first two bytes used as a memory address pointing to the first character of the print name, and the third and fourth bytes used to hold the value (a form) of the atom. Now the address of this 4 byte object can specify the atom uniquely from all other atoms and from all other dotted pairs.

Unfortunately this does not provide a simple way of distinguishing atoms from dotted pairs, when just given the form. Several solutions to this problem are possible. One is to restrict dotted pairs to a certain part of memory, then the address would determine whether the form specified an atom or a dotted pair. A second method is to add an additional byte to both dotted pairs and atoms which simply contains a type specifier, say 1 for dotted pairs and 2 for atoms. This method makes future expansion of types simple, but is somewhat wasteful in terms of space. The third method, the one chosen for the author's system, is to align all dotted pairs and atoms on 4 byte boundaries, that is, with addresses which are a multiple of four. This means that the low order two bits of the address are expected to always be zero, and hence may be used to encode type information. In the author's system, dotted pairs are specified by forms with both bits zero, and symbolic atoms by 01 in the lower two bits. One of the bits is still unused, but will become very handy when _garbage collection_ methods are discussed below.

With numeric atoms, their name determines their value, and hence only their name (or their value) need be specified by a form. On the author's M6800 system only hexadecimal memory addresses `0000` thru `7FFF` were accessible for storage of dotted pairs and atoms, meaning that the high order bit of forms specifying either of these was always zero. A representation for numeric atoms was chosen to be a form with the high order bit set, 14 bits of numeric value, and one bit left for _garbage collection_.

A special representation for the `NIL` atom is used both because the value of `NIL` is, like numeric atoms, required always to be the atom itself, and because it is used universally to represent the end of a list. The form chosen to specify `NIL` is simply the value zero. In fact any form with the high order byte zero is treated like `NIL` to simplify the test for `NIL` in certain cases. This means that the 256 byte page starting at zero is not usable for storing atoms or dotted pairs, but this restnction causes no problem at all, since both are allocated starting at the highest address available, and the allocator runs into program long before it reaches page zero.

When writing a LISP interpreter, the implementor nust decide relatively early-on how forms will specity all types of LISP objects. Unfortunately, it may not be until well into the implementation that the implementor discovers that certain choices were inefficient or inconvenient.

One important requirement affecting this decision not yet mentioned is the need to implement the LISP `EQ` function. This function takes two arbitrary forms, and returns the atom `T` or the atom `NIL` depending on whether the forms specify the same dotted pair, or whether the forms specify the same atom. Whenever an atom is input by `READ`, it must return the form specifying that atom to the caller. Whenever the same symbolic atom name is typed, `READ` must return the same form, ie: a pointer to the same 4 byte cell. This is accomplished by retaining a linked list of all defined symbolic atoms (called the `OBLIST`).

Before allocating a new 4 byte cell for an atom, `READ` scans the `OBLIST` for an atom of the given print name. If found, `READ` returns a form specifying that pre-exisiting atom. (Otherwise it must copy the name into some area used for storing names, allocate a 4 byte cell, initialize the left cell to point to the name, and the right cell to NIL, and return a form specifying the new atom.) This method guarantees that two forms specify the same symbolic atom if and only if they have the same address.

In some implementations of numeric atoms, this same rule cannot be guaranteed. In such systems, numeric atoms are simply allocated an appropriately large cell to store their numeric value (and hence allowing numeric atoms greater than 14 bits), a new cell being allocated every time a new number is generated (which happens at every `ADD`, `MUL`, etc). In these systems it would be impractical to scan a list like the `OBLIST` every time any arithmetic calculation is done, and so the LISP function `EQ` may not rely on the rule that unequal forms indicate unequal atoms. In such systems, `EQ` must look at the contents of the cell specified by a numeric atom form, and make the comparison that way. In systems like the author's, `EQ` simply compares the forms themselves, no matter what type of atom the form may specify.

The choices made in representing the various types of LISP objects can be summarized in the high level language (Pascal-like) data structure specification in listing 1.

```
type lisptype =
        (dtprtype, symamtype, numamtype, nilatmlype);
  dptr =
    record
      car: form; 
      cdr: form
    end;
  symatm = 
    name: ↑array [0..n] of char;
    value: form
  end;
  form =
    packed record 
      gcbit: boolean:
      case objtype: lisptype of 
        dptrrtype:    (dtprform:    ↑dtpr);
        symatmtype:   (symatmform:  ↑symatm);
        numatmtype:   (numatmform:  -5000..4999):
        nilatmtype:   ()
    end.
```

Listing 1: A Pascal data structure specification that could be used to represent various types of LISP objects.

### READ Function
`READ` is the basic input routine for the LISP interpreter. `READ `accepts a fully parenthesized expression from the terminal, and builds up the internal representation, allocating new dotted pairs and atoms as necessary. If the expression is a list, `READ` returns a form specifying the tirst dotted pair of the constructed list. It the expression typed in is simply an atom, `READ` returns a form specifying the atom.

The logic of the `READ` routine is straightforward because the syntax of LISP expressions is so simple. `READ` calls a function `RATOM` to return the next input atom. `RATOM` actually does the work of allocating new 4 byte cells for symbolic atoms (when necessary) as explained above. `RATOM` returns a form specifying the atom typed. If this atom is anything but the atom `y` `READ` simply returns the atom as its result. If the atom returned by `RATOM` is `"("`, `READ` calls itself recursively until it gets the atom `")"`. meanwhile stringing the forms returned together as the `CAR`s on a linked list of dotted pairs. This could be written as in listing 2.

```
(a) 
(DEF READ (LAMBDA ()
  (READR (RATCM))
))

(DEF READR (LAMBDA (A)
  (COND
    ((EQ A LPAREN) (READL (READ)))
  )
))

(b)

READ   JSR RATOM  pick up the next input atom
       CPX LPARAT is it equal to the "(" atom?
       BNE RDRET  no, simply return the arom
       JSR LSTINI yes, initialize a linked list
RDLUP  BSR READ   call READ recursively to pick up next form
       CPX RPARAT is it equal to the ")" atom?
       BEQ RDLDUN yes, finalize the list and return
       JSR LSTADD no, allocate a new dotted pair,
                  fill in the CAR, and add it to the list,
       BRA RDLUP  and loop around
RDLDUN JSR LSTEND finalize the list, get a pointer to
                  the first dotted pair of the list
RDRET  RTS        and return.
```
Listing 2: LISP (a) and M6800 assembly (b) code for the `READ` function.

In the LISP functions we are assuming that the atoms `LPAREN` and `RPAREN` were initialized to point to the atoms with print names `"("` and `")"` respectively. Notice that in the LISP version, `READ` accomplishes the loop of the machine code version with recursion in `READL`. The routines `LSTINI`, `LSTADD`, and `LSTEND` used in the assembly language version build up a linked list of dotted pairs, using two pointers on a stack, one to the first dotted pair, one to the dotted pair at the current end of the linked list. The pointers are on a stack so that `READ` may call itself recursively. The stack is actually a linked list itself. The linked-list stack is manipulated with the routines in listing 3. 

```
* PUSHX saves the value of the X-reg on a linked-list stack.
PUSHX BSR  GETCEL   allocate a new dotted pair and fill in the CAR
      LDAA STKPTR   fill in the CDR from STKPTR
      STAA CDR, X   (CAR and CDR are symbols defined as
      LDAA STKPTR+1 0 and 2 respectively)
      STAA CDR+1, X
      STX STKPTR    update the STKPTR
      LDX CAR, X    restore the X-reg
      RTS           and return.
* allocate a new dotted pair, save X-reg in the CAR,
* and set the CDR to NIL
GETCEL STX XTMP     save the X-reg temporarily
       LDX FREPTR   pick up a free dotted pair off the FREE list.
       BEQ NOFREE   no more left, so it goes...
       LDAA CDR, X  update the FREE list pointer
       STAA FREPTR  
       LDAA CDR+1, X
       STAA FREPTR+1
       CLR CDR, X   set the CDR to NIL
       CLR CDR+1, X 
       LDAA XTMP    fill in the CAR from the X-reg
       STAA CAR, X  
       LDAA XTMP+1  
       STAA CAR+1, X
       RTS          and return.
* restore the X-reg from the linked-list stack
POPX LDX STKPTR point to the top of the stack
     LDAA CDR,X     update the STKPTR
     STAA STKPTR    
     LDAA CDR+1, X  
     STAA STKPTR+1  
     BRA FRECEL     and free up the dotted pair used.
* free up a dotted pair, and load X-reg from the CAR
FRECEL LDAA FREPTR  link the cell onto the FREE list
       STAA CDR, X  
       LDAA FREPTR+1
       STAA CDR+1, X
       LDX CAR, X   load X-reg from CAR
       RTS          and return.
* return value at top of stack in X-reg
* (but leave it on stack)
TOPX LDX STKPTR     return CAR of cell on top of stack.
     LDX CAR, X
     RTS
```
Listing 3: M6800 implementation of linked-list stack manipulation routines.

With these routines it is straightforward to implement `LSTINI`, `STADD`, and `LSTEND` for use in `READ`. These routines are shown in listing 4.

```
* set up the stack for ISTADD.
* first stack a NIL, then a pointer to the NIL.
LSTINI LDX ZERO  stack a NIL form
       JSR PUSHX
       LDX STKPTR stack a pointer to the NIL
       DEX        as though it were the CDR of a cell.
       DEX
       JMP PUSHX  push it and return.
* add form in X-reg to list pointed to by value on top of stack
* clobbers X-reg, A-reg, and B-reg.
LSTADD JSR  GETCEL get a dotted pair and fill in the CAR
       STX  CELPTR save address of new cell
       JSR  TOPX   retrieve pointer from top of stack
       LDAA CELPTR link new cell onto list
       STAA CDR,X
       LDAB CELPTR+1
       STAB CDR+1, X
       LDX STKPTR  updated list pointer
       STAA CAR,X  (which is top of stack)
       STAB CAR+1, X
       RTS         and return.
* pop off list end pointer
* return pointer to first dotted pair of list in X-reg
LSTEND: JSR POPX pop off and ignore list end pointer
        JSR POPX pop off and return list begin pointer.
```
Listing 4: `LSTINI`, `LSTADD`, and `LSTEND` build up a linked list of dotted pairs using two pointers on a stack, one to the first dolled pair, one to the dotted pair at the current end of the linked list.

The primitive function `RATOM` turns out to be the real workhorse of `READ`. It is stuck with the job of accepting characters one at a time from the terminal, and building them up into an atom. `RATOM` must distinguish symbolic atoms from numeric atoms, and build up the corresponding forms. Atoms are in general separated by spaces, tabs, or carriage returns. However a few special characters always form single-character atoms when encountered (eg: `"("` and `")"`) without any separator characters necessary.

In the author's LISP system `RATOM` is relatively sophisticated, allowing for atoms with spaces in their names if they are quoted (`"..."`). Also the single quote character (`"'"`) is given special significance, as are `"["` and `"]"`. However a simpler `RATOM` is quite enough for an initial implementation. To make this exposition simpler, only single digit numeric atoms will be allowed. Certainly in an eventual implementation, multidigit numeric atoms, optionally preceded by a minus sign would be accepted.

In this `RATOM`, the characters are copied into an area set aside to hold the names of atoms as they are input. A null character (ASCII code zero) is used to terminate the name, when a separator or special character is encountered. If the name is entirely numeric, then the atom is a numeric atom, and the form is simply the value of the number, with the high order bit set, and one other bit left zero for use in the garbage collector. Otherwise the atom is a symbolic atom, and a scan is made of the `OBLIST` for a pre-existing atom with the same name. If one is found, the characters just typed in are thrown away and a form specifying the pre-existing atom is returned. If the atom is a new one, a 4 byte cell is allocated (using `GETCEL` defined in listing 4) and a pointer to the new atom is added to the `OBLIST`. A form specifying the new atom is returned.
The M6800 assembly language code for this is in listing 5.

### PRINT Function
`PRINT` is the second major recursive function comprising the LISP interpreter. It takes a single form as argument, and types the value as a fully parenthesized LISP expression. `PRINT` simply calls the more primitive function `PATOM` when it is given an atom to type. Otherwise, `PRINT` types a left parenthesis, calls itself recursively to type out the elements of the list, and then types a right parenthesis. In any case, `PRINT` always types out a carriage-return/line-feed at the end. This can be coded as in listing 6.

In the LISP routines, the special function `PROGN` is used. `PROGN` simply evaluates all of its arguments in sequence, and then returns the value of the last one as the value of the entire `PROGN`. The two functions `ATOM` and `DTPR` are used to test the type of a LISP object. `ATOM` returns `T` if the argument evaluates to an atom - symbolic, numeric, or `NIL`. Otherwise `ATOM` returns `NIL`. `DTPR` is the exact opposite. It returns `T` if the argument evaluates to a dotted pair, and returns `NIL` otherwise. Such functions which return either `T` or `NIL` are called "predicates" in LISP in analogy with predicates as used in symbolic logic. Such functions in other languages are called _Boolean_ functions.

Nowhere in the routines for `PRINT`, nor for that matter in the routines given earlier for `READ`, is the allowance made for the input or output of list structures which require the use of "dot" notation. A structure like `(A B C. D)` could not be input, and the above `PRINT` routines would type it out as `(A B C)`, simply assuming that the atom which ended the linked list was `NIL`. It turns out that the changes necessary to implement dot notation are quite straightforward. For example, to add it to the LISP version of `PRINT`, only the routine `PRINL` need be rewritten, as follows:
```
(DEF PRINL (LAMBDA (L)
  (COND
    ((DTPR L) (PROGN
      (PATOM SPACE)
      (PRINR (CAR L))
      (PRINL (CDR L))
    ))
    ((EQ L NIL) NIL)
    (T (PROGN
      (PATOM SPACE)
      (PATOM DOT)
      (PATOM SPACE)
      (PATOM L)
    ))
  )
))
```

A corresponding change could be made to the assembly language routines.

As with the primitive function `RATOM`, the function `PATOM` turns out to be more difficult to implement than the recursive `PRINT`. `PATOM` must distinguish between symbolic atoms, numeric atoms, and `NIL`, and act accordingly. With symbolic atoms, `PATOM` simply types the null-terminated name of the atom. With numeric atoms, `PATOM` must convert back from the internal representation of the numeric value, to the string of ASCII characters which represent the number. With `NIL`, `PATOM` simply types `'NIL'`. Listing 7 is a simplified version of `PATOM` with numeric atoms of only a single digit.

### EVAL Function
The `EVAL` function is the heart of the LISP interpreter. `EVAL` accepts one form as an argument, and evaluates it according to the LISP convention: the value of `NIL` is `NIL`, the value of a numeric atom is itself, the value of a symbolic atom is the form associated with the atom, and the value of a list is determined by applying the function specified by the `CAR` of the list to the list of arguments which make up the `CDR` of the list.

In most LISP systems at least two distinct kinds of functions exist, `SUBR`s and `LAMBDA`s. `SUBR`s are the built-in functions of the LISP system, written in machine code (like `CAR`, `CDR`, `PATOM`). `LAMBDA`s are the user-defined functions, defined like `(DEF GCD (LAMBDA (X Y) ...))`. The effect of such a `DEF` is simply to define the list `(LAMBDA (X Y) ...)` as the value associated with the atom `GCD`.

The type of object used to specify a `SUBR` function varies among LISP systems. Frequently a new type of object is defined, called `CODE`, distinct from atoms and dotted pairs. A second alternative is to treat `SUBR`s like a funny kind of atom. The author's LISP system treats the bytes which make up the machine code of the `SUBR` like the print name of an atom. The `SUBR` is then specified by a dotted pair, with the `CAR` being the atom `"SUBR"` to identify the type of function, and the `CDR` being this atom with the funny print name. In fact the print name is prefixed with a special string which is unlikely to occur in a normal atom's print name, and hence `PATOM` could detect that the print name was not typeable, and simply type, say, `"!"` instead. In addition `EVAL` can check for the presence of this special string at the beginning of the print name to avoid treating a normal atom's print name as machine code. This method for specifying `SUBR`s avoids introducing an additional type, but the added complication in `PATOM` and `EVAL`, may rule out the method in some implementations.

When `EVAL` is given a list to evaluate, it first evaluates the `CAR` of the list (recursively). The evaluation of the `CAR` should be either a `LAMBDA` expression, or a `SUBR` expression. If the evaluation of the `CAR` is an atom, or a list not headed by `LAMBDA` or `SUBR`, then `EVAL` stops, and indicates an error to the user.

If the `CAR` of the list gives a `LAMBDA` expression, the arguments to the function call are evaluated one at a time and saved on a list. The value associated with the "formal" arguments of the `LAMBDA` expression (eg: `X` and `Y` to the `GCD` routine given earlier) are saved on the stack. These formal arguments are then set one at a time to have the value of the corresponding actual arguments to the function (which were evaluated already). Finally, the
"body" of the `LAMBDA` expression is evaluated, with the formal arguments now holding their new values. The result of evaluating the body is the result of the original function call. As a last step, `EVAL` restores the original values of the formal arguments.

Following the details of evaluation of such a function call is very difficult at first. The sequence of these steps is critical: evaluate actual arguments, save old values of formal arguments, set new values of formal arguments, evaluate body of `LAMBDA`, restore old values of formals. With any other sequence there is a chance that changes to the formal arguments of this function might interfere undesirably with the values of atoms in the calling routine's environment. These formal arguments are supposed to be strictly "local," that is, the choice of a name for a formal argument should be a strictly local decision, having no impact on variables with the same name in calling routines. Observing these rules allows LISP functions to be freely recursive. As the above examples of routines demonstrate, this recursion is in fact heavily used in LISP programming.

The steps in applying a `SUBR` function are simpler, because there are no formal arguments to worry about. `EVAL` simply evaluates the arguments to the `SUBR`, and passes them as a list to the machine code subroutine. `EVAL` expects the result of the `SUBR` to be left in register `X` when the subroutine returns.

This much of `EVAL` can be implemented on the M6800 as in listing 8.

The routines `EVLALS`, `POPFRE`, `EVLNSV`, `EVIRSO`, and `EVLRST` have not been included in listing 8 for brevity's sake. They are all relatively straightforward routines, making heavy use of `GETCEL`, `PUSHX`, `POPX`, and `FRECEL` to build up and then release the lists of saved values.

Two additional types of LISP functions, normally recognized by an `EVAL` function, are called `NLAMBDA`s and `NSUBR`s (or `FSUBR`s, or `FEXPR`s if you prefer). These types of functions take their argument lists un-`EVAL`ed. `NSUBR`s are simply passed the `CDR` of the original function call list, instead of a list of evaluated arguments. Similarly, `NLAMBDA`s are provided with only a single argument, the list of unevaluated arguments. Without `NSUBR`s it is necessary for `EVAL` to recognize functions like `COND` as special cases, so that their argument list is not immediately evaluated. `NSUBR`s are specified in the same way as `SUBR`s, with the atom `"NSUBR"` replacing `"SUBR"` in the `CAR` of the dotted pair. `PRINT` will type out `NSUBR`s as `"NSUBR !)"` 

`NLAMBDA`s are very useful tor creating elaborate user-defined functions which take argument lists that are as or more complicated than `COND`. `LAMBDA`s are necessary anytime the number of arguments is variable, or some of the arguments are wanted unevaluated.

To incorporate `NLAMBDA`s and `NSUBR`s in the above `EVAL` routines, two additional checks must be added immediately prior to `EVLERR`:

```
 .
 .
 .
BEQ EVLLAM ...
CPX NSUBAT NSBUR?
BEQ EVLNSU yes, go call machine code subroutine
CPX NAMAT  NLAMBA?
BEQ EVENLA yes, pass list of args as single argument
* illegal exp...
EVLERR
 .
 .
 .
```

and the additional routines `EVLNSU` and `EVLNLA` must be included. Both of these routines are simpler than the corresponding routines `EVLSUB` and `EVLLAM`.

To make `EVAL` useful, some number of built-in `SUBR`s and `NSUBR`s must be written. The number of such built-in primitives can be kept quite small in LISP if they are chosen carefully. Most routines can be implemented as user functions if a few primitives exist. The primitives will certainly include `PATOM`, `RATOM`, `EVAL`, `CAR`, `CDR`, `CONS`, `COND`, `SET`, `ADD`, `SUB`, `EQ`, `GREATER`, `ATOM`, and `NUMBER`. All but `SET` and `NUMBER` have been used in the LISP function listings. `SET` is the primitive LISP assignment function. `SET` takes an atom and a value, and sets the value associated with the given atom to be the given value. `NUMBER` is a predicate function like `ATOM`, and simply returns `T` when its argument is a numeric atom. Listing 9 is an example of one of these primitives, the `SUBR EQ`.

Notice that the `SUBR`s and `NSUBR`s will start with the preface string (hex 21, 00 is used in this system). The argument list is always pointed to by `ALP`. Also notice that the `SUBR` may not assume that the proper number of arguments were supplied. The general rule is to treat unspecified arguments as though they were `NIL`. In `EQ` above, this gives some rather strange behavior, where simply `(EQ)` will always return `T`. It still remains for the implementor to initialize the atom `EQ` to point to a dotted pair, `(SUBR. funny-atom)`, with the print name of the funny atom set to point to the code at `EQSBR` as shown in listing 9. The final section of this article goes over some of the problems involved with this kind of initialization.

### Garbage Collector
A _garbage collector_ eventually becomes essential in any LISP system. It is possible to create dotted pairs that are no longer accessible to a LISP program by any path. This happens, for example, if a function like `REPLACE` is called and then the value returned simply `PRINT`ed but not saved as a LISP atom. This cannot go on for long before all of the free space is used up with dotted pairs. The garbage collector's job is to find all of the dotted pairs.

The various algorithms for locating such jetsom of the LISP function evaluation process are all quite intricate. The basic idea is always to trace systematically down every list structure to its component atoms, marking every dotted pair encountered along the way. If a dotted pair is encountered which is already marked, then that branch of the list structure is assumed to be already fully traced. The garbage collector then makes a sequential scan of all of memory space occupied by dotted pairs, and links together all unmarked dotted pairs onto a special list, the free list. During the scan, the marked dotted pairs are simply skipped over, because they are assumed to still be a part of some useful list structure. When a marked dotted pair is skipped over, its mark is also cleared in anticipation of future garbage collections, when it might no longer be so lucky.

The difficulty with this trace and collect algorithm is that each dotted pair points to possibly two more dotted pairs, so during the tracing phase the garbage collector must eventually follow both paths. What this means is that a second indication must be made on each dotted pair, indicating that the garbage collector is now busy tracing the `CAR` of this dotted pair, and will be retuming later to trace the `CDR` of the dotted pair.

During the tracing phase, the garbage collector might very well be thought of as an ant determined to visit every branch of a tree. It goes out to the tip of each branch, but as it returns it must remember whether it has already traversed the other paths going out from each branching point. Even this analogy underrepresents the difficulty of a garbage collector, because the ant can simply turn around when it reaches the tip of a branch, but the garbage collector would normally have no clue as to how to climb back toward the root of a list structure once it gets out on a distant dotted pair.

The solution to the garbage collector's problem is to either reverse all the pointers in the list structure as it forays out to the terminating atoms and then reset the pointers on the way back in, or to keep a list of all dotted pairs which still require that their `CDR`s be traced. The first solution is like stringing a spool of thread behind you as you venture into an unexplored cave, following the thread back toward the mouth of the cave when you reach a dead end. Of course the same danger exists; that the delicate thread leading you back to the starting point might get tangled or broken.

The second solution is simpler, but suffers from the grave problem that it requires room to store the list of partially visited dotted pairs, and garbage collectors tend to be called upon at times when there is no more room to spare. In fact, the list of partially visited pairs need get no longer than the maximum "depth" of any list structure in the system, so that by setting aside a small portion of memory reserved for the use of the garbage collector's list, the implementor can get by with coding a much simpler tracing algorithm.

The author's system uses the pointer reversal method, and he will testify to the unlimited number of obscure problems which can appear during the debugging phase of its implementation.

It should be clear now why it was important to leave one bit in each form, and hence two bits per dotted pair, free for the use of the garbage collector. The bit in the `CAR` form can be used to indicate that the dotted pair has been visited once, and the bit in the `CDR` can be used to indicate that both paths from the dotted pair have been traced. These bits are only used during garbage collection, but because the garbage collector may be called at any time when `GETCEL` finds that there are no more 4 byte cells on the free list it may, in fact, run at almost any moment.

Because of this unpredictability, a LISP system with a garbage collector must be coded "defensively," jealously protecting any dotted pair allocated but not yet added to some accessible list structure. The machine code routines given in the listings do not all adhere to this rule. The reason for ignoring the garbage collector in the development thus far was simply to keep the design of the routines simple and relatively intuitive.

If the reader intends to include a garbage collector in an implementation of a LISP interpreter, more care must be taken. For example, two versions of the routine `PUSHX` would be defined. normal `PUSHX` and `PROPSH` (protected push). The `PROPSH` would be used when the 16 bit value being pushed on the stack pointed to list structure which might not be accessible in any other way, and hence might get collected in the next garbage collection scan. `PROPSH` avoids this danger by marking the cell used to store the saved value so that the garbage collector will know to trace this form and its descendents.

### Initialization
It is ironic, but somehow appropriate, that the section on initialization comes at the end of this article. Frequently it is in fact one of the last things an implementor thinks about. That is probably because initialization is one of the biggest difficulties facing the implementor of any language: assembler, interpreter, or compiler. By initialization is meant the inevitably awkward methods of getting the symbol tables, or the `OBLIST` in LISP preloaded with the names which are to be built-in to the system. Most of the routines written to enter symbols into symbol tables, or to add new atoms to the `OBLIST`, are all oriented toward names entered by the user of the language processor. The initialization phase of the system becomes quite complicated because of this orientation. The methods finally chosen are, in general, tedious, requiring a lot of special preparation by the writer of the intialization routine.

The best way to avoid these initialization difficulties is to spend a little extra effort in designing a few nice routines for iaking information out of tables which are convenient for the implementor to set up and modify, and let these routines do the intricate bit-twiddling work necessary to get the objects in shape for the symbol table, or the `OBLIST`.

In the author's LISP initialization module are routines to build up dotted pairs in the form required for `SUBR`s and `NSUBRs`, and routines to allocate 4 byte cells for built-in atoms. The atom initialization routines are given the address of a contiguous table of null-terminated ASCII names, each followed by the address of a memory cell where the form specifying the new atom should be stored. This is where the symbols like `TATOM`, `SUBRAT`, `LAMBAT`, etc came from. They reter to memory locations in the base page of the M6800 (0 thru 255), where the forms specifying the atom `T`, `SUBR`, and `LAMBDA`, etc, are stored. The table to initialize these atoms was simply:

```
ATMTAB FCC 'T'
       FCB 0
       FDB TATOM
       FCC 'SUBR'
       FCB 0
       FDB SUBRAT
       FCC 'LAMBDA'
       FCB 0
       FDB LAMBAT
       FCC ...
        .
        .
        .
       FDB ...
       FCB 0 null-name terminates table
```

Although writing the special initialization routines was initially time-consuming, it was more than compensated for by the ease of adding more built-in atoms as the system grew.

### Conclusion
We have traced through the implementation of a LISP interpreter and looked at a specific example for the M6800 processor. For further information on the garbage collecting routines and a complete listing of the interpreter, see listing 10.