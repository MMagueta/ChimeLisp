
# Table of Contents

1.  [About](#orgef03188)
2.  [Features <code>[4/8]</code>](#orge774d1f)
3.  [Examples](#org9b42f70)

<p align="center"><img src="docs/assets/LispColorRed.png" /></p>

> The way the mind functions provides a further proof.  A being can be
> grasped by an act of simple understanding, and in such a case the
> thing which is true is known. But the truth value itself is known only
> by an act of judgment.  Simple understanding, however, precedes an act
> of judgment &#x2013; John Duns Scotus, Concerning Human Knowledge


<a id="orgef03188"></a>

# About

My friends started a book club over Build Your Own Lisp, so I started my very dialect.


<a id="orge774d1f"></a>

# Features <code>[4/8]</code>

-   [-] Target VMs
    -   [-] Target the .NET IL
    -   [ ] Target the [Chime VM](https://github.com/Dr-Nekoma/chime)
-   [X] Functions
-   [X] Variables
-   [X] Applications
-   [-] Expansion
    -   [ ] Macros
    -   [X] Expansion Function
-   [X] Basic Typing
-   [ ] System-F (omega) Type System
-   [ ] REPL
    -   [ ] Compile and replace defined methods


<a id="org9b42f70"></a>

# Examples

    [+ 1.5 2.0]

    [= 1 2]

    "Hello World! 🍬"

    [defun hello [x y]
      [progn
        [println [int->string y]]
        [println x]]]
    [hello "Hello" 1]

    [if [= 10 11]
        [println "They are equal!"]
      [println "They are not equal!"]]

    [[lambda [x] [+ x 1]] 1]

