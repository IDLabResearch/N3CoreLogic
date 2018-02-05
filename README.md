# N3CoreLogic

Our program is written in haskell, to run it, make sure that you have a haskell compiler installed (e.g. the Glasgow haskell grammar compiler https://www.haskell.org/ghc/).
To install packages, it is also handy to use cabal (https://www.haskell.org/cabal/). 



Our attribute grammar is written using the Utrecht University Attribute Grammar Compiler (see https://hackage.haskell.org/package/uuagc and http://foswiki.cs.uu.nl/foswiki/HUT/AttributeGrammarSystem).
To install it follow the instructions from the corresponding websites and then run:

uuagc -dcfswH attGr.ag

This produces the file: 

attGr.hs

In case you don't want to install uuag or have problems with the installation, we also added this file to our repository.


To be able to run the file make sure the packages text, parsec,  parsec-numbers are installed (you can install it by using cabal, e.g.: cabal install text).

You can then use your haskell compiler to compile the file.

Therefore:
1. start ghci in your console (>ghci)
2. load the file attGr.hs (:l attGr.hs)

To test which files of a folder are interpreted differently by cwm and EYE, run:

      compareAll "pathToYourFolder" "output.txt"

The generated output file is a table containing the names of the files in your folder which have an *.n3-ending and contain universal variables. In the colum cwm/eye you find
the information whether the file has the same interpretation in cwm and EYE (1) or not (0). The columns next to this column 
compare EYE and Cwm to a reference interpretation where the existentials are interpreted differently 
(according to an older version of EYE).

the resulting document also shows information about the different documents, in particular:
1. builtin: does the file contain knows builtins (0=no, 1=yes)
2. proof: is the file a proof (0=no, 1=yes)
3. nested: how deeply nested is the deepest nested formula expression? This function counts the nesting of curly brackets {}.
4. depth: how deeply nested do we find the deepest variable (note that some variables occurring in brackets are already scoped by another variables, look into the paper for details).
5. reason: the reason for a different interpretation between EYE and Cwm (no problem = 0,  nesting = 1, proof = 2, builtin = 3)


The program also shows the distribution of the different cases.

Additional functions:

To get the parse tree of a formula (in a file) use:

parseToCoreTree "pathToFile" for the Cwm tree
eyeToCore "pathToFile" for the EYE tree

To get a (hopefully) more readable version use
makeTree "file" "cwm"
or
makeTree "file" "eye"


The function

deepvars "file" -> gives the names of the deepest nested variables in a file
compareU "file" -> shows the first difference found between EYE's and Cwm's interpretation for the formula in the file.
 

