# N3CoreLogic

Before running the program, first compile the attribute grammar using uuag:

uuagc -dcfswH attGr.ag

In case you don't have uuag installed, we also added the haskell file produced.

You can then use your haskell compiler to compile the file attGr.hs

In order to test which files of a folder are interpreted differently by cwm and EYE, run:

compareAll "pathToYourFolder" "output.txt"

The generated output file is a table containing the names of the files in your folder and in the colum cwm/eye 
the information whether the file has the same interpretation in cwm and EYE (1) or not (0). The columns next to this column 
compare EYE and Cwm to a reference interpretation where the existentials are interpreted differently 
(according to an older version of EYE).
The last column shows the depth of the deepest nested universal variable. If this number is bigger than 1, the interpretations 
of Cwm and EYE differ.


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
 

