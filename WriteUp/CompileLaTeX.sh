#!/bin/bash
# Author: Oenone Scott, ojs19@imperial.ac.uk
# Script: CompileLaTeX.sh
# Desc:   Sript to compile a LaTeX '.tex' file to a pdf, with a bibliography
# Arguments: 1 - tex file
# Date: 1 October 2019



pdflatex --interaction=batchmode OScott_Thesis.tex # The following commands compile the .tex doc and add the bibliography
pdflatex --interaction=batchmode OScott_Thesis.tex
biber thesisRef
pdflatex --interaction=batchmode OScott_Thesis.tex
pdflatex --interaction=batchmode OScott_Thesis.tex


