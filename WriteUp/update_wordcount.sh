#!bin/env/bash 

# this script counts the words in the finalised pdf and saves them down 
texcount -1 -sum ../WriteUp/OScott_Thesis.tex > thesisWordcount.sum 