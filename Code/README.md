README FOR MRES PROJECT          
LIST OF SCRIPTS 



# STAGE ONE OF PROJECT: LOOKING AT THE DATA



## Sandbox scripts

1. Sandbox/Fishtree_test.R
This script is an exploration of the "fishtree" , "fishbase", and phylogenetic tree r packages, and some of the tree packages

Inputs: 
../../Data/Fishlist_breakdown.CSV # this was a manual list I compiled from the website. don't use!

../../Data/actinopt_full.trees # testing out how to use the phylogenetic tree funcions 



2. explore_JS_file.R
This script 


# Getting data from the worms download
Step one: download the WoRMS database. Will need to ask permission to do this
http://marinespecies.org/aphia.php?p=taxdetails&id=10194

1. WoRMS_subsetforActinopdata.R
This scipt opens the data downloaded from the WoRMS database
it reads it into R, screens it for the correct data (i.e. only the Actiopterygii) and then re-saves down the relevant information

Inputs: 
../Data/WoRMS/taxon.txt
This huge text file is the entire WoRMS database. We need to narrow it down to just the Actinopterygii before we look into it

Output: 
../Data/Full_Actiopterygii_WoRMS_data.csv
This csv contains the WoRMS entries for Actinopterygii (46k entries)

../Data/WoRMS_AcceptedSpecies.Rdata
This is an R dataframe with all of the relevant columns (species name, taxanomic info) on all of the species. 

# Getting data from the Red List 

initially: do a search on the Red List website to get the list of species in the required group
To download the search results, create a red list account: https://www.iucnredlist.org/
Results are 2 CSV files - one has species & taxanomic data ("taxonomy.csv") one has the IUCN scores "assessments.csv")
Save the results of this search as red.csv in the "Data" directory

To combine the two files, and saves into one csv, use the foltlowing: 

1. ConsolidateIUCNdownloads.R

Inputs: 
../Data/taxonomy.csv
../Data/assessments.csv

Outputs: 
../Data/RedListDownload.csv
This contains the combined data


2.. RedListAPIScript.R t
This script calls the Red List API script and gets additional information about a required list of species. 
NB TURNS OUT YOU DON'T NEED THIS SCRIPT - you can actually download the taxanomic data along with the assessments. 
How annoying!

Inputs: 
../Data/red.csv 
Contains species names, IUCN category, IDs, and population trend
Accessed from downloading the results of the 'https://us04web.zoom.us/j/321327965actinopterygii' seach from the IUCN red list 


Outputs: 
../Data/APIScriptOutput.Rdata
This is a R-list. Each list entry is the resuhttps://us04web.zoom.us/j/321327965



# STAGE TWO: RUNNING THE EDGE AND HEDGE CODE 

# Testing it on Acipensuridaet

1. ACIPEN_EDGE_code.R
This script 


Inputs: 
../Data/AcipenTree.Rdata
This Rdata file contains a single Acipensuridae phylgenetic tree

../Data/AcipenseridaeForEDGE.Rdata
This Rdata file contains a list of the Acipensuridae species, the Red List status, and the EDGE scores

../Data/100AcipenTrees.Rdata
This Rdata file contains 100 Acipensuridae phylogenetic trees

t
Outputs: 
../Results/AcipenTreePlot.png/pdf
The plot of the Acipensuridae phylogenetic treeis saved into the results folder (PDF & PNG format)
          

../Results/ED_EDGE_scores_Acipenseridae.csv
CSV containing the ED, GE and EDGE scores for each species


../Results/EDGE_spp_Acipenseridae.csv
Subsection of the list above of all the species that are considered 'EDGE' - species that are greater 
than the median ED and in a threatened RL category (VU, EN, CR)



t


          
AnalysisAllData.R           FunctionsforComparisons.R
ComparisonAllData.R         Habitats.R
ImportData.R
CONTINUEWITHTHISSCRIPT.R    Master_Table.R
ED_EverySpecies.R           
ED_everyspecies_results.R   RedListAPIScript.R
EDGE_croc.R                 RedList_test.R
EDGE_setup_test.R           Sandbox
Example_EDGE_code.R         
ExplorationAllData.R        TEST_EDGE_code.R
 WIP.R









# OTHER SCRIPTS: WRITEUP 




1. OScott_thesis.tex 
Writing up my thesis!

2. update_wordcount.sh 
Bash script to count the words in the compiled pdf and create an output that the compile file can later reference

3. Compile_latex.sh
Bash script to run bieber and compile the latex script of the thesis 

3. FishConsumption.R
This script reads in FAO data and creates a chart for my write-up

