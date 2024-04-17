# Utilizing biochemical biomarkers to characterize *Mytilus trossulus* response to pollutants in Puget Sound waterways
--- 
### This work is being conducted in collaboration with the Washington Department of Fish & Wildlife. The samples are a subset of mussels from the 2021/22 mussel outplanting for the ongoing 'Mussel Watch' program.   

# Team
Chris Mantegna, UW SAFS, Contact: mantegna@uw.edu\
Mariko Langeness, WDFW\
Molly Shuman-Goodier, WDFW\
Danielle Nordstrom, WDFW\
Steven Roberts, UW SAFS\
Alison Gardell, UW Tacoma 

## Undergraduate Support
Ly Vuthy\
Noah Krebs 


## Project Dates
Mussels from the 2021/22 outplanting season\
Bench work December 2022 - February 2024\
Analysis and writing February 2024 - March 2024

## Kits
1. [Pierce BCA Protein Assay Kits](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/TFS-Assets_LSG_manuals_MAN0011430_Pierce_BCA_Protein_Asy_UG.pdf)
2. [Cayman SOD](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/Cayman_SOD_Assay_Protocol.pdf)
## Protocols 
1. [Dissection](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/Frozen%20mussel%20dissection%20SOP.docx.pdf)
2. BCA- see Pierce protocol above
3. [p450](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/SOP%20Bivalve%20Biomarkers%20P450.docx.pdf)
4. [SOD](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/SOP%20Bivalve%20Biomarkers%20SOD.docx.pdf)
# Repo Contents
## Output
### Exploratory: all visualizations used to clarify the data
### Figures: visualizations that can or will be used in the manuscript
### Miscellaneous: screenshots or snippets of code used in issue pposting
### Tables: raw tables, prior to formatting, that will be used in the manuscript

## Code:
[01-explore](https://rpubs.com/cmantegna/mb01explore): initial explanatory visualizations\
[02-statistics](https://rpubs.com/cmantegna/mb02statistics): initial statistics performed on the dataset\
[03-map](https://rpubs.com/cmantegna/mb03map): Washington state map of biomarker values plotted by coordinates of sites\
[04-spatial](https://rpubs.com/cmantegna/mb04spatial): Geospatial statistical analysis of the biomarkers\

## Data

### Primary analysis file
All work to date utilizing [biomarkerfull.csv](https://github.com/ChrisMantegna/WDFWmussels/blob/main/data/biomarkerfull.csv)\
lat - latitude\
long - longitide\
site_name - name of cage location given to us from WDFW\
site_number - number assigned by our team for labeling ease\
sample - sample number assigned by our tean for dissection labeling\
p450 - final cytochrome p450 activity in (activity/ (mg/protein)). All values post standardization by BCA assay values for each tissue sample.\ 
SOD - final superoxide dimutase activity in (activity/ (mg/protein)). All values post assay and standardization by BCA assay values for each tissue sample.\
weight_initial - full weight of the frozen mussel in (mg) prior to dissection.\
length	- full length of the frozen mussel in (mm) prior to dissection.\
width	- width of the frozen mussel in (mm) prior to dissection.\
height	- full height of the frozen mussel in (mm) prior to dissection.\
weight_final - weight of the shells scraped clean in (mg) post dissection.\
weight_change - the calculation in (mg) of weight_initial  - weight_final.\
condition_factor	- a unitless calculation of the approximate tissue weight/ length. Tissue weight is the weight_change value in this data set.\
avg_thickness	- the average in (mm) of four pre-determined measurements per valve of each sample.\

### Not for analysis
These files are individual files with raw data, not used in analysis but in creating the analysis files
