# Utilizing biochemical biomarkers to characterize *Mytilus trossulus* response to pollutants in Puget Sound waterways
--- 
### This work is being conducted in collaboration with the Washington Department of Fish & Wildlife. The samples are a subset of mussels from the 2021/22 mussel outplanting for the ongoing 'Mussel Watch' program.   

# Team
Chris Mantegna, UW SAFS, Contact: mantegna@uw.edu\
Mariko Langeness, WDFW\
Molly Shuman-Goodier, WDFW\
Danielle Nordstrom, WDFW\
Steven Roberts, UW SAFS\
Alison Gardell, lead PI, UW Tacoma 

## Undergraduate Support
Ly Vuthy\
Noah Krebs 


## Project Dates
Mussels from the 2021/22 outplanting season\
Bench work September 2023 - February 2024\
Analysis and writing February 2024 - April 2026

## Kits
1. [Pierce BCA Protein Assay Kits](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/TFS-Assets_LSG_manuals_MAN0011430_Pierce_BCA_Protein_Asy_UG.pdf)
2. [Cayman SOD](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/Cayman_SOD_Assay_Protocol.pdf)

## Protocols 
1. [Dissection](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/Frozen%20mussel%20dissection%20SOP.docx.pdf)
2. BCA- see Pierce protocol above
3. [p450](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/SOP%20Bivalve%20Biomarkers%20P450.docx.pdf)
4. [SOD](https://github.com/ChrisMantegna/WDFWmussels/blob/main/protocol/SOP%20Bivalve%20Biomarkers%20SOD.docx.pdf)

# Repo Contents

## Data
### Folders
**`start_here`**: raw data\
**`cleaned`**: raw data with names and format cleaned up for tidyverse manupulation\
**`interim_df`**: log- transformed data and index creation\
**`plotting`**: interim data where table structure has been manipulated for visualizations\
**`don't_use`**: exactly what it sounds like\

## Code:
`00-data_cleaning`: initial data cleaning.\
`01-01.1-exploratory_stats`: exploratory stats and visualizations.\
`02-ibr_cci_creation`: creating the integrated biomarker response score (IBR) and Contaminant Class Indices (CCI).\
`03-03.2-analyses`: KW comparisons, correlations, and spatial analyses.\
`04-visualizations` and `11-11.1-visualizations`: initial analysis visualizations - not used in end products.\
`12-12.2-visualizations: visualizations to support the manuscript and/or the supplement.\
`13-modeling_biomarkers`: early attempt to fit biomarkers and contaminant concentrations into a non-linear model, not used.\
`99-junkyard`: code remnants for other processes that were not effective for this work, not used.

## Output
`Exploratory`: all visualizations used to clarify the data before transformation.\
`Figures`: visualizations that can or will be used in the manuscript.\
`Man_viz`: visualizations that can or will be used in the manuscript.\
`Spatial Stats`: spatial analysis outputs, tables and plots to be overlayed in GIS.\
`Tables`: significant results tables for KW comparisons, correlations, and spatial stats.

## Images
Bench processing photos.
## Protocols
Protocols in `.pdf` format.
