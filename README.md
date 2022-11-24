# Municipal Waste Project for the Philadelphia Office of Sustainability (2022)

This project was completed in August of 2022 for the Philadelphia Office of Sustainability. It aggregates waste data from over 800 municipalities to investigate how certain policy choices might impact waste diversion rates and the amount of waste produced per household. It also combines information collected by interviewing sustainability directors and solid waste professionals in cities around the country. 

The Code and Data folders contain all of the code and data needed to produce figures and values used in the final report. First, run the scripts for each state in any order: FL.R, NC.R, MA.R, RI.R, and ME.R. They draw on data from their accompanying folders within the data folder, and output into the CleanStateData folder. 

Then run the MergingStates script to combine all of the state data into one dataset, placing it in the Files Folder. The final script, AnalysisAndFigures, also outputs into Files. The PDF is the final report written based on the analysis and figures.

When running the R Scripts, make sure you change the working directory so it directs to where you have downloaded the files onto your computer.  For example, setwd("~/downloads/MunicipalWasteProject-OOS/Data/")