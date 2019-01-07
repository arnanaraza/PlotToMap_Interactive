# PlotToMap
Biomass validation app via Shiny


# Global variables
dataDir - data directory consisting files like growth rates 
scriptsDir - directory of scripts
outDir - results directory
agbTilesDir - location of GlobBiomass tiles
treeCoverDir - location of Hansen tree cover tiles
SRS - default coordinates (WGS 1984)
forestTHs - threshold for forests

# Features
1. Overview of plots per year and per eco-zone (if a global dataset)
2. Temporal fix effect on histogram and change table
3. Validation results (binned graphs and accuracy table)
4. Comparison results highlighting effect of temporal fix and/or forest scaling/aggregation

# Errors encountered:
Error in if: argument is of length zero - if statement condition is NULL
No or wrong access to either Hansen tree cover or AGB tiles resulting into NULL map and/or weighted plot AGB values. 
