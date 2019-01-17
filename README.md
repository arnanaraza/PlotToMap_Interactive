# PlotToMap
Biomass validation app via Shiny


# Global variables
SRS - default coordinates (WGS 1984)
forestTHs - threshold for forests (10%)

# Local variables (user-end)
dataDir - data directory consisting files like growth rates 
agbDir -  folder directory of AGB tiles
treeCoverDir - folder location of Hansen Tree Cover tiles
scriptsDir - directory of extracted scripts *currently sourcing, but coulds be included at app.R
outDir - results directory

# Widgets
1. Upload plot data, table appears
2. Select AGB tiles directory
3. Select Tree Cover tiles directory
4. Select if local or global data
5. If global, select overview field (year or eco-zone) for a pie chart
6. If global, select if validation is per continent or biome
7. Select sub-scale, name of continent or type of biome
8. Apply temporal fix with forest scaling?
9. Apply aggregation with forest scaling?
10. Show effects of one or both?

# Features
1. Overview of plots per year and per eco-zone (if a global dataset)
2. Temporal fix effect on histogram and change table
3. Validation results (binned graphs and accuracy table)
4. Comparison results highlighting effect of temporal fix and/or forest scaling/aggregation

# Output saving
-Tables are automatically saved
-Figures aren't, manual saving is possible

# Errors encountered:
Error in if: argument is of length zero - if statement condition is NULL
No or wrong access to either Hansen tree cover or AGB tiles resulting into NULL map and/or weighted plot AGB values. 

# To be added:
1. Progress bar
2. Image download button
3. Nicer window format
4. Transparency and data-use agreement section

# To be tested:
1. Use of local forest cover dataset
2
