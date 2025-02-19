### Function TO APPLY GROWTH RATES BY SUBSETTING AND CONDITIONS

TempFixed <- function(df, domain, year){
  gr <- read.csv(paste0(dataDir,'/GrowthData.csv'))
  
  #filter eco-region first
  df0 <- filter (df, GEZ == domain) #this part doesn't give NAs already
  
  
  #join growth rate table using 3 variables to assure uniqueness
  df.old <- left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone')) 
  
  
  #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
  below <- subset(df.old, AVG_YEAR < year) #non-NAs
  above <- subset(df.old, AVG_YEAR > year) #non-NAs
  static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets

  
  #apply growth rates (GR1 = primary, GR2 = old secondary, GR3 = young secondary)
  below$AGB_ORIG <- below$AGB_T_HA
  above$AGB_ORIG <- above$AGB_T_HA
  below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2) * (year - below$AVG_YEAR))  
  above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2) * (above$AVG_YEAR -  year))  
  
  below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152, 
                           below$AGB_ORIG + (below$GR1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152, 
                           above$AGB_ORIG - (above$GR1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/GR3 class 
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0, 
                           above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative
  
  #remove extra column of baseline agb
  below <- below[ , !(names(below) %in% 'AGB_ORIG')]
  above <- above[ , !(names(above) %in% 'AGB_ORIG')]
  
  
  #combine all: static and recomputed
  df.new <- rbind(below,above,static)
  
  
  #remove last joined growth rates columns for further row binding 
  remove <- c('GR1', 'GR2', 'GR3')
  df.new <- df.new[ , !(names(df.new) %in% remove)]

  
  return(df.new)
}
