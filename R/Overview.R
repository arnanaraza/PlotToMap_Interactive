### FUNCTION TO MAKE OVERVIEW PIE GRAPHS FROM PLOT DATA

FezPie <- function(df) {
  #melt
  melted <- df %>% 
    group_by(FAO.ecozone) %>%
    tally()
  melted <- as.data.frame(melted)
  melted$name <- paste0(melted$FAO.ecozone, '_', round((melted$n/sum(melted$n)) * 10), 2, '%')

  #summarize per unique FEZ and year top 10
  summ <- melted[order(melted$n, decreasing = T),]
  #summ <- summ[c(1:10),]

  #plot
  pie(summ$n, labels=summ$name,  main=paste('FEZ breakdown'))
  
  return(summ)
}

YearPie <- function(df) {
  #melt
  melted <- df %>% 
    group_by(AVG_YEAR) %>%
    tally()
  melted <- as.data.frame(melted)
  melted$name <- paste0(melted$AVG_YEAR, '_', round((melted$n/sum(melted$n)) * 10), 2, '%')

  #summarize per unique FEZ and year top 10
  summ <- melted[order(melted$n, decreasing = T),]
#  summ <- summ[c(1:10),]

  #plot
  pie(summ$n, labels=summ$name,  main=paste('Plot year breakdown'))
  
  return(summ)
}

