###Function to summarize AGB plot and map per bin


Accuracy <- function(df, intervals){
  
  #assign AGB bins
  if (intervals == 7){
    bins <- c(-1,50,100,150,200,250,300,Inf) #7 intervals
    bins.str <-c('0-50','050-100','100-150','150-200','200-250','250-300', '300_above')
  }
  if (intervals == 6){
    bins <- c(-1,100,150,200,250,300,Inf) #6 intervals
    bins.str <-c('0-100','100-150','150-200','200-250','250-300', '300_above')
  }
    
    #bins for Aussie
    if (nrow(subset(df, plotAGB_10 > 3000)) > 1){ #if there are large AGB rows
      bins <- c(-1,100,150,200,250,300,600,Inf)
      bins.str <-c('0-100','100-150','150-200', '200-250','250-300','300-600', '600_above')
    }
  
  #assign grouping of AGB values for plot and map separately per bin
  grp1 <- transform(df, group=cut(plotAGB_10,  breaks=bins))

  #aggregate the mean AGB of bins
  agg.plot <- ddply(grp1, .(group), summarise, plotAGB_10=mean(plotAGB_10), .drop=F) 
  agg.map <- ddply(grp1, .(group), summarise, mapAGB=mean(mapAGB, na.rm=T), .drop=F)
                                                                  #drop is important, if not set bins will move upwards 
                                                                  #if there's an NA bin
  
  ##calculate accuracy metrics -- assures values derived are from PLOT BINS
 # grp2 <- cbind(grp1,grp2)
  grp2 <- grp1[,c(1,3,6)] #retains -- plot agb,    plot bin grouping,    map agb
  
  #rmse per mean agb bin
  rmse <- grp2 %>% 
    group_by(group) %>%
    summarise(val=rmse(plotAGB_10, mapAGB))
  
  #relative rmse per mean agb bin
  div <- grp2 %>% 
    group_by(group) %>%
    summarise(val= mean(plotAGB_10))
  
  #error (residual) SD per mean agb bin
  sde <- grp2 %>% 
    group_by(group) %>%
    summarise(val= sd(plotAGB_10-mapAGB))
  
  
  
  len <- length(bins.str) #row control
  agg.plot <- agg.plot[c(1:len),]
  agg.map <- agg.map[c(1:len),]
  
  #join accuracy metrics with original table
  df.new <- data.frame(agg.plot, agg.map)
  df.new <- left_join(df.new, rmse, by = c('group'='group'))
  df.new <- left_join(df.new, div, by = c('group'='group'))
  df.new <- left_join(df.new, sde, by = c('group'='group'))
  
  df.new <- df.new[,-c(1,3)]
  
  #add plot tally
  plot.count <- grp2 %>% 
    group_by(group) %>%
    tally()
  plot.count <- plot.count [c(1:len),]
  
  #combine all
  df.new <- data.frame(plot.count, df.new)
  names(df.new) <- c('bins', 'plot_count', 'plot', 'map', 'rmse', 'rrmse', 'sd_err')
  df.new$rrmse <- df.new$rmse / df.new$rrmse * 100
  df.new$bins <- bins.str
  
  # add last row for totals
  col1 <- 'total'
  col2 <- sum(df.new$plot_count, na.rm=T)
  col3 <- mean(df$plotAGB_10)
  col4 <- mean(df$mapAGB)
  col5 <- rmse(df$plotAGB_10, df$mapAGB)
  col6 <- rmse(df$plotAGB_10, df$mapAGB) / mean(df$plotAGB_10) * 100
  col7 <- sd(df$plotAGB_10 - df$mapAGB , na.rm=T)
  lastrow <- data.frame(col1,col2,col3,col4,col5,col6,col7)
  names(lastrow) <- names(df.new)
  df.new <- rbind(df.new,lastrow)
  
  #round df into 2 decimals
  round_df <- function(x, digits) {
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  df.new <- round_df(df.new, 2)
  
  return(df.new)
}
