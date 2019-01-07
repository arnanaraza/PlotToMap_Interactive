## FUNCTION TO COMPARE TWO BINNED PLOTS

ThreePlots <- function(x, y, x1, y1, x2,y2, caption = "", fname="", title=''){
  intervals <- c(0:12*25, 7:8*50, Inf)
  
 # col1 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.5)
  #col2 <- rgb(red = 0, green = 1, blue = 0, alpha = 0.5)
  #col3 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
  cols <- gray((1:6)/6)
  
  ct <- findInterval(x, intervals, left.open=TRUE)
  r <- c(0,625)
  ux <- aggregate(x, list(ct), FUN=mean, na.rm=T)[,2]
  uy <- aggregate(y, list(ct), FUN=mean, na.rm=T)[,2]
  nu <- aggregate(y, list(ct), FUN=function(x) length(na.omit(x)))[,2]
  q1 <- aggregate(y, list(ct), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q3 <- aggregate(y, list(ct), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx <- (2:7*0.25)[findInterval(nu, c(0,10,20,50,100,200))]

  # are there extremely high values on the x-axis?
  if(ux[length(ux)] > 400){
    ux[length(ux)] <- 500
    r <- c(0, 525)
  }
  
##  setwd(outDir)
  
#  png(fname, 1000, 1000, res=150)
  
  plot(ux,uy, las=1, main=caption, pch=16, cex=cx,col= cols[1],
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux, q1, ux, q3, length=0, col = cols[1])
  
  
  if (title == 'comp'){
    t <- "#/bin, default"
  }else if (title == 'inter'){
    t <- "#/bin, Baccini"}
  else{t <- "#/bin"}
  
  legend("topleft",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, col= cols[1],
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  
  ############ plot 2##############
  
  ct1 <- findInterval(x1, intervals, left.open=TRUE)
  ux1 <- aggregate(x1, list(ct1), FUN=mean, na.rm=T)[,2]
  uy1 <- aggregate(y1, list(ct1), FUN=mean, na.rm=T)[,2]
  nu1 <- aggregate(y1, list(ct1), FUN=function(x1) length(na.omit(x1)))[,2]
  q11 <- aggregate(y1, list(ct1), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q33 <- aggregate(y1, list(ct1), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx1 <- (2:7*0.25)[findInterval(nu1, c(0,10,20,50,100,200))]
  
  
  # are there extremely high values on the x-axis?
  if(ux1[length(ux1)] > 800){
    ux1[length(ux1)] <- 800
  }
  
  par(new=TRUE)
  cols <- gray((1:6)/6, alpha = 0.4)
  
  plot(ux1,uy1, las=1, main=caption, pch=16,  cex=cx1, col= cols[2],
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux1, q11, ux1, q33, length=0, col= cols[2])
  
  
  abline(0,1, lty=2, lwd=1)
  
  if (title == 'comp'){
    t <- "#/bin, TF"
  }else if (title == 'inter'){
    t <- "#/bin, GEOCARBON"}
  else{t <- "#/bin"}
  

  legend("top",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, col=cols[2],
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  
  
  ######## plot 3###############
  
  
  ct2 <- findInterval(x2, intervals, left.open=TRUE)
  ux2 <- aggregate(x2, list(ct2), FUN=mean, na.rm=T)[,2]
  uy2 <- aggregate(y2, list(ct2), FUN=mean, na.rm=T)[,2]
  nu2 <- aggregate(y2, list(ct2), FUN=function(x2) length(na.omit(x2)))[,2]
  q111 <- aggregate(y2, list(ct2), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q333 <- aggregate(y2, list(ct2), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx2 <- (2:7*0.25)[findInterval(nu2, c(0,10,20,50,100,200))]
  
  
  # are there extremely high values on the x-axis?
  if(ux2[length(ux2)] > 800){
    ux2[length(ux2)] <- 800
  }
  
  par(new=TRUE)
  col3 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
  plot(ux2,uy2, las=1, main=caption, pch=16, cex=cx2, col=col3,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux2, q111, ux2, q333, length=0, col= col3)
  
  
  abline(0,1, lty=2, lwd=1)
  
  if (title == 'comp'){
    t <- "#/bin, agg01, TF"
  }else if (title == 'inter'){
    t <- "#/bin, GlobBiomass"}
    else{t <- "#/bin"}
    
  legend("topright",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, col=col3,
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  
  

  ###### axis fix on high values ############

  
  
  if(ux[length(ux)] == 500){
    axis(1,at=0:5*100, labels=c(0:4*100, ">400"))
    axis.break(breakpos=450)
  }
  else{axis(1,at=0:4*100, labels=c(0:4*100))
  }
  
 # dev.off()
#  setwd(mainDir)
}


