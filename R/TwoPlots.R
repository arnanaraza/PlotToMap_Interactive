## FUNCTION TO COMPARE TWO BINNED PLOTS

TwoPlots <- function(x, y, x1, y1, caption = "",fname="", title=""){
  intervals <- c(0:12*25, 7:12*50, Inf)
  
  col1 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.5)
  col2 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
  
  ct <- findInterval(x, intervals, left.open=TRUE)
 # r <- c(0, 1.1 * max(x1, na.rm=T))
 r <- c(0,625)
  ux <- aggregate(x, list(ct), FUN=mean, na.rm=T)[,2]
  uy <- aggregate(y, list(ct), FUN=mean, na.rm=T)[,2]
  nu <- aggregate(y, list(ct), FUN=function(x) length(na.omit(x)))[,2]
  q1 <- aggregate(y, list(ct), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q3 <- aggregate(y, list(ct), FUN=quantile, probs=0.75, na.rm=T)[,2]
  cx <- (2:7*0.25)[findInterval(nu, c(0,10,20,50,100,200))]

  # are there extremely high values on the x-axis?
  if(ux[length(ux)] > 800){
    ux[length(ux)] <- 800
    r <- c(0, 825)
  }
  
  setwd(outDir)
  
   png(fname, 1000, 1000, res=150)
  plot(ux,uy, las=1, main=caption, pch=16, col= col1,cex=cx,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux, q1, ux, q3, length=0, col = col1)
  
  
  ct1 <- findInterval(x1, intervals, left.open=TRUE)
 # r1 <- c(0, 1.1 * max(x1, na.rm=T))
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
  plot(ux1,uy1, las=1, main=caption, pch=16, col= col2, cex=cx1,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux1, q11, ux1, q33, length=0,col= col2)
  
  
  
  abline(0,1, lty=2, lwd=1)
  
  if (title == 'agg'){
    t <- "#/bin, Agg01 pre-TF"
  }else if (title == 'inter'){
    t <- "#/bin, Baccini"
  }else if (title == 'inter1'){
    t <- "#/bin, GEOCARBON"
  }
  else{t <- "#/bin, NonAgg pre-TF"}
  
  legend("topleft",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25,col=col1,
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  par(new=TRUE)
  
  if (title == 'agg'){
    t <- "#/bin, Agg01 post-TF"
  }else if (title == 'inter'){
    t <- "#/bin, GlobBiomass"
  }else if (title == 'inter1'){
    t <- "#/bin, GlobBiomass"
  }else{t <- "#/bin, NonAgg post-TF"}
  
  legend("top",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, col= col2,
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)
  if(ux[length(ux)] == 800){
    axis(1,at=0:4*200, labels=c(0:3*200, ">600"))
    axis.break(breakpos=700)
  }
  else{axis(1,at=0:6*100, labels=c(0:6*100))
}
 dev.off()
#  setwd(main.dir)
  
}
