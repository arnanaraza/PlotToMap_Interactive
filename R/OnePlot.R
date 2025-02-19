## FUNCTION TO GRAPH VALIDATION RESULTS IN BINS

OnePlot <- function(x, y, caption = "", fname=""){
  # aggregate high AGBs first
  x <- ifelse(x > 400, 500, x)
  
  ct <- findInterval(x, c(0:12*25, 7:12*50, Inf), left.open=TRUE)
  ux <- aggregate(x, list(ct), FUN=mean, na.rm=T)[,2]
  uy <- aggregate(y, list(ct), FUN=mean, na.rm=T)[,2]
  nu <- aggregate(y, list(ct), FUN=function(x) length(na.omit(x)))[,2]
  q1 <- aggregate(y, list(ct), FUN=quantile, probs=0.25, na.rm=T)[,2]
  q3 <- aggregate(y, list(ct), FUN=quantile, probs=0.75, na.rm=T)[,2]

  cx <- (2:7*0.25)[findInterval(nu, c(0,10,20,50,100,200))]
  
  # for high AGBs bin
  if(ux[length(ux)] == 500){
    r <- c(0,525)
  }else{
    r <- c(0,400)
  }
  
  plot(ux,uy, las=1, main=caption, pch=16,cex=cx,
       xlab="Mean reference AGB (Mg/ha)", xlim=r,
       ylab="Mapped AGB (Mg/ha)", ylim=r,xaxt="n")
  arrows(ux, q1, ux, q3, length=0)
  

  abline(0,1, lty=2, lwd=1)
  
  par(new=TRUE)
  
  t <- "#/bin"
  legend("topleft",bty="n",pch=16,ncol=2,pt.cex=2:7*0.25, 
         cex=1,c("< 10","10-20","20-50","50-100","100-200","> 200"),
         title=t)

  if(ux[length(ux)] == 500){
    axis(1,at=0:5*100, labels=c(0:4*100, ">400"))
    axis.break(breakpos=450)
  }
  else{axis(1,at=0:4*100, labels=c(0:4*100))
  }

}



