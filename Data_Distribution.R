read_file= read.csv('Data.csv',sep=";" )
new_file = read_file[-1,]

cols_number=ncol(new_file)
par(mfrow = c(4, 3))
calor_list <- list("steelblue1", "#CC0066", "#FF3300" ,"#FF8000", "#FF007F", "#666600","#009900","#FFFF33","#994C00", "#CC0066", "#FF3300" ,"#FF8000")
i=1
for (number in 1:12){
  calor=calor_list[number]
  
  x=new_file[ , c(number)]
  y=new_file[ , c(number+1)]
  mean_data=  mean(as.numeric(y))
  std_data=  sd(as.numeric(y))
  corr_data=  cor(as.numeric(x),as.numeric(y))
  
  plot(x,y
       , ylab=expression(paste("y-axix title"))
       , xlab=expression(paste("x-axix titles"))
       , ylim=c(0,100)
       , xlim=c(0,100)
       , pch=16
       , cex=0.8
       #, typ="l"
       , lty=3
       , lwd=3
       , col = calor_list[[number]]
  )
  #text(20,90, bquote(paste("11111")), family= "sans", cex=1,col = calor_list[[number]])
  text(90,90, bquote(paste("Mean"*'=', .(mean_data))), family= "sans", cex=1,col = calor_list[[number]])
  text(90,70, bquote(paste("Std"*'=', .(std_data))), family= "sans", cex=1,col = calor_list[[number]])
  text(90,50, bquote(paste("Cor"*'=', .(corr_data))), family= "sans", cex=1,col = calor_list[[number]])
  
  
  axis(4, labels=NA, tcl=0.3, lwd=0.9, lwd.ticks=0.5)
  axis(3, labels=NA, tcl=0.3, lwd=0.9, lwd.ticks=0.5)
  
  i=i+1
  
  
}






