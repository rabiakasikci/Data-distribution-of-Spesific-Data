deneme_list <- list("10","25","50","100","150","200")
par(mfrow = c(3, 2))
for (list_num in 1:6) { 
  size=as.numeric(deneme_list[list_num])
  b=matrix(1, nrow=1, ncol=size)
  a=runif(size,min=0,max=1)
  heads=0 
  tails=0 
  
  for (i in 1:size) { 
    
    if (a[i]>0.5) { 
      heads=heads+1 
      b[i]=1} 
    else { tails=tails+1
    b[i]=0} }
  
  total_head=cumsum(b)
  pro_coin=matrix(1, nrow=1, ncol=size)
  for (i in 1:size) { 
    pro_coin[i]=total_head[i]/i
 
  }

x_label=matrix(1:size, nrow=1, ncol=size)



plot(x_label,pro_coin
     , ylab=expression(paste("Proportion of Heads"))
     , xlab=expression(paste("Number of Coin Tosses"))
     , ylim=c(0,1)
     , xlim=c(0,size)
     , pch=16
     , cex=0.8
     #, typ="l"
     , lty=3
     , lwd=3
     , col = c("#009900")
)
abline(h = 0.5, 
       col = c("red"),
       lwd = 1,
       lty = 2:3)
text(size/2+size/5,0.8, bquote(paste("Heads"*'=', .(heads))), family= "sans", cex=1)
text(size/2+size/5,0.2, bquote(paste("Tails"*'=', .(tails))), family= "sans", cex=1)

}

