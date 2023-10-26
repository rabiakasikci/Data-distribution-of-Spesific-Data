number_of_game <- list("450","10000")
par(mfrow = c(2, 1))

for (list_num in 1:2) { 

size=as.numeric(number_of_game[list_num])

one_dice=rbinom(size,4,1/6)
two_dice=rbinom(size,24,1/36)


gain_one_dice=matrix(1, nrow=1, ncol=size)
gain_two_dice=matrix(1, nrow=1, ncol=size)
for (i in 1:size) { 
  
  if (two_dice[i]>0) { 
    gain_two_dice[i]=1 } 
  else { gain_two_dice[i]=0 }}

for (i in 1:size) { 
  
  if (one_dice[i]>0) { 
    gain_one_dice[i]=1 } 
  else { gain_one_dice[i]=0 }}

cum_oyun_one=matrix(1, nrow=1, ncol=size)
cum_oyun_two=matrix(1, nrow=1, ncol=size)
x_label=matrix(1:size, nrow=1, ncol=size)

for (i in 1:size) { 
  size1=as.numeric(gain_one_dice)
  size2=as.numeric(gain_two_dice)
  cum_oyun_one=cumsum(size1)
  cum_oyun_two=cumsum(size2)
  
  
}
print(cum_oyun_one)

probability_one=matrix(1, nrow=1, ncol=size)
probability_two=matrix(1, nrow=1, ncol=size)

for (i in 1:size) { 

  probability_one[i]=cum_oyun_one[i]/x_label[i]
  probability_two[i]=cum_oyun_two[i]/x_label[i]
}
print(probability_one)

plot(x_label,probability_one
     ,type="l"
     , ylim=c(0,1)
     , xlim=c(0,size)
     , ylab=expression(paste("Proportion of wins"))
     , xlab=expression(paste("Number of games"))
     
     , col = c("#3399FF")
     , lwd=1
     
)
lines(x_label,probability_two, lwd=3, col="#CC0066")
abline(h = 0.5, 
       col = c("black"),
       lwd = 1,
       lty = 2:3)
legend( x = "topright",
        legend = c("One dice", "Two dice"),
        col = c("#3399FF","#CC0066"), lwd=2,
        pch=c(NA, NA), cex = 0.5 )

}