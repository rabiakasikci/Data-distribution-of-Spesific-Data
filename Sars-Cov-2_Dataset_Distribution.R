data= read.csv('WHO-COVID-19-global-data.csv' )
Morocco= data[data$Country=='Morocco',]

number_of_simulation=1000

mean_data=matrix(1, nrow=10, ncol=1)
varyans_data=matrix(1, nrow=10, ncol=1)
Morocco_death=matrix(1, nrow=10, ncol=7)
poisson_data=matrix(1, nrow=as.numeric(number_of_simulation), ncol=7)
varyans_poisson=matrix(1, nrow=as.numeric(number_of_simulation), ncol=10)


i=1

for (number in seq(167647,167710,7)){
  number=as.numeric(number)
  Morocco_death[i,1:7]=data[number:as.numeric(number+6),7]
  mean_data[i,1]=mean(Morocco_death[i,1:7])
  varyans_data[i,1]=var(Morocco_death[i,1:7])
  
  
  i=i+1
}

for (f in 1:10){
  
  for (t in 1:as.numeric(number_of_simulation)){
    
    #poisson_data[t,1:7]=rpois(n=7,lambda=mean(mean_data[f,1]))
    varyans_poisson[t,f]=var(rpois(n=7,lambda=mean_data[f,1]))
  }
  
}

number_of_lower_varyans=matrix(1, nrow=10, ncol=1)
probability_of_lower_varyans=matrix(1, nrow=10, ncol=1)


for (track in 1:10){
  
  number_of_lower=0
  for (j in 1:as.numeric(number_of_simulation)){
    if (varyans_poisson[j,track]<varyans_data[track,1]) { 
      number_of_lower=number_of_lower+1 
    } 
    else {
      number_of_lower=number_of_lower
    }
  }
  number_of_lower_varyans[track,1]=number_of_lower
  probability_of_lower_varyans[track,1]=(number_of_lower/as.numeric(number_of_simulation))*100
  
  
}


#Change or not 
lower_pro=0
for (i in 1:10){
  if (probability_of_lower_varyans[i,1]<5) {
    lower_pro=lower_pro+1
    
  }
  
  
}
print(lower_pro)


#drawn graph



#Draw Graph


Data_Amerika= data[data$Country=='United States of America',]
Amerika_death=data[270657:271034,7]
Morocco_one_death=data[167801:168178,7]
Amerika_nufus=331.9#milyon
Morocco_nufus=37.08#milyon



Amerika_death_y=Amerika_death*1/Amerika_nufus

Morocco_one_death_y=Morocco_one_death*1/Morocco_nufus

x_label=matrix(1:378, nrow=1, ncol=378)


plot(x_label,Amerika_death_y
     ,type="l"
     , ylim=c(0,15)
     , xlim=c(0,378)
     , ylab=expression(paste("Covid19 deaths per million"))
     , xlab=expression(paste("Days from 28th Dec 2020 to 9th Jan 2022"))
     
     , col = c("#3399FF")
     , lwd=1
     
)
lines(x_label,Morocco_one_death_y, lwd=3, col="#CC0066")
legend( x = "topright",
        legend = c("USA", "Morocco"),
        col = c("#3399FF","#CC0066"), lwd=2,
        pch=c(NA, NA), cex = 0.5 )

