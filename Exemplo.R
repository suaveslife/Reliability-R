#Create a lifetime data
lifetime<- c(4,5,6,4,5,6,7,4,5,6,7,5,5,6,7,8)

#Checking which distribution better fit the data
plot_comp(lifetime,"Exemplo")

#checking the lifetime analisys
plot_rely(lifetime,"Exemplo")

#Finding the reliability on 7 days
Reliability_conf(lifetime,7)

#Finding the days that matches 0.7 of reliability
Reliability_dias(lifetime,0.7)



