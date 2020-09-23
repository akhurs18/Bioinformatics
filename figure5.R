library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
library(ggplot2)
library(reshape2)
library(plyr)
library(plotly)
library(usmap)

Deaths = fread(file = "US_MAP_DATA.txt", sep = "\t", header = T)
Pop = fread(file = "data.txt", sep = "\t", header = T)

Deathscon= Deaths[,c(3,4)]
#covidconfmelt = melt(covidconf2, id.vars = "Country/Region")
names(Deathscon)= c("State","Deaths")
#covidconfmelt3= ddply(covidage, c("State"), summarize, Age=max(Age, na.rm = T))
#covidconfmelt3$Country= revalue(covidconfmelt3$Country,c("United States"= "United States of America", "Korea , south"= "Republic of Korea"))

Pop1= Pop[,c(2,3)]
names(Pop1)= c("State","Population")

Statefips= fips(unique(Deathscon$State))
fipconUS=cbind(Statefips,Deathscon )
states1= fipconUS[,c(1,3)]
names(states1)=c("fips","Deaths")
Order1= states1[order(fips)]
Order1= Order1[-c(51:60)]

Statefips2= fips(unique(Pop1$State))
fipconUS2=cbind(Statefips2,Pop1 )
states2= fipconUS2[,c(1,3)]
names(states2)=c("fips","Population")
Order2= states2[order(fips)]
Order2= Order2[-c(51,52)]

Order2$help= Order1$Deaths/Order2$Population

plot_usmap(data= Order2, values = "help")+
  theme(legend.position = "right")+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


world= ne_countries(scale="medium", returnclass= "sf")
ggplot(world)+geom_sf()+theme_classic()

covidconftot = ddply(covidconfmelt3, "Country", summarize, MedianAge=max(Age))

world = merge(world, covidconftot, by.x= "admin", by.y="Country")
library(wesanderson)
pal= wes_palette("FantasticFox",100,type="continuous")

ggplot(world)+
  geom_sf(aes(fill=MedianAge))+
  theme_classic()+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")