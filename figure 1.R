library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)
library(ggplot2)
library(reshape2)
library(plyr)
library(plotly)


df = fread(file = "case-fatality-rate-of-covid-19-vs-median-age.txt", sep = "\t", header = T)

covidage= df[,c(1,3,5)]
#covidconfmelt = melt(covidconf2, id.vars = "Country/Region")
names(covidage)= c("Country", "Date", "Age")
covidconfmelt3= ddply(covidage, c("Country"), summarize, Age=max(Age, na.rm = T))
covidconfmelt3$Country= revalue(covidconfmelt3$Country,c("United States"= "United States of America", "Korea , south"= "Republic of Korea"))



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




ggsave("covid.pdf")