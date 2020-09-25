library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)
library(plotly)
library(viridis)
library(hrbrthemes)

df = fread(file = "case-fatality-rate-of-covid-19-vs-median-age.txt", sep = "\t", header = T)

covidage= df[,-c(2,3,6)]
#covidconfmelt = melt(covidconf2, id.vars = "Country/Region")
names(covidage)= c("Country", "Date", "Age")
covidconfmelt3= ddply(covidage, c("Country"), summarize, Age=max(Age, na.rm = T))
covidconfmelt3$Country= revalue(covidconfmelt3$Country,c("United States"= "United States of America", "Korea , south"= "Republic of Korea"))

covidrate= df[,-c(2,3,5)]
#covidconfmelt = melt(covidconf2, id.vars = "Country/Region")
names(covidrate)= c("Country", "Date", "Rate")
covidratemelt3= ddply(covidrate, c("Country"), summarize, Rate=max(Rate, na.rm = T))
covidratemelt3$Country= revalue(covidratemelt3$Country,c("United States"= "United States of America", "Korea , south"= "Republic of Korea"))
covidratemelt3$Rate=log(covidratemelt3$Rate)

covidprop= merge(covidratemelt3, covidconfmelt3, by=("Country"))

covidprop= covidprop[-c(267:270,2,12:13,29,68,76,77,181,198,228,265,102,139,140,159,256),]

covidpropplot = ggplot(covidprop, aes(x= Age, y=Rate))+
  geom_point(aes(alpha=10,color= Country, size=Rate))+
  theme(legend.position = "none")+
  #scale_size(range = c(.1, 24), name="Population (M)") +
  theme_bw()+
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  labs(title = "Covid19 fatality rate vs median age", x= " Median Age", y=" Fatality Rate")
covidpropplot= ggplotly(covidpropplot)  %>% layout(showlegend= FALSE)
covidpropplot
