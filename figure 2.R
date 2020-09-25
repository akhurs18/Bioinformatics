library(ggplot2)
library(reshape2)
library(plyr)
library(plotly)

df = fread(file = "total-confirmed-cases-of-covid-19-per-million-people-vs-gdp-per-capita.txt", sep = "\t", header = T)


covidtest= df[,c(1,4)]
#covidconfmelt = reshape2::melt(covidtest, id.vars = "Entity")
names(covidtest)= c("Country","Cases")
covidtestmelt2= ddply(covidtest, c("Country"), summarize, Cases=max(Cases, na.rm = T))
#covidtestmelt2$Date= as.Date(covidtestmelt2$Date,format  = "%d-%b-%y")
#str(covidtestmelt2$Date)




covidGDP= df[,c(1,5)]
#covidtestmelt =reshape2::melt(covidtest, id.vars = "Confirmed COVID-19 cases per million people (cases per million)")
names(covidGDP)= c("Country", "GDP")
covidGDPmelt2= ddply(covidGDP, c("Country"), summarize, GDP=max(GDP, na.rm = T))
#covidGDPmelt2$Date= as.Date(covidGDPmelt2$Date,format  = "%d-%b-%y")
#covidGDPmelt2$Date=sort(covidGDPmelt2$Date)

covidprop= merge(covidtestmelt2, covidGDPmelt2, by= c("Country"))
#covidprop$Date= log10(covidprop$Date)
covidprop= covidprop[-c(211),]

covidpropplot = ggplot(covidprop, aes(x= GDP, y=Cases))+
  geom_point(aes(color= Country, ids= Country))+
  theme(legend.position = "none")+
  theme_classic()+
  labs(title = "Confirmed Cases vs GDP per Capita", x= "GDP", y=" Confirmed Cases")
covidpropplot= ggplotly(covidpropplot)  %>% layout(showlegend= FALSE)
covidpropplot

ggsave("covid.pdf")