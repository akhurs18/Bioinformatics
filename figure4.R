library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)
library(plotly)
library(viridis)
library(hrbrthemes)

df = fread(file = "case-fatality-rate-of-covid-19-vs-median-age.txt", sep = "\t", header = T)

covidage= df[,c(1,6)]
#covidconfmelt = melt(covidconf2, id.vars = "Country/Region")
names(covidage)= c("Continent", "Death")
covidconfmelt3= ddply(covidage, c("Continent"), summarize, Death=max(Death, na.rm = T))
covidconfmelt3$Country= revalue(covidconfmelt3$Country,c("United States"= "United States of America", "Korea , south"= "Republic of Korea"))

covidconfmelt3= covidconfmelt3[c(2,12,76,177,223,14),c(1,2)]

covidconfmelt3 <- covidconfmelt3 %>%
  arrange(desc(Continent)) %>%
  mutate(prop = Death / sum(Death) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

PIE= ggplot(covidconfmelt3, aes(x="", y=Death, fill=Continent)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = Continent), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

PIE