library(foreign)
library(tidyverse)
library(plotly)

Deciles2008<-read.dbf(choose.files())
Deciles2018<-read.dbf(choose.files())

GICNacionalTotal<-data.frame(Deciles2008,Deciles2018)

names(GICNacionalTotal)[1]<-c("Year2008")

names(GICNacionalTotal)[2]<-c("Year2018")

GICNacionalTotal<-GICNacionalTotal[-c(1),]

GICNacionalTotal<-GICNacionalTotal%>%
  mutate(Rate=(Year2018-Year2008)/Year2008, Deciles=c("I","II","III","IV","V","VI","VII","VIII","IX","X"))

GIC<-ggplot(GICNacionalTotal,aes(Deciles,Year2018))

GIC+geom_line()

