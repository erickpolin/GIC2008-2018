library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/GIC2008-2018/GIC2008-2018"))

Deciles2008<-read.dbf(choose.files())
Deciles2018<-read.dbf(choose.files())

GICNacionalTotal<-data.frame(Deciles2008,Deciles2018)

names(GICNacionalTotal)[1]<-c("Year2008")

names(GICNacionalTotal)[2]<-c("Year2018")

GICNacionalTotal<-GICNacionalTotal[-c(1),]

#
GICNacionalTotal<-GICNacionalTotal%>%
  mutate(Rate=((Year2018-Year2008)/Year2008)*100,Deciles=c("I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:10)

theme_update(plot.title = element_text(hjust = 0.5))

GIC_Nacional_total<-GICNacionalTotal%>%
  mutate(Deciles=fct_relevel(Deciles,"I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  ggtitle("Growth Incidence Curve
          Mexico (2008-2018)")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Rate growth (total)")+
  xlab("Decile")+
  theme_minimal()

GIC_Nacional_total

GIC_Nacional_total<-ggplotly(GIC_Nacional_total)

GIC_Nacional_total

saveWidget(GIC_Nacional_total,fil="GIC_Nacional_total.html")

