library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/GIC2008-2018/GIC2008-2018"))

Deciles2008<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/Nacional ingresos por decil y GINI 2008.dbf")
Deciles2018<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/Nacional ingresos por decil y GINI .dbf")

GICNacionalTotal<-data.frame(Deciles2008,Deciles2018)

names(GICNacionalTotal)[1]<-c("Year2008")

names(GICNacionalTotal)[2]<-c("Year2018")


GICNacionalTotal<-GICNacionalTotal[-c(1),]


GICNacionalTotal<-GICNacionalTotal%>%
  mutate(Rate=((Year2018-Year2008)/Year2008)*100,Deciles=c("I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:10)

media<-mean(GICNacionalTotal$Rate)
mediana<-median(GICNacionalTotal$Rate)

mytheme<-theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5), 
               legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
               legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
               axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
               axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))



GIC_Nacional_total<-GICNacionalTotal%>%
  mutate(Deciles=fct_relevel(Deciles,"I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title = "Growth Incidence Curve Mexico 2008-2018",
                    y="Growth rate (total)",
                    x="Decile")+
  geom_abline(intercept = media,slope=0,col="red")+
  annotate("text", label = "Mean = -2.34", x = 1, y = media-0.7, size = 4, colour = "red")+
  geom_abline(intercept = mediana,slope=0,col="blue")+
  annotate("text", label = "Median = -1.07", x = 1, y = mediana+0.5, size = 4, colour = "blue")+
  theme_minimal()

GIC_Nacional_total

GIC_Nacional_total<-ggplotly(GIC_Nacional_total)

GIC_Nacional_total

saveWidget(GIC_Nacional_total,fil="GIC_Nacional_total.html")

