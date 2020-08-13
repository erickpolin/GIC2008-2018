#GIC NAcional indígena y No indígena
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)
library(ggrepel)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/GIC2008-2018/GIC2008-2018"))

deciles2008indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/Nacional ingresos por decil y GINI 2008 Indigena.dbf")

deciles2018indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/Nacional Indigena ingresos por decil y GINI .dbf")

deciles2008No<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/Nacional ingresos por decil y GINI 2008 NO Indigena.dbf")

deciles2018No<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/Nacional NO Indigena ingresos por decil y GINI .dbf")

GIC_nacional_por_raza<-data.frame(deciles2008indigena,deciles2018indigena,deciles2008No,deciles2018No)

names(GIC_nacional_por_raza)[1]<-c("Indigena2008")

names(GIC_nacional_por_raza)[2]<-c("Indigena2018")

names(GIC_nacional_por_raza)[3]<-c("No2008")

names(GIC_nacional_por_raza)[4]<-c("No2018")

GIC_nacional_por_raza<-GIC_nacional_por_raza[-c(1),]

GIC_nacional_por_raza<-GIC_nacional_por_raza%>%
  mutate("Indigenous"=((Indigena2018-Indigena2008)/Indigena2008)*100,
         "Non-indigenous"=((No2018-No2008)/No2008)*100,
         "Deciles"=c("I","II","III","IV","V","VI","VII","VIII","IX","X"))

media_indigena<-mean(GIC_nacional_por_raza$Indigenous)
mediana_indigena<-median(GIC_nacional_por_raza$Indigenous)

media_no<-mean(GIC_nacional_por_raza$`Non-indigenous`)
mediana_no<-median(GIC_nacional_por_raza$`Non-indigenous`)

GIC_reducida<-GIC_nacional_por_raza%>%
  select(Indigenous,`Non-indigenous`,Deciles)

GIC_nacional_por_raza_derretida<-GIC_reducida%>%
  melt(id.vars="Deciles",variable.name="Rates")

GIC_nacional_por_raza_derretida$Rates<-as.factor(as.character(GIC_nacional_por_raza_derretida$Rates))


mytheme<-theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15),hjust = 0.5), 
               legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
               legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
               axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
               axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

GIC_por_raza<-GIC_nacional_por_raza_derretida%>%
  mutate(Deciles=fct_relevel(Deciles,"I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles,y=value,fill=Rates))+
  geom_col(position=position_dodge())+
  labs(title = "Growth Incidence Curve Mexico 2008-2018",
       subtitle = "by ethnic group",
       y="Growth rate (total)",
       x="Decile")+
  geom_abline(intercept = media_indigena,slope=0,col="pink")+
  annotate("text", label = "Mean growth for indigenous people = -2.47", x = 1, y = media_indigena-0.5, size = 2, colour = "pink")+
  geom_abline(intercept = mediana_indigena,slope=0,col="pink")+
  annotate("text", label = "Median growth for indigenous people = -0.44", x = 1, y = mediana_indigena+0.3, size = 2, colour = "pink")+
  geom_abline(intercept = media_no,slope=0,col="blue")+
  annotate("text", label = "Mean growth for non-indigenous people = -2.38", x = 1, y = media_no-0.7, size = 2, colour = "blue")+
  geom_abline(intercept = mediana_no,slope=0,col="blue")+
  annotate("text", label = "Median growth for non-indigenous people = -1.12", x = 1, y = mediana_no+0.5, size = 2, colour = "blue")+
  theme_minimal()

GIC_por_raza


ggplotly(GIC_por_raza)
