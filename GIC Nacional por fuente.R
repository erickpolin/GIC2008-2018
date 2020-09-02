library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/GIC2008-2018/GIC2008-2018"))

Deciles_por_fuente_2008<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/Total/Nacional Ingresos por fuente por DECIL estimaciones 2008.dbf")

names(Deciles_por_fuente_2008)=c("ING COR2008", "TRABAJO2008", "SUBORDINADO2008", "NEGOCIOS2008","OTROS TRAB2008", "RENTAS2008","UTILIDAD2008", "ARRENDA2008", "TRANSFER2008","JUBILACION2008", "BECAS2008", "DONATIVOS2008", "REMESAS2008", "BENEGOBIERNO2008", "TRANS HOG2008", "TRANS INST2008", "ESTIM ALQU2008", "OTROS INGRESOS2008")

Deciles_por_fuente_2008<-Deciles_por_fuente_2008%>%
  mutate(prueba=Deciles_por_fuente_2008$TRABAJO2008+Deciles_por_fuente_2008$RENTAS2008+
  Deciles_por_fuente_2008$JUBILACION2008+Deciles_por_fuente_2008$BECAS2008+
  Deciles_por_fuente_2008$DONATIVOS2008+Deciles_por_fuente_2008$REMESAS2008+
  Deciles_por_fuente_2008$BENEGOBIERNO2008+Deciles_por_fuente_2008$`TRANS HOG2008`+
  Deciles_por_fuente_2008$`TRANS INST2008`+Deciles_por_fuente_2008$`ESTIM ALQU2008`+
  Deciles_por_fuente_2008$`OTROS INGRESOS2008`)

Deciles_por_fuente_2018<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/Total/Nacional Ingresos por fuente por DECIL estimaciones.dbf")

names(Deciles_por_fuente_2018)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba=Deciles_por_fuente_2018$TRABAJO2018+Deciles_por_fuente_2018$RENTAS2018+
  Deciles_por_fuente_2018$JUBILACION2018+Deciles_por_fuente_2018$BECAS2018+
  Deciles_por_fuente_2018$DONATIVOS2018+Deciles_por_fuente_2018$REMESAS2018+
  Deciles_por_fuente_2018$BENEGOBIERNO2018+Deciles_por_fuente_2018$`TRANS HOG2018`+
  Deciles_por_fuente_2018$`TRANS INST2018`+Deciles_por_fuente_2018$`ESTIM ALQU2018`+
  Deciles_por_fuente_2018$`OTROS INGRESOS2018`)


Tasa_total<-((Deciles_por_fuente_2018$`ING COR2018`- Deciles_por_fuente_2008$`ING COR2008`)/Deciles_por_fuente_2008$`ING COR2008`)*100


########################## Trabajo ##########################################

trabajo<-data.frame(trabajo2008=Deciles_por_fuente_2008$TRABAJO2008,
                    trabajo2018=Deciles_por_fuente_2018$TRABAJO2018,
                    ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                    ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2018-trabajo2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Rentas ########################################

rentas<-data.frame(rentas2008=Deciles_por_fuente_2008$RENTAS2008,rentas2018=Deciles_por_fuente_2018$RENTAS2018,
                   ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                   ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2018-rentas2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Jubilaciones ########################################

jubilaciones<-data.frame(jubilaciones2008=Deciles_por_fuente_2008$JUBILACION2008,jubilaciones2018=Deciles_por_fuente_2018$JUBILACION2018,
                         ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                         ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                         Tasa_total)
jubilaciones<-jubilaciones%>%
  mutate(jubilaciones_aporte=((jubilaciones2018-jubilaciones2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Becas ########################################
becas<-data.frame(becas2008=Deciles_por_fuente_2008$BECAS2008,becas2018=Deciles_por_fuente_2018$BECAS2018,
                  ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                  ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                  Tasa_total)
becas<-becas%>%
  mutate(becas_aporte=((becas2018-becas2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Donativos ########################################

donativos<-data.frame(donativos2008=Deciles_por_fuente_2008$DONATIVOS2008,donativos2018=Deciles_por_fuente_2018$DONATIVOS2018,
                      ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                      ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                      Tasa_total)

donativos<-donativos%>%
  mutate(donativos_aporte=((donativos2018-donativos2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Remesas ########################################

remesas<-data.frame(remesas2008=Deciles_por_fuente_2008$REMESAS2008,remesas2018=Deciles_por_fuente_2018$REMESAS2018,
                    ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                    ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                    Tasa_total)

remesas<-remesas%>%
  mutate(remesas_aporte=((remesas2018-remesas2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Benegobierno ########################################

benegobierno<-data.frame(benegob2008=Deciles_por_fuente_2008$BENEGOBIERNO2008,
                         benegob2018=Deciles_por_fuente_2018$BENEGOBIERNO2018,
                         ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                         ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2018-benegob2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Transdehogares ########################################

transdehogares<-data.frame(transdehogares2008=Deciles_por_fuente_2008$`TRANS HOG2008`, 
                           transdehogares2018=Deciles_por_fuente_2018$`TRANS HOG2018`,
                           ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                           ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                           Tasa_total)

transdehogares<-transdehogares%>%
  mutate(transdehogares_aporte=((transdehogares2018-transdehogares2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### instituciones ########################################

instituciones<-data.frame(instituciones2008=Deciles_por_fuente_2008$`TRANS INST2008`, 
                          instituciones2018=Deciles_por_fuente_2018$`TRANS INST2018`,
                          ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                          ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                          Tasa_total)

instituciones<-instituciones%>%
  mutate(instituciones_aporte=((instituciones2018-instituciones2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### alquiler ########################################

alquiler<-data.frame(alquiler2008=Deciles_por_fuente_2008$`ESTIM ALQU2008`,alquiler2018=Deciles_por_fuente_2018$`ESTIM ALQU2018`,
                     ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                     ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                     Tasa_total)
alquiler<-alquiler%>%
  mutate(alquiler_aporte=((alquiler2018-alquiler2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### otros ########################################

otros<-data.frame(otros2008=Deciles_por_fuente_2008$`OTROS INGRESOS2008`,
                  otros2018=Deciles_por_fuente_2018$`OTROS INGRESOS2018`,
                  ing_cor2008=Deciles_por_fuente_2008$`ING COR2008`,
                  ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                  Tasa_total)
otros<-otros%>%
  mutate(otros_aporte=((otros2018-otros2008)/((ing_cor2018-ing_cor2008)))*Tasa_total)

################################### Cuadro final ########################################

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  Pensions=jubilaciones$jubilaciones_aporte,
  Scholarships=becas$becas_aporte,
  Donations=donativos$donativos_aporte,
  Remittances=remesas$remesas_aporte,
  "Government transfers"=benegobierno$benegob_aporte,
  "Household transfers"=transdehogares$transdehogares_aporte,
  "Instituion transfers"=instituciones$instituciones_aporte,
  "Rent estimate"=alquiler$alquiler_aporte,
  "Others"=otros$otros_aporte)

#cuadro_final<-cuadro_final%>%
 # mutate(Prueba=Labor+Capital+Pensions+Scholarships+Donations+Remittances+Government.transfers+Household.transfers+
  #         Instituion.transfers+Rent.estimate+Others)



names(cuadro_final)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                       "Household transfers","Instituion transfers","Imputed rent","Others")


cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Total","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Total","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-melt(cuadro_final)

GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Total","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Growth Incidence Curve Mexico 2008-2018",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
    geom_hline(yintercept = 0)+
  annotate("text", x= "Total", y= -11.5, label="-9.29")+
  annotate("text", x= "I", y= 13.5, label="9.25")+
  annotate("text", x= "II", y= 10.5, label="6.22")+
  annotate("text", x= "III", y= 9.5, label="3.45")+
  annotate("text", x= "IV", y= 6.5, label="2.26")+
  annotate("text", x= "V", y= 4.5, label="0.25")+
  annotate("text", x= "VI", y= -5.5, label="-2.40")+
  annotate("text", x= "VII", y= -9.5, label="-5.60")+
  annotate("text", x= "VIII", y= -11.5, label="-7.91")+
  annotate("text", x= "IX", y= -15.5, label="-11.83")+
  annotate("text", x= "X", y= -21.5, label="-17.16")+
  coord_cartesian(ylim = c(-24, 14),)+
  scale_y_continuous(breaks=seq(-25,15,1))+
  theme_minimal()
 
GIC<-ggplotly(GIC)

GIC

saveWidget(GIC, file="C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/Entrega septiembre/Growth Incidence Curve Mexico by source of income 2008-2018.html")



