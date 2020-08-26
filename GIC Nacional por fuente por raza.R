library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)


setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/GIC2008-2018/GIC2008-2018"))

Deciles_por_fuente_2008_indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/Indigena/Nacional Ingresos por fuente por DECIL estimaciones 2008 indigena.dbf")

Deciles_por_fuente_2008_indigena<-Deciles_por_fuente_2008_indigena%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+DONATIVOS+REMESAS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)


Deciles_por_fuente_2018_indigena<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/Indigena/Nacional Ingresos por fuente por DECIL INDIGENA estimaciones.dbf")

Deciles_por_fuente_2018_indigena<-Deciles_por_fuente_2018_indigena%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+DONATIVOS+REMESAS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)



################################## Indigenous #####################################
Tasa_total_indigena<-((Deciles_por_fuente_2018_indigena$ING.COR-Deciles_por_fuente_2008_indigena$ING.COR)/
                        Deciles_por_fuente_2008_indigena$ING.COR)*100

#### trabajo

Trabajo_indigena<-data.frame(trabajo2008=Deciles_por_fuente_2008_indigena$TRABAJO,
                             trabajo2018=Deciles_por_fuente_2018_indigena$TRABAJO,
                             ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                             ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                             Tasa_total_indigena)

Trabajo_indigena<-Trabajo_indigena%>%
  mutate(trabajo_aporte=((trabajo2018-trabajo2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

#### renta

Renta_indigena<-data.frame(renta2008=Deciles_por_fuente_2008_indigena$RENTAS,
                           renta2018=Deciles_por_fuente_2018_indigena$RENTAS,
                           ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                           ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                           Tasa_total_indigena)

Renta_indigena<-Renta_indigena%>%
  mutate(rentas_aporte=((renta2018-renta2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

#### Jubilaciones

Jubilaciones_indigena<-data.frame(jubilaciones2008=Deciles_por_fuente_2008_indigena$JUBILACION,
                                  jubilaciones2018=Deciles_por_fuente_2018_indigena$JUBILACION,
                                  ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                                  ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                                  Tasa_total_indigena)

Jubilaciones_indigena<-Jubilaciones_indigena%>%
  mutate(jubilaciones_aporte=((jubilaciones2018-jubilaciones2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

## Becas

Becas_indigena<-data.frame(becas2008=Deciles_por_fuente_2008_indigena$BECAS,
                           becas2018=Deciles_por_fuente_2018_indigena$BECAS,
                           ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                           ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                           Tasa_total_indigena)

Becas_indigena<-Becas_indigena%>%
  mutate(becas_aporte=((becas2018-becas2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)
                             
## Donativos

Donativos_indigena<-data.frame(donativos2008=Deciles_por_fuente_2008_indigena$DONATIVOS,
                               donativos2018=Deciles_por_fuente_2018_indigena$DONATIVOS,
                               ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                               ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                               Tasa_total_indigena)

Donativos_indigena<-Donativos_indigena%>%
  mutate(donativos_aporte=((donativos2018-donativos2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)
                             
### Remesas

Remesas_indigena<-data.frame(remesas2008=Deciles_por_fuente_2008_indigena$REMESAS,
                             remesas2018=Deciles_por_fuente_2018_indigena$REMESAS,
                             ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                             ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                             Tasa_total_indigena)

Remesas_indigena<-Remesas_indigena%>%
  mutate(remesas_aporte=((remesas2018-remesas2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

### Bene Gob

Benegob_indigena<-data.frame(benegob2008=Deciles_por_fuente_2008_indigena$BENEGOBIER,
                             benegob2018=Deciles_por_fuente_2018_indigena$BENEGOBIER,
                             ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                             ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                             Tasa_total_indigena)

Benegob_indigena<-Benegob_indigena%>%
  mutate(benegob_aporte=((benegob2018-benegob2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

### Trans de hogares

Trans_hogares_indigena<-data.frame(transdehogares2008=Deciles_por_fuente_2008_indigena$TRANS.HOG,
                                   transdehogares2018=Deciles_por_fuente_2018_indigena$TRANS.HOG,
                                   ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                                   ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                                   Tasa_total_indigena)

Trans_hogares_indigena<-Trans_hogares_indigena%>%
  mutate(transdehogares_aporte=((transdehogares2018-transdehogares2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

### Trans de Instituciones

Instituciones_indigena<-data.frame(instituciones2008=Deciles_por_fuente_2008_indigena$TRANS.INST,
                                   instituciones2018=Deciles_por_fuente_2018_indigena$TRANS.INST,
                                   ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                                   ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                                   Tasa_total_indigena)

Instituciones_indigena<-Instituciones_indigena%>%
  mutate(instituciones_aporte=((instituciones2018-instituciones2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

### Alquiler

Alquiler_indigena<-data.frame(alquiler2008=Deciles_por_fuente_2008_indigena$ESTIM.ALQU,
                              alquiler2018=Deciles_por_fuente_2018_indigena$ESTIM.ALQU,
                              ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                              ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                              Tasa_total_indigena)

Alquiler_indigena<-Alquiler_indigena%>%
  mutate(alquiler_aporte=((alquiler2018-alquiler2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

### otros

Otros_indigena<-data.frame(otros2008=Deciles_por_fuente_2008_indigena$OTROS.INGR,
                           otros2018=Deciles_por_fuente_2018_indigena$OTROS.INGR,
                           ing_cor2008=Deciles_por_fuente_2008_indigena$ING.COR,
                           ing_cor2018=Deciles_por_fuente_2018_indigena$ING.COR,
                           Tasa_total_indigena)

Otros_indigena<-Otros_indigena%>%
  mutate(otros_aporte=((otros2018-otros2008)/((ing_cor2018-ing_cor2008)))*Tasa_total_indigena)

Cuadro_indigena<-data.frame(Trabajo_indigena$trabajo_aporte,
                            Renta_indigena$rentas_aporte,
                            Jubilaciones_indigena$jubilaciones_aporte,
                            Becas_indigena$becas_aporte,
                            Donativos_indigena$donativos_aporte,
                            Remesas_indigena$remesas_aporte,
                            Benegob_indigena$benegob_aporte,
                            Trans_hogares_indigena$transdehogares_aporte,
                            Instituciones_indigena$instituciones_aporte,
                            Alquiler_indigena$alquiler_aporte,
                            Otros_indigena$otros_aporte,
                            Tasa_total_indigena)

#Cuadro_indigena<-Cuadro_indigena%>%
 # mutate(Prueba=Trabajo_indigena.trabajo_aporte+Renta_indigena.rentas_aporte+Jubilaciones_indigena.jubilaciones_aporte+Becas_indigena.becas_aporte+Donativos_indigena.donativos_aporte+Remesas_indigena.remesas_aporte+Benegob_indigena.benegob_aporte+Trans_hogares_indigena.transdehogares_aporte+Instituciones_indigena.instituciones_aporte+Alquiler_indigena.alquiler_aporte+Otros_indigena.otros_aporte)


names(Cuadro_indigena)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                       "Household transfers","Instituion transfers","Rent estimate","Others","Total")                             

                             
                             
#No indigena                         

Deciles_por_fuente_2008_NO<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/No indigena/Nacional Ingresos por fuente por DECIL estimaciones 2008 NO indigena.dbf")

Deciles_por_fuente_2018_NO<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/No indigena/Nacional Ingresos por fuente por DECIL NO INDIGENA estimaciones.dbf")

Deciles_por_fuente_2008_NO<-Deciles_por_fuente_2008_NO%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+REMESAS+DONATIVOS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)

Deciles_por_fuente_2018_NO<-Deciles_por_fuente_2018_NO%>%
  mutate(prueba=TRABAJO+RENTAS+JUBILACION+BECAS+REMESAS+DONATIVOS+BENEGOBIER+TRANS.HOG+TRANS.INST+ESTIM.ALQU+OTROS.INGR)

Tasa_total_NO<-((Deciles_por_fuente_2018_NO$ING.COR-Deciles_por_fuente_2008_NO$ING.COR)/Deciles_por_fuente_2008_NO$ING.COR)*100


# Trabajo

Trabajo_NO<-data.frame(trabajo2008=Deciles_por_fuente_2008_NO$TRABAJO,
                       trabajo2018=Deciles_por_fuente_2018_NO$TRABAJO,
                       ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                       ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                       Tasa_total_NO)

Trabajo_NO<-Trabajo_NO%>%
  mutate(Trabajo_aporte=((trabajo2018-trabajo2008)/((ingcor2018-ingcor2008)))*Tasa_total_NO)


# Rentas

Rentas_NO<-data.frame(rentas2008=Deciles_por_fuente_2008_NO$RENTAS,
                      rentas2018=Deciles_por_fuente_2018_NO$RENTAS,
                      ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                      ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                      Tasa_total_NO)

Rentas_NO<-Rentas_NO%>%
  mutate(rentas_aporte=((rentas2018-rentas2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

#Jubilacion

Jubilacion_NO<-data.frame(jubilacion2008=Deciles_por_fuente_2008_NO$JUBILACION,
                          jubilacion2018=Deciles_por_fuente_2018_NO$JUBILACION,
                          ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                          ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                          Tasa_total_NO)

Jubilacion_NO<-Jubilacion_NO%>%
  mutate(jubilacion_aporte=((jubilacion2018-jubilacion2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

#Becas

Becas_NO<-data.frame(becas2008=Deciles_por_fuente_2008_NO$BECAS,
                     becas2018=Deciles_por_fuente_2018_NO$BECAS,
                     ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                     ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                     Tasa_total_NO)

Becas_NO<-Becas_NO%>%
  mutate(becas_aporte=((becas2018-becas2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

#Remesas

Remesas_NO<-data.frame(remesas2008=Deciles_por_fuente_2008_NO$REMESAS,
                       remesas2018=Deciles_por_fuente_2018_NO$REMESAS,
                       ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                       ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                       Tasa_total_NO)

Remesas_NO<-Remesas_NO%>%
  mutate(becas_aporte=((remesas2018-remesas2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

#Donativos

Donativos_NO<-data.frame(donativos2008=Deciles_por_fuente_2008_NO$DONATIVOS,
                         donativos2018=Deciles_por_fuente_2018_NO$DONATIVOS,
                         ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                         ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                         Tasa_total_NO)

Donativos_NO<-Donativos_NO%>%
  mutate(donativos_aporte=((donativos2018-donativos2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

#BENEGOBIER

Benegob_NO<-data.frame(bene2008=Deciles_por_fuente_2008_NO$BENEGOBIER,
                       bene2018=Deciles_por_fuente_2018_NO$BENEGOBIER,
                       ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                       ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                       Tasa_total_NO)

Benegob_NO<-Benegob_NO%>%
  mutate(bene_aporte=((bene2018-bene2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

#TRANS.HOG

trans_hog_NO<-data.frame(transhog2008=Deciles_por_fuente_2008_NO$TRANS.HOG,
                         transhog2018=Deciles_por_fuente_2018_NO$TRANS.HOG,
                         ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                         ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                         Tasa_total_NO)

trans_hog_NO<-trans_hog_NO%>%
  mutate(trans_hog_aporte=((transhog2018-transhog2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)


#TRANS.INST

trans_inst_NO<-data.frame(trans_inst2008=Deciles_por_fuente_2008_NO$TRANS.INST,
                          trans_inst2018=Deciles_por_fuente_2018_NO$TRANS.INST,
                          ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                          ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                          Tasa_total_NO)

trans_inst_NO<-trans_inst_NO%>%
  mutate(trans_inst_aporte=((trans_inst2018-trans_inst2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)


#ESTIM.ALQU

Estim_alqu_NO<-data.frame(estim2008=Deciles_por_fuente_2008_NO$ESTIM.ALQU,
                          estim2018=Deciles_por_fuente_2018_NO$ESTIM.ALQU,
                          ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                          ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                          Tasa_total_NO)

Estim_alqu_NO<-Estim_alqu_NO%>%
  mutate(estim_aporte=((estim2018-estim2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)


#OTROS.INGR   

Otros_NO<-data.frame(otros2008=Deciles_por_fuente_2008_NO$OTROS.INGR,
                     otros2018=Deciles_por_fuente_2018_NO$OTROS.INGR,
                     ingcor2008=Deciles_por_fuente_2008_NO$ING.COR,
                     ingcor2018=Deciles_por_fuente_2018_NO$ING.COR,
                     Tasa_total_NO)

Otros_NO<-Otros_NO%>%
  mutate(otors_aporte=((otros2018-otros2008)/(ingcor2018-ingcor2008))*Tasa_total_NO)

Cuadro_NO<-data.frame(Trabajo_NO$Trabajo_aporte,
                      Rentas_NO$rentas_aporte,
                      Jubilacion_NO$jubilacion_aporte,
                      Becas_NO$becas_aporte,
                      Remesas_NO$becas_aporte,
                      Donativos_NO$donativos_aporte,
                      Benegob_NO$bene_aporte,
                      trans_hog_NO$trans_hog_aporte,
                      trans_inst_NO$trans_inst_aporte,
                      Estim_alqu_NO$estim_aporte,
                      Otros_NO$otors_aporte,
                      Tasa_total_NO)  

#Cuadro_NO<-Cuadro_NO%>%mutate(prueba_de_fuego=Trabajo_NO.Trabajo_aporte+Rentas_NO.rentas_aporte+Jubilacion_NO.jubilacion_aporte+
           #Becas_NO.becas_aporte+
           #Remesas_NO.becas_aporte+
           #Donativos_NO.donativos_aporte+
           #Benegob_NO.bene_aporte+
           #trans_hog_NO.trans_hog_aporte+
           #trans_inst_NO.trans_inst_aporte+
           #Estim_alqu_NO.estim_aporte+
           #Otros_NO.otors_aporte)
names(Cuadro_NO)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                          "Household transfers","Instituion transfers","Rent estimate","Others","Total")      

######### Gráficos

Cuadro_indigena<-Cuadro_indigena%>%
  mutate("Ethnic group"="Indigenous", Deciles=c("Total","I","II","III","IV","V","VI","VII","VIII","IX","X"))

indigena_melt<-melt(Cuadro_indigena)

Cuadro_NO<-Cuadro_NO%>%
  mutate("Ethnic group"="Non-indigenous", Deciles=c("Total","I","II","III","IV","V","VI","VII","VIII","IX","X"))

NO_melt<-melt(Cuadro_NO)

Cuadro_final<-rbind(indigena_melt,NO_melt)
