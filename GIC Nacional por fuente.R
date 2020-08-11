library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/GIC2008-2018/GIC2008-2018"))

Deciles_por_fuente_2008<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH2008/Nacional/Nacional Ingresos por fuente por DECIL estimaciones 2008.dbf")

names(Deciles_por_fuente_2008)=c("ING COR2008", "TRABAJO2008", "SUBORDINADO2008", "NEGOCIOS2008","OTROS TRAB2008", "RENTAS2008","UTILIDAD2008", "ARRENDA2008", "TRANSFER2008","JUBILACION2008", "BECAS2008", "DONATIVOS2008", "REMESAS2008", "BENEGOBIERNO2008", "TRANS HOG2008", "TRANS INST2008", "ESTIM ALQU2008", "OTROS INGRESOS2008")

Deciles_por_fuente_2008$TRABAJO2008+Deciles_por_fuente_2008$RENTAS2008+
  Deciles_por_fuente_2008$JUBILACION2008+Deciles_por_fuente_2008$BECAS2008+
  Deciles_por_fuente_2008$DONATIVOS2008+Deciles_por_fuente_2008$REMESAS2008+
  Deciles_por_fuente_2008$BENEGOBIERNO2008+Deciles_por_fuente_2008$`TRANS HOG2008`+
  Deciles_por_fuente_2008$`TRANS INST2008`+Deciles_por_fuente_2008$`ESTIM ALQU2008`+
  Deciles_por_fuente_2008$`OTROS INGRESOS2008`

Deciles_por_fuente_2018<-read.dbf("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018/Nacional/Nacional Ingresos por fuente por DECIL estimaciones.dbf")

names(Deciles_por_fuente_2018)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

Deciles_por_fuente_2018$TRABAJO2018+Deciles_por_fuente_2018$RENTAS2018+
  Deciles_por_fuente_2018$JUBILACION2018+Deciles_por_fuente_2018$BECAS2018+
  Deciles_por_fuente_2018$DONATIVOS2018+Deciles_por_fuente_2018$REMESAS2018+
  Deciles_por_fuente_2018$BENEGOBIERNO2018+Deciles_por_fuente_2018$`TRANS HOG2018`+
  Deciles_por_fuente_2018$`TRANS INST2018`+Deciles_por_fuente_2018$`ESTIM ALQU2018`+
  Deciles_por_fuente_2018$`OTROS INGRESOS2018`

Tasa_total<-((Deciles_por_fuente_2018$`ING COR2018`- Deciles_por_fuente_2008$`ING COR2008`)/Deciles_por_fuente_2008$`ING COR2008`)*100
media_total<-Tasa_total[1]
Tasa_total<-Tasa_total[-1]


Tasa_trabajo<-((Deciles_por_fuente_2018$TRABAJO2018-Deciles_por_fuente_2008$TRABAJO2008)/Deciles_por_fuente_2008$TRABAJO2008)*100
media_trabajo<-Tasa_trabajo[1]
Tasa_trabajo<-Tasa_trabajo[-1]  


Tasa_rentas<-((Deciles_por_fuente_2018$RENTAS2018-Deciles_por_fuente_2008$RENTAS2008)/Deciles_por_fuente_2008$RENTAS2008)*100
media_rentas<-Tasa_rentas[1]
Tasa_rentas<-Tasa_rentas[-1]


Tasa_jubilaciones<-((Deciles_por_fuente_2018$JUBILACION2018-Deciles_por_fuente_2008$JUBILACION2008)/Deciles_por_fuente_2008$JUBILACION2008)*100
media_jubilaciones<-Tasa_jubilaciones[1]
Tasa_jubilaciones<-Tasa_jubilaciones[-1]

Tasa_becas<-((Deciles_por_fuente_2018$BECAS2018-Deciles_por_fuente_2008$BECAS2008)/ Deciles_por_fuente_2008$BECAS2008)*100
media_becas<-Tasa_becas[1]
Tasa_becas<-Tasa_becas[-1]  
  

Tasa_donativos<-((Deciles_por_fuente_2018$DONATIVOS2018-Deciles_por_fuente_2008$DONATIVOS2008)/Deciles_por_fuente_2008$DONATIVOS2008)*100
media_donativos<-Tasa_donativos[1]
Tasa_donativos<-Tasa_donativos[-1]


Tasa_remesas<-((Deciles_por_fuente_2018$REMESAS2018-Deciles_por_fuente_2008$REMESAS2008)/Deciles_por_fuente_2008$REMESAS2008)*100
media_remesas<-Tasa_remesas[1]
Tasa_remesas<-Tasa_remesas[-1]


Tasa_benegobierno<-(Deciles_por_fuente_2018$BENEGOBIERNO2018-Deciles_por_fuente_2008$BENEGOBIERNO2008)/Deciles_por_fuente_2008$BENEGOBIERNO2008
media_benegobierno<-Tasa_benegobierno[1]
Tasa_benegobierno<-Tasa_benegobierno[-1]


Tasa_transdehogares<-((Deciles_por_fuente_2018$`TRANS HOG2018`-Deciles_por_fuente_2008$`TRANS HOG2008`)/Deciles_por_fuente_2008$`TRANS HOG2008`)*100
media_transdehogares<-Tasa_transdehogares[1]
Tasa_transdehogares<-Tasa_transdehogares[-1]


Tasa_instituciones<-((Deciles_por_fuente_2018$`TRANS INST2018`-Deciles_por_fuente_2008$`TRANS INST2008`)/Deciles_por_fuente_2008$`TRANS INST2008`)*100
media_instituciones<-Tasa_instituciones[1]
Tasa_instituciones<-Tasa_instituciones[-1]


Tasa_alquiler<-((Deciles_por_fuente_2018$`ESTIM ALQU2018`-Deciles_por_fuente_2008$`ESTIM ALQU2008`)/Deciles_por_fuente_2008$`ESTIM ALQU2008`)*100
media_alquiler<-Tasa_alquiler[1]
Tasa_alquiler<-Tasa_alquiler[-1]

Tasa_otros<-((Deciles_por_fuente_2018$`OTROS INGRESOS2018`-Deciles_por_fuente_2008$`OTROS INGRESOS2008`)/Deciles_por_fuente_2008$`OTROS INGRESOS2008`)*100
media_otros<-Tasa_otros[1]
Tasa_otros<-Tasa_otros[-1]

Tasas<-data.frame(Tasa_total,Tasa_trabajo,Tasa_rentas,Tasa_jubilaciones,Tasa_becas,Tasa_donativos,Tasa_remesas,Tasa_benegobierno,
Tasa_transdehogares,Tasa_instituciones,Tasa_alquiler,Tasa_otros)

