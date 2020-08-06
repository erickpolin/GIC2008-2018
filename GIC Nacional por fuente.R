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

