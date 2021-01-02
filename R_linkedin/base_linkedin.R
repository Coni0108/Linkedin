{install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("car")
install.packages("psych")
install.packages("ggpubr")
install.packages("ggthemes")
install.packages("tidyr")
install.packages("RCurl")
install.packages("magrittr")
}

{library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)
library(psych)
library(ggpubr)
library(ggthemes)
library(tidyr)
library(RCurl)
library(magrittr)
library(knitr)
}

{base= read_excel("Linkedin.xlsx")
base$...1= NULL
}
#LIMPIAR BASE#----------------------
{base$inicioa[base$inicioa=="-" |base$inicioa== "Sin info"|
               base$inicioa=="//////////////////"|
               base$inicioa=="Sin Info."|
               base$inicioa=="Sin Info"|
               base$inicioa=="Sin info."|
               base$inicioa=="n"|
               base$inicioa=="1196"|
               base$inicioa=="X"|
               base$inicioa=="x"|
               base$inicioa=="sin registro"|
               base$inicioa=="Sin registro"|
               base$inicioa=="Sin informacion"|
               base$inicioa=="Sin Informacion"|
               base$inicioa=="sin informacion"|
               base$inicioa=="No info"|
               base$inicioa=="No especifica"] <- NA
base$inicioa[base$inicioa=="2108"]="2018"
}

{base$fina[base$fina=="Actualidad"|
            base$fina=="Actual"|
            base$fina=="presente"|
            base$fina=="actualidad"|
            base$fina=="ACTUALIDAD"|
            base$fina=="Atctual"|
            base$fina=="actualiad"|
            base$fina=="actual"|
            base$fina=="Actialidad"|
             base$fina=="Actiualidad"]="2020"
}
{base$fina[base$fina=="-"|
            base$fina=="Sin info"|
            base$fina=="Sin info."|
            base$fina=="Sin Info."|
            base$fina=="n"|
            base$fina==""|
            base$fina=="1015"|
            base$fina=="//////////////////"|
            base$fina=="Sin registro"|
            base$fina=="Sin informacion"|
            base$fina=="sin info"|
            base$fina=="Sin Info"|
            base$fina=="No info"|
            base$fina=="sin informacion"|
            base$fina=="No especifica"
]= NA
}
{base$fina[base$fina=="´2017"]="2017"
  base$empresa[base$empresa=="-"]=NA
  base$posicion[base$posicion=="-"]=NA
  base$posicion= toupper(base$posicion)
  base$posicion= iconv(base$posicion, to="ASCII//TRANSLIT")
  base$empresa= toupper(base$empresa)
  base$empresa= iconv(base$empresa, to="ASCII//TRANSLIT")
  base$empresa[base$empresa=="CGE DISTRIBUCION"|
                 base$empresa=="CGE S.A."|
                 base$empresa=="CGE DISTRIBUCION S.A"|
                 base$empresa=="GRUPO CGE"|
                 base$empresa=="TRANSET S.A - GRUPO CGE"|
                 base$empresa=="TECNET, GRUPO CGE"|
                 base$empresa=="GRUPO CGE S.A"|
                 base$empresa=="CGE S.A. Y FILIALES"|
                 base$empresa=="CGE S.A Y  FILLIALES"|
                 base$empresa=="IGSA S.A/FILIAL GRUPO CGE"|
                 base$empresa=="CGE-CONAFE-EMEL"|
                 base$empresa=="NONAVET - GRUPO CGE"|
                 base$empresa=="NOVANET, GRUPO CGE"|
                 base$empresa=="NOVANET S.A/FILIAL GRUPO CGE"]="CGE S.A"
  base$fina[base$fina=="20014"]="2014"
  base$inicioa[base$inicioa=="2914"]="2014"
  base$fina[base$fina=="1889"]="1989"
  base$inicioa[base$inicioa=="2918"]="2018"
  base$inicioa[base$inicioa=="no hay info"]=NA
base= select(base, -iniciom, -finm)
base= na.omit(base)
View(base)
}
#base limpia-------------------------------------------

{employ= select(base,-female) %>%
  filter(tipoclean3=="empleo")
View(employ) #base de empleo de cada empleado y año de ingreso
}

{educ= select(base,-female) %>%
  filter(tipoclean3=="educacion") #base de educacion de cada empl y año de ing
}
View(educ)

#PORCENTAJE DE GÉNERO

{data1= distinct(base, exec_id, female)
data1= data1 %>%
  filter(female < 2)
View(data1)
}
{
  data1$female= factor(data1$female, labels=c("Masculino", "Femenino"))
  View(data1)
  porcentajes= as.numeric(round(((prop.table(table(data1$female)))*100),2))
  etiquetas= c("Masculino", "Femenino")
  etiquetas= paste(etiquetas, porcentajes)
  etiquetas= paste(etiquetas,"%", sep= "")
  pie(porcentajes,etiquetas, main = "Porcentaje de Género", col =c(4,3))
  }
#62.65 hombres, 37.35% mujeres (primera pregunta)

#--------------------
#CAMBIO DE NOMBRES Y MAYUSCULAS EN INSTITUCION

{
  educ= educ %>% rename(institucion=empresa, carrera=posicion)
  educ$institucion= toupper(educ$institucion)
  educ$institucion= iconv(educ$institucion, to="ASCII//TRANSLIT")
  View(educ)
}
#####
{
  educ$institucion[educ$institucion=="PONTIFCIA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="PONFICIA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="PONTIFFICIA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="PONTIFICA UNIVERSIDAD CATOLICA"|
                     educ$institucion=="PONTIFICA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="PONTIFICIA UNIVERISDAD CATOLICA DE CHILE"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD CATOLICA"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD CATOLICA CHILE"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD CATOLICAS DE CHILE"|
                     educ$institucion=="PONTIFICIE UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="PORTIFICIA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="POTINFICIA UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="UNIVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="UNIVERSIDAD CATOLICA"|
                     educ$institucion=="UNIVERSIDAD PONTIFICA CATOLICA DE CHILE"|
                     educ$institucion=="UNIVERSIDAD PONTIFICA CATOLICA DE CHILE -POST GRADO DE INGENERIA"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD DE CHILE"|
                     educ$institucion=="PONTIFICIA UNOVERSIDAD CATOLICA DE CHILE"|
                     educ$institucion=="U. CATOLICA"|
                     educ$institucion=="PUC"|
                     educ$institucion=="U.CATOLICA"|
                     educ$institucion=="P. UNIVERSIDAD CATÓLICA DE CHILE"]="P. UNIVERSIDAD CATOLICA DE CHILE"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD ADOLFO IBAÑEZ"|
                     educ$institucion=="U ADOLFO IBAÑEZ"|
                     educ$institucion=="UNIVERSIDAD ADOLFO IBANES"|
                     educ$institucion=="UNIVERISAD ADOLFO IBANEZ"|
                     educ$institucion=="UNIVERSIDAD ADOLFO IBALEZ"|
                     educ$institucion=="UNIVERSIDAAD ADOLFO IBANEZ"|
                     educ$institucion=="UNIVERSIAD ADOLFO IBANEZ"|
                     educ$institucion=="UNIVERSIDAD ADOLFO IBANEZ"|
                     educ$institucion=="UNIVERSIDFAD ADOLFO IBANEZ"|
                     educ$institucion=="ADOLFO IBANEZ"|
                     educ$institucion=="U. ADOLFO IBANEZ"|
                     educ$institucion=="UAI"]=
    "UNIVERSIDAD ADOLFO IBAÑEZ"
}
{
  educ$institucion[educ$institucion=="UNIVERIDAD DIEGO PORTALES"|
                     educ$institucion=="UDP"]=
    "UNIVERSIDAD DIEGO PORTALES"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD DE CHILE"|
                     educ$institucion=="UNIVERSIDASD DE CHILE"|
                     educ$institucion=="UNIVERSIDAD DE CHILE."|
                     educ$institucion=="UNIVERSISDAD DE CHILE"|
                     educ$institucion=="UNIVERSIDAD, CHILE"|
                     educ$institucion=="ESCUELA DE NEGOCIOS UNIVERSIDAD DE CHILE"|
                     educ$institucion=="U. DE CHILE"|
                     educ$institucion=="UCH"|
                     educ$institucion=="ESCUELA DE NEGOCIOS U DE CHILE"]="UNIVERSIDAD DE CHILE"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD DE SANTIAGO DE CHILE"|
                     educ$institucion=="UNIVERSIDAD SANTIAGO DE CHILE"|
                     educ$institucion=="UNIVERSIDAD DE SANTIAGO"|
                     educ$institucion=="UNIVERSIDAD DE SANTIAGO DE CHHILE"|
                     educ$institucion=="USACH"]=
    "UNIVERSIDAD DE SANTIAGO DE CHILE"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD AUSTRAL"|
                     educ$institucion=="UNIVERSIDAD AUSTRAL DE CHILE"|
                     educ$institucion=="UA"]=
    "UNIVERSIDAD AUSTRAL"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD TECNICO FEDERICO SANTA MARIA"|
                     educ$institucion=="UNIVERSIDAD TECNICA FERERICO SANTA MARIA"|
                     educ$institucion=="UNIVERSIDAD TECNICA FEDERICO SANTA MARIA"|
                     educ$institucion=="UNIVERSIDAD TECNICA FEDERICA SANTA MARIA"|
                     educ$institucion=="UNIVERSIDAD FEDERICO SANTA MARIA"|
                     educ$institucion=="UNIVERSIDAD SANTA MARIA"|
                     educ$institucion=="U. TECNICA FEDERICO SANTA MARIA"|
                     educ$institucion=="U. FEDERICO SANTA MARIA"|
                     educ$institucion=="UTFSM"]=
    "UNIVERSIDAD T. FEDERICO SANTA MARÍA"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD SANTO TOMAS"|
                     educ$institucion=="UNIVERSIDAD SANTO TOMAS (CL)"|
                     educ$institucion=="UST"|
                     educ$institucion=="SANTO TOMAS"|
                     educ$institucion=="SANTO TOMAS CHILE"]=
    "UNIVERSIDAD SANTO TOMAS"
  educ$institucion[educ$institucion=="IP SANTO TOMAS"]="INSTITUTO SANTO TOMAS"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD CENTRAL DE CHILE"|
                     educ$institucion=="UNIVERSIDAD CENTRAL (CL)"|
                     educ$institucion=="UNIVERSIDAD CENTRAL"|
                     educ$institucion=="UCEN"]="UNIVERSIDAD CENTRAL"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD TECNOLOGICA DE CHILE INACAP"|
                     educ$institucion=="UNIVERSIDAD TECNOLOGICA DE CHILE"|
                     educ$institucion=="INACAP UNIVERSIDAD TECNOLOGICA"|
                     educ$institucion=="INACAP CHILE"|
                     educ$institucion=="INACAP"|
                     educ$institucion=="INACAP/GLOBAL QUALITY"]="INSTITUTO INACAP"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD TECNOLOGICO METROPOLITANA"|
                     educ$institucion=="UNIVERSIDAD TECNOLOGICA METROPOLITANA"|
                     educ$institucion=="UNIVERSIDAD TECNOLOGIC METROPOLITANA"|
                     educ$institucion=="UTEM"|
                     educ$institucion=="UTNIVERSIDAD TECNOLOGOCA METROPOLITANA"]=
    "UNIVERSIDAD TECNOLOGICA METROPOLITANA"
}
{
  educ$institucion[educ$institucion=="UNIVERSIDAD NACIONAL ANDRES BELLO"|
                     educ$institucion=="UNAB"]="UNIVERSIDAD ANDRES BELLO"
}
{
  educ$institucion[educ$institucion=="AIEP"|
                     educ$institucion=="INSTITUTO PROFESIONAL AIEP"|
                     educ$institucion=="SONDA TRAINING-AIEP"]=
  "INSTITUTO AIEP"
}
{
  educ$institucion[educ$institucion=="INSTITUTO PROFESIONAL LOS LEONES"]=
    "INSTITUTO LOS LEONES"
}
{
  educ$institucion[educ$institucion=="UANDES"|
                     educ$institucion=="UNIVERSIDAD DE LOS ANDES (CL)"|
                     educ$institucion=="ESE UNIVERSIDAD DE LOS ANDES"|
                     educ$institucion=="UNIVERSIDAD DE LOS ANDES ESE"|
                     educ$institucion=="ESE BUSINESS SCHOOL, UNIVERSIDAD DE LOS ANDES"]=
    "UNIVERSIDAD DE LOS ANDES"
}
{
 educ$institucion[educ$institucion=="UDD"]="UNIVERSIDAD DEL DESARROLLO"
}
{
  educ$institucion[educ$institucion=="USS"|
                     educ$institucion=="UNIVERSIDAD  SAN SEBASTIAN"]="UNIVERSIDAD SAN SEBASTIAN"
}
{
  educ$institucion[educ$institucion=="UNIACC"|
                     educ$institucion=="UNIVERSIDAD DE ARTES, CIENCIAS Y COMUNICACION"]="UNIVERSIDAD UNIACC"
}
{
  educ$institucion[educ$institucion=="DUOC"|
                     educ$institucion=="DUOC UC"|
                     educ$institucion=="DUOCC"]="INSTITUTO DUOC UC"
}

{
  educ$institucion[educ$institucion=="PUCV"|
                     educ$institucion=="U. CATOLICA VALPARAISO"|
                     educ$institucion=="UNIVERSIDAD CATOLICA DE VALPARAISO"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD CATOLICA, VALPARAISO"|
                     educ$institucion=="PONTIFICIA UNIVERSIDAD CATOLICA DE VALPARAISO"|
                     educ$institucion=="PONTIFICA UNIVERSIDAD CATOLICA DE VALPARAISO"]=
    "P. UNIVERSIDAD CATOLICA DE VALPARAISO"
}
{
  educ$institucion[educ$institucion=="UAH"]="UNIVERSIDAD ALBERTO HURTADO"
  educ$institucion[educ$institucion=="UDLA"]="UNIVERSIDAD DE LAS AMERICAS"
}
##### 
#ESTANDARIZAR POSICION

employ$posicion= toupper(employ$posicion)
employ$posicion= iconv(employ$posicion, to="ASCII//TRANSLIT")
View(employ)
employ$posicion[employ$posicion= starts_with("GERENTE")]="GERENTE GENERAL"

#####
educ$ae= educ$fina- educ$inicioa
at= educ %>% group_by(ae)
at %>% summarise(ae=(sum(ae)))
View(at)
at
View(educ)
{
  educ$inicioa= as.numeric(educ$inicioa)
  educ$fina= as.numeric(educ$fina)
  mibase= educ
  mibase$ae= educ$fina - educ$inicioa
  mibase= distinct(mibase, exec_id, ae, institucion)
  View(mibase)
  Ranking= table(educ$institucion)
  View(Ranking)
  Ranking= data.frame(Ranking)
  View(base)
}
{
  univer <- educ %>%
    group_by(`Nombre Institución`) %>%
    summarise(sum(`Nº Laboratorios`))
  # Copiado de Connie
} 
#####

# Comentario de prueba ignacio :3
