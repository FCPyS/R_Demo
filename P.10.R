
# Práctica 10: Pirámides y mapas 


## Paquetería
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, 
              readxl,writexl,googlesheets4, # importar hojas de cálculo
              haven, foreign, # importación de dta y sav
              sjlabelled, # etiquetas
              janitor, skimr, #limpieza y verificación
              imputeTS, # para imputar valores
              srvyr, # Para el diseño muestral
              esquisse, # para usar ggplot de manera más amigable
              #DescTools, # Paquete para estimaciones y pruebas
              #infer, # tidy way 
              #broom,  # Una escobita para limpiar (pero es para arreglar)
              #estimatr, car, stargazer, ggpubr, # Para la regresión práctica 7
              #jtools, lm.beta, robustbase, sandwich, effects,
              #officer,flextable,huxtable, ggstance, kableExtra, # Para la regresión práctica 8
              #ResourceSelection, lmtest, mlogit, nnet # Práctica 9
              apyramid)


rm(list=ls()) # mala práctica pero quizás necesaria porque vamos a cambiar de tema
setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")

pob_mit_proyecciones <- read.csv("https://github.com/aniuxa/R_Demo/raw/master/datos/pob_mit_proyecciones.csv", encoding = "latin1")
pob_mit_proyecciones<-clean_names(pob_mit_proyecciones)

ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")

skimr::skim(pob_mit_proyecciones)

pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>% 
  count(edad, sexo, wt=poblacion)


pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% # República Mexicana
  filter(ano==2020) %>% # Año
  ggplot(
    aes(edad, weight=poblacion)
  ) + geom_histogram(binwidth = 5)



pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>% 
  ggplot(
    aes(edad, weight=poblacion)
  ) + geom_histogram(binwidth = 10)  + # verificar el ancho de clase
  coord_flip() + theme_light()




pob_mit_proyecciones<-pob_mit_proyecciones %>% 
  mutate(eda5=cut(edad, # la variable a cortar
                  breaks=c(0,1,seq(5,110, # El rango válido
                             by=5)), # El ancho del intervalo
                  include.lowest=T, # para que incluya el valor más bajo dentro del intervalo 
                  right=F)) # indica si el intervalo irá abierto en la derecha, ponemos un no con "FALSE" 





### gráfico de barras de edades quinquenales
pob_mit_proyecciones %>%   
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>% 
  ggplot(aes(eda5, weights=poblacion))+
  geom_bar() # dibuja la geometría de barra


pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>% 
  mutate(poblacion=if_else(
    sexo=="Hombres",
    -poblacion, 
    poblacion)) %>%  # una variable accesoria
  ggplot(aes(eda5, fill=sexo, weights=poblacion))+ # fill sexo
  geom_bar() # dibuja la geometría de barra


pob_mit_proyecciones<-pob_mit_proyecciones %>% 
  mutate(poblacion2=if_else(sexo=="Hombres", -poblacion, poblacion))

pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>%  
  ggplot(aes(eda5, fill=sexo, weights=poblacion2))+
  geom_bar() + coord_flip() +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() 

pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>%  
  count(eda5, sexo, wt=poblacion2)%>% 
  summarise(max=max(n), min=min(n))


pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>%  
  ggplot(aes(eda5, fill=sexo, weights=poblacion2))+
  geom_bar() + coord_flip() +
  scale_y_continuous(breaks = seq(-6000000, 6000000, by=1000000), # cuántos 
                     limits = c(-6000000,6000000),
                     labels = paste0(
                       as.character(c(6:0,# sustituye negativos
                                      1:6) # Para lo positivo 
                       ) 
                     ) 
  )+ 
  labs(y="Poblacion - millones", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() 



pob_mit_proyecciones<-pob_mit_proyecciones %>% 
  group_by(cve_geo, ano) %>% #agrupa por año y entidad
  mutate(p_edo=sum(poblacion))%>% 
  ungroup() # paso importante

head(pob_mit_proyecciones)

pob_mit_proyecciones<-pob_mit_proyecciones %>% 
  mutate(poblacion3=poblacion2/p_edo)


pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>%  
  ggplot(aes(eda5, fill=sexo, weights=poblacion3))+
  geom_bar() + coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01))+ 
  labs(y="Poblacion - %", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() +   theme(axis.text.x  =  element_text(angle = 90))  



pob_mit_proyecciones %>% 
  filter(cve_geo==30) %>% 
  filter(ano%in%c(2020, 2030, 2040, 2050)) %>%  
  ggplot(aes(eda5, fill=sexo, weights=poblacion3))+
  geom_bar() + coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy=0.01))+ 
  labs(y="Poblacion - %", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light()   +
  facet_wrap(~ano)


# Mi función 

mi_funcion<-function(x) {
  x<-x+1
  x
}
mi_funcion(5)

mi_funcion<-function(x,a) {
  x<-x+a
  x
}

mi_funcion(6,8)


piramide<-function(ent, ano) {
  x<-ent
  a<-ano 
  pob_mit_proyecciones %>% 
    filter(cve_geo==x) %>% 
    filter(ano==a) %>%  
    ggplot(aes(eda5, fill=sexo, weights=poblacion3))+
    geom_bar() + coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy=0.01))+ 
    labs(y="Poblacion - %", x="Grupos de edad") +
    scale_fill_brewer(palette = "Set2") + 
    theme_light()   
}

piramide(1, 2050) + ggtitle("Aguascalientes, 2050")

# Datos no agrupados

ecovid0420 %>% 
  mutate(dummy=if_else(pb1==1,
                       -1, # Mover a los hombres a negativo
                       1)) %>% 
  ggplot(aes(pb2, 
             fill=as_label(pb1), 
             weights=dummy))+
  geom_histogram(binwidth = 5) + coord_flip() 


# Paquete apyramid

pira <- pob_mit_proyecciones %>% 
  filter(cve_geo==0) %>% 
  filter(ano==2020) %>%
  age_pyramid(eda5, # edad
              split_by = sexo,# sexo
              count=poblacion) # weights, porque son agrupados

pira + theme_minimal()



pira2 <- ecovid0420 %>% 
  mutate(pb2=as_factor(pb2),
         pb1=as_label(pb1)) %>% 
  age_pyramid(pb2, split_by = pb1)
  

pira2 + theme_minimal()

## Mapas 

#if (!require("devtools")) {
#  install.packages("devtools")
# }
#devtools::install_github("diegovalle/mxmaps")
library(mxmaps)

url <- "https://github.com/aniuxa/R_Demo/raw/master/datos/ICE_2018.xlsx"
destfile <- "ICE_2018.xlsx"
curl::curl_download(url, destfile)

ICE_2018 <- read_excel(destfile, sheet = "para_importar")
ICE_2018 <- clean_names(ICE_2018) # limpia los nombres


data("df_mxstate") # carga la base estatal del paquete
glimpse(df_mxstate)

df_mxstate$value<-df_mxstate$pop

mxstate_choropleth(df_mxstate,
                   title = "Total población, por Estado") 


ICE_2018$value<- ICE_2018$homicidios

mxstate_choropleth(ICE_2018,
                   title = "Tasa de homicidios") 

ggsave(plot=last_plot(), #objeto donde está el gráfico
       device="png", # formato del gráfico
       filename = "mapa.png") # nombre el archivo de salida


data("df_mxmunicipio")
glimpse(df_mxmunicipio)




df_mxmunicipio$value <- df_mxmunicipio$indigenous
mxmunicipio_choropleth(df_mxmunicipio, 
                       zoom = subset(df_mxmunicipio, state_name %in% c("Veracruz"))$region,
                       n=1)


