
# Sesión 5 ----
# Paquetes
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, 
               readxl,writexl,googlesheets4, # importar hojas de cálculo
               haven, foreign, # importación de dta y sav
               sjlabelled, # etiquetas
               janitor, skimr, #limpieza y verificación
               imputeTS, # para imputar valores
               srvyr, # Para el diseño muestral
               esquisse, RColorBrewer) # para usar ggplot de manera más amigable


setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")


# Bases
ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")

url <- "https://github.com/aniuxa/R_Demo/raw/master/datos/ICE_2018.xlsx"
destfile <- "ICE_2018.xlsx"
curl::curl_download(url, destfile)
ICE_2018 <- read_excel(file.choose(), sheet="para_importar")
ICE_2018 <- clean_names(ICE_2018) # limpia los nombres

# Gráfico simple

plot(as_label(ecovid0420$pb3))
barplot(table(as_label(ecovid0420$pb3), as_label(ecovid0420$pb1)))

hist(ecovid0420$pb2)
boxplot(ecovid0420$pb2)

#Gráficos ggplot ----

g1<-ecovid0420 %>%
  ggplot(aes(as_label(pb3)))

g1 # imprime el lienzo

g1 + geom_bar(aes(weight = fac_per))

g1 +  geom_bar(aes(fill = as_label(pb3))) # colorea la geometría



g2<-ecovid0420 %>%
  ggplot(aes(pb2))

g2 # imprime el lienzo
g2 +  geom_histogram(binwidth = 5)
g2 +  geom_histogram(bins = 10, aes(weight = fac_per))


g2 + geom_density()

# Gráfico bivariados

g1 +  geom_bar(aes(fill = as_label(pb1)),
               position="dodge") #pone las categorías lado a lado y no apiladas


g_bivariado <- g1 +  geom_bar(aes(fill = as_label(pb1)),
                              position="fill") # cada categoría "llena" a una unidad

g_bivariado

g_bivariado + scale_fill_brewer(palette = "Accent") # lo nuevo

g_bivariado + scale_fill_brewer(palette = "Dark2") + theme_minimal()

library(esquisse)

ggplot(ecovid0420) +
 aes(x = as_label(pb3), fill = as_label(pb1)) +
 geom_bar() +
 scale_fill_viridis_d(option = "viridis") +
 labs(x = "ESCOLARIDAD", y = "FRECUENCIA", title = "Este es un título", subtitle = "Este un subtítulo", caption = "Fuente: ECOVIDML-2020", fill = "SEXO") +
 coord_flip() +
 theme_minimal()



## Scatters

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_point() #puntito

ICE_2018 %>% 
  ggplot(aes(homicidios, percepcion_de_seguridad)) +
  geom_jitter() # puntito pero "separado"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_text(aes(label=edo2)) # un texto en lugar de punto

# geometría "label"

ICE_2018 %>% 
  ggplot(aes(homicidios,percepcion_de_seguridad)) +
  geom_label(aes(label=edo2)) # etiqueta


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2)
  ) +
  geom_point()


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             shape=region2)
  ) +
  geom_point(size = 1.5) # ojo, nos da un "warning"


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() + facet_wrap(~region2, nrow=2, ncol=4)


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() + facet_grid(region2~as_factor(matrimonio_igualitario))


## Smooth

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(region2~.)


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2)) +
  geom_text(aes(label=edo2)) +
  geom_smooth(method="lm", se=F) + scale_fill_brewer(palette = "Dark2") +
  theme_minimal()


ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2)) +
  geom_point(aes(size=costos_del_delito))+ # ojo
  theme_minimal()

ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2,
             size=costos_del_delito)) +
  geom_point()+ 
  theme_minimal()



ICE_2018 %>% 
  ggplot(aes(x=homicidios,
             y=percepcion_de_seguridad,
             color=region2,
             size=costos_del_delito, alpha=I(0.8))) +
  geom_text(aes(label=edo2),
            check_overlap = TRUE)+
  theme_minimal()


ecovid0420 %>% 
  ggplot(aes(pb2, pe10_1, alpha=I(0.5))) +
  geom_jitter()