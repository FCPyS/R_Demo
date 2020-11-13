# Práctica 8 ----

#install.packages("sjPlot", dependencies=T) # solito porque da problmas

library(sjPlot)

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, 
              readxl,writexl,googlesheets4, # importar hojas de cálculo
              haven, foreign, # importación de dta y sav
              sjlabelled, # etiquetas
              janitor, skimr, #limpieza y verificación
              imputeTS, # para imputar valores
              srvyr, # Para el diseño muestral
              esquisse, # para usar ggplot de manera más amigable
              DescTools, # Paquete para estimaciones y pruebas
              infer, # tidy way 
              broom,  # Una escobita para limpiar (pero es para arreglar)
              estimatr, car, stargazer, ggpubr, # Para la regresión práctica 7
              jtools, lm.beta, robustbase, sandwich, effects, 
              officer,flextable,huxtable, ggstance, kableExtra) # Para la regresión práctica 8


setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")

ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")


mydata<- ecovid0420 %>% 
  filter(clase2==1) %>% # me quedo con los ocupados
  mutate(pb1=as_label(pb1)) %>%  # Para hacer gráficos sin problemas
  select(ent, pa1,starts_with("pb"), pos_ocu, pe10_1, fac_per, pa4_1)

mydata$log_hrs<-log(mydata$pe10_1)

modelo2<-lm(log_hrs ~ pb2 + pb1 + pa1, data = mydata, # es ligeramente diferente al de la clse pasada
            na.action = na.exclude)
summary(modelo2)


jtools::summ(modelo2)

tidy(modelo2) %>%
  kbl(caption = "Modelo") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# Estandarización

lm.beta(modelo2)
modelo_beta<-lm.beta(modelo2)
modelo_beta

plot_model(modelo2, type="std") + theme_minimal() +
  ggtitle("Coeficientes asociados al logaritmo de horas trabajadas") + labs(y="Estimadores")


sjPlot::plot_model(modelo2, type="pred", terms = "pb2")

sjPlot::plot_model(modelo2, type="pred", terms = "pb1")

plot_model(modelo2, type="pred", terms = c("pb2","pb1")) + theme_blank()

plot_model(modelo2, type="pred", terms = c("pb1","pb2")) + theme_blank()


plot_model(modelo2, type="eff", terms = "pb2")
plot_model(modelo2, type="eff", terms = "pb1")

eff<-plot_model(modelo2, type="eff", terms = "pb2")

eff$data

## Interacciones
modelo_int1<-lm(log_hrs ~ pb2 * pb1 , data = mydata, na.action=na.exclude)
summ(modelo_int1)


plot_model(modelo_int1, type="int", terms = c("pb1", "pb2"))

## Efectos no lineales

modelo_log<-lm(log_hrs ~ log(pb2) + pb1, data = mydata, na.action = na.exclude)
summary(modelo_log)

modelo_log2<-lm(log(pe10_1) ~ log(pb2) + pb1, data = mydata, na.action = na.exclude)
summary(modelo_log2)


plot_model(modelo_log, type="pred", terms ="pb2")
plot_model(modelo_log2, type="pred", terms ="pb2")

plot_model(modelo_log2, type="pred", terms ="pb1")


modelo_quadr<-lm(log_hrs ~ pb2 + I(pb2^2) + pb1, 
                 data=mydata, 
                 na.action=na.exclude)
summary(modelo_quadr)
vif(modelo_quadr)

plot_model(modelo_quadr, type="pred", terms = c("pb2"))

## Heterocedasticidad

modelo2rob1 <- estimatr::lm_robust(log_hrs ~ pb2 + pb1 + pa1, data = mydata)
summary(modelo2rob1)
tidy(modelo2rob1)


# cluster robust standard errors
modelo2rob2<- lm_robust(log_hrs ~ pb2 + as_label(pb1) + pa1, data = mydata, clusters = ent)
# standard summary view also available
summary(modelo2rob2)

# jtools
summ(modelo2) # 
summ(modelo2, robust = "HC1") # sandwich -- equivalente "robust"
summ(modelo2, robust = "HC2") # sandwich -- equivalente "robust"


summ(modelo2, scale=T) # Estandarizado

summ(modelo2,
     robust="HC1", # errores robustos (sandwich)
     scale=T, # Estandarizado
     confint=TRUE, # intervalos de confianza
     digits=4, # número de dígitos
     vifs=TRUE) # multicolinealidad

jt_modelo<-summ(modelo2,
       robust="HC1", # errores robustos (sandwich)
       scale=T, # Estandarizado
       confint=TRUE, # intervalos de confianza
       digits=4, # número de dígitos
       vifs=TRUE) # multicolinealidad

jt_modelo$coeftable
jt_modelo$model$residuals


## Regresión robusta a los outliers (datos atípicos)

library(robustbase)

modelo2rob3<-lmrob(log_hrs ~ pb2 + pb1+ pa1, data = mydata, 
                   na.action = na.exclude)
summary(modelo2rob3)

# Comparando modelos

stargazer(modelo2, modelo2rob3, type = 'text', header=FALSE)


jtools:::export_summs(modelo2, modelo2rob1, modelo2rob2, modelo2rob3)


jtools::export_summs(modelo2, modelo2rob1, modelo2rob2, modelo2rob3, 
                     to.file = "HTML", file.name = "hola.html")

jtools::export_summs(modelo2,to.file = "HTML", file.name = "hola.html")


# Extra

jtools::plot_summs(modelo2, modelo_quadr,
           scale=T,
           #plot.distributions = TRUE, 
           inner_ci_level = .95)


plot_models(modelo2, modelo_quadr)

