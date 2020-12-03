
#Paqueterías

#install.packages("sjPlot", dependencies=T) # solito porque da problmas

library(sjPlot)

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
               officer,flextable,huxtable, ggstance, kableExtra, # Para la regresión práctica 8
               ResourceSelection, lmtest, mlogit, nnet) # Práctica 9

setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")

ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")

mydata<- ecovid0420 %>% 
  mutate(pb1=as_label(pb1)) %>%  # Para hacer gráficos sin problemas
  select(ent, pa1,starts_with("pb"), clase1, clase2, clase3, fac_per)

mydata$y_binomial<-NA
mydata$y_binomial[mydata$clase1==1]<-1 # Población económicamente activa (Ocupados + desempleados)
mydata$y_binomial[mydata$clase1==2]<-0 # Población NO económicamente activa (disponibles y no disponibles)

newdata <- na.omit(mydata)

modelo0<-glm(y_binomial ~ pb2, 
             family = binomial("logit"),
             data=newdata, 
             na.action=na.exclude)

summary(modelo0)

confint(modelo0)

summ(modelo0)

# Predicción de probabilidades ----

range(newdata$pb2)
xpb2 <- 18:98

y_logito <- predict(modelo0, list(pb2 = xpb2))
y_prob<- predict(modelo0, list(pb2 = xpb2), type= "response")

results_m0<-cbind(y_logito, y_prob, xpb2)
results_m0<-as.data.frame(results_m0)

ggplot(data=results_m0, aes(x=xpb2, y=y_logito)) +
  geom_point()

exp(
  coef(modelo0)
  )

summ(modelo0, exp=T ) # exponencia los coeficentes.

# Agregando predictores ----
modelo1<-glm(y_binomial ~ pb2 + pb1, # formula de mi modelo
             family = binomial(link="logit"), # familia y el enlace del modelo
             data=newdata, # objeto de mi base de datos
             na.action=na.exclude) # sobre los missings
summary(modelo1)

confint(modelo1)

summ(modelo1, exp=T)

lrtest0<-lrtest(modelo0, modelo1)
lrtest0



modelo2<-glm(y_binomial ~ pb2 + pb1 + pa1,
             family = binomial("logit"), 
             data=newdata, 
             na.action=na.exclude)
summary(modelo2)

lrtest1<-lrtest(modelo1, modelo2)
lrtest1

hoslem.test(newdata$y_binomial, fitted(modelo2))

# En la pelea entre test, gana el lrtest! 

# Tablita de modelos ----

stargazer(modelo0, modelo1,modelo2, 
          type = 'text', header=FALSE)


stargazer(modelo0, modelo1,modelo2, 
          type = 'text', header=FALSE,
          apply.coef = exp)


export_summs(modelo0, modelo1,modelo2, exp=T)

plot_model(modelo2)

get<-plot_model(modelo2)
get$data

objeto<-
  plot_model(modelo2, 
           terms= c("pb2[20,50]", "pb1"), 
           type="pred")
objeto$data

predict(modelo2, list(pb2 = 65, pb1="Hombre", pa1=3), type= "response")


plot_models(modelo1, modelo2) + ggtitle("P(actividad económica)")


# Probit ----

mprobit<-glm(y_binomial ~ pb2, 
             family = binomial("probit"), # cambia la función de enlace
             data=newdata,
             na.action=na.exclude)
summary(mprobit)


stargazer(modelo0, mprobit,   type = 'text', header=FALSE)

#Regresión logística multinomial

mlogit <- multinom(clase2 ~ pb1 + pb2, data = newdata)

summary(mlogit)

exp(coef(mlogit))
