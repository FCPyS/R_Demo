
# Paquetería
install.packages("sjPlot", dependencies=T) # solito porque da problmas
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
               estimatr, car, stargazer, ggpubr) # Para la regresión


setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")

ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")

lienzo_bi <-ecovid0420 %>% 
  filter(clase2==1  & !pe10_1==0) %>% 
  ggplot(aes(x=pe10_1, fill=as_factor(pos_ocu), 
             color=as_factor(pos_ocu),
             alpha=I(0.5)))

lienzo_bi + geom_density()

tidy(anova)



anova<-ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(aov(pe10_1 ~ as_factor(pos_ocu)))


TukeyHSD(anova)
summary(anova)


# Supuestos de la anova

ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(bartlett.test(pe10_1 ~ as_factor(pos_ocu)))

# No cumplo con el supuesto :(

ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(shapiro.test(pe10_1))


# Kruskall-Wallis

kruskal<-ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(kruskal.test(pe10_1 ~ as_factor(pos_ocu)))

kruskal
tidy(kruskal)


ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(DunnTest(pe10_1 ~ as_factor(pos_ocu)))

ecovid0420 %>% 
  filter(clase2==1) %>% 
  ggpubr::ggviolin(x = "pos_ocu", y = "pe10_1", fill = "pos_ocu",
                   add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(label.y = 120)  # Add the p-value 

comparaciones <- list( c("1", "2"), c("2", "3"), c("1", "3") )


ecovid0420 %>% 
  filter(clase2==1) %>% 
  ggpubr::ggviolin(x = "pos_ocu", y = "pe10_1", fill = "pos_ocu",
    #               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = comparaciones, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 120)     # Add global the p-value 



# Sub-setting para modelos

mydata<- ecovid0420 %>% 
  filter(clase2==1) %>% # me quedo con los ocupados
  select(ent, pa1,starts_with("pb"), pos_ocu, pe10_1, fac_per, pa4_1)

tail(mydata)


gg <- ggplot(mydata, aes(pb2, log(pe10_1)))
gg +  geom_jitter()


cor(mydata$pe10_1, mydata$pb2,  use = "pairwise")

cor.test(mydata$pe10_1, mydata$pb2, use="pairwise.complete.obs")

cor_test<-mydata %>% 
  with(cor.test(mydata$pe10_1, mydata$pb2, use = "pairwise")) # prueba de hipótesis.

#dos modos de visualizar el resultado
cor_test 


tidy(cor_test)
hist(mydata$pe10_1)
hist(log(mydata$pe10_1))


mydata$log_hrs<-log(mydata$pe10_1)

modelo <-lm(log_hrs ~pb2, data=mydata)

summary(modelo) # resultado forma1

tidy(modelo) # Pruebas de hipótesis de los coeficientes

confint(modelo, level=.99)
glance(modelo) # resultado ajuste global
anova(modelo)

# Diagnóstico

plot(modelo)

# Outliers o datos atípicos.

outlierTest(modelo)
out<-outlierTest(modelo) # guardamos en objeto

ggpubr::ggqqplot(mydata$log_hrs)
car::qqPlot(modelo, main="QQ Plot") #qq plot for studentized resid


ncvTest(modelo)
spreadLevelPlot(modelo)


### Modelo lineal múltiple

modelo1<-lm(log_hrs ~ pb2 + as_label(pb1), data=mydata, na.action=na.exclude)
summary(modelo1)


pruebaf0<-anova(modelo, modelo1)
pruebaf0


modelo2<-lm(log_hrs ~ pb2 + as_label(pb1) + pa1, data=mydata, na.action=na.exclude)
summary(modelo2)

pruebaf1<-anova(modelo1, modelo2)

vif(modelo2)

# Para mostrar resultados

stargazer(modelo, modelo1, modelo2, type = 'latex', header=F)

#library(sjPlot)

plot_model(modelo2) + theme_minimal() + ggtitle("Este es mi resultado")

plot_models(modelo, modelo1, modelo2)

