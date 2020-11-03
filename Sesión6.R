#Sesión 6

# Paquetería

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
               broom) # Una escobita para limpia

setwd("/Users/anaescoto/Dropbox/2020/2021-1 R para Demográfos/repo/R_Demo")

ecovid0420 <- read_dta("https://github.com/aniuxa/R_Demo/raw/master/datos/ecovid0420.dta")

# t.test ####

t.test(ecovid0420$pe10_1)

# Ho=40 horas
# Ha distinto a 40 horas
t.test(ecovid0420$pe10_1, mu=40)

t.test(ecovid0420$pe10_1, mu=40, alternative = "two.sided") #default y de dos colas
t.test(ecovid0420$pe10_1, mu=40, alternative = "less") # cola izquierda
t.test(ecovid0420$pe10_1, mu=40, alternative = "greater") #cola derecha 

t.test0<-t.test(ecovid0420$pe10_1, mu=40, alternative = "less", conf.level=0.90)
t.test0
t.test0$stderr

tidy(t.test0)

ecovid0420 %>% 
  t_test(response = pe10_1, mu = 40, alternative ="greater", conf_level=0.99)

ecovid0420 %>% 
  t_stat(response = pe10_1, mu = 40)


ecovid0420 %>% 
  with(t.test(pe10_1))


# Proporciones

table(ecovid0420$clase1)

prop.test(table(ecovid0420$clase1))



prop.test(3095, 3095+2498)


### Diferencias de medias

ecovid0420 %>% 
  filter(ecovid0420$clase2==1) %>% # nos quedamos con los trabajadores
  group_by(as_label(pb1)) %>%
  summarise(avg_hrs = mean(pe10_1, na.rm=T))


ecovid0420 %>% 
  filter(ecovid0420$clase2==1) %>%
  with(t.test(pe10_1 ~ pb1))

# infer
ecovid0420 %>% 
  mutate(pb1=as_label(pb1)) %>% 
  t_test(pe10_1 ~ pb1, order = c("Mujer", "Hombre") )


## Estimaciones para varianzas #####

VarTest(ecovid0420$pe10_1)

ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(VarTest(pe10_1))

ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(VarTest(pe10_1, sigma.squared = 100))

test2<-ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(VarTest(pe10_1))
test2$conf.int

sqrt(test2$conf.int) ## sacamos la raíz cuadrada para tener las

rbind(tidy(t.test0), tidy(test2))

#ESTO NO SE HACE
ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(var.test(x=pe10_1, y=pb2, ratio=1))


ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(var.test(pe10_1 ~ as_label(pb1), ratio=1))


# Prueba chi-cuadrado

table(ecovid0420$clase2, ecovid0420$pb1)
chi<-chisq.test(ecovid0420$clase2, ecovid0420$pb1)

chi$residuals
chi$expected
chi$observed


ecovid0420 %>% 
  dplyr::mutate_all(vars(clase2, pb1), as_factor) %>% 
  tabyl(clase2, pb1) %>% 
  janitor::chisq.test() #ojo

ecovid0420 %>% 
  mutate(clase2=as.factor(clase2)) %>% 
  mutate(pb1=as.factor(pb1)) %>% 
  chisq_test(clase2 ~ pb1)

tidy(chi)

# ANOVA

lienzo_bi <-ecovid0420 %>% 
  filter(clase2==1  & !pe10_1==0) %>% 
  ggplot(aes(x=pe10_1, fill=as_factor(pos_ocu), 
             color=as_factor(pos_ocu),
             alpha=I(0.5)))

g_anova<-lienzo_bi + geom_density()
g_anova

ggsave(g_anova,
       device="png",
       filename = "/Users/anaescoto/Dropbox/2020/anova.png")


anova<-ecovid0420 %>% 
  filter(clase2==1) %>% 
  with(aov(pe10_1 ~ as_factor(pos_ocu)))

anova
summary(anova)

tidy(anova)

TukeyHSD(anova)



