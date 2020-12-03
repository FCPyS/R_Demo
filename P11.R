# Práctica 11

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
               #DescTools, # Paquete para estimaciones y pruebas
               #infer, # tidy way 
               #broom,  # Una escobita para limpiar (pero es para arreglar)
               #estimatr, car, stargazer, ggpubr, # Para la regresión práctica 7
               #jtools, lm.beta, robustbase, sandwich, effects,
               #officer,flextable,huxtable, ggstance, kableExtra, # Para la regresión práctica 8
               #ResourceSelection, lmtest, mlogit, nnet # Práctica 9
               apyramid, LexisPlotR, ipumsr,  fmsb ) # Práctica 10 y 11


# Lexis! ----

# Dibuje una cuadrícula de Lexis desde el año 2010 hasta el año 2015, que representa las edades de 0 a 5 años.
lexis_grid(year_start = 2010, year_end = 2015, age_start = 0, age_end = 5) 
lexis_grid(year_start = 2010, year_end = 2015, age_start = 2, age_end = 10)


lexis_grid(year_start = 1950, year_end = 2000, age_start = 0, age_end = 50, delta = 5)


mi_diagrama <- lexis_grid(year_start = 1995, year_end = 2000, age_start = 0, age_end = 5)
class(mi_diagrama)
mi_diagrama

# edad ----
# Destacar todos los puntos que pertenecen a la edad de 2 años
lexis_age(lg = mi_diagrama, age = 2)

lexis_age(lg = mi_diagrama, age = 2, fill = "red", alpha=0.5)

# periodo ----
lexis_year(lg = mi_diagrama, year=1998)
lexis_year(lg = mi_diagrama, year=1998, fill = "grey70", alpha = 0.5)

# cohorte ----
lexis1<-lexis_cohort(lg = mi_diagrama, cohort=1994) 
lexis_cohort(lg = lexis1, cohort=1993) 


lexis_cohort(lg = mi_diagrama, cohort=1994, fill="plum1", alpha=0.8)

# Líneas de vida ----

lexis_lifeline(lg = mi_diagrama, birth = "1996-09-23")
lexis_lifeline(lg = mi_diagrama, birth =  "1996-09-23", exit="1999-09-23")


polygons <- data.frame(group = c(1,
                                 1,
                                 1), # es un triángulo
                       x = c("1996-01-01", 
                             "1997-01-01", 
                             "1997-01-01"), # van en fechas
                       y = c(1,
                             1,
                             2)) # van en edades

lexis_polygon(lg = mi_diagrama, x = polygons$x, y = polygons$y, group = polygons$group)


polygons <- data.frame(group = c(1,
                                 1,
                                 1, # es un triángulo
                                 2, 
                                 2,
                                 2), # es otro triángulo
                       x = c("1996-01-01", 
                             "1997-01-01", 
                             "1997-01-01", # van en fechas 1
                             "1998-01-01", 
                             "1998-01-01", 
                             "1999-01-01"), # van en fechas 2
                       y = c(1,
                             1,
                             2, # van en edades 1
                             3,
                             4,
                             4))# van en edades 2

lexis1<-lexis_cohort(lg=mi_diagrama, cohort=1995)
lexis_polygon(lg = lexis1, x = polygons$x, y = polygons$y, group = polygons$group)



anotaciones <- data.frame(angle = c(0, # sentido anotacion 1
                                45), # sentido anotacion2
                       x = c("1996-07-01",  # van en fechas 1
                             "1998-01-01"), # van en fechas 2
                       y = c(1.25, #edad1
                             3.5),# edad2
                       anota= c("Hola",
                                "Adiós"))

lexis_polygon(lg = lexis1, x = polygons$x, y = polygons$y, group = polygons$group) + 
 geom_text(data=anotaciones,
           aes(x=as.Date(x), y=y, 
               label=anota,
               angle=angle),
                size=7)


mi_diagrama2 <- lexis_grid(year_start = 1995, year_end = 2000, age_start = 0, age_end = 5)
mi_diagrama2<-lexis_age(lg = mi_diagrama2, age = 2, fill = "red", alpha = 0.5)
mi_diagrama2<-lexis_year(lg = mi_diagrama2, year = 1998)
mi_diagrama2<-lexis_cohort(lg = mi_diagrama2, cohort=1994)
mi_diagrama2<-lexis_lifeline(lg = mi_diagrama2, birth = "1996-09-23", exit="1999-09-23")
mi_diagrama2 + geom_text(data=anotaciones,
                         aes(x=as.Date(x), y=y, 
                             label=anota,
                             angle=angle),
                         size=7)


mi_diagrama  + 
  annotate("rect", 
           xmin = as.Date("1996-01-01"),
           xmax = as.Date("1997-01-01"), 
           ymin = 0, ymax = 2, alpha = .2)


mi_diagrama  + 
  geom_vline(xintercept = as.Date("1995-11-05"), colour = "red")

mi_diagrama  + 
  geom_hline(yintercept = 4, colour = "red")

# IPUMS ----
# Establecer el directoria


#NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("ipumsi_00006.xml")
data <- read_ipums_micro(ddi)

skimr::skim(data)


data <- data %>%
  mutate(COUNTRY_factor = as_label(COUNTRY),
         sex0=as_label(SEX),
         eda5=as_label(AGE2))

data %>% 
  mutate(pop_pi=if_else(SEX==1,
                        -PERWT, 
                        PERWT)) %>% 
  ggplot(aes(eda5, fill=sex0, weights=pop_pi))+
  geom_bar() + coord_flip() +
  labs(y="Poblacion", x="Grupos de edad") +
  scale_fill_brewer(palette = "Set2") + 
  theme_light() + facet_wrap(~COUNTRY_factor, scales="free") 

