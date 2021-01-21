# ==============================================================================
# Fecha: 2021-01-21 
# Sesión 14 - No tenemos libreta
# Si me van entregar código que tenga un encabezado porfa :)
# Autora: Ana Escoto
# ==============================================================================

# Paquetes                        -----------------------------------------------
if (!require("pacman")) install.packages("pacman") #instala pacman si se requiere
#                                       no me paso de aquí al escribir --------->
pacman::p_load(tidyverse, readxl,
               rmarkdown,
               rticles,
               rmdfiltr, # conteo de palabras
               prettydoc, tufte)


# Veremos las cosas más simples de Rmarkdown ------------------------------------
#https://rmarkdown.rstudio.com/lesson-1.html

# Algunas modificaciones al YAML (yet another markdown language)

# Para las presentaciones esto está padre
#https://hartwork.org/beamer-theme-matrix

#https://learnxinyminutes.com/docs/yaml/


# Vamos a ver cómo funciona rticles --------------------------------------------

# Aquí agregamos el conteo de palabras
# https://cran.r-project.org/web/packages/rmdfiltr/vignettes/wordcount.html

# Publicando en RPubs ----------------------------------------------------------

# Haciéndolo pretty el html ----------------------------------------------------

#https://github.com/yixuan/prettydoc/


# El Señor Tufte

# https://bookdown.org/yihui/rmarkdown/tufte-handouts.html
