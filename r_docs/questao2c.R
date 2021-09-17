# QUESTA0 2 c  ----

# d) Compare a estrutura de mortalidade por causas (Capítulos da CID10 - reagrupados em 
# 6/7 grandes causas) segundo sexo de 2015 e 2020. Comente os resultados. Destacar a mortalidade 
# por Covid-19.


library(tidyverse)
library(viridis)
library(readxl)



### Leitura dos dados ---

dados_sim <- readRDS("./data/SIM/sim_consolidado.rds")

dados_nasc <- readRDS("./data/sinasc/sinasc_consolidado.rds")

pop_ac_sx_etaria <- read.csv("./data/pop_AC_sexo_etaria.csv")

