library(read.dbc)
library(dplyr)
library(stringr)
library(foreign)
library(LexisPlotR)
library(lubridate)
library(visdat)
library(ggplot2)
library(tidyverse)

sim_consolidado <- readRDS("./data/SIM/sim_consolidado.rds")

# Preparação do banco: Idade de código para número, remoção de variáveis que não vão ser utilizadas,
# introdução de um ifelse comparativo para definir se a pessoa nasceu antes ou depois da metade exata do ano
# para posterior plotagem, filtro para nascidos após 1999, conforme se pede nos exercícios, etc
sim_lexis<-sim_consolidado %>% select(DTOBITO,DTNASC,CODMUNRES,IDADE) %>%
  mutate(DTOBITO=dmy(DTOBITO),
         DTNASC=dmy(DTNASC))
sim_consolidado$IDADE <- as.numeric(as.character(sim_consolidado$IDADE))

sim_lexis$IDADEA <- (sim_consolidado$IDADE - 400)

sim_lexis$IDADEA[sim_lexis$IDADEA < 0] <- 0 
sim_lexis$IDADEA[sim_lexis$IDADEA > 200] <- NA 
summary(sim_lexis$IDADEA)

sim_lexis$DTOBITO <- as.Date(sim_lexis$DTOBITO)
sim_lexis$DTNASC <- as.Date(sim_lexis$DTNASC)

sim_lexis$ANONASC <- year(sim_lexis$DTNASC)
sim_lexis$ANOOBITO <- year(sim_lexis$DTOBITO)

sim_lexis$ANONASC <- as.numeric(sim_lexis$ANONASC)
sim_lexis$ANOOBITO <- as.numeric(sim_lexis$ANOOBITO)

sim_lexis <- sim_lexis %>%
  filter (ANONASC > 1999)

sim_lexis$comparativo <- ymd(paste(sim_lexis$ANONASC,"-06-01"))
sim_lexis$TRI <- ifelse(sim_lexis$DTNASC <= sim_lexis$comparativo,"0","1")          
sim_lexis$TRI <- as.numeric(sim_lexis$TRI)

#Filtro Utilizado pois constavam pessoas nascidas >2000 com mais de 20 anos (alguns com mais de 70)
sim_lexis <- sim_lexis %>%
  filter (IDADEA < 21)

as_tibble(sim_lexis)

# Agrupando os dados: Essa parte que está dando pau
# Provavelmente arrumado (?)
dados2 <- sim_lexis %>%
  mutate(
    IDOBITO= as.numeric(difftime(DTOBITO, DTNASC, units = "days")/365),
    ano_nasc=year(DTNASC),
    ano_obito=year(DTOBITO)) %>%
  group_by(ano_nasc, ano_obito, TRI) %>%
  mutate(quantidade=n()) %>%
  ungroup() %>%
  distinct(ano_nasc, ano_obito, TRI, .keep_all = T) %>%
  select(ano_nasc,ano_obito, quantidade, TRI) %>%
  arrange(ano_obito, desc(ano_nasc))

dados2$id_ob_anos_comp <- (dados2$ano_obito - dados2$ano_nasc)

## Lexis Plot (Dados fora de posição[ex o que deveria estar na coorte 2000, está na coorte 1999])
diagrama <- lexis_grid(year_start=2000,year_end=2020,age_start=0,age_end=5,delta=1)


diagrama <- diagrama +
  annotate(geom="text", x=as.Date(paste0(dados2$ano_obito[dados2$TRI==0]
                                         ,"-08-06"))
           ,y=dados2$id_ob_anos_comp[dados2$TRI==0]+0.3,
           label=c(paste0(dados2$quantidade[dados2$TRI==0])),
           color="red") +
  annotate(geom="text", x=as.Date(paste0(dados2$ano_obito[dados2$TRI==1]
                                         ,"-05-06"))
           ,y=dados2$id_ob_anos_comp[dados2$TRI==1]+0.75,
           label=c(paste0(dados2$quantidade[dados2$TRI==1])),
           color="blue")
diagrama