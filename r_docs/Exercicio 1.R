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

dados <- sim_lexis %>%
  mutate(IDOBITO= as.numeric(difftime(DTOBITO, DTNASC, units = "days")/365),id_ob_anos_comp=floor(IDOBITO)) %>% 
  group_by(ANONASC, ANOOBITO, TRI) %>%
  mutate(quantidade=n()) %>%
  ungroup() %>% 
  distinct(ANONASC, ANOOBITO, TRI,.keep_all = T) %>%
  select(ANONASC,ANOOBITO, quantidade, TRI,id_ob_anos_comp)

#QUESTÃO 1) 
# a)
### Este diagrama ainda apresenta erros. (os números vêm empilhados em cada triângulo ???)
### Esta questão pede para trabalhar com os dados do SINASC, mas o mesmo não vêm com dados de mortalidade,
### portanto, optei por utilizar os dados do SIM.


diagrama <- lexis_grid(year_start=2000,year_end=2020,age_start=0,age_end=5,delta=1)

diagrama <- diagrama +
  annotate(geom="text", x=as.Date(paste0(dados$ANOOBITO[dados$TRI==0]
                                         ,"-08-06"))
           ,y=dados$id_ob_anos_comp[dados$TRI==0]+0.3,
           label=c(paste0(dados$quantidade[dados$TRI==0])),
           color="red") +
  annotate(geom="text", x=as.Date(paste0(dados$ANOOBITO[dados$TRI==1]
                                         ,"-05-06"))
           ,y=dados$id_ob_anos_comp[dados$TRI==1]+0.75,
           label=c(paste0(dados$quantidade[dados$TRI==1])),
           color="blue")
diagrama

# Segue [muito] bugado
# Parece que, além de empilhar, os dados estão entrando na coorte errada (por exemplo, a obs. "207" que aparece no canto inferior esquerdo, deveria estar
# no losando a direita, indicando que nasceu em 2000 e morreu em 2000, porém aparece como nascido em 1999)