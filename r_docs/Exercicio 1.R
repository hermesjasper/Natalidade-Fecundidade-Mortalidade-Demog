library(read.dbc)
library(dplyr)
library(stringr)
library(foreign)
library(LexisPlotR)
library(lubridate)
library(visdat)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

#################################################################################################################################################
#################################################################################################################################################

# sim_consolidado <- readRDS("./data/SIM/sim_consolidado.rds")
# sinasc_consolidado <- readRDS("./data/sinasc/sinasc_consolidado.rds")

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

dados2 <- sim_lexis %>%
  filter (ANONASC > 1999)

dados2$comparativo <- ymd(paste(dados2$ANONASC,"-06-01"))
dados2$TRI <- ifelse(dados2$DTNASC <= dados2$comparativo,"0","1")          
dados2$TRI <- as.numeric(dados2$TRI)

#Filtro Utilizado pois constavam pessoas nascidas >2000 com mais de 20 anos (alguns com mais de 70)

dados2 <- dados2 %>%
  filter (IDADEA < 21)

as_tibble(dados2)

# Agrupando os dados: Arrumado!

dados2 <- dados2 %>%
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

## Lexis Plot 

#1)
#
# a)  Construir o Diagrama de Lexis para os dados de nascidos vivos de 2000 a 2020 
#     da UF escolhida (SINASC) e de óbitos menores de 5 anos (idades simples) para o mesmo período segundo ano de nascimento.
#

paleta <- brewer.pal(n = 8, name = "Set2")
paleta2 <- brewer.pal(n = 8, name = "Set1")
paleta3 <- brewer.pal(n = 12, name = "Set3")

dados2 <- dados2 %>%
  mutate(cor=case_when (ano_nasc=="2000" ~ paleta[1],
                        ano_nasc=="2001" ~ paleta[2],
                        ano_nasc=="2002" ~ paleta[3],
                        ano_nasc=="2003" ~ paleta[4],
                        ano_nasc=="2004" ~ paleta[5],
                        ano_nasc=="2005" ~ paleta[6],
                        ano_nasc=="2006" ~ paleta[7],
                        ano_nasc=="2007" ~ paleta[8],
                        ano_nasc=="2008" ~ paleta2[1],
                        ano_nasc=="2009" ~ paleta2[2],
                        ano_nasc=="2010" ~ paleta2[3],
                        ano_nasc=="2011" ~ paleta2[4],
                        ano_nasc=="2012" ~ paleta2[5],
                        ano_nasc=="2013" ~ paleta3[6],
                        ano_nasc=="2014" ~ paleta2[7],
                        ano_nasc=="2015" ~ paleta2[8],
                        ano_nasc=="2016" ~ paleta3[1],
                        ano_nasc=="2017" ~ paleta3[7],
                        ano_nasc=="2018" ~ paleta3[3],
                        ano_nasc=="2019" ~ paleta3[4],
                        ano_nasc=="2020" ~ paleta3[5],
                        ))


  diagrama <- lexis_grid(year_start=2000,year_end=2020,age_start=0,age_end=5,delta=1)
  
  
  diagrama <- diagrama +
    annotate(geom="text", x=as.Date(paste0((dados2$ano_obito)[dados2$TRI==0]
                                           ,"-09-06"))
             ,y=dados2$id_ob_anos_comp[dados2$TRI==0]+0.3,
             label=c(paste0(dados2$quantidade[dados2$TRI==0])),
             color=(dados2$cor)[dados2$TRI==0])+
    annotate(geom="text", x=as.Date(paste0((dados2$ano_obito + 1)[dados2$TRI==1]
                                           ,"-05-06"))
             ,y=dados2$id_ob_anos_comp[dados2$TRI==1]+0.75,
             label=c(paste0(dados2$quantidade[dados2$TRI==1])),
             color=(dados2$cor)[dados2$TRI==1])
  
  diagrama <- diagrama + ggtitle("Diagrama de Lexis") +
    xlab("Coortes") + ylab("Anos Completos")
  
  diagrama

#################################################################################################################################################
#################################################################################################################################################

#
# b) Supondo população fechada (inexistência de migração), calcule a probabilidade 
#    de um recém-nascido na UF ou território de escolha sobreviver à idade exata 5 
#    para as coortes de 2000 a 2015.
#

sinasc_lexis <- sinasc_consolidado %>% select(DTNASC,CODMUNRES) %>%
  mutate(DTNASC=dmy(DTNASC))

sinasc_lexis$ANONASC <- year(sinasc_lexis$DTNASC)

b <- sinasc_lexis %>%
  filter (ANONASC < 2016)

#265743 obs.

b2 <- dados2 %>%
  filter (ano_obito < 2016)

sum(b2$quantidade) #6260

# b2 tem 6260 indivíduos nascidos em 2000-2015 e falecidos neste intervalo, enquanto b tem 265743 indivíduos nascidos no mesmo
# intervalo, logo a probabilidade de sobrevivência é  1 - (6260/265743) = 0.9764434

#################################################################################################################################################
#################################################################################################################################################

#
# c) Considerando o mesmo pressuposto, calcule a probabilidade de sobreviver ao
#    primeiro aniversário dos recém-nascidos no período de 2000 a 2019.
#

c <- dados2 %>%
  filter (ano_nasc < 2020) %>%
  filter ((ano_obito - ano_nasc) < 1)

sum (c$quantidade)

# 5190 obitos em menores de 1 ano entre 2000 e 2019

c2 <- sinasc_lexis %>%
  filter (ANONASC < 2020)

# 330697 nascimentos entre 2000 e 2019

# Logo, a probabilidade de sobreviver ao primeiro ano é 1-(5190/330697) = 0.9843059 

#
# d) Comente sobre os valores encontrados. Não esquecer a qualidade da informação trabalhada.
#
#  Dentro dos valores utilizados para a elaboração das questões, a maior parte das informações estava contida, não sendo comum encontrar valores em branco.
#  Porém, não temos como ter certeza se os dados trabalhados refletem a totalidade dos casos de nascimentos e mortes ocorridos, visto que pode haver fuga de registro.
#  Foram observados alguns valores impossíveis (ex: nascido em 2000 com 70 anos de idade), porém foram poucos e são de se esperar em bancos de dados gigantescos como esses (SIM e SINASC).
#  
## Para a qualidade da informação trabalhada, seria bom uma análise geral de ambos os bancos utilizando os métodos mencionados no WEBINÁRIO do dia 22/09, com os pacotes DemoTools e DDM
#
#


# rm(sim_consolidado)
# rm(sinasc_consolidado)
rm(sim_lexis)
rm(sinasc_lexis)
rm(paleta)
rm(paleta2)
rm(paleta3)
#rm(b)
#rm(b2)
#rm(c)
#rm(c2)
#rm(dados2)

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################