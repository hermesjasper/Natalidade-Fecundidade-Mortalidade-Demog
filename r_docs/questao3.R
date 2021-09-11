# QUESTA0 3 ----

library(tidyverse)


dados_sim <- readRDS("./data/SIM/sim_consolidado.rds")

dados_nasc <- readRDS("./data/sinasc/sinasc_consolidado.rds")

pop_ac_sx_etaria <- read.csv("./data/pop_AC_sexo_etaria.csv")



## Item a ----

#TBM
# Relação entre o número de óbitos
# ocorridos em um ano e a população total
# média no período (anos-pessoa vividos)
# • Usualmente expressa por mil habitantes


# Populacao de 2019 pelo IBGE
pop_2019 <- 881935

#obitos_2019 <- 4178
obitos_2019 <- dados_sim %>%
  select(DTOBITO) %>% #seleciona data de obito
  mutate(DTOBITO = as.character(DTOBITO),
         ano = str_sub(DTOBITO, start = 5, end = 8))%>% #retira apenas ano da data
  group_by(ano)%>%
  summarise(quant = n())%>% #contagem de obitos agregado por ano
  filter(ano == 2019)%>% #filtragem de 2019
  pull(quant) #obter apenas o valor

#TBM - um pouco mais alto que a planilha do IBGE
TBM <- (obitos_2019/pop_2019)*1000


#nMx

#1. tabela pelo SIM de óbitos M e F por grupo etário quinquenal

#labels do cut
faixas_quinq <- pop_ac_sx_etaria %>%
  select(Total)%>%
  filter(!is.na(Total) & Total!= " ")%>%
  pull()

obit_sx_et <- dados_sim %>%
  select(SEXO, IDADE)%>% #puxa apenas essas 2 variaveis
  mutate(
    #ajusta a variavel sexo com label
    SEXO = factor(SEXO, levels = c(0, 1, 2), labels = c('I', 'Masc', 'Fem')),
    #ajustndo a variavel idade
    IDADE = as.character(IDADE),
    IDADE = case_when(
      as.numeric(IDADE) <= 400 ~ "0",
      as.numeric(IDADE) > 400 & as.numeric(IDADE) < 500 ~ str_sub(as.character(IDADE), start = 2),
      as.numeric(IDADE) >= 500 & as.numeric(IDADE) < 515 ~ str_c("1", str_sub(as.character(IDADE), start = 2), sep = "")
    ),
    IDADE = as.numeric(IDADE),
    faixa_et = cut(IDADE, breaks = c(seq(from = 0, to = 90, by = 5), 115),
                   right = FALSE, include.lowest = TRUE, labels = faixas_quinq)
  ) %>%
  group_by(faixa_et, SEXO) %>%
  summarise(obitos = n())


#2. populacao por M e F e grupo quinquenal
pop_quinq <- pop_ac_sx_etaria%>%
  filter(!is.na(M_2019))%>%
  select(M_2019, F_2019)%>%
  mutate(faixa_et = faixas_quinq) %>%
  pivot_longer(cols = c(M_2019, F_2019),names_to = "SEXO", values_to = "populacao")%>%
  mutate(SEXO = if_else(SEXO == "M_2019", "Masc", "Fem"))

#3. calculo das colunas nMx

nMx <- obit_sx_et %>%
  inner_join(pop_quinq, by = c("faixa_et", "SEXO")) %>%
  mutate(nmx = obitos/populacao) #abrir essa tabela em wider