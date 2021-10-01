# QUESTA0 3 a ----

library(tidyverse)
library(viridis)
library(readxl)
library(zoo)


### AJUSTAR TRATAMENTO DE 0-1 E 1-4 ###



### Leitura dos dados ---

dados_sim <- readRDS("./data/SIM/sim_consolidado.rds")%>%
  mutate(estado = str_sub(CODMUNRES, end = 2))%>%
  filter(estado == "12")

dados_nasc <- readRDS("./data/sinasc/sinasc_consolidado.rds")




#populacao por sexo e faixa etária do AC centrada em 2019

faixas_et <- read_xls("./data/projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AC",
                          range = "AC!A5:A25") %>%
  pull()

M_2019 <- read_xls("./data/projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AC",
                            range = "AC!J5:L25")%>%
  rowSums()/3
  
F_2019 <- read_xls("./data/projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AC",
                     range = "AC!J28:L48")%>%
  rowSums()/3

pop_ac_sx_etaria <- tibble(faixas_et, M_2019, F_2019)[-1,]



# Populacao centrada em 2019 pelo IBGE
pop_2019_m <- read_xls("./data/projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AC",
                   range = "AC!J5:L6") %>%
  rowSums()/3

pop_2019_f <- read_xls("./data/projecoes_2018_populacao_2010_2060_20200406.xls", sheet = "AC",
                      range = "AC!J28:L29") %>%
  rowSums()/3
  
pop_2019 <- pop_2019_m + pop_2019_f



## Item a ----

#TBM
# Relação entre o número de óbitos
# ocorridos em um ano e a população total
# média no período (anos-pessoa vividos)
# • Usualmente expressa por mil habitantes



#obitos_2019 <- 4178
obitos_2019 <- dados_sim %>%
  filter(TIPOBITO == 2)%>%
  select(DTOBITO, CODMUNRES) %>% #seleciona data de obito
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
  select(faixas_et)%>%
  filter(!is.na(faixas_et) & faixas_et!= " ")%>%
  pull()

faixas_quinq <- c("0-1", "1-4", faixas_quinq[-1]) #adiciona faixas iniciais

obit_sx_et <- dados_sim %>%
  mutate(DTOBITO = as.character(DTOBITO),
         ano = str_sub(DTOBITO, start = 5, end = 8))%>% #retira apenas ano da data
  filter(ano == 2019)%>%
  filter(TIPOBITO == 2)%>%
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
    faixa_et = cut(IDADE, breaks = c(0,1,seq(from = 5, to = 90, by = 5), 115),
                   right = FALSE, include.lowest = TRUE, labels = faixas_quinq)
  ) %>%
  group_by(faixa_et, SEXO) %>%
  summarise(obitos = n())


obit_wide <- obit_sx_et %>% #transforma a ultima tabela em formato wide
  pivot_wider(names_from = SEXO, values_from = obitos)%>%
  rename("obit_masc" = Masc, "obit_fem" = Fem)%>%
  filter(!is.na(faixa_et))


#2. populacao por M e F e grupo quinquenal

#projeção da população inicial, obitos subtraídos ano a ano

zero_1_2019 <- dados_nasc %>%
  select(SEXO, DTNASC)%>%
  mutate(ano = str_sub(DTNASC, start = -4, end = -1))%>%
  filter(ano %in% c("2018", "2019", "2020"))%>%
  group_by(SEXO, ano) %>%
  summarise(n = n())%>%
  group_by(SEXO)%>%
  summarise(media = mean(n))%>%
  filter(SEXO != "0")%>%
  mutate(SEXO = factor(SEXO, labels = c("M_2019", "F_2019")),
         faixas_et = "0-1")%>%
  pivot_wider(names_from = SEXO, values_from = media)


pop_ac_sx_etaria <- bind_rows(zero_1_2019, pop_ac_sx_etaria)

pop_ac_sx_etaria[2,] <- c("1-4", pop_ac_sx_etaria[2,-1] - pop_ac_sx_etaria[1,-1])


pop_quinq <- pop_ac_sx_etaria%>%
  select(M_2019, F_2019)%>%
  mutate(faixa_et = faixas_quinq) %>%
  rename("pop_masc" = M_2019, "pop_fem" = F_2019)

#3. calculo das colunas nMx

nMx <- obit_wide %>%
  filter(!is.na(faixa_et))%>% #removendo a linha com faixa etária NA
  bind_cols(pop_quinq, .name_repair = "universal") %>% #juntando as colunas das 2 tabelas
  select(-c(I, faixa_et...1))%>% #removendo 2 variaveis indesejadas
  rename('faixa_et' = faixa_et...7)%>% #dando nome adequado a faixa etaria
  mutate(nMx_masc = (obit_masc/pop_masc),
         nMx_fem = (obit_fem/pop_fem),
         faixa_et = factor(faixa_et,
                           levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                           "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                           "75-79","80-84","85-89","90+"))
  )


## grafico nMx ----

#exportar em 1100x420
graf_nmx <- nMx %>%
  select(faixa_et, nMx_masc, nMx_fem)%>%
  pivot_longer(cols = -faixa_et, names_to = "Sexo", values_to = "nMx")%>%
  mutate(Sexo = if_else(Sexo == "nMx_masc", "Masculino", "Feminino"))%>%
  ggplot(aes(x = faixa_et, y = nMx, group = Sexo, colour = Sexo))+
  geom_line(size = 1)+
  scale_color_manual(values = c("skyblue", "orange"))+
  scale_y_log10()+
  xlab("Faixa etária")+
  theme_bw()+
  theme(axis.title.y = element_text(angle = 0, size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.ticks = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "gray"))




# QUESTA0 3 e ----

## nkx ----

nkx_primario <- dados_sim %>%
  select(SEXO, IDADE)%>% #puxa apenas essas 2 variaveis
  mutate(
    #ajusta a variavel sexo com label
    SEXO = factor(SEXO, levels = c(0, 1, 2), labels = c('I', 'Masc', 'Fem')),
    #ajustndo a variavel idade
    COD_IDADE = IDADE,
    IDADE = as.character(IDADE),
    IDADE = case_when(
      as.numeric(IDADE) <= 400 ~ "0",
      as.numeric(IDADE) > 400 & as.numeric(IDADE) < 500 ~ str_sub(as.character(IDADE), start = 2),
      as.numeric(IDADE) >= 500 & as.numeric(IDADE) < 515 ~ str_c("1", str_sub(as.character(IDADE), start = 2), sep = "")
    ),
    IDADE = as.numeric(IDADE),
    faixa_et = cut(IDADE, breaks = c(0,1,seq(from = 5, to = 90, by = 5), 115),
                   right = FALSE, include.lowest = TRUE, labels = faixas_quinq)
  ) 

#localizacao dos hifens para facilitar o calculo da média
locais_traco <- str_locate(nkx_primario$faixa_et, "-")[,1]

nkx_long <- nkx_primario %>% #para achar a idade média por faixa
  bind_cols(locais_traco) %>%
  mutate(
    faixa_low = if_else(faixa_et != "90+", str_sub(faixa_et, end = locais_traco-1), "90"),
    faixa_low = as.numeric(faixa_low),
    vida_na_faixa = IDADE - faixa_low
  ) %>%
  select(SEXO, IDADE, faixa_et, vida_na_faixa) %>%
  filter(SEXO != "I")%>%
  group_by(SEXO, faixa_et) %>%
  summarise(nkx = mean(vida_na_faixa))

#nkx para menores de 1 ano
#acurácia para nível mínimo de dia
nkx_01 <- nkx_primario %>%
  mutate(COD_IDADE = as.character(COD_IDADE),
         COD_IDADE = as.numeric(COD_IDADE)) %>%
  filter(COD_IDADE <= 400) %>%
  mutate(tempo_ano = case_when(
    COD_IDADE <= 200 ~ 1/365,
    COD_IDADE > 200 & COD_IDADE < 300 ~ (COD_IDADE-200)/365,
    COD_IDADE > 300 & COD_IDADE < 400 ~ (COD_IDADE-300)/12,
    COD_IDADE == 200 ~ 1/365, #menor que 1 dia, um dia
    COD_IDADE == 300 ~ 1/24, #menor que 1 mes, média em meio mes
    COD_IDADE == 400 ~ 1/2 #menor que 1 ano, média em meio ano
  )) %>%
  group_by(SEXO)%>%
  summarise(nkx = mean(tempo_ano))%>%
  filter(SEXO != "I")%>%
  mutate(faixa_et = "0-1")

nkx <- left_join(nkx_long, nkx_01, by = c("SEXO", "faixa_et"), suffix = c("", ".y"))%>%
  mutate(nkx = if_else(nkx == 0, nkx.y, nkx))%>%
  select(-nkx.y)

nkx_wide <- pivot_wider(nkx, names_from = SEXO, values_from = nkx) %>% filter(!is.na(faixa_et))


### tabua de vida ----


#### tabua_masc ----
tabua_masc <- tibble(
  classe = nMx$faixa_et,
  x = c(0,1, seq(from = 5, to = 90, by = 5)),
  n = c(1,4, rep(5, 17), 0),
  nMx = nMx$nMx_masc,
  nkx = nkx_wide$Masc,
  nqx = n*nMx/(1+(n-nkx)*nMx)
)

#ajustes
tabua_masc[20,3] <- NA
tabua_masc[20,6] <- 1

lx <- 100000
ndx <- lx * tabua_masc$nqx[1]

for (i in 2:20){
  lx[i] <- lx[i-1]-ndx[i-1]
  ndx[i] <- lx[i] * tabua_masc$nqx[i]
}

tabua_masc <- bind_cols(tabua_masc, "lx" = lx, "ndx" = ndx)

tabua_masc <- tabua_masc %>%
  mutate(nLx = if_else(is.na(n) == F, lx*n + ndx*nkx, ndx*nkx))

Tx <- c(rep(NA, 19),tabua_masc$nLx[20])

for (i in 19:1){
  Tx[i] <- tabua_masc$nLx[i] + Tx[i+1]
}

tabua_masc <- bind_cols(tabua_masc, "Tx" = Tx)

tabua_masc <- tabua_masc %>%
  mutate(ex = Tx/lx)
  

#### tabua_fem ----

tabua_fem <- tibble(
  classe = nMx$faixa_et,
  x = c(0,1, seq(from = 5, to = 90, by = 5)),
  n = c(1,4, rep(5, 17), 0),
  nMx = nMx$nMx_fem,
  nkx = nkx_wide$Fem,
  nqx = n*nMx/(1+(n-nkx)*nMx)
)

#ajustes
tabua_fem[20,3] <- NA
tabua_fem[20,6] <- 1

lx <- 100000
ndx <- lx * tabua_fem$nqx[1]

for (i in 2:20){
  lx[i] <- lx[i-1]-ndx[i-1]
  ndx[i] <- lx[i] * tabua_fem$nqx[i]
}

tabua_fem <- bind_cols(tabua_fem, "lx" = lx, "ndx" = ndx)

tabua_fem <- tabua_fem %>%
  mutate(nLx = if_else(is.na(n) == F, lx*n + ndx*nkx, ndx*nkx))

Tx <- c(rep(NA, 19),tabua_fem$nLx[20])

for (i in 19:1){
  Tx[i] <- tabua_fem$nLx[i] + Tx[i+1]
}

tabua_fem <- bind_cols(tabua_fem, "Tx" = Tx)

tabua_fem <- tabua_fem %>%
  mutate(ex = Tx/lx)

rm(dados_nasc, dados_sim, F_2019, faixas_et, faixas_quinq, i, locais_traco, lx, M_2019,
   ndx, nkx, nkx_01, nkx_long, nkx_primario, nkx_wide, obit_sx_et, obit_wide, obitos_2019,
   pop_2019, pop_2019_f, pop_2019_m, pop_ac_sx_etaria, pop_quinq, Tx, zero_1_2019)
