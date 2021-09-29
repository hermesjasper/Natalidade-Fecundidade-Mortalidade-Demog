# QUESTA0 2 c  ----


library(tidyverse)
library(viridis)
library(readxl)
library(tidymodels)
library(magrittr)



## Leitura dos dados ---

dados_nasc <- readRDS("./data/sinasc/sinasc_consolidado.rds")%>%
  mutate(estado = str_sub(CODMUNRES, end = 2),
         IDADEMAE = as.character(IDADEMAE),
         IDADEMAE = as.numeric(IDADEMAE),)%>% #retira apenas ano da data
  filter(estado == "12")

maes <- dados_nasc %>%
  select(IDADEMAE, ESCMAE, PARTO) %>%
  filter(IDADEMAE != 99 & is.na(IDADEMAE) == FALSE)

## limpeza dos dados ----

data_limpa <- maes %>%
  filter(ESCMAE %in% c('1', '2', '3', '4', '5'),
         PARTO %in% c('1', '2'),
         ESCMAE %in% c('1', '2', '3', '4', '5'))%>%
  mutate(
    escolaridade = factor(ESCMAE, levels = c(1:5),
                          labels = c("Nenhuma", "1 a 3 anos","4 a 7 anos","8 a 11 anos",
                                     "12 ou mais")),
    IDADEMAE = as.character(IDADEMAE),
    IDADEMAE = as.numeric(IDADEMAE),
    faixa_et = cut(IDADEMAE, breaks = c(seq(from = 10, to = 45, by = 5), max(IDADEMAE)), 
                   right = FALSE, include.lowest = TRUE,
                   labels = c('10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', "45+"))
    
  )


## descritivas ----

# descritivas idade da mae
desc_idade <- data_limpa%>%
  group_by(faixa_et)%>%
  summarise(n = n())


#grafico
data_limpa%>%
ggplot(aes(faixa_et))+
  geom_histogram(stat = "count")



# descritivas escolaridade

# 1: Nenhuma
# 2: 1 a 3 anos
# 3: 4 a 7 anos
# 4: 8 a 11 anos
# 5: 12 e mais
# 9: Ignorado


desc_esc <- data_limpa %>%
  group_by(escolaridade)%>%
  summarise(n = n())

ggplot(data_limpa, aes(escolaridade))+geom_histogram(stat = "count")



# descritivas tipo de parto
# 9: Ignorado
# 1: Vaginal
# 2: Cesáreo

desc_parto <- data_limpa%>%
  group_by(PARTO)%>%
  summarise(n = n())



## qualidade dos dados ----

# aqui sao usados os dados brutos para verificar erros de digitação ou 
# codificação de ignorados

#TOTAL de maes no estado é de 345147

# NA e ignorados em cada variável
maes %>% #ESCOLARIDADE
  filter(ESCMAE %in% c('1', '2', '3', '4', '5') == FALSE)%>%
  group_by(ESCMAE)%>%
  summarise(n = n())

# IDADE
dados_nasc %>%
  filter(IDADEMAE == 99 | is.na(IDADEMAE) == TRUE)%>%
  group_by(IDADEMAE)%>%
  summarise(n = n())

# PARTO
maes %>%
  filter(PARTO %in% c('1', '2') == FALSE)%>%
  select(PARTO)%>%
  droplevels()%>%
  summary()






## Associacao ----

### idade e escolaridade da mãe ----



data_limpa %$%
  cor(IDADEMAE, as.numeric(escolaridade), method = "spearman") #não sugere cor linear
#obviamente, já que a distribuicao parece um sino com assimetria à direita

data_limpa %>%
  filter(escolaridade != '12 ou mais') %$%
  cor(IDADEMAE, as.numeric(escolaridade), method = "spearman")



data_limpa %$%
  kruskal.test(IDADEMAE ~ escolaridade) #chi quadrado pelo KW sugere associacao


#grafico de distribuicao das idades, com linhas de medianas
#para cada grupo de escolaridade

vline_df <- data.frame(escolaridade = levels(data_limpa$escolaridade),
                       medianas = tapply(X = data_limpa$IDADEMAE, INDEX = data_limpa$escolaridade,
                                       FUN = median))

data_limpa %>%
  ggplot(aes(IDADEMAE))+
  geom_histogram(stat = "count")+
  facet_grid(rows = vars(escolaridade))+
  geom_vline(data = vline_df, aes(xintercept = medianas), linetype = "dashed",
             colour = "red4")


data_limpa %>%
  group_by(faixa_et, escolaridade)%>%
  summarise(n = n())%>%
  ggplot(aes(faixa_et,n, group = escolaridade, color = escolaridade))+
    geom_line()

#a maioria das maes com mais anos de educação formal têm menos de 30 anos


### tipo de parto e escolaridade da mãe ----
  #sob consulta

data_limpa %$%
  chisq.test(PARTO, ESCMAE) # Qui quadrado altíssimo, sugete associacao

prop_partos <- data_limpa %>%
  group_by(escolaridade)%>%
  mutate(total = n(),
         PARTO = if_else(PARTO == '1', 'vaginal', 'cesario'))%>%
  group_by(escolaridade,PARTO)%>%
  mutate(prop_parto = n()/total) %>%
  select(escolaridade, PARTO, prop_parto)%>%
  unique()%>%
  arrange(escolaridade)

prop_partos %>%
  ggplot(aes(x = escolaridade, y = prop_parto, group = PARTO, color = PARTO))+
  geom_line()
