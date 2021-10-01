# QUESTA0 2 c  ----


library(tidyverse)
library(viridis)
library(readxl)
library(tidymodels)
library(magrittr)
library(RColorBrewer)



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
hist_idademae <- desc_idade%>%
ggplot(aes(x = faixa_et, y = n, label = n))+
  labs(title = "Distribuição da população de mães por faixa etária",
       x = "Faixa etária")+
  geom_bar(stat = "identity", fill = "#a6bddb")+
  coord_cartesian(ylim = c(0, 100000))+
  geom_text(size = 3.5, vjust = -.5)+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0))



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

hist_escomae <- desc_esc%>%
  ggplot(aes(x = escolaridade, y = n, label = n))+
  labs(title = "Distribuição da população de mães por escolaridade")+
  coord_cartesian(ylim = c(0, 120000))+
  geom_bar(stat = "identity", fill = "#a6bddb")+
  geom_text(size = 3.5, vjust = -.5)+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0))



# descritivas tipo de parto
# 9: Ignorado
# 1: Vaginal
# 2: Cesáreo

data_limpa%>%
  group_by(PARTO)%>%
  summarise(n = n())%>%
  ungroup()%>%
  mutate(total = sum(n),
         prop = n/total)



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



data_limpa %$% #0.04
  cor(IDADEMAE, as.numeric(escolaridade), method = "spearman") #não sugere cor linear
#obviamente, já que a distribuicao parece um sino com assimetria à direita

data_limpa %>% #-0.08
  filter(escolaridade != '12 ou mais') %$%
  cor(IDADEMAE, as.numeric(escolaridade), method = "spearman")


data_limpa %$% #pvalor < 0.001
  kruskal.test(IDADEMAE ~ escolaridade) %>%
  tidy()#chi quadrado pelo KW sugere associacao


#grafico de distribuicao das idades, com linhas de medianas
#para cada grupo de escolaridade

vline_df <- data.frame(escolaridade = levels(data_limpa$escolaridade),
                       medianas = tapply(X = data_limpa$IDADEMAE, INDEX = data_limpa$escolaridade,
                                       FUN = median))

graf_dist_idade_esc <- data_limpa %>%
  ggplot(aes(IDADEMAE))+
  geom_histogram(stat = "count", fill = "#a6bddb")+
  facet_grid(rows = vars(escolaridade))+
  labs(title = "Distribuição da idade das mães por grupo de escolaridade", x = "Idade da mãe")+
  geom_vline(data = vline_df, aes(xintercept = medianas), linetype = "dashed",
             colour = "red4")+
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60), limits = c(10, 60))+
  theme_bw()+
  theme(axis.line.x = element_line(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(hjust = 0))

graf_esc_faixaet <- data_limpa %>%
  group_by(faixa_et, escolaridade)%>%
  summarise(n = n())%>%
  ggplot(aes(faixa_et,n, group = escolaridade, color = escolaridade))+
  labs(title = "Distribuição da idade das mães por grupo de escolaridade", 
       x = "Faixa etária da mãe",
       color = "Escolaridade")+  
  geom_line(size = 1)+
  scale_color_manual(values = c('#4f5659', '#89a4b0', '#79bddb', '#2898c9', '#006591'))+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(),
        axis.line = element_line(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0))

#a maioria das maes com mais anos de educação formal têm menos de 30 anos


### tipo de parto e escolaridade da mãe ----
  #sob consulta

data_limpa %$% # pvalor < 0.001
  chisq.test(PARTO, ESCMAE) %>% tidy() # Qui quadrado altíssimo, sugete associacao

prop_partos <- data_limpa %>%
  group_by(escolaridade)%>%
  mutate(total = n(),
         PARTO = if_else(PARTO == '1', 'vaginal', 'cesario'))%>%
  group_by(escolaridade,PARTO)%>%
  mutate(prop_parto = n()/total) %>%
  select(escolaridade, PARTO, prop_parto)%>%
  unique()%>%
  arrange(escolaridade)

prop_partos_wide <- prop_partos %>%
  pivot_wider(values_from = "prop_parto", names_from = "escolaridade")

graf_prop_partos <-  prop_partos %>%
  ggplot(aes(x = escolaridade, y = prop_parto, group = PARTO, color = PARTO))+
  geom_line(size = 1)+
  labs(title = "Proporção de tipo de parto por escolaridade da mãe",
       color = "Parto",
       x = "Escolaridade da mãe")+
  scale_colour_manual(values = c("#2b8cbe", "#d1495b"), labels = c("Cesário", "Vaginal"))+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(),
        axis.line = element_line(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0))

rm(dados_nasc, maes, data_limpa, desc_idade, desc_esc, vline_df, prop_partos)
