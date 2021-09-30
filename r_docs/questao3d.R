# QUESTA0 3 d ----

# d) Compare a estrutura de mortalidade por causas (Capítulos da CID10 - reagrupados em 
# 6/7 grandes causas) segundo sexo de 2015 e 2020. Comente os resultados. Destacar a mortalidade 
# por Covid-19.

### Capítulos CID 10 ----
# 001  I.   Algumas doenças infecciosas e parasitárias    A00-B99
# 002  II.  Neoplasias (tumores)                          C00-D48
# 003  III. Doenças do sangue e org. hemato. e tr. imunit D50-D89
# 004  IV.  Doenças endócrinas nutricionais e metabólicas E00-E90
# 005  V.   Transtornos mentais e comportamentais         F00-F99
# 006  VI.  Doenças do sistema nervoso                    G00-G99
# 007  VII. Doenças do olho e anexos                      H00-H59
# 008  VIII.Doenças do ouvido e da apófise mastóide       H60-H95
# 009  IX.  Doenças do aparelho circulatório              I00-I99
# 010  X.   Doenças do aparelho respiratório              J00-J99
# 011  XI.  Doenças do aparelho digestivo                 K00-K93
# 012  XII. Doenças da pele e do tecido subcutâneo        L00-L99
# 013  XIII.Doenças sist osteomuscular e tec conjuntivo   M00-M99
# 014  XIV. Doenças do aparelho geniturinário             N00-N99
# 015  XV.  Gravidez parto e puerpério                    O00-O99
# 016  XVI. Algumas afec originadas no período perinatal  P00-P96
# 017  XVII.Malf cong deformid e anomalias cromossômicas  Q00-Q99
# 018  XVIII.Sint sinais e achad anorm ex clín e laborat  R00-R99
# 019  XIX. Lesões enven e alg out conseq causas externas S00-S99,T00-T98
# 020  XX.  Causas externas de morbidade e mortalidade    V01-V99,W00-W99,X00-X99,Y00-Y98
# 021  XXI. Contatos com serviços de saúde                Z00-Z99


### CID COVID 19 ----
# B34.2, U04.9


### Pacotes ----
library(tidyverse)

library(RColorBrewer)

paleta <- brewer.pal(n = 10, name = "Set3")


### Leitura dos dados ----

dados_ac <- readRDS("./data/SIM/SIM_AC_2015_20.rds")

dados <- dados_ac %>%
  filter(TIPOBITO == 2, is.na(LINHAA) == FALSE) %>% #obito nao fetal
  #LINHAA é a causa que causou diretamente a morte, unico campo obrigatório
  mutate(ano = str_sub(DTOBITO, start = 5),
         ano = as.integer(ano),
         CID = str_sub(LINHAA, start = 2, end =  5), #selecao do primeiro CID
         CID_cap = str_sub(CID, start = 1, end = 1),
         CID_num = str_sub(CID, start = 2, end = 4),
         CID_num = str_replace(CID_num, pattern = "X", replace = "0" ),
         IDADE = as.character(IDADE), #ajuste da idade
         IDADE = case_when(
           as.numeric(IDADE) <= 400 ~ "0",
           as.numeric(IDADE) > 400 & as.numeric(IDADE) < 500 ~ str_sub(as.character(IDADE), start = 2),
           as.numeric(IDADE) >= 500 & as.numeric(IDADE) < 515 ~ str_c("1", str_sub(as.character(IDADE), start = 2), sep = "")
         ),
         SEXO = factor(SEXO, levels = c('0', '1', '2'), labels = c('ignorado', 'Masculino', 'Feminino'))
                     )%>%
  select(-LINHAA)%>%
  filter(SEXO != "ignorado")


#### Categorizacao da causa de morte ----

dados_cid <- dados %>%
  mutate(causa = case_when(
    str_starts(CID, "A|B") == TRUE ~ "Algumas doenças infecciosas e parasitárias",
    str_starts(CID, "C") == TRUE ~ "Neoplasias",
    str_starts(CID, "D") == TRUE & CID_num < 500 ~ "Neoplasias",
    str_starts(CID, "D") == TRUE & CID_num >= 500 ~ "	Doenças do sangue e dos órgãos hematopoéticos e alguns transtornos imunitários",
    str_starts(CID, "E") == TRUE ~ "Doenças endócrinas, nutricionais e metabólicas",
    str_starts(CID, "F") == TRUE ~ "Transtornos mentais e comportamentais",
    str_starts(CID, "G") == TRUE ~ "Doenças do sistema nervoso",
    str_starts(CID, "H") == TRUE & CID_num < 600 ~ "Doenças do olho e anexos",
    str_starts(CID, "H") == TRUE & CID_num >= 600 ~ "Doenças do ouvido e da apófise mastóide",
    str_starts(CID, "I") == TRUE ~ "Doenças do aparelho circulatório",
    str_starts(CID, "J") == TRUE ~ "Doenças do aparelho respiratórios",
    str_starts(CID, "K") == TRUE ~ "Doenças do aparelho digestivo",
    str_starts(CID, "L") == TRUE ~ "Doenças da pele e do tecido subcutâneo",
    str_starts(CID, "M") == TRUE ~ "Doenças sistema osteomuscular e tecido conjuntivo",
    str_starts(CID, "N") == TRUE ~ "Doenças do aparelho geniturinário",
    str_starts(CID, "O") == TRUE ~ "Gravidez parto e puerpério",
    str_starts(CID, "P") == TRUE ~ "Algumas afecções originadas no período perinatal",
    str_starts(CID, "Q") == TRUE ~ "Malformações congênitas, deformidades e anomalias cromossômicas",
    str_starts(CID, "R") == TRUE ~ "Sintomas, sinais e achados anormais de exames clínicos e de laboratório, não classificados em outra parte",
    str_starts(CID, "S|T") == TRUE ~ "Lesões, envenenamentos e algumas outras consequências de causas externas",
    str_starts(CID, "U") == TRUE ~ "Códigos para propósitos especiais",
    str_starts(CID, "V|W|X|Y") == TRUE ~ "Causas externas de morbidade e mortalidade",
    str_starts(CID, "Z") == TRUE ~ "Fatores que influenciam o estado de saúde e o contato com os serviços de saúde"
    )
  )


#### Atribuicao de faixa etaria ----

# faixa_et <- tibble(
#   x = seq(0, 90, 5),
#   y = c(seq(4, 89, 5), "+"),
#   faixa = paste(x, "-", y, sep = "")
# ) %>% 
#   pull(faixa)
# 
# faixa_et <- if_else(faixa_et == "90-+", "90+", faixa_et)
# 
# dados_cid_etaria <- dados_cid %>%
#   mutate(
#     IDADE = as.numeric(IDADE),
#     faixa_et = cut(IDADE, breaks = c(seq(from = 0, to = 90, by = 5), 115),
#                    right = FALSE, include.lowest = TRUE, labels = faixa_et)
#   )


### Estrutura da mortalidade agrupada, segundo sexo ----

#### por capitulo ----

morte_caps <- dados_cid %>%
  group_by(SEXO, causa, ano)%>%
  summarise(n = n())%>%
  arrange(SEXO,desc(n))%>%
  group_by(SEXO, ano) %>%
  top_n(6)%>%
  mutate(ano = as.character(ano))

morte_covid <- dados_cid %>%
  filter(CID == "B342" | CID == "U049")%>%
  group_by(SEXO, CID) %>%
  summarise(causa = "COVID-19",
            ano = "2020",
            n = n())%>%
  select(-CID)

morte_caps <- bind_rows(morte_caps, morte_covid)

tabela_morte_caps <- morte_caps %>%
  pivot_wider(names_from = c('SEXO', 'ano'), values_from = 'n')%>%
  select(causa, Feminino_2015, Masculino_2015, Feminino_2020, Masculino_2020)

#grafico - fazer um tabset com a tabela

caps_masc <- ggplot(subset(morte_caps, SEXO == "Masculino" & causa != 'COVID-19'),
                    aes(x = ano, y = n, group = causa))+
  xlab("")+
  geom_line(size = 1, color = "#a6bddb")+
  geom_point(data = subset(morte_caps, SEXO == "Masculino" & causa == 'COVID-19'),
             color = "#d1495b")+
  labs(title = "Masculino")+
  geom_point(color = "#2b8cbe")+
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5))+
  annotate("text", x = 2.25, y = 900, label = "Sintomas e achados \nanormais")+
  annotate("text", x = 2.3, y = 575, label = "Infecciosas e parasitárias")+
  annotate("text", x = 2.25, y = 510, label = "Aparelho respiratório")+
  annotate("text", x = 2.21, y = 375, label = "Causas externas")+
  annotate("text", x = 2.25, y = 238, label = "Aparelho circulatório")+
  annotate("text", x = 2.15, y = 85, label = "Neoplasias")+
  annotate("text", x = 2.15, y = 150, label = "COVID-19")

caps_fem <- ggplot(subset(morte_caps, SEXO == "Feminino" & n > 45),
                    aes(x = ano, y = n, group = causa))+
  xlab("")+
  geom_line(size = 1, color = "#a6bddb")+
  geom_point(color = "#2b8cbe")+
  geom_point(data = subset(morte_caps, SEXO == "Feminino" & causa == 'COVID-19'),
               color = "#d1495b")+
  labs(title = "Feminino")+
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5))+
  annotate("text", x = 2.25, y = 590, label = "Sintomas e achados \nanormais")+
  annotate("text", x = 2.3, y = 393, label = "Infecciosas e parasitárias")+
  annotate("text", x = 2.25, y = 350, label = "Aparelho respiratório")+
  annotate("text", x = 2.245, y = 167, label = "Aparelho circulatório")+
  annotate("text", x = 2.15, y = 95, label = "Neoplasias")+
  annotate("text", x = 2.21, y = 45, label = "Período perinatal")+
  annotate("text", x = 2.145, y = 69, label = "COVID-19")


# ```{r, figures-side, fig.show = "hold", out.width = "50%"}
# caps_masc
# caps_fem
# ```


# apenas mencionar e mostrar uma tabela deitada
#dar nome às mais frequentes
morte_cid <- dados_cid %>%
  group_by(SEXO, CID, ano)%>%
  summarise(n = n())%>%
  arrange(SEXO,desc(n))%>%
  group_by(SEXO, ano) %>%
  top_n(6)%>%
  mutate(nome_causa = case_when(
    CID == 'A419' ~ 'Septicemia não especificada',
    CID == 'B342' ~ 'Infecção por coronavírus de localização não especificada',
    CID == 'I219' ~ 'Infarto agudo do miocárdio não especificado',
    CID == 'J960' ~ 'Insuficiência respiratória aguda',
    CID == 'R092' ~ 'Parada respiratória',
    CID == 'R570' ~ 'Choque cardiogênico',
    CID == 'R688' ~ 'Outros sintomas e sinais gerais especificados',
    CID == 'R99X' ~ "Outras causas mal definidas e as não especificadas de mortalidade",
    CID == 'S069' ~ 'Traumatismo intracraniano, não especificado'
  )) %>%
  select(-CID) %>%
  pivot_wider(names_from = c("SEXO", ano), values_from = "n")

# exemplo agrupamento para a tabela no rmkd
# kbl(dt) %>%
#   kable_classic() %>%
#   add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))


rm(morte_caps, morte_covid, dados_cid, dados, dados_ac, paleta)


