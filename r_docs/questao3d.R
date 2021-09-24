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
# Pneumonia devido a COVID-19	                                                J12.82
# Síndrome inflamatória multissistêmica (MIS) com COVID-19	                  M35.81
# Outro envolvimento sistêmico especificado do tecido conjuntivo com COVID-19	M35.89
# Sequelas de COVID-19	                                                      B94.8
# História pessoal do COVID-19	                                              Z86.16
# Contato com e (suspeita) exposição a COVID-19 ou SARS-CoV-2	                Z20.822
# Infecção por coronavírus de localização não especificada                    B34.2


### Pacotes ----
library(tidyverse)


### Leitura dos dados ----

dados_sim <- readRDS("./data/SIM/sim_consolidado.rds")

dados <- dados_sim %>%
  filter(TIPOBITO == 2, is.na(LINHAA) == FALSE) %>% #obito nao fetal
  select(IDADE, SEXO, LINHAA)%>% 
  #LINHAA é a causa que causou diretamente a morte, unico campo obrigatório
  mutate(CID = str_sub(LINHAA, start = 2, end =  5), #selecao do primeiro CID
         CID_cap = str_sub(CID, start = 1, end = 1),
         CID_num = str_sub(CID, start = 2, end = 4),
         CID_num = str_replace(CID_num, pattern = "X", replace = "0" ),
         IDADE = as.character(IDADE), #ajuste da idade
         IDADE = case_when(
           as.numeric(IDADE) <= 400 ~ "0",
           as.numeric(IDADE) > 400 & as.numeric(IDADE) < 500 ~ str_sub(as.character(IDADE), start = 2),
           as.numeric(IDADE) >= 500 & as.numeric(IDADE) < 515 ~ str_c("1", str_sub(as.character(IDADE), start = 2), sep = "")
         )
                     )%>%
  select(-LINHAA)


#### Categorizacao da causa de morte ----

dados_cid <- dados %>%
  mutate(causa = case_when(
    str_starts(CID, "A|B") == TRUE ~ "Algumas doenças infecciosas e parasitárias",
    str_starts(CID, "C") == TRUE ~ "Neoplasias",
    str_starts(CID, "D") == TRUE & CID_num < 500 ~ "Neoplasias",
    str_starts(CID, "D") == TRUE & CID_num >= 500 ~ "Doenças do sangue e org. hemato. e tr. imunit",
    str_starts(CID, "E") == TRUE ~ "Doenças endócrinas nutricionais e metabólicas",
    str_starts(CID, "F") == TRUE ~ "Transtornos mentais e comportamentais",
    str_starts(CID, "G") == TRUE ~ "Doenças do sistema nervoso",
    str_starts(CID, "H") == TRUE & CID_num < 600 ~ "Doenças do olho e anexos",
    str_starts(CID, "H") == TRUE & CID_num >= 600 ~ "Doenças do ouvido e da apófise mastóide",
    str_starts(CID, "I") == TRUE ~ "Doenças do aparelho circulatório",
    str_starts(CID, "J") == TRUE ~ "Doenças do aparelho respiratórios",
    str_starts(CID, "K") == TRUE ~ "Doenças do aparelho digestivo",
    str_starts(CID, "L") == TRUE ~ "Doenças da pele e do tecido subcutâneo",
    str_starts(CID, "M") == TRUE ~ "Doenças sist osteomuscular e tec conjuntivo",
    str_starts(CID, "N") == TRUE ~ "Doenças do aparelho geniturinário",
    str_starts(CID, "O") == TRUE ~ "Gravidez parto e puerpério",
    str_starts(CID, "P") == TRUE ~ "Algumas afec originadas no período perinatal",
    str_starts(CID, "Q") == TRUE ~ "Malf cong deformid e anomalias cromossômicass",
    str_starts(CID, "R") == TRUE ~ "Sint sinais e achad anorm ex clín e laborat",
    str_starts(CID, "S|T") == TRUE ~ "Lesões enven e alg out conseq causas externas",
    str_starts(CID, "V|W|X|Y") == TRUE ~ "Causas externas de morbidade e mortalidade",
    str_starts(CID, "Z") == TRUE ~ "Contatos com serviços de saúde"
    )
  )


#### Atribuicao de faixa etaria ----

faixa_et <- tibble(
  x = seq(0, 90, 5),
  y = c(seq(4, 89, 5), "+"),
  faixa = paste(x, "-", y, sep = "")
) %>% 
  pull(faixa)

faixa_et <- if_else(faixa_et == "90-+", "90+", faixa_et)

dados_cid_etaria <- dados_cid %>%
  mutate(
    IDADE = as.numeric(IDADE),
    faixa_et = cut(IDADE, breaks = c(seq(from = 0, to = 90, by = 5), 115),
                   right = FALSE, include.lowest = TRUE, labels = faixa_et)
  )



### Analise da estrutura da mortalidade, por sexo ----

dados_cid_etaria %>%
  group_by(faixa_etaria)