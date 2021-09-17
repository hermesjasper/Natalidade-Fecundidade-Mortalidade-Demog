sinasc_consolidado <- readRDS("./data/sinasc/sinasc_consolidado.rds")

library(stringr)
library(dplyr)
library(ggplot2)

sinasc_consolidado$ANO_NASC <- sinasc_consolidado$DTNASC %>%str_extract('(?<=[0-9]{4})[0-9]{4}')
sinasc<-sinasc_consolidado

sinasc18<- sinasc[sinasc$ANO_NASC==2018, ]
sinasc19<-sinasc[sinasc$ANO_NASC==2019, ]
sinasc20<-sinasc[sinasc$ANO_NASC==2020, ]

library(readxl)
Popidadesexo_total <- read_excel("./data/Popidadesexo - total.xlsx")
Popidadesexo_mulheres <- read_excel("./data/Popidadesexo - mulheres.xlsx")

##nascidos vivos de m?es em certas idades 2018
library(fdth)
idademae18<-as.numeric(sinasc18$IDADEMAE)
d1 <- fdt (idademae18, start = 15, end = 50, h =5)
print(d1,columns = 1:2)
d18 <- data.frame("GRUPO ETARIO" = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),freqnasc18 = d1$table$f)

d18<-d18 %>% inner_join(Popidadesexo_mulheres[c(1,2)], by = c('GRUPO.ETARIO' = 'GRUPO ETÁRIO'))


##nascidos vivos de m?es em certas idades 2018
idademae19<-as.numeric(sinasc19$IDADEMAE)
d2 <- fdt (idademae19, start = 15, end = 50, h =5)
print(d2,columns = 1:2)
d19 <- data.frame("GRUPO ETARIO" = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),freqnasc19= d2$table$f)

d19<-d19 %>% inner_join(Popidadesexo_mulheres[c(1,3)], by = c('GRUPO.ETARIO' = 'GRUPO ETÁRIO'))

##nascidos vivos de m?es em certas idades 2018
idademae20<-as.numeric(sinasc20$IDADEMAE)
d3 <- fdt (idademae20, start = 15, end = 50, h =5)
print(d3,columns = 1:2)
d20 <- data.frame("GRUPO ETARIO" = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),freqnasc20 = d3$table$f)

d20<-d20 %>% inner_join(Popidadesexo_mulheres[c(1,4)], by = c('GRUPO.ETARIO' = 'GRUPO ETÁRIO'))


##########################################################################

#Taxa Bruta de Natalidade - 2018
tbn18<-((nrow(sinasc18))/Popidadesexo_total[1,2])*1000

#Taxa Bruta de Natalidade - 2019
tbn19<-((nrow(sinasc19))/Popidadesexo_total[1,3])*1000

#Taxa Bruta de Natalidade - 2020
tbn20<-((nrow(sinasc20))/Popidadesexo_total[1,4])*1000

#Taxa de Fecundidade Geral - 2018
tgf18<-((nrow(sinasc18))/sum(Popidadesexo_mulheres[5:11,2]))*1000

#Taxa de Fecundidade Geral - 2019
tgf19<-((nrow(sinasc19))/sum(Popidadesexo_mulheres[5:11,3]))*1000

#Taxa de Fecundidade Geral - 2020
tgf20<-((nrow(sinasc20))/sum(Popidadesexo_mulheres[5:11,4]))*1000

#Taxa de Fecundidade Espec?ficas - 2018
d18_2 <- d18 %>% mutate(TEF18 = (d18$freqnasc18/d18$`2018`)*1000)
d18_2
ggplot(d18_2, mapping = aes(GRUPO.ETARIO,TEF18))+
  geom_point()

#Taxa de Fecundidade Espec?ficas - 2019
d19_2 <- d19 %>% mutate(TEF19 = (d19$freqnasc19/d19$`2019`)*1000)
d19_2
ggplot(d19_2, mapping = aes(GRUPO.ETARIO,TEF19))+
  geom_point()

#Taxa de Fecundidade Espec?ficas - 2020
d20_2 <- d20 %>% mutate(TEF20 = (d20$freqnasc20/d20$`2020`)*1000)
d20_2
ggplot(d20_2, mapping = aes(GRUPO.ETARIO,TEF20))+
  geom_point()

#Taxa de Fecundidade Total - 2018
TFT18 <- 5*sum(d18_2$TEF18/1000)
TFT18

#Taxa de Fecundidade Total - 2019
TFT19 <- 5*sum(d19_2$TEF19/1000)
TFT19

#Taxa de Fecundidade Total - 2020
TFT20 <- 5*sum(d20_2$TEF20/1000)
TFT20
#############################################################

sinasc18_2<-sinasc18[sinasc18$SEXO==2,]
sinasc19_2<-sinasc19[sinasc19$SEXO==2,]
sinasc20_2<-sinasc20[sinasc20$SEXO==2,]


idademae18_2<-as.numeric(sinasc18_2$IDADEMAE)
df1 <- fdt (idademae18_2, start = 15, end = 50, h =5)
print(df1,columns = 1:2)
f18 <- data.frame("GRUPO ETARIO" = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),freqnascfem18 = df1$table$f)
f18<-f18 %>% inner_join(Popidadesexo_mulheres[c(1,2)], by = c('GRUPO.ETARIO' = 'GRUPO ETÁRIO'))

idademae19_2<-as.numeric(sinasc19_2$IDADEMAE)
df2 <- fdt (idademae19_2, start = 15, end = 50, h =5)
print(df2,columns = 1:2)
f19 <- data.frame("GRUPO ETARIO" = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),freqnascfem19 = df2$table$f)
f19<-f19 %>% inner_join(Popidadesexo_mulheres[c(1,3)], by = c('GRUPO.ETARIO' = 'GRUPO ETÁRIO'))

idademae20_2<-as.numeric(sinasc20_2$IDADEMAE)
df3 <- fdt (idademae20_2, start = 15, end = 50, h =5)
print(df3,columns = 1:2)
f20 <- data.frame("GRUPO ETARIO" = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),freqnascfem20 = df3$table$f)
f20<-f20 %>% inner_join(Popidadesexo_mulheres[c(1,4)], by = c('GRUPO.ETARIO' = 'GRUPO ETÁRIO'))

Tabua_de_vida_2018_mulheres <- read_excel("./data/Tabua de vida 2018 - mulheres.xlsx")
Tabua_de_vida_2019_mulhers <- read_excel("./data/Tabua de vida 2019- mulhers.xlsx")
Tabua_de_vida_2020_mulhers <- read_excel("./data/Tabua de vida 2020 - mulhers.xlsx")

Tabua_de_vida_2018_mulheres <- Tabua_de_vida_2018_mulheres[6:12,2]
Tabua_de_vida_2019_mulhers <- Tabua_de_vida_2019_mulhers[6:12,2]
Tabua_de_vida_2020_mulhers <- Tabua_de_vida_2020_mulhers[6:12,2]

#############################################################

#Taxas espec?ficas de fecundidade feminina - 2018
f18_2 <- f18 %>% mutate(TEFfem18 = (f18$freqnascfem18/f18$`2018`)*1000)
f18_2

#Taxas espec?ficas de fecundidade feminina - 2019
f19_2 <- f19 %>% mutate(TEFfem19 = (f19$freqnascfem19/f19$`2019`)*1000)
f19_2

#Taxas espec?ficas de fecundidade feminina - 2020
f20_2 <- f20 %>% mutate(TEFfem20 = (f20$freqnascfem20/f20$`2020`)*1000)
f20_2

#Taxa Bruta de Reprodu??o - 2018
TBR18 <- 5*sum(f18_2$TEFfem18/1000)
TBR18

#Taxa Bruta de Reprodu??o - 2019
TBR19 <- 5*sum(f19_2$TEFfem19/1000)
TBR19

#Taxa Bruta de Reprodu??o - 2020
TBR20 <- 5*sum(f20_2$TEFfem20/1000)
TBR20

#Taxa L?quida de Reprodu??o - 2018
f18_3 <- f18_2 %>% mutate(TDVfem18 = (Tabua_de_vida_2018_mulheres$nLx/100000))
TLR18<-sum((f18_3$TEFfem18/1000)*f18_3$TDVfem18)

#Taxa L?quida de Reprodu??o - 2019
f19_3 <- f19_2 %>% mutate(TDVfem19 = (Tabua_de_vida_2019_mulhers$nLx/100000))
TLR19<-sum((f19_3$TEFfem19/1000)*f19_3$TDVfem19)

#Taxa L?quida de Reprodu??o - 2020
f20_3 <- f20_2 %>% mutate(TDVfem20 = (Tabua_de_vida_2020_mulhers$nLx/100000))
TLR20<-sum((f20_3$TEFfem20/1000)*f20_3$TDVfem20)