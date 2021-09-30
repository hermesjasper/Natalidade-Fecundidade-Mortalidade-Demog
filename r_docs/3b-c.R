options(scipen = 999)
pacman::p_load(dplyr, stringr, scales, ggplot2, reshape2,Rcpp, forcats,RColorBrewer, gridExtra, here)

sim_consolidado <- readRDS("./data/sim/sim_consolidado.rds")
sinasc_consolidado <- readRDS("./data/sinasc/sinasc_consolidado.rds")

sim_consolidado$ANO_OBITO <- sim_consolidado$DTOBITO %>% str_extract('(?<=[0-9]{4})[0-9]{4}')
sinasc_consolidado$ANO_NASC <- sinasc_consolidado$DTNASC %>% str_extract('(?<=[0-9]{4})[0-9]{4}')

paleta <- brewer.pal(n = 10, name = "Set3")

## Questão 3: Mortalidade
### b) Calcule a TMI, utilizando o número médio de óbitos ocorridos entre 2018 e 2020 e o número de nascimentos de 2019. Calcule os indicadores: taxa de mortalidade neonatal, neonatal precoce, neonatal tardia, posneonatal. Agregando a informação sobre óbitos fetais para os mesmos anos, calcule a taxa de mortalidade perinatal.

sim_b <- sim_consolidado %>% filter(ANO_OBITO >=2018  & ANO_OBITO<=2020)

nascimentos_19 <- sinasc_consolidado %>% filter(ANO_NASC == 2019) %>% nrow()

# Menor que 1 ano de idade
mortes_infantil_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 400) %>%  nrow()
TMI <- (mortes_infantil_18_20/3)/nascimentos_19

# Menor que 28 dias de idade
mortes_neotal_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 227) %>% nrow()
tx_mort_neotal <- (mortes_neotal_18_20/3)/nascimentos_19

# Menor que 1 semana de vida
mortes_neotal_prec_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 207) %>% nrow()
tx_mort_neotal_precoce <- (mortes_neotal_prec_18_20/3)/nascimentos_19

# 1 semana a 1 mes de vida
mortes_neotal_tard_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 227 & 
                                     as.numeric(as.character(sim_b$IDADE))> 207) %>% nrow()
tx_mort_neotal_tardia <- (mortes_neotal_tard_18_20/3)/nascimentos_19

# 1 mes a 1 ano de vida
mortes_posneotal_18_20 <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 400 & 
                                   as.numeric(as.character(sim_b$IDADE))>= 301) %>% nrow()
tx_mort_posneotal <- (mortes_posneotal_18_20/3)/nascimentos_19

# de 22 semanas de gestação a 7 dias de vida

mortes_neotal_perinatal <- filter(sim_b,as.numeric(as.character(sim_b$IDADE))<= 207) %>% nrow()
mortes_neotal_perinatal <- mortes_neotal_perinatal + filter(sim_b,as.numeric(as.character(sim_b$SEMAGESTAC))>= 22) %>% nrow()

tx_mort_neotal_perinatal <- (mortes_neotal_perinatal/3)/nascimentos_19

# antes do nascimento
obitos_fetais <- filter(sim_b,as.numeric(as.character(sim_b$TIPOBITO))== 1) %>% nrow()

tx_mort_obitos_fetais <- (obitos_fetais/3)/nascimentos_19

tabela3b <- data.frame('Dados' = "Acre 2018-2020",
                       'Infantil' = TMI*1000,
                       'Neonatal' = tx_mort_neotal*1000,
                       'Neonatal Precoce' = tx_mort_neotal_precoce*1000,
                       'Neonatal Tardia' = tx_mort_neotal_tardia*1000,
                       'Pós Neonatal' = tx_mort_posneotal*1000,
                       'Neonatal Perinatal' = tx_mort_neotal_perinatal*1000,
                       'Obitos Fetais' = tx_mort_obitos_fetais*1000)

colnames(tabela3b) <- c('Dados', 'Infantil','Neonatal', 'Neonatal Precoce', 'Neonatal Tardia', 'Pós Neonatal', 'Neonatal Perinatal', 'Obitos Fetais')


### c) Compare os valores encontrados para todos os indicadores com aqueles publicados pela RIPSA (2011) e GBD. Para a TMI, compare com os valores obtidos na questão 1. Comente sobre os aspectos metodológicos dessas duas formas de cálculo.

ripsa <- data.frame('Dados' = c("Norte 1991","Norte 1997","Norte 2000","Norte 2004"),
                    'Infantil' = c(42.3,32.2,28.7,25.5),
                    'Neonatal' = c(22.6,20.6,18.7,16.3),
                    'Neonatal Precoce' = c(18.1,16.5,14.8,12.8),
                    'Neonatal Tardia' = c(4.5,4.1,3.9,3.5),
                    'Pós Neonatal' = c(21.7,11.6,10.1,8.2))
colnames(ripsa) <- c('Dados', 'Infantil', 'Neonatal', 'Neonatal Precoce', 'Neonatal Tardia', 'Pós Neonatal')

tabela3c <- full_join(ripsa,tabela3b[,-c(ncol(tabela3b)-1,ncol(tabela3b))]) %>% 
  mutate(temp=c(0,1,2,3,4)) %>% mutate(tipo = c("RIPSA","RIPSA","RIPSA","RIPSA","Demografia 1/2021"))


# infantil
plot3c_a <- ggplot(tabela3c, aes(x=fct_reorder(Dados,temp),y = Infantil, fill = tipo))+
  geom_bar(stat = 'identity', width = 0.6)+
  geom_line(aes(group = 0), size = 1.2, alpha = 0.7, color = paleta[6])+
  geom_text(aes(label = round(Infantil,2), y = Infantil),
            size = 3.1, position = position_stack(vjust = 1.05))+
  labs(title = "Gráfico x: Taxa de mortalidade infantil, por 1000 nascidos vivos",
       x = "", y = "")+
  ylim(0,50)+
  scale_fill_brewer()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 11))

# Neonatal
plot3c_b <- ggplot(tabela3c, aes(x=fct_reorder(Dados,temp),y = Neonatal, fill = tipo))+
  geom_bar(stat = 'identity', width = 0.6)+
  geom_line(aes(group = 0), size = 1.2, alpha = 0.7, color = paleta[6])+
  geom_text(aes(label = round(Neonatal,2), y = Neonatal),
            size = 3.1, position = position_stack(vjust = 1.05))+
  labs(title = "Gráfico x: Taxa de mortalidade neonatal, por 1000 nascidos vivos",
       x = "", y = "")+
  ylim(0,30)+
  scale_fill_brewer()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 11))

# Neonatal Precoce
plot3c_c <- ggplot(tabela3c, aes(x=fct_reorder(Dados,temp),y =`Neonatal Precoce`, fill = tipo))+
  geom_bar(stat = 'identity', width = 0.6)+
  geom_line(aes(group = 0), size = 1.2, alpha = 0.7, color = paleta[6])+
  geom_text(aes(label = round(`Neonatal Precoce`,2), y = `Neonatal Precoce`),
            size = 3.1, position = position_stack(vjust = 1.05))+
  labs(title = "Gráfico x: Taxa de mortalidade neonatal precoce, por 1000 nascidos vivos",
       x = "", y = "")+
  ylim(0,25)+
  scale_fill_brewer()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 11))

# Neonatal Tardia
plot3c_d <- ggplot(tabela3c, aes(x=fct_reorder(Dados,temp),y =`Neonatal Tardia`, fill = tipo))+
  geom_bar(stat = 'identity', width = 0.6)+
  geom_line(aes(group = 0), size = 1.2, alpha = 0.7, color = paleta[6])+
  geom_text(aes(label = round(`Neonatal Tardia`,2), y = `Neonatal Tardia`),
            size = 3.1, position = position_stack(vjust = 1.05))+
  labs(title = "Gráfico x: Taxa de mortalidade neonatal tardia, por 1000 nascidos vivos",
       x = "", y = "")+
  ylim(0,8)+
  scale_fill_brewer()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 11))

# Pós Neonatal
plot3c_e <- ggplot(tabela3c, aes(x=fct_reorder(Dados,temp),y =`Pós Neonatal`, fill = tipo))+
  geom_bar(stat = 'identity', width = 0.6)+
  geom_line(aes(group = 0), size = 1.2, alpha = 0.7, color = paleta[6])+
  geom_text(aes(label = round(`Pós Neonatal`,2), y = `Pós Neonatal`), 
            size = 3.1, position = position_stack(vjust = 1.05))+
  labs(title = "Gráfico x: Taxa de mortalidade pós neonatal, por 1000 nascidos vivos",
       x = "", y = "")+
  ylim(0,25)+
  scale_fill_brewer()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 11))


rm(ripsa)
rm(sim_b)
rm(mortes_infantil_18_20)
rm(mortes_neotal_18_20)
rm(mortes_neotal_perinatal)
rm(mortes_neotal_prec_18_20)
rm(mortes_neotal_tard_18_20)
rm(mortes_posneotal_18_20)
rm(nascimentos_19)
rm(obitos_fetais)
rm(paleta)
rm(TMI)
rm(tx_mort_neotal)
rm(tx_mort_neotal_perinatal)
rm(tx_mort_neotal_precoce)
rm(tx_mort_neotal_tardia)
rm(tx_mort_obitos_fetais)
rm(tx_mort_posneotal)

