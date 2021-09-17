pacman::p_load(read.dbc, dplyr, stringr,foreign)
#sim_2020_brasil <- read.dbf("D:/Arquivos/unb2021/TrabDem/pasta/DOBR20DA.DBF")
#sinasc_2020_brasil <- read.dbf("D:/Arquivos/unb2021/TrabDem/pasta/DNBR20DA.DBF")

#códigos municipios Acre:
#      1  120001 Acrelândia                                  120001                            
#2  120005 Assis Brasil                                120005                            
#3  120010 Brasiléia                                   120010                            
#4  120013 Bujari                                      120013                            
#5  120017 Capixaba                                    120017                            
#6  120020 Cruzeiro do Sul                             120020                            
#7  120025 Epitaciolândia                              120025                            
#8  120030 Feijó                                       120030                            
#9  120032 Jordão                                      120032                            
#10  120033 Mâncio Lima                                 120033                            
#11  120034 Manoel Urbano                               120034                            
#12  120035 Marechal Thaumaturgo                        120035                            
#13  120038 Plácido de Castro                           120038                            
#14  120080 Porto Acre                                  120080                            
#15  120039 Porto Walter                                120039                            
#16  120040 Rio Branco                                  120040                            
#17  120042 Rodrigues Alves                             120042                            
#18  120043 Santa Rosa do Purus                         120043                            
#19  120050 Sena Madureira                              120050                            
#20  120045 Senador Guiomard                            120045                            
#21  120060 Tarauacá                                    120060                            
#22  120070 Xapuri                                      120070                            
#23  120000 Município ignorado - AC                     120000,129999

#sinasc_2020_ACRE <- sinasc_2020_brasil %>%
  #filter(CODMUNNASC %in% c("120001", "120005", "120010", "120013", "120017", "120020", "120025", 
 #                          "120030", "120032", "120033", "120034", "120035", "120038", "120039", 
  #                         "120040", "120042", "120043", "120045", "120050", "120060", "120070", 
   #                        "120080", "120000", "129999"))

#sim_2020_ACRE <- sim_2020_brasil %>%
 # filter(CODMUNNATU %in% c("120001", "120005", "120010", "120013", "120017", "120020", "120025", 
    #                       "120030", "120032", "120033", "120034", "120035", "120038", "120039", 
     #                      "120040", "120042", "120043", "120045", "120050", "120060", "120070", 
      #                     "120080", "120000", "129999"))

#saveRDS(sinasc_2020_ACRE, file = "./data/sinasc/sinasc_2020_ACRE.rds")
#saveRDS(sim_2020_ACRE, file = "./data/SIM/sim_2020_ACRE.rds")

#remove(sinasc_2020_brasil)
#remove(sim_2020_brasil)

sim <- paste("./data/SIM/",list.files("./data/SIM/", pattern = "*.dbc"), sep = "")

sinasc <- paste("./data/sinasc/",list.files("./data/sinasc/", pattern = "*.dbc"), sep = "")

sim_consolidado <- read.dbc(sim[1]) 
colnames(sim_consolidado) <- colnames(sim_consolidado) %>% toupper()

sinasc_consolidado <- read.dbc(sinasc[1])
colnames(sinasc_consolidado) <- colnames(sinasc_consolidado) %>% toupper()

for (i in c(2:length(sim))){
  temp <- read.dbc(sim[i])
  colnames(temp) <- colnames(temp) %>% toupper()
  sim_consolidado <- full_join(sim_consolidado, temp)
  
  temp <- read.dbc(sinasc[i])
  colnames(temp) <- colnames(temp) %>% toupper()
  sinasc_consolidado <- full_join(sinasc_consolidado, temp)
  
}
rm(temp)

sim_consolidado <- full_join(sim_consolidado, readRDS("./data/sim/SIM_2020_ACRE.rds"))
#sim_consolidado <- readRDS("./data/sim/sim_consolidado.rds")
sim_consolidado$ac <- sim_consolidado$CODMUNRES %>% str_extract('[0-9]{2}')
sim_consolidado <- sim_consolidado %>% filter(ac == '12')

sinasc_consolidado <- full_join(sinasc_consolidado, readRDS("./data/sinasc/sinasc_2020_ACRE.rds"))
#sinasc_consolidado <- readRDS("./data/sinasc/sinasc_consolidado.rds")
sinasc_consolidado$ac <- sinasc_consolidado$CODMUNRES %>% str_extract('[0-9]{2}')
sinasc_consolidado <- sinasc_consolidado %>% filter(ac == '12')


saveRDS(sim_consolidado, file = "./data/SIM/sim_consolidado.rds")
saveRDS(sinasc_consolidado, file = "./data/sinasc/sinasc_consolidado.rds")

