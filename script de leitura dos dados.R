pacman::p_load(read.dbc, dplyr, stringr)

anos <-c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

#digite aqui o caminho do seu computador
caminho <- "C:\\Users\\herme\\OneDrive\\Documentos\\UnBanda\\5º Semestre\\Demografia\\Natalidade-Fecundidade-Mortalidade-Demog"

dados_sim <- read.dbc(str_c(caminho, "\\SIM\\", "DOAC", anos[1], ".dbc"))

for (i in anos[2:length(anos)]) {
  print(i)
  temp <- read.dbc(str_c(caminho, "\\SIM\\", "DOAC", i, ".dbc"))
  dados_sim <- full_join(dados_sim, temp)
  }

dados_sinasc <- read.dbc(str_c(caminho, "\\sinasc\\", "DNAC", anos[1], ".dbc"))

for (i in anos[2:length(anos)]) {
  print(i)
  temp <- read.dbc(str_c(caminho, "\\sisnac\\", "DNAC", i, ".dbc"))
  dados_sim <- full_join(dados_sim, temp)
}

