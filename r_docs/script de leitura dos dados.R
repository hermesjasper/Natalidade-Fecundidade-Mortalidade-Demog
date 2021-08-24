pacman::p_load(read.dbc, dplyr, stringr)

# Para rodar o script, vc deve ter no seu computador, na pasta 'data', duas pastas:
# - uma chamada "SIM" com os arquivos.dbc do SIM 
# - e outra chamada sinasc, com os arquivos.dbc do sinasc

anos <-c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

#digite aqui o caminho inicial do projeto no seu computador
caminho <- "C:\\Users\\herme\\OneDrive\\Documentos\\UnBanda\\5º Semestre\\Demografia\\Natalidade-Fecundidade-Mortalidade-Demog"

dados_sim <- read.dbc(str_c(caminho, "\\data\\SIM\\", "DOAC", anos[1], ".dbc"))
dados_sinasc <- read.dbc(str_c(caminho, "\\data\\sinasc\\", "DNAC", anos[1], ".dbc"))

for (i in anos[2:length(anos)]) {
  dados_sim <- full_join(dados_sim, read.dbc(str_c(caminho, "\\data\\SIM\\", "DOAC", i, ".dbc")))
  dados_sinasc <- full_join(dados_sinasc, read.dbc(str_c(caminho, "\\data\\sinasc\\", "DNAC", i, ".dbc")))
}

