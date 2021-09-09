pacman::p_load(read.dbc, dplyr, stringr,foreign,LexisPlotR,lubridate,visdat,ggplot2)

sim_consolidado <- readRDS("../data/SIM/sim_consolidado.rds") # caminho ajustado para agir pelo Rproject

### Ajustando o banco SIM:
idade <- function(x){
  if(str_sub(x,1,1)<4){ #comentar esse caso
    x = 0} else if (str_sub(x,1,1)==4){ #comentar esse caso
      x = 0+as.numeric(str_sub(x,2))}
    else if (str_sub(x,1,1)==5){ #comentar esse caso
      x = 100+as.numeric(str_sub(x,2))}
    else{ #comentar esse caso
      x = NA}
    return (x)
}

sim_lexis <- sim_consolidado %>%
  select (DTOBITO,SEXO,DTNASC,CODMUNRES,IDADE) %>%
  mutate(DTOBITO=dmy(DTOBITO),
         DTNASC=dmy(DTNASC))

sim_lexis$IDADEA <- sapply(sim_consolidado$IDADE, idade) #funcao idade não roda. Verificar e comentar os casos

# vis_miss(sim_lexis)

dados <- sim_lexis %>%
  mutate(
    IDOBITO= as.numeric(difftime(DTOBITO,DTNASC,units = "days")/365),
    ano_nasc=year(DTNASC),
    ano_obito=year(DTOBITO),
    id_ob_anos_comp=floor(IDOBITO)) %>%
  group_by(id_ob_anos_comp, ano_nasc, ano_obito, SEXO) %>%
  mutate(quantidade=n())%>%
  ungroup()%>%
  distinct(id_ob_anos_comp,SEXO,ano_nasc,ano_obito,.keep_all = TRUE)%>%
  arrange(ano_obito,desc(ano_nasc),id_ob_anos_comp,SEXO)

#QUESTÃO 1) 
# a)
### Este diagrama ainda apresenta erros. (os números vêm empilhados em cada triângulo ???)
### Esta questão pede para trabalhar com os dados do SINASC, mas o mesmo não vêm com dados de mortalidade,
### portanto, optei por utilizar os dados do SIM.

dados1 <- dados %>%
filter(ano_nasc >= "2000" & ano_nasc <= "2020")

diagrama <- lexis_grid(year_start=2000,year_end=2020,age_start=0,age_end=5,delta=1)

diagrama <- diagrama +
  annotate(geom="text", x=as.Date(paste0(dados1$ano_obito,"-08-06"))
           ,y=dados1$id_ob_anos_comp+0.3,
           label=c(paste0(dados1$quantidade)),
           color="red") +
  annotate(geom="text", x=as.Date(paste0(dados1$ano_obito,"-05-06"))
           ,y=dados1$id_ob_anos_comp+0.75,
           label=c(paste0(dados1$quantidade)),
           color="blue")
diagrama



# b)

# c)

# d)
