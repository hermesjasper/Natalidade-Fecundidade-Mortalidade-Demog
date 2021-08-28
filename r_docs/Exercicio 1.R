pacman::p_load(read.dbc, dplyr, stringr,foreign,LexisPlotR,lubridate,visdat,ggplot2)

sim_consolidado <- readRDS("~/GitHub/Natalidade-Fecundidade-Mortalidade-Demog/data/sim/sim_consolidado.rds")

### Ajustando o banco SIM:
idade <- function(x){
  if(str_sub(x,1,1)<4){
    x = 0}else if (str_sub(x,1,1)==4){
      x = 0+as.numeric(str_sub(x,2))}
    else if (str_sub(x,1,1)==5){
      x = 100+as.numeric(str_sub(x,2))}
    else{
      x = NA}
    return (x)
}
sim_lexis <- sim_consolidado %>%
  select (DTOBITO,SEXO,DTNASC,CODMUNRES,IDADE) %>%
  mutate(DTOBITO=dmy(DTOBITO),
         DTNASC=dmy(DTNASC))
sim_lexis$IDADEA <- sapply(sim_consolidado$IDADE, idade)

vis_miss(sim_lexis)

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

dados

### Este diagrama ainda apresenta erros.

diagrama <- lexis_grid(year_start=2010,year_end=2016,age_start=0,age_end=5,delta=1)

diagrama <- diagrama +
  annotate(geom="text", x=as.Date(paste0(dados$ano_obito[dados$SEXO==2]
                                         ,"-08-06"))
           ,y=dados$id_ob_anos_comp[dados$SEXO==2]+0.3,
           label=c(paste0(dados$quantidade[dados$SEXO==2])),
           color="black") +
  annotate(geom="text", x=as.Date(paste0(dados$ano_obito[dados$SEXO==2]
                                         ,"-05-06"))
           ,y=dados$id_ob_anos_comp[dados$SEXO==2]+0.75,
           label=c(paste0(dados$quantidade[dados$SEXO==2])),
           color="black")
diagrama