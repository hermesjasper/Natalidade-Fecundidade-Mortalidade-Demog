<img align="right" width="140" height="140" src="http://s3-sa-east-1.amazonaws.com/descomplica-blog/wp-content/uploads/2016/04/trabalhar-na-UnB.png">

# Natalidade/Fecundidade e Mortalidade da população do Acre entre os anos de 2000 e 2020

## Descrição do projeto :memo:

O seguinte projeto tem como objetivo:

- Aplicar os conceitos e técnicas para a análise das componentes natalidade/fecundidade e mortalidade a dados reais da população do Acre entre os anos 2000 e 2020.

- Avaliar a qualidade da informação dos sistemas de informação sobre nascidos vivos e mortalidade do Ministério da Saúde para análise da fecundidade e mortalidade das unidades do Acre.

Os dados utilizados são do SIM e SINASC e são disponibilizados no site do [DataSus](https://datasus.saude.gov.br/transferencia-de-arquivos/).

<br>

## Integrantes :dancers:

> Hermes Jasper Winarski | **190029498**  <!---Nome, Telefone, Email -->

> Bruno Gondim Toledo | **150167636**

> Ana Luiza Carneiro de Almeida | **180012801**

> César Galvão | **190011572**

<br>

## Links

- [Instruções do Trabalho](https://aprender3.unb.br/mod/assign/view.php?id=457193) <!---Colocar aqui o link do projeto ou qualquer outro link :) -->
- [Tabnet população](http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/projpopuf.def)

<br>

## Instruções do trabalho

### Objetivos

* Aplicar os conceitos e técnicas para a análise das componentes natalidade/fecundidade e mortalidade a dados reais.
* Avaliar a qualidade da informação dos Sistemas de Informação sobre Nascidos Vivos e Mortalidade do Ministério da Saúde para análise da fecundidade e mortalidade das Unidades da Federação e outras regiões no Brasil
* Familiarizar com os bancos de dados de nascidos vivos e óbitos do Ministério da Saúde.
* Para a Unidade da Federação escolhida, obtenha os dados de nascimentos vivos e óbitos de 2000 a 2020.

<br>

#### Questão 1: Diagrama de Lexis

a) Construir o Diagrama de Lexis para os dados de nascidos vivos de 2000 a 2020 da UF escolhida (SINASC) e de óbitos menores de 5 anos (idades simples) para o mesmo período segundo ano de nascimento.

b) Supondo população fechada (inexistência de migração), calcule a probabilidade de um recém-nascido na UF ou território de escolha sobreviver à idade exata 5 para as coortes de 2000 a 2015.

c) Considerando o mesmo pressuposto, calcule a probabilidade de sobreviver ao primeiro aniversário dos recém-nascidos no período de 2000 a 2019.

d) Comente sobre os valores encontrados. Não esquecer a qualidade da informação trabalhada.

 <br>

#### Questão 2: Natalidade/Fecundidade 

a) Com base nos dados do SINASC para os de 2018 a 2020 e na população por sexo e idade estimada (projetada - 2019), construa os seguintes indicadores para a Unidade da Federação:

* Taxa Bruta de Natalidade
* Taxa Global de Fecundidade e Taxas específicas de fecundidade - nfx (Grafique esses valores)
* Taxa de Fecundidade Total
* Taxas específicas de fecundidade feminina (apenas os nascimentos femininos)
* Taxa Bruta de Reprodução
* Taxa Líquida de Reprodução (é necessária a informação da função L da Tábua de Vida)

b) Compare os valores obtidos com os publicados pela RIPSA (2011) e GBD. Como a RIPSA não divulga os indicadores de reprodução, com base nos indicadores publicados, calcule esses indicadores.

c) Para os dados do SINASC para 2020, analise a associação entre (apresente ao menos uma medida de associação):

* idade e escolaridade da mãe 
* tipo de parto e escolaridade da mãe

d) Comente esses resultados (inclusive os gráficos das nfx), fazendo referência a artigos já publicados sobre o assunto.

<br>

#### Questão 3: Mortalidade

a) Com base nos dados sobre óbitos do SIM para 2018 a 2020 e a população por sexo e idade estimada (projetada) em 2019 para a UF, obtenha os seguintes indicadores:

* Taxa Bruta de Mortalidade
* Taxas específicas de mortalidade por sexo e idade - nMx (grafique)

b) Calcule a TMI, utilizando o número médio de óbitos ocorridos entre 2018 e 2020 e o número de nascimentos de 2019. Calcule os indicadores: taxa de mortalidade neonatal, neonatal precoce, neonatal tardia, posneonatal. Agregando a informação sobre óbitos fetais para os mesmos anos, calcule a taxa de mortalidade perinatal.

c) Compare os valores encontrados para todos os indicadores com aqueles publicados pela RIPSA (2011) e GBD. Para a TMI, compare com os valores obtidos na questão 1. Comente sobre os aspectos metodológicos dessas duas formas de cálculo.

d) Compare a estrutura de mortalidade por causas (Capítulos da CID10 - reagrupados em 6/7 grandes causas) segundo sexo de 2015 e 2020. Comente os resultados. Destacar a mortalidade por Covid-19.

e) Construa Tábuas de Vida para cada sexo para a UF escolhida para 2019, a partir das taxas específicas de mortalidade obtidas no item a:

* Utilize a TMI obtida no item b ou do estudo Global Burden of Disease - GBD. Lembre que deve-se obter a TMI para cada sexo em separado.
* Estime os fatores de separação, para cada sexo, para as idades 0-1 e 1-4 com base nos dados do SIM.
* Compare os valores da função esperança de vida para as idades exatas 0 e 60 com aqueles publicados pela RIPSA e pelo estudo GBD. Comente sobre os resultados obtidos e sobre o significado desses indicadores.
* Com base na TV calculada, grafique as funções lx e nqx para cada sexo e comente os resultados. Se lo=1, qual o significado da função lx? Interprete, neste caso, l20 e l60.
* Comente os resultados à luz de artigos recém publicados.
