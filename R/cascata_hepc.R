########### Analise cascata hepatite C - Hemodiálise ###############
####################################################################
###### script desenvolvido por Mikael Lemos ########################
###### versão 1.0 - 12.09.2020 #####################################
####################################################################

######
### Carregando / instalando pacotes
######

library('plyr')

#install.packages('dplyr')
library('dplyr')

#install.packages("tidyr")
library('tidyr')

#install.packages("data.table")
library('data.table')

#install.packages('stringr')
library('stringr')

# install.packages("tidyverse")
library("tidyverse")

# install.packages("lubridate")
library("lubridate")

#install.packages("Amelia")
library("Amelia")

#install.packages('microbenchmark')
library("microbenchmark")

#install.packages('ggplot2movies')
library("ggplot2movies")

#install.packages('profvis')
library("profvis")

#install.packages('Rcpp')
library("Rcpp")

#install.packages('compiler')
library("compiler")

#install.packages('memoise')
library("memoise")

#install.packages('DiagrammeR')
library("DiagrammeR")

#install.packages('rio')
library("rio")

#install.packages('readr')
library("readr")

#install.packages('data.table')
library("data.table")

#install.packages('feather')
library("feather")

#install.packages('WDI')
library("WDI")

#install.packages('eeptools')
library("eeptools")

#install.packages("waffle")
library("waffle")

#install.packages("ggthemes")
library("ggthemes")

#install.packages("waffle")
library("waffle")

#install.packages("plotly")
library("plotly")

library("stringr")

library("ggplot2")

library(RColorBrewer)

#install.packages("pryr")
library(pryr)

library("openxlsx")

##### Memória ######
memory.size(max=T) # mem obtida pelo OS
memory.size(max=F) # mem usada
mem_used()
memory.limit()

gc() # garbage colector
rm(list = ls())
.rs.restartR()
memory.limit(size=)


#####################
######## Infectados   -  AIH (hemod) + APAC (hemod) x GAL (PCR - RNA)
####################        

#### 2015

#### 0305010042 hemodialise continua + 0305010093 hemod max 1 sessao por semana + 305010107  max 3 sessoes por semana + 0305010115 hemod HIV e/ou Hep B e/ou Hep C max 3 sessoes por semana + 
#### 0305010123  hemod HIV e/ou Hep B e/ou Hep C max 1 sessao por semana + 0305010131 HEMODIALISE P/ PACIENTES RENAIS AGUDOS / CRONICOS AGUDIZADOS S/ TRATATAMENTO DIALITICO INICIADO

#### AIH

AIH_hemod_2015 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2015.csv")

AIH_hemod_2015$DT_INTERNACAO <- as.character(AIH_hemod_2015$DT_INTERNACAO)
AIH_hemod_2015$DT_SAIDA <- as.character(AIH_hemod_2015$DT_SAIDA)

AIH_hemod_2015_01_01_2015 <- filter(AIH_hemod_2015, DT_INTERNACAO=="01/01/2015" | DT_SAIDA=="01/01/2015" )

#### APAC

APAC_hemod <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/TB_ESPELHO_APAC_201911261121.csv")

APAC_hemod$COMPETENCIA <- as.character(APAC_hemod$COMPETENCIA)

APAC_hemod_01_2015 <-  filter(APAC_hemod, COMPETENCIA=="201501" )

#### AIH (hemod) + APAC (hemod)

AIH_hemod_01_01_2015_APAC_hemod_01_2015 <- full_join( AIH_hemod_2015_01_01_2015, APAC_hemod_01_2015, by = "ID_PACIENTE"  )

AIH_hemod_01_01_2015_APAC_hemod_01_2015_un <- distinct(AIH_hemod_01_01_2015_APAC_hemod_01_2015, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_01_01_2015_APAC_hemod_01_2015_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_01_01_2015_APAC_hemod_01_2015_un.csv")

#### GAL

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_LIBERACAO <- as.character(GAL_hepc_RNA$DT_LIBERACAO)

############################################################
###### Encontrado na análise do AIH e APAC - Mikael - Cenário 1 - PCR-RNA

# AIH_hemod_01_01_2015_APAC_hemod_01_2015_un (88.626) X prevalência PCR-RNA Benzaken,2019 - Hepatitis C disease burden and strategies for elimination by 2030 in Brazil. A mathematical modeling approach (0,31% x 5 = 1,55% = 0,015 ) = 1.330

###### Encontrado na análise do AIH e APAC - Mikael - Cenário 2 - Sorológico

# AIH_hemod_01_01_2015_APAC_hemod_01_2015_un (88.626) X prevalência inquérito brasileiro de diálise crônica (0,033 - 3,3%) = 2.925
##########################################################

#### 2016

#### 0305010042 hemodialise continua + 0305010093 hemod max 1 sessao por semana + 305010107  max 3 sessoes por semana + 0305010115 hemod HIV e/ou Hep B e/ou Hep C max 3 sessoes por semana + 
#### 0305010123  hemod HIV e/ou Hep B e/ou Hep C max 1 sessao por semana + 0305010131 HEMODIALISE P/ PACIENTES RENAIS AGUDOS / CRONICOS AGUDIZADOS S/ TRATATAMENTO DIALITICO INICIADO

#### AIH

AIH_hemod_2016 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2016.csv")

AIH_hemod_2016$DT_INTERNACAO <- as.character(AIH_hemod_2016$DT_INTERNACAO)
AIH_hemod_2016$DT_SAIDA <- as.character(AIH_hemod_2016$DT_SAIDA)

AIH_hemod_2016_01_01_2016 <- filter(AIH_hemod_2016, DT_INTERNACAO=="01/01/2016" | DT_SAIDA=="01/01/2016" )

#### APAC

APAC_hemod <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/TB_ESPELHO_APAC_201911261121.csv")

APAC_hemod$COMPETENCIA <- as.character(APAC_hemod$COMPETENCIA)

APAC_hemod_01_2016 <-  filter(APAC_hemod, COMPETENCIA=="201601" )

#### AIH (hemod) + APAC (hemod)

AIH_hemod_01_01_2016_APAC_hemod_01_2016 <- full_join( AIH_hemod_2016_01_01_2016, APAC_hemod_01_2016, by = "ID_PACIENTE"  )

AIH_hemod_01_01_2016_APAC_hemod_01_2016_un <- distinct(AIH_hemod_01_01_2016_APAC_hemod_01_2016, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_01_01_2016_APAC_hemod_01_2016_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_01_01_2016_APAC_hemod_01_2016_un.csv")

#### GAL

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_LIBERACAO <- as.character(GAL_hepc_RNA$DT_LIBERACAO)

############################################################
###### Encontrado na análise do AIH e APAC - Mikael - Cenário 1 - PCR-RNA

# AIH_hemod_01_01_2016_APAC_hemod_01_2016_un (91.042) X prevalência PCR-RNA Benzaken,2019 - Hepatitis C disease burden and strategies for elimination by 2030 in Brazil. A mathematical modeling approach (0,31% x 5 = 1,55% = 0,015 ) = 1.366

###### Encontrado na análise do AIH e APAC - Mikael - Cenário 2 - Sorológico

# AIH_hemod_01_01_2016_APAC_hemod_01_2016_un (91.042) X prevalência inquérito brasileiro de diálise crônica (0,033 - 3,3%) = 3.005
##########################################################

#### 2017

#### 0305010042 hemodialise continua + 0305010093 hemod max 1 sessao por semana + 305010107  max 3 sessoes por semana + 0305010115 hemod HIV e/ou Hep B e/ou Hep C max 3 sessoes por semana + 
#### 0305010123  hemod HIV e/ou Hep B e/ou Hep C max 1 sessao por semana + 0305010131 HEMODIALISE P/ PACIENTES RENAIS AGUDOS / CRONICOS AGUDIZADOS S/ TRATATAMENTO DIALITICO INICIADO

#### AIH

AIH_hemod_2017 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2017.csv")

AIH_hemod_2017$DT_INTERNACAO <- as.character(AIH_hemod_2017$DT_INTERNACAO)
AIH_hemod_2017$DT_SAIDA <- as.character(AIH_hemod_2017$DT_SAIDA)

AIH_hemod_2017_01_01_2017 <- filter(AIH_hemod_2017, DT_INTERNACAO=="01/01/2017" | DT_SAIDA=="01/01/2017" )

#### APAC

APAC_hemod <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/TB_ESPELHO_APAC_201911261121.csv")

APAC_hemod$COMPETENCIA <- as.character(APAC_hemod$COMPETENCIA)

APAC_hemod_01_2017 <-  filter(APAC_hemod, COMPETENCIA=="201701" )

#### AIH (hemod) + APAC (hemod)

AIH_hemod_01_01_2017_APAC_hemod_01_2017 <- full_join( AIH_hemod_2017_01_01_2017, APAC_hemod_01_2017, by = "ID_PACIENTE"  )

AIH_hemod_01_01_2017_APAC_hemod_01_2017_un <- distinct(AIH_hemod_01_01_2017_APAC_hemod_01_2017, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_01_01_2017_APAC_hemod_01_2017_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_01_01_2017_APAC_hemod_01_2017_un.csv")

#### GAL

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_LIBERACAO <- as.character(GAL_hepc_RNA$DT_LIBERACAO)

###########################################
##################### Número estimado de infectados no ano em AIH 01/01/2017 e APAC *01/2017 e Inquérito  brasileiro de diálise crônica
###########################################

#############################################################
###### Inquérito brasileiro de diálise crônica - Cenário 1 - PCR-RNA

# inquérito brasileiro de diálise crônica (126.583) X prevalência PCR-RNA Benzaken,2019 - Hepatitis C disease burden and strategies for elimination by 2030 in Brazil. A mathematical modeling approach (0,31% x 5 = 1,55% = 0,015 ) = 1.899

###### Inquérito brasileiro de diálise crônica - Cenário 2 - Sorológico

# inquérito brasileiro de diálise crônica (126.583) x prevalência inquérito brasileiro de diálise crônica (0,033 - 3,3%) = 4.178
############################################################

############################################################
###### Encontrado na análise do AIH e APAC - Mikael - Cenário 3 - PCR-RNA

# AIH_hemod_01_01_2017_APAC_hemod_01_2017_un (92.980) X prevalência PCR-RNA Benzaken,2019 - Hepatitis C disease burden and strategies for elimination by 2030 in Brazil. A mathematical modeling approach (0,31% x 5 = 1,55% = 0,015 ) = 1.395

###### Encontrado na análise do AIH e APAC - Mikael - Cenário 4 - Sorológico

# AIH_hemod_01_01_2017_APAC_hemod_01_2017_un (92.980) X prevalência inquérito brasileiro de diálise crônica (0,033 - 3,3%) = 3.069
##########################################################

#### 2018

#### 0305010042 hemodialise continua + 0305010093 hemod max 1 sessao por semana + 305010107  max 3 sessoes por semana + 0305010115 hemod HIV e/ou Hep B e/ou Hep C max 3 sessoes por semana + 
#### 0305010123  hemod HIV e/ou Hep B e/ou Hep C max 1 sessao por semana + 0305010131 HEMODIALISE P/ PACIENTES RENAIS AGUDOS / CRONICOS AGUDIZADOS S/ TRATATAMENTO DIALITICO INICIADO

#### AIH

AIH_hemod_2018 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2018.csv")

AIH_hemod_2018$DT_INTERNACAO <- as.character(AIH_hemod_2018$DT_INTERNACAO)
AIH_hemod_2018$DT_SAIDA <- as.character(AIH_hemod_2018$DT_SAIDA)

AIH_hemod_2018_01_01_2018 <- filter(AIH_hemod_2018, DT_INTERNACAO=="01/01/2018" | DT_SAIDA=="01/01/2018" )

#### APAC

APAC_hemod <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/TB_ESPELHO_APAC_201911261121.csv")

APAC_hemod$COMPETENCIA <- as.character(APAC_hemod$COMPETENCIA)

APAC_hemod_01_2018 <-  filter(APAC_hemod, COMPETENCIA=="201801" )

#### AIH (hemod) + APAC (hemod)

AIH_hemod_01_01_2018_APAC_hemod_01_2018 <- full_join( AIH_hemod_2018_01_01_2018, APAC_hemod_01_2018, by = "ID_PACIENTE"  )

AIH_hemod_01_01_2018_APAC_hemod_01_2018_un <- distinct(AIH_hemod_01_01_2018_APAC_hemod_01_2018, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_01_01_2018_APAC_hemod_01_2018_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_01_01_2018_APAC_hemod_01_2018_un.csv")

#### GAL

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_LIBERACAO <- as.character(GAL_hepc_RNA$DT_LIBERACAO)

############################################################
###### Encontrado na análise do AIH e APAC - Mikael - Cenário 1 - PCR-RNA

# AIH_hemod_01_01_2018_APAC_hemod_01_2018_un (96.761) X prevalência PCR-RNA Benzaken,2019 - Hepatitis C disease burden and strategies for elimination by 2030 in Brazil. A mathematical modeling approach (0,31% x 5 = 1,55% = 0,015 ) = 1.452

###### Encontrado na análise do AIH e APAC - Mikael - Cenário 2 - Sorológico

# AIH_hemod_01_01_2018_APAC_hemod_01_2018_un (96.761) X prevalência inquérito brasileiro de diálise crônica (0,033 - 3,3%) = 3.194
##########################################################

#####################
######## Diagnosticados  -  AIH (hemod) + APAC (hemod) x GAL (PCR - RNA) - cura em 2017 - diagnosticados apenas por sorologia - cura espontânea ano anterior ao ano investigado 
####################

#### 2015

#### AIH

AIH_hemod_2015 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2015.csv")

#### APAC

APAC_hemod_2015 <-  filter(APAC_hemod, COMPETENCIA=="201501"  | COMPETENCIA=="201502"  | COMPETENCIA=="201503"  | COMPETENCIA=="201504"  | COMPETENCIA=="201505"  | COMPETENCIA=="201506" | COMPETENCIA=="201507" | COMPETENCIA=="201508" | COMPETENCIA=="201509" | COMPETENCIA=="201510" | COMPETENCIA=="201511" | COMPETENCIA=="201512" )

#### GAL - PCR - RNA

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_SOLICITACAO <- as_date(GAL_hepc_RNA$DT_SOLICITACAO)
GAL_hepc_RNA$DT_LIBERACAO <- as_date(GAL_hepc_RNA$DT_LIBERACAO)

data_inicial <- ymd("2015-01-01")

data_final <- ymd("2015-12-31")

GAL_hepc_RNA_filtered <- GAL_hepc_RNA %>% 
  filter(DT_LIBERACAO<=data_final & DT_SOLICITACAO>=(data_final-days(364)))

#### AIH 2015 + APAC 2015 X GAL (PCR - RNA)

AIH_hemod_2015_APAC_hemod_2015 <- full_join( AIH_hemod_2015, APAC_hemod_2015, by = "ID_PACIENTE"  )

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado <- inner_join( AIH_hemod_2015_APAC_hemod_2015, GAL_hepc_RNA_filtered, by = "ID_PACIENTE"  )

##### - Cura 2015 - cura espontânea ano anterior ao ano investigado 

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado$RESULTADO <- as.character(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado$RESULTADO)

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel <- AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Detectavel"))

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_un <- distinct(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_un.csv")

SIM <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/SIM_PR_BDcompleto.csv")

mortos_diagn_2015 <- inner_join(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_un, SIM, by="ID_PACIENTE")
  
#### 2016

#### AIH

AIH_hemod_2016 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2016.csv")

#### APAC

APAC_hemod_2016 <-  filter(APAC_hemod, COMPETENCIA=="201601"  | COMPETENCIA=="201602"  | COMPETENCIA=="201603"  | COMPETENCIA=="201604"  | COMPETENCIA=="201605"  | COMPETENCIA=="201606" | COMPETENCIA=="201607" | COMPETENCIA=="201608" | COMPETENCIA=="201609" | COMPETENCIA=="201610" | COMPETENCIA=="201611" | COMPETENCIA=="201612" )

#### GAL - PCR - RNA

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_SOLICITACAO <- as_date(GAL_hepc_RNA$DT_SOLICITACAO)
GAL_hepc_RNA$DT_LIBERACAO <- as_date(GAL_hepc_RNA$DT_LIBERACAO)

data_inicial <- ymd("2016-01-01")

data_final <- ymd("2016-12-31")

GAL_hepc_RNA_filtered <- GAL_hepc_RNA %>% 
  filter(DT_LIBERACAO<=data_final & DT_SOLICITACAO>=(data_final-days(364)))

#### AIH 2016 + APAC 2016 X GAL (PCR - RNA)

AIH_hemod_2016_APAC_hemod_2016 <- full_join( AIH_hemod_2016, APAC_hemod_2016, by = "ID_PACIENTE"  )

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado <- inner_join( AIH_hemod_2016_APAC_hemod_2016, GAL_hepc_RNA_filtered, by = "ID_PACIENTE"  )

##### - Cura 2016 - cura espontânea ano anterior ao ano investigado 

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado$RESULTADO <- as.character(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado$RESULTADO)

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel <- AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Detectavel"))

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_un <- distinct(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_un.csv")

SIM <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/SIM_PR_BDcompleto.csv")

mortos_diagn_2016 <- inner_join(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_un, SIM, by="ID_PACIENTE")

#### 2017

#### AIH

AIH_hemod_2017 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2017.csv")

#### APAC

APAC_hemod_2017 <-  filter(APAC_hemod, COMPETENCIA=="201701"  | COMPETENCIA=="201702"  | COMPETENCIA=="201703"  | COMPETENCIA=="201704"  | COMPETENCIA=="201705"  | COMPETENCIA=="201706" | COMPETENCIA=="201707" | COMPETENCIA=="201708" | COMPETENCIA=="201709" | COMPETENCIA=="201710" | COMPETENCIA=="201711" | COMPETENCIA=="201712" )

#### GAL - PCR - RNA

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_SOLICITACAO <- as_date(GAL_hepc_RNA$DT_SOLICITACAO)
GAL_hepc_RNA$DT_LIBERACAO <- as_date(GAL_hepc_RNA$DT_LIBERACAO)

data_inicial <- ymd("2017-01-01")

data_final <- ymd("2017-12-31")

GAL_hepc_RNA_filtered <- GAL_hepc_RNA %>% 
  filter(DT_LIBERACAO<=data_final & DT_SOLICITACAO>=(data_final-days(364)))

#### AIH 2017 + APAC 2017 X GAL (PCR - RNA)

AIH_hemod_2017_APAC_hemod_2017 <- full_join( AIH_hemod_2017, APAC_hemod_2017, by = "ID_PACIENTE"  )

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado <- inner_join( AIH_hemod_2017_APAC_hemod_2017, GAL_hepc_RNA_filtered, by = "ID_PACIENTE"  )

##### - Cura 2017 - cura espontânea ano anterior ao ano investigado 

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado$RESULTADO <- as.character(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado$RESULTADO)

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel <- AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Detectavel"))

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_un <- distinct(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_un.csv")

#### 2018

#### AIH

AIH_hemod_2018 <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hemod_AIH_2018.csv")

#### APAC

APAC_hemod_2018 <-  filter(APAC_hemod, COMPETENCIA=="201801"  | COMPETENCIA=="201802"  | COMPETENCIA=="201803"  | COMPETENCIA=="201804"  | COMPETENCIA=="201805"  | COMPETENCIA=="201806" | COMPETENCIA=="201807" | COMPETENCIA=="201808" | COMPETENCIA=="201809" | COMPETENCIA=="201810" | COMPETENCIA=="201811"  )

#### GAL - PCR - RNA

GAL_hepc_RNA <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/GAL_hepc_rna.csv")

GAL_hepc_RNA$DT_SOLICITACAO <- as_date(GAL_hepc_RNA$DT_SOLICITACAO)
GAL_hepc_RNA$DT_LIBERACAO <- as_date(GAL_hepc_RNA$DT_LIBERACAO)

data_inicial <- ymd("2018-01-01")

data_final <- ymd("2018-11-30")

GAL_hepc_RNA_filtered <- GAL_hepc_RNA %>% 
  filter(DT_LIBERACAO<=data_final & DT_SOLICITACAO>=(data_final-days(331)))


#### AIH 2018 + APAC 2018 X GAL (PCR - RNA)

#AIH_hemod_2018_APAC_hemod_2018 <- full_join( AIH_hemod_2018, APAC_hemod_2018, by = "ID_PACIENTE"  )
#AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado <- inner_join( AIH_hemod_2018_APAC_hemod_2018, GAL_hepc_RNA_filtered, by = "ID_PACIENTE"  )

AIH2018xGAL_RNA <- inner_join( AIH_hemod_2018, GAL_hepc_RNA_filtered, by = "ID_PACIENTE"  )

APAC2018xGAL_RNA <- inner_join( APAC_hemod_2018, GAL_hepc_RNA_filtered, by = "ID_PACIENTE"  )

AIH2018xGAL_RNA_editado <- select(AIH2018xGAL_RNA, ID_PACIENTE, COMPETENCIA = DT_CMPT, UF = SG_UF, RESULTADO )

APAC2018xGAL_RNA_editado <- select(APAC2018xGAL_RNA, ID_PACIENTE, COMPETENCIA, UF = UF_HOSPITAL, RESULTADO )

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado <- do.call("rbind", list(AIH2018xGAL_RNA_editado, APAC2018xGAL_RNA_editado))

##### - Cura 2018 - cura espontânea ano anterior ao ano investigado 

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado$RESULTADO <- as.character(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado$RESULTADO)

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel <- AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Detectavel"))

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel_un <- distinct(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel, ID_PACIENTE, .keep_all = TRUE) 

write.csv(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel_un, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel_un.csv")


####################
######## Tratados   -  AIH (hemod) + APAC (hemod) x GAL (PCR - RNA) x tratamentos de hepatite C - anos de 2015 e 2016
####################

hepc_apac <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/hepc_apac.csv")

hepc_apac_tratamentos <- hepc_apac %>% filter (CO_CID_PRINCIPAL=="B182" | CO_CID_PRINCIPAL=="B171" | CO_CID_SECUNDARIO=="B182" | CO_CID_SECUNDARIO=="B171" & CO_PROCEDIMENTO_PRINCIPAL=="604450010" | CO_PROCEDIMENTO_PRINCIPAL=="601120035" | CO_PROCEDIMENTO_PRINCIPAL=="604390041" | CO_PROCEDIMENTO_PRINCIPAL=="604760019" | CO_PROCEDIMENTO_PRINCIPAL=="604760027" | CO_PROCEDIMENTO_PRINCIPAL=="604760035" |CO_PROCEDIMENTO_PRINCIPAL=="604640030" | CO_PROCEDIMENTO_PRINCIPAL=="604760043" |  CO_PROCEDIMENTO_SECUNDARIO=="604450010" | CO_PROCEDIMENTO_SECUNDARIO=="601120035" |  CO_PROCEDIMENTO_SECUNDARIO=="604390041" | CO_PROCEDIMENTO_SECUNDARIO=="604760019"| CO_PROCEDIMENTO_SECUNDARIO=="604760027"| CO_PROCEDIMENTO_SECUNDARIO=="604760035"| CO_PROCEDIMENTO_SECUNDARIO=="604640030"| CO_PROCEDIMENTO_SECUNDARIO=="604760043")   
hepc_apac_tratamentos$COMPETENCIA <- as.character(hepc_apac_tratamentos$COMPETENCIA)

#### 2014

hepc_apac_tratamentos_2014 <- filter(hepc_apac_tratamentos, COMPETENCIA=="201401" | COMPETENCIA=="201402" | COMPETENCIA=="201403" | COMPETENCIA=="201404" | COMPETENCIA=="201405" | COMPETENCIA=="201406" | COMPETENCIA=="201407" | COMPETENCIA=="201408" | COMPETENCIA=="201409" | COMPETENCIA=="201410" | COMPETENCIA=="201411" | COMPETENCIA=="201412")

#### 2015

hepc_apac_tratamentos_2015 <- filter(hepc_apac_tratamentos, COMPETENCIA=="201501" | COMPETENCIA=="201502" | COMPETENCIA=="201503" | COMPETENCIA=="201504" | COMPETENCIA=="201505" | COMPETENCIA=="201506" | COMPETENCIA=="201507" | COMPETENCIA=="201508" | COMPETENCIA=="201509" | COMPETENCIA=="201510" | COMPETENCIA=="201511" | COMPETENCIA=="201512")

hepc_apac_tratamentos_2015_ini <- anti_join(hepc_apac_tratamentos_2015, hepc_apac_tratamentos_2014, by="ID_PACIENTE")

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_trat_ini <- inner_join(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel, hepc_apac_tratamentos_2015_ini, by="ID_PACIENTE")

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_trat_ini_un <- distinct(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_trat_ini, ID_PACIENTE, .keep_all = TRUE) 

cura_2016_6m <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2016_6m.csv")

tratados2015_curados2016_6m <- inner_join(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_detectavel_trat_ini_un, cura_2016_6m, by="ID_PACIENTE")

#### 2016

hepc_apac_tratamentos_2016 <- filter(hepc_apac_tratamentos, COMPETENCIA=="201601" | COMPETENCIA=="201602" | COMPETENCIA=="201603" | COMPETENCIA=="201604" | COMPETENCIA=="201605" | COMPETENCIA=="201606" | COMPETENCIA=="201607" | COMPETENCIA=="201608" | COMPETENCIA=="201609" | COMPETENCIA=="201610" | COMPETENCIA=="201611" | COMPETENCIA=="201612")

hepc_apac_tratamentos_2014_2015 <- do.call("rbind", list(hepc_apac_tratamentos_2014, hepc_apac_tratamentos_2015))

hepc_apac_tratamentos_2016_ini <- anti_join(hepc_apac_tratamentos_2016, hepc_apac_tratamentos_2014_2015, by="ID_PACIENTE")

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_trat_ini <- inner_join(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel, hepc_apac_tratamentos_2016_ini, by="ID_PACIENTE")

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_trat_ini_un <- distinct(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_trat_ini, ID_PACIENTE, .keep_all = TRUE) 

cura_2017_6m <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2017_6m.csv")

tratados2016_curados2017_6m <- inner_join(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_detectavel_trat_ini_un, cura_2017_6m, by="ID_PACIENTE")

#### 2017

hepc_apac_tratamentos_2017 <- filter(hepc_apac_tratamentos, COMPETENCIA=="201701" | COMPETENCIA=="201702" | COMPETENCIA=="201703" | COMPETENCIA=="201704" | COMPETENCIA=="201705" | COMPETENCIA=="201706" | COMPETENCIA=="201707" | COMPETENCIA=="201708" | COMPETENCIA=="201709" | COMPETENCIA=="201710" | COMPETENCIA=="201711" | COMPETENCIA=="201712")

hepc_apac_tratamentos_2014_2015_2016 <- do.call("rbind", list(hepc_apac_tratamentos_2014, hepc_apac_tratamentos_2015, hepc_apac_tratamentos_2016))

hepc_apac_tratamentos_2017_ini <- anti_join(hepc_apac_tratamentos_2017, hepc_apac_tratamentos_2014_2015_2016, by="ID_PACIENTE")

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_trat_ini <- inner_join(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel, hepc_apac_tratamentos_2017_ini, by="ID_PACIENTE")

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_trat_ini_un <- distinct(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_trat_ini, ID_PACIENTE, .keep_all = TRUE) 

cura_2018_6m <- read.csv("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2018_6m.csv")

tratados2017_curados2018_6m <- inner_join(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_detectavel_trat_ini_un, cura_2018_6m, by="ID_PACIENTE")

#### 2018

hepc_apac_tratamentos_2018 <- filter(hepc_apac_tratamentos, COMPETENCIA=="201801" | COMPETENCIA=="201802" | COMPETENCIA=="201803" | COMPETENCIA=="201804" | COMPETENCIA=="201805" | COMPETENCIA=="201806" | COMPETENCIA=="201807" | COMPETENCIA=="201808" | COMPETENCIA=="201809" | COMPETENCIA=="201810" | COMPETENCIA=="201811" )

hepc_apac_tratamentos_2014_2015_2016_2017 <- do.call("rbind", list(hepc_apac_tratamentos_2014, hepc_apac_tratamentos_2015, hepc_apac_tratamentos_2016, hepc_apac_tratamentos_2017))

hepc_apac_tratamentos_2018_ini <- anti_join(hepc_apac_tratamentos_2018, hepc_apac_tratamentos_2014_2015_2016_2017, by="ID_PACIENTE")

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel_trat_ini <- inner_join(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel, hepc_apac_tratamentos_2018_ini, by="ID_PACIENTE")

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel_trat_ini_un <- distinct(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_detectavel_trat_ini, ID_PACIENTE, .keep_all = TRUE) 

####################
######## Curados   -  AIH (hemod) + APAC (hemod) x GAL (PCR - RNA)
####################

#### 2015

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura1 <- AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Abaixo do limite de quantificacao" ))

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura2 <- AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Nao Detectavel"))

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura <- do.call("rbind", list(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura1, AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura2))

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura_tratamento <- inner_join(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura, hepc_apac_tratamentos_2015_ini, by="ID_PACIENTE")

AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura_tratamento_un <-  distinct(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura_tratamento, ID_PACIENTE, .keep_all = TRUE) 

cura_2015_6m <- filter(AIH_hemod_2015_APAC_hemod_2015xGAL_RNA_filtrado_cura_tratamento_un, DT_CMPT== "201501" | DT_CMPT== "201502" | DT_CMPT== "201503" | DT_CMPT== "201504" | DT_CMPT== "201505" | DT_CMPT== "201506" | COMPETENCIA.x== "201501" | COMPETENCIA.x== "201502" | COMPETENCIA.x== "201503" | COMPETENCIA.x== "201504" | COMPETENCIA.x== "201505" | COMPETENCIA.x== "201506" |  COMPETENCIA.y== "201501" | COMPETENCIA.y== "201502" | COMPETENCIA.y== "201503" | COMPETENCIA.y== "201504" | COMPETENCIA.y== "201505" | COMPETENCIA.y== "201506" )

write.csv(cura_2015_6m, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2015_6m.csv")

#### 2016

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura1 <- AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Abaixo do limite de quantificacao" ))

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura2 <- AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Nao Detectavel"))

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura <- do.call("rbind", list(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura1, AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura2))

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura_tratamento <- inner_join(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura, hepc_apac_tratamentos_2016_ini, by="ID_PACIENTE")

AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura_tratamento_un <-  distinct(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura_tratamento, ID_PACIENTE, .keep_all = TRUE) 

cura_2016_6m <- filter(AIH_hemod_2016_APAC_hemod_2016xGAL_RNA_filtrado_cura_tratamento_un, DT_CMPT== "201601" | DT_CMPT== "201602" | DT_CMPT== "201603" | DT_CMPT== "201604" | DT_CMPT== "201605" | DT_CMPT== "201606" | COMPETENCIA.x== "201601" | COMPETENCIA.x== "201602" | COMPETENCIA.x== "201603" | COMPETENCIA.x== "201604" | COMPETENCIA.x== "201605" | COMPETENCIA.x== "201606" |  COMPETENCIA.y== "201601" | COMPETENCIA.y== "201602" | COMPETENCIA.y== "201603" | COMPETENCIA.y== "201604" | COMPETENCIA.y== "201605" | COMPETENCIA.y== "201606" )

write.csv(cura_2016_6m, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2016_6m.csv")

#### 2017

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura1 <- AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Abaixo do limite de quantificacao" ))

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura2 <- AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Nao Detectavel"))
 
AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura <- do.call("rbind", list(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura1, AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura2))

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura_tratamento <- inner_join(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura, hepc_apac_tratamentos_2017_ini, by="ID_PACIENTE")

AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura_tratamento_un <-  distinct(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura_tratamento, ID_PACIENTE, .keep_all = TRUE) 

cura_2017_6m <- filter(AIH_hemod_2017_APAC_hemod_2017xGAL_RNA_filtrado_cura_tratamento_un, DT_CMPT== "201701" | DT_CMPT== "201702" | DT_CMPT== "201703" | DT_CMPT== "201704" | DT_CMPT== "201705" | DT_CMPT== "201706" | COMPETENCIA.x== "201701" | COMPETENCIA.x== "201702" | COMPETENCIA.x== "201703" | COMPETENCIA.x== "201704" | COMPETENCIA.x== "201705" | COMPETENCIA.x== "201706" |  COMPETENCIA.y== "201701" | COMPETENCIA.y== "201702" | COMPETENCIA.y== "201703" | COMPETENCIA.y== "201704" | COMPETENCIA.y== "201705" | COMPETENCIA.y== "201706" )

write.csv(cura_2017_6m, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2017_6m.csv")

#### 2018

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura1 <- AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Abaixo do limite de quantificacao" ))

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura2 <- AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado %>% filter(str_detect(RESULTADO,"Resultado: Nao Detectavel"))

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura <- do.call("rbind", list(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura1, AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura2))

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura_tratamento <- inner_join(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura, hepc_apac_tratamentos_2018_ini, by="ID_PACIENTE")

AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura_tratamento_un <-  distinct(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura_tratamento, ID_PACIENTE, .keep_all = TRUE) 

cura_2018_6m <- filter(AIH_hemod_2018_APAC_hemod_2018xGAL_RNA_filtrado_cura_tratamento_un,   COMPETENCIA.x== "201801" | COMPETENCIA.x== "201802" | COMPETENCIA.x== "201803" | COMPETENCIA.x== "201804" | COMPETENCIA.x== "201805" | COMPETENCIA.x== "201806" |  COMPETENCIA.y== "201801" | COMPETENCIA.y== "201802" | COMPETENCIA.y== "201803" | COMPETENCIA.y== "201804" | COMPETENCIA.y== "201805" | COMPETENCIA.y== "201806" )

write.csv(cura_2018_6m, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cura_2018_6m.csv")


############################
########## Plots = cascatas
###########################

##### Cascata 2015

cascata2015 <- matrix(c(1330,"Infectados", 338, "Diagnosticados", 3, "Tratados", 0, "Curados"),ncol=2,byrow=TRUE)
colnames(cascata2015) <- c("Freq","tipo")
cascata2015 <- as.data.frame(cascata2015)

cascata2015$Freq <- as.integer(cascata2015$Freq)

ggplot(data=cascata2015, aes(x=reorder(tipo, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Freq), vjust=-0.3, size=7)+
  theme_minimal()  + labs(x="", y = "Frequência") + ggtitle("Cascata de cuidado de pessoas com hepatite C em diálise no ano de 2015") +theme(axis.text=element_text(size=17),
                                                                                                                                             axis.title=element_text(size=18,face="bold")) + theme(plot.title = element_text(size = 20, face = "bold"))



##### Cascata 2016

cascata2016 <- matrix(c(1366,"Infectados", 492, "Diagnosticados", 57, "Tratados", 52, "Curados"),ncol=2,byrow=TRUE)
colnames(cascata2016) <- c("Freq","tipo")
cascata2016 <- as.data.frame(cascata2016)

cascata2016$Freq <- as.integer(cascata2016$Freq)

ggplot(data=cascata2016, aes(x=reorder(tipo, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Freq), vjust=-0.3, size=7)+
  theme_minimal()  + labs(x="", y = "Frequência") + ggtitle("Cascata de cuidado de pessoas com hepatite C em diálise no ano de 2016") +theme(axis.text=element_text(size=17),
                                                                                                                                             axis.title=element_text(size=18,face="bold")) + theme(plot.title = element_text(size = 20, face = "bold"))

##### Cascata 2017

cascata2017 <- matrix(c(1395,"Infectados", 621, "Diagnosticados", 140, "Tratados", 101, "Curados"),ncol=2,byrow=TRUE)
colnames(cascata2017) <- c("Freq","tipo")
cascata2017 <- as.data.frame(cascata2017)

cascata2017$Freq <- as.integer(cascata2017$Freq)

ggplot(data=cascata2017, aes(x=reorder(tipo, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Freq), vjust=-0.3, size=7)+
  theme_minimal()  + labs(x="", y = "Frequência") + ggtitle("Cascata de cuidado de pessoas com hepatite C em diálise no ano de 2017") +theme(axis.text=element_text(size=17),
                                                                                                                                             axis.title=element_text(size=18,face="bold")) + theme(plot.title = element_text(size = 20, face = "bold"))

##### Cascata 2018

cascata2018 <- matrix(c(1492,"Infectados", 374, "Diagnosticados", 43, "Tratados", 65, "Curados"),ncol=2,byrow=TRUE)
colnames(cascata2018) <- c("Freq","tipo")
cascata2018 <- as.data.frame(cascata2018)

cascata2018$Freq <- as.integer(cascata2018$Freq)

cascata2018$tipo <- factor(cascata2018$tipo,levels = c("Infectados", "Diagnosticados", "Tratados", "Curados"))

ggplot(data=cascata2018, aes(x=tipo, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=Freq), vjust=-0.3, size=7)+
  theme_minimal()  + labs(x="", y = "Frequência") + ggtitle("Cascata de cuidado de pessoas com hepatite C em diálise no ano de 2018") +theme(axis.text=element_text(size=17),
                                                                                                                                             axis.title=element_text(size=18,face="bold")) + theme(plot.title = element_text(size = 20, face = "bold"))

##############
####### Scatter plots
###############

cascata <- matrix(c(1330,"Infectados", 2015,1366, "Infectados", 2016, 1395, "Infectados", 2017, 3, "Tratados", 2015, 57, "Tratados", 2016, 101, "Tratados", 2017, 3,"Tratados - Agregado" , 2015, 60, "Tratados - Agregado", 2016, 161, "Tratados - Agregado", 2017),ncol=3,byrow=TRUE)
colnames(cascata) <- c("Frequência","tipo", "ano")
cascata <- as.data.frame(cascata)

cascata$Frequência <- as.integer(cascata$Frequência)
cascata$ano <- as.numeric(cascata$ano)

write.xlsx(cascata, "C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cascata.xlsx")

cascata <- read.xlsx("C:/Users/lemos/Downloads/produtos_opas/contrato_2020/produto2/cascata.xlsx")

ggplot(data = cascata, 
       mapping = aes(x = ano, y = Frequência)) +
  geom_line(color="steelblue", size=1.2) +
  facet_wrap(vars(tipo)) +
  theme_bw() +  facet_grid(rows = vars(tipo)) +geom_point(color="steelblue", size=3.2) + scale_x_continuous(breaks = seq(2015, 2030, by = 1)) + theme(text=element_text(size = 18))


