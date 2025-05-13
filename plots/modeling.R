library(tidymodels)
library(kernlab)

dados <- read_delim("BD_ESTUDO.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
dados_transformados <- dados %>% 
  select(CO_ALUNO, CO_ALUNO_SITUACAO, NO_EVENTO, CO_TURNO_ALUNO, IN_SEXO_ALUNO, CO_COR_RACA_ALUNO, NU_IDADE_ALUNO) %>% 
  mutate(CO_TURNO_ALUNO=ifelse(CO_TURNO_ALUNO==1,"MORNING",
                               ifelse(CO_TURNO_ALUNO==3,"NIGHT",
                                      ifelse(CO_TURNO_ALUNO==2,"AFTERNOON",
                                             ifelse(CO_TURNO_ALUNO==4,"FULL-TIME",
                                                    ifelse(is.na(CO_TURNO_ALUNO)==TRUE,NA,NA))))),
         CO_ALUNO_SITUACAO= ifelse(CO_ALUNO_SITUACAO==4,"EVADED",
                                   ifelse(CO_ALUNO_SITUACAO==5,"EVADED",      
                                          ifelse(CO_ALUNO_SITUACAO==6,"GRADUATED","CENSORSHIP"))),
         CO_COR_RACA_ALUNO=ifelse(CO_COR_RACA_ALUNO==0,"NAO DECLARADO",
                                  ifelse(CO_COR_RACA_ALUNO==1,"BRANCA",
                                         ifelse(CO_COR_RACA_ALUNO==2,"PRETA",
                                                ifelse(CO_COR_RACA_ALUNO==3,"PARDA",
                                                       ifelse(CO_COR_RACA_ALUNO==4,"AMARELA","INDIGENA"))))),
         SEXO_ALUNO=(ifelse(IN_SEXO_ALUNO==1,"MALE","FEMALE"))) %>% 
  drop_na(CO_TURNO_ALUNO)

attach(dados_transformados)

# Dividir os dados em treino e teste
set.seed(123)
data_split <- initial_split(dados_transformados, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

''