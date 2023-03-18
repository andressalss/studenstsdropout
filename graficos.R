library(ggplot2)
library(tidyr)
library(tidyverse)
library(esquisse)
library(ggsave)
library(RColorBrewer)

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
         SEXO_ALUNO=(ifelse(IN_SEXO_ALUNO==0,"MALE","FEMALE"))) %>% 
  drop_na(CO_TURNO_ALUNO)

attach(dados_transformados)

g_turno <- ggplot(data = dados_transformados, aes(CO_TURNO_ALUNO, na.rm = TRUE)) +
  geom_bar(aes(fill = CO_ALUNO_SITUACAO), position = "fill") +
  theme_minimal()

shift <- ggplot(dados_transformados, na.rm = TRUE) +
 aes(x = CO_TURNO_ALUNO, fill = CO_ALUNO_SITUACAO) +
 geom_bar(position = "fill") +
  scale_fill_brewer(palette = "YlOrRd",direction = -1)+
 labs(x = "SHIFT", y = "PROPORTION OF STUDENTS", fill = NULL) +
 theme_minimal()
shift
ggsave("shift_plot.pdf", shift,width = 6, height=3)


gender <- ggplot(dados_transformados) +
  aes(x = SEXO_ALUNO, fill = CO_ALUNO_SITUACAO) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "YlOrRd",direction = -1)+
  labs(x = "GENDER", y = "PROPORTION OF STUDENTS", fill = NULL) +
  theme_minimal()
gender
ggsave("gender_plot.pdf", gender,width = 6, height=3)

age <- ggplot(dados_transformados) +
  aes(x = NU_IDADE_ALUNO, fill = CO_ALUNO_SITUACAO) +
  geom_histogram(bins = 30L) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1)+
  labs(x = "AGE", y = "NUMBER OF STUDENS", fill = NULL)+
  theme_minimal()
age
ggsave("age_plot.pdf", age,width = 6, height=3)


situation <- ggplot(dados_transformados) +
    aes(x = CO_ALUNO_SITUACAO) +
    geom_bar(fill = rev(brewer.pal(3,"YlOrRd"))) +
    labs(x = NULL, y = "NUMBER OF STUDENTS")+
    theme_minimal()
situation

ggsave("situation_plot.pdf", situation,width = 6, height=3)

