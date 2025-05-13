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
         SEXO_ALUNO=(ifelse(IN_SEXO_ALUNO==1,"MALE","FEMALE"))) %>% 
  drop_na(CO_TURNO_ALUNO)

attach(dados_transformados)


# SHIFT PLOT --------------------------------------------------------------
shift_gray <- ggplot(dados_transformados, na.rm = TRUE) +
  aes(x = CO_TURNO_ALUNO, fill = CO_ALUNO_SITUACAO) +
  geom_bar(position = "fill") +
  scale_fill_grey(start = 0.8, end = 0.2) +  # Escala de cinza
  labs(x = "SHIFT", y = "PROPORTION OF STUDENTS", fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 15),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", color = "grey25", size = 17),
    axis.title.x = element_text(face = "bold", color = "grey25", size = 17),
    axis.text.x = element_text(color = "grey25", size = 14),
    axis.text.y.right = element_text(color = "grey25", size = 15),
    panel.background = element_rect(fill = "white", colour = "white")
  )

# Salvar o gráfico em formato TIFF
ggsave("shift_gray.tiff", plot = shift_gray, width = 8, height = 6, dpi = 300, compression = "lzw")


# EPS 
setEPS()
postscript("shift_plot.eps",width = 6, height = 4,family = "Times")
shift
dev.off()

# PDF
ggsave("shift_plot.pdf", shift,width = 6, height=3,family = "Times")


# SEX PLOT  ---------------------------------------------------------------
sex_gray <- ggplot(dados_transformados) +
  aes(x = SEXO_ALUNO, fill = CO_ALUNO_SITUACAO) +
  geom_bar(position = "fill") +
  scale_fill_grey(start = 0.8, end = 0.2) +  # Escala de cinza
  labs(x = "SEX", y = "PROPORTION OF STUDENTS", fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 15),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", color = "grey25", size = 17),
    axis.title.x = element_text(face = "bold", color = "grey25", size = 17),
    axis.text.x = element_text(color = "grey25", size = 14),
    axis.text.y.right = element_text(color = "grey25", size = 15),
    panel.background = element_rect(fill = "white", colour = "white")
  )

# Salvar o gráfico em formato TIFF
ggsave("sex_gray.tiff", plot = sex_gray, width = 8, height = 6, dpi = 300, compression = "lzw")

#EPS
setEPS()
postscript("sex_plot.eps",width = 6, height = 4,family = "Times")
sex
dev.off()

# PDF
ggsave("sex_plot.pdf", sex,width = 6, height=3,family = "Times")


# AGE PLOT ----------------------------------------------------------------
age_gray <- ggplot(dados_transformados) +
  aes(x = NU_IDADE_ALUNO, fill = CO_ALUNO_SITUACAO) +
  geom_histogram(bins = 30L) +
  scale_fill_grey(start = 0.8, end = 0.2) +  # Escala de cinza
  labs(x = "AGE", y = "NUMBER OF STUDENTS", fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 15),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", color = "grey25", size = 17),
    axis.title.x = element_text(face = "bold", color = "grey25", size = 17),
    axis.text.x = element_text(color = "grey25", size = 14),
    axis.text.y.right = element_text(color = "grey25", size = 15),
    panel.background = element_rect(fill = "white", colour = "white")
  )

# Salvar o gráfico em formato TIFF
ggsave("age_gray.tiff", plot = age_gray, width = 8, height = 6, dpi = 300, compression = "lzw")

# EPS
setEPS()
postscript("age_plot.eps",width = 6, height = 4,family = "Times")
age
dev.off()

# PDF
ggsave("age_plot.pdf", age,width = 6, height=3,family = "Times")


# SITUATION ---------------------------------------------------------------
situation_gray <- ggplot(dados_transformados) +
  aes(x = CO_ALUNO_SITUACAO) +
  geom_bar(fill = rev(grey.colors(3, start = 0.8, end = 0.2))) +  # Escala de cinza
  labs(x = "SITUATION", y = "NUMBER OF STUDENTS") +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 15),
    legend.text = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", color = "grey25", size = 17),
    axis.title.x = element_text(face = "bold", color = "grey25", size = 17),
    axis.text.x = element_text(color = "grey25", size = 14),
    axis.text.y.right = element_text(color = "grey25", size = 15),
    panel.background = element_rect(fill = "white", colour = "white")
  )

# Salvar o gráfico em formato TIFF
ggsave("situation_gray.tiff", plot = situation_gray, width = 8, height = 6, dpi = 300, compression = "lzw")

# EPS
setEPS()
postscript("situation_plot.eps",width = 6, height = 4,family = "Times")
situation
dev.off()

# PDF
ggsave("situation_plot.pdf", situation,width = 6, height=3,family = "Times")

