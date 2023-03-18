library(flexdashboard)
# library(plotly)
library(tidyverse)
library(brazilmaps)
library(RColorBrewer)
library(leaflet)
# library(gridExtra)
library(kableExtra)
#library(GADMTools)
# library(egg)
library(readxl)
library(sf)
evasao <- read_excel("evasao.xlsx")

dados<- evasao %>%
  group_by(STATE) %>%
  summarise(n=n(), evasao=sum(evadido),alunos=sum(total),tx=evasao/alunos*100,tx_estado=round(tx,2)) %>% 
  ungroup() %>% 
  mutate(NO_IES= c("Fundacao Universidade Federal de Rondonia",
                   "Universidade Federal do Amazonas",
                   "Universidade Federal do Para",
                   "Universidade Federal do Piaui",
                   "Universidade Federal do Ceara",
                   "Universidade Federal do Rio Grande do Norte",
                   paste("Universidade Estadual da Paraiba,", "<br/>",
                         "Universidade Federal da Paraiba", #"<br/>",
                         "e", "<br/>",
                         "Universidade Federal de Campina Grande"),
                   "Universidade Federal de Pernambuco",
                   "Universidade Federal de Sergipe",
                   "Universidade Federal da Bahia",
                   paste("Universidade Federal de Juiz de Fora,", "<br/>",
                         "Universidade Federal de Minas Gerais,", "<br/>",
                         "Universidade Federal de Uberlandia", #"<br/>",
                         "e", "<br/>",
                         "Universidade Federal de Ouro Preto"),
                   "Universidade Federal do Espirito Santo",
                   paste("Universidade Federal do Rio de Janeiro,", "<br/>",
                         "Universidade do Estado do Rio de Janeiro,", "<br/>",
                         "Universidade Federal Fluminense",# "<br/>",
                         "e", "<br/>",
                         "Escola Nacional de Ciencias Estatisticas"), 
                   paste("Universidade de Sao Paulo,", "<br/>",
                         "Universidade Estadual Paulista Julio de Mesquita Filho,", "<br/>",
                         "Universidade Estadual de Campinas,", "<br/>",
                         "Centro Universitario Capital", #"<br/>",
                         "e", "<br/>",
                         "Universidade Federal de Sao Carlos"),
                   paste("Universidade Federal do Parana", #"<br/>",
                         "e", "<br/>",
                         "Universidade Estadual de Maringa"),
                   paste("Universidade Federal do Rio Grande do Sul", #"<br/>",
                         "e", "<br/>",
                         "Universidade Federal de Santa Maria"),
                   "Universidade Federal de Mato Grosso",
                   "Universidade Federal de Goias",
                   "Universidade De Brasilia")
  )

dados<- get_brmap("State") %>%  
  left_join(dados, c("State" = "STATE")) %>% 
  replace_na(list(n=0,alunos=0,tx_estado="No statistics course")) 
my_orange = brewer.pal(n = 9, "YlOrRd")

pal.state <- colorFactor(palette = my_orange[c(3,5,6,7,8,9)], domain = dados$n)
mytext<- paste(
  dados$nome,"<br/>", 
  "Number of Higher Education Institutions: ", dados$n, "<br/>", 
  "State dropout rate (%): ",dados$tx_estado, "<br/>",
  "Name of Higher Education Institution: ", dados$NO_IES,
  sep="") %>%
  lapply(htmltools::HTML)

m<-leaflet(dados) %>% 
  addTiles() %>% 
  # setView( lat=-30.5, lng=-53 , zoom=6) %>%
  addPolygons(color = "#444444", weight = .05, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor =  ~pal.state(n),
              label = mytext,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
  ) %>%
  addLegend("bottomright",
            pal = pal.state,
            values = ~n,
            title = "Number of Higher Education Institution: ",
            opacity = 1
  )
m

pal.state <- colorBin(palette = my_orange[4:9], domain = dados$tx,na.color = my_orange[1],bins=4)

mytext<- paste(
  dados$nome,"<br/>",
  "State dropout rate (%): ",dados$tx_estado, "<br/>",
  "Number of Higher Education Institution: ", dados$n, "<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

m<-leaflet(dados) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444", weight = .05, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor =  ~pal.state(tx),
              label = mytext,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
  ) %>%
  addLegend("bottomright",
            pal = pal.state,
            values = ~tx,
            title = "Droupout rates (%)",
            na.label = "No course registered",
            opacity = 1
  )
m