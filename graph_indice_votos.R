#Bruna Mirelle 

#Pacotes:
library(foreign)
library(reshape2)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)
library(magick)
library(ggplot2)


#Bases: Indice de isolamento por municipio (codigo municipio IBGE)
 setwd("~/XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

dataset_indice= read.csv2("dataset_indice.csv", sep=";", stringsAsFactors = F)

#Variável região
norte = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins")
nordeste = c("Maranhão","Piauí","Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco" ,"Alagoas","Sergipe","Bahia")
sudeste = c("Minas Gerais","Espírito Santo", "Rio de Janeiro","São Paulo" )
sul = c("Paraná", "Santa Catarina","Rio Grande do Sul")
centro_oeste = c("Mato Grosso do Sul","Mato Grosso" ,"Goiás","Distrito Federal" )
regiao = c("norte","nordeste","sudeste","sul","centro_oeste")

dataset_indice$regiao = 0
for (reg in regiao){
  dataset_indice$regiao[dataset_indice$name_state%in%get(reg)] = reg
}

#Saber o número das colunas que precisamos 
which(colnames(dataset_indice)%in%c("indice_isolamento"))
which(colnames(dataset_indice)%in%c("frvoto_bolso_2turno"))

# Preparando as variáveis de eixo x,eixo y e legenda

covid =dataset_indice[,c("indice_isolamento","frvoto_bolso_2turno","regiao","id_municipality", "date")]
covid$indice_isolamento = as.numeric(covid$indice_isolamento)
covid$frvoto_bolso_2turno =  as.numeric(covid$frvoto_bolso_2turno)

#Mantendo apenas Março
covid = covid[grep("mar",covid$date),]

#Media
covid = aggregate(covid[, c(1,2)], list(covid$id_municipality, covid$regiao), mean)
colnames(covid)[2] = "regiao" 
covid = covid[complete.cases(covid),]

#Delta
# covid_befor =(covid[covid$date=="04feb2020",c(1,4)])
# covid_after =(covid[covid$date=="07apr2020",c(1,2,3,4)])
# covid= merge(covid_after,covid_befor,by=c("id_municipality"))
# covid$indice_isolamento = covid$indice_isolamento.x - covid$indice_isolamento.y


############################ Gráfico indice de isolamento x Votos no Bolsonaro   ######################################

#Preparando os pontos
camadas <- list(geom_point(size = 1, shape = 19, fill = "white", stroke = 1))

# Adicionando a reta 
camadas <- list(camadas, geom_smooth(method= "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.5, fullrange = TRUE, orientation = "y"))


#Acrescentando legenda e o segmento que vai do ponto a legenda
camadas <- list(camadas,
                #Definindo uma cor para cada Continente     
                scale_colour_manual(" ",values = c('centro_oeste'='#621107','nordeste'='#6DBBD8','norte'='#01344A','sudeste'='#D24131','sul'='#228DBD'), labels = c("Central-West","Northeast","North", "Southeast","South"), aesthetics = c("colour", "fill")),

                #Definindo os números de cada eixo
                scale_x_continuous(breaks=seq(0,1,by=0.1)),
                scale_y_continuous(breaks=seq(0,1,by=0.1),limits=c(0.2,0.7))
)


########## Gráfico total ######################

ggplot(covid, aes(x = frvoto_bolso_2turno, y = indice_isolamento, color =regiao)) +
  camadas +
  #Definindo títulos
  labs(title = "Social Distancing Index x Votes for Bolsonaro",
       subtitle ="(by municipality)" ,
       x = "Votes for Bolsonaro",
       y ="Social Distancing Index")  +
  coord_fixed(ratio = 7/8)+
  #Definindo fundo branco e detalhes estéticos e de legenda
  theme_bw()+
  guides(colour = guide_legend(nrow = 1)) +
  theme(
    legend.position="top",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.justification = 0,
    plot.title = element_text(face = 'bold'),
    axis.title = element_text(size = 12, color = "gray8"),
    plot.caption = element_text(size = 10, hjust = 0),
    panel.border = element_blank(),
    axis.line.x  = element_line(color = "gray8"),
    axis.line.y  = element_line(color = "gray8"),
    axis.ticks.y = element_line()
)

ggsave("graph_indice_votos_2turno.png")

