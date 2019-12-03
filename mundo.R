#Autor: Bruna Mirelle

#Pacotes:
library(foreign)
library(reshape2)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)
library(magick)
library(ggplot2)


#Bases:

#Produtividade - Banco Mundial:
#https://databank.worldbank.org/source/wdi-database-archives-(beta)#
#GDP per person employed (constant 2011 PPP $)

#Aqui é utilizada apenas para capturar o nome dos Continentes de cada país
# Historical labor productivities in Brazil
# Data downloaded from:  https://www.rug.nl/ggdc/productivity/10-sector/



#Reshape data
#base SD
base = read.dta("10SD_jan15.dta")
base = base[(base$Variable%in%c("VA_Q05", "EMP")),]
base_wide = reshape(base, idvar = c("Country", "Year") , v.names = c("AGR","MIN","MAN","PU","CON","WRT","TRA","FIRE","GOV","OTH","SUM"), timevar = "Variable", direction = "wide")
base_wide = base_wide[!(base_wide$Year%in%c(1947:1979)),]
base_wide = base_wide[,c(1,2,3,4,26)]

#Base Banco Mundial 
#base de produtividade
base_gdp = read.csv("6dee4fa3-023c-455a-ba38-c4a2ea625a33_Data.csv", stringsAsFactors = FALSE)
base_gdp = base_gdp[,c(2,7:37)]
colnames(base_gdp) = c("Country",paste("gdp",c(1980:2010), sep="."))
base_gdp = reshape(data = base_gdp,  direction = "long", varying = 2:32)
names(base_gdp)[2] = "Year"
base_gdp$id = NULL

#merge das bases
base_total = merge(base_wide,base_gdp)

#################################### Preparando a base  ############################################

# Realocando a base para ficar no formato de colunas
mundo = base_total[base_total$Year==1980,c(1,2,3,4,6)]
mundo_temp= base_total[base_total$Year==2010,c(1,6)]
mundo = merge(mundo,mundo_temp, by="Country")
mundo = mundo[c(1:3,5:23,25:34,36:40),]
remove(mundo_temp)
mundo = mundo[complete.cases(mundo),]
mundo$Region = factor(mundo$Region)
colnames(mundo)[5]="gdp.1980" 
colnames(mundo)[6]="gdp.2010" 
mundo$gdp.1980=as.numeric(mundo$gdp.1980)
mundo$gdp.2010=as.numeric(mundo$gdp.2010)
mundo$Region=as.character(mundo$Region)
mundo=mundo[,c(1,3:6)]

############################ Gráfico Produtividade 1970 x 1980  ######################################

#Preparando os pontos
camadas <- list(geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1))

# Adicionando a reta 
camadas <- list(camadas, geom_smooth(method= "lm", formula = y ~ x, se = FALSE, color = "red", size = 0.5, fullrange = TRUE))


#Definindo os países a serem destacados
KORy = mundo[mundo$Country=="KOR","gdp.2010"]
KORx = mundo[mundo$Country=="KOR","gdp.1980"]
BRAy = mundo[mundo$Country=="BRA","gdp.2010"]
BRAx = mundo[mundo$Country=="BRA","gdp.1980"]
CHNy = mundo[mundo$Country=="CHN","gdp.2010"]
CHNx = mundo[mundo$Country=="CHN","gdp.1980"]
USAy = mundo[mundo$Country=="USA","gdp.2010"]
USAx = mundo[mundo$Country=="USA","gdp.1980"]



#Acrescentando legenda e o segmento que vai do ponto a legenda
camadas <- list(camadas, annotate("text", y = KORy-4000, x=KORx, 
                                                  label = "Coreia",
                                                  color = "darkred"),
                        annotate("text", y = BRAy-4000 , x=BRAx, 
                                 label = "Brasil",
                                 color = "darkred"), 
                        annotate("text", y = CHNy+5000 , x=CHNx, 
                                 label = "China",
                                 color = "darkred"), 
                        annotate("text", y = USAy-4000 , x=USAx-2000, 
                                 label = "EUA",
                                 color = "darkred"),
                        geom_segment( y = KORy, x=KORx, xend=KORx, yend=KORy - 3000,   alpha = 0.7, color = "gray"),
                        geom_segment( y = BRAy, x=BRAx, xend=BRAx, yend=BRAy - 3000,   alpha = 0.7, color = "gray"),
                        geom_segment( y = CHNy, x=CHNx, xend=CHNx, yend=CHNy + 3000,   alpha = 0.7, color = "gray"),
                        geom_segment( y = USAy, x=USAx, xend=USAx-1000, yend=USAy - 2000,   alpha = 0.7, color = "gray"),
                       
                #Definindo uma cor para cada Continente     
                 scale_colour_manual(" ",values = c('Asia'='#01344A','Europe'='#6DBBD8','Latin America'='#D24131','Middle East and North Africa'='#228DBD','North America'='#621107','Sub-saharan Africa'='#1B6E64'), labels = c("Ásia", "Europa","América Latina","Oriente Médio & \nnorte da África","América do Norte", "África subsariana")),
                      #Definindo os números de cada eixo
                        scale_x_continuous(breaks=seq(0,10,by=2.5)),
                        scale_y_continuous(breaks=seq(0,10,by=2.5)))



########## Gráfico total ######################

ggplot(mundo, aes(x = gdp.1980, y = gdp.2010, color = Region)) +
  camadas +
  #Definindo títulos
  labs(title = "Produtividade em 1980 e 2010",
       subtitle ="(PIB [US$] por trabalhador empregado)" ,
       x = "Produtividade 1980",
       y ="Produtividade 2010")  +
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
    axis.title = element_text(face='italic', size = 12, color = "gray8"),
    plot.caption = element_text(size = 10, hjust = 0),
    panel.border = element_blank(),
    axis.line.x  = element_line(color = "gray8"),
    axis.line.y  = element_line(color = "gray8"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x =  element_blank(),
    axis.text.y =  element_blank()
  )

ggsave("mundo_completo.png")

