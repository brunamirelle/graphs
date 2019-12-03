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

#Bases:

#Produtividade - Banco Mundial:
#https://databank.worldbank.org/source/wdi-database-archives-(beta)#
#GDP per person employed (constant 2011 PPP $)

# Historical labor productivities in Brazil
# Data downloaded from:  https://www.rug.nl/ggdc/productivity/10-sector/


#Reshape data
#base SD
base = read.dta("10SD_jan15.dta")
base = base[(base$Variable%in%c("VA_Q05", "EMP")),]
base_wide = reshape(base, idvar = c("Country", "Year") , v.names = c("AGR","MIN","MAN","PU","CON","WRT","TRA","FIRE","GOV","OTH","SUM"), timevar = "Variable", direction = "wide")
base_wide = base_wide[!(base_wide$Year%in%c(1947:1979)),]
base_wide = base_wide[,c(1,2,3,4,26)]
#Não necessariamente precisamos utilizar a base SD, mas por questões de padronização com outros códigos manteremos

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

#Selecionando apenas os países do meu interesse
mundo_paises =  base_total[base_total$Country%in%c("BRA","CHN","KOR","JPN","USA","ARG","MEX","CHL"),c(1,2,6)]
mundo_paises = reshape(mundo_paises,  idvar = "Year" , v.names= "gdp" , timevar = "Country", direction = "wide")
mundo_paises = mundo_paises[complete.cases(mundo_paises),]
mundo_paises =  data.frame(mundo_paises[1],apply(mundo_paises[,2:9],2, FUN=as.numeric))

############################ Gráfico Crescimento Produtividade ##################################

#Defindo as linhas e os pontos para diferenciar mesmo queo gráfico esteja esteja em preto e branco
#É importante definir color e shape com o nome de cada país para associar mais pra frente de forme específica
paises <- ggplot(mundo_paises)+
  geom_line( aes(Year, gdp.BRA,  color = "Brasil") ,size=1)+
  geom_point( aes(Year, gdp.BRA,  color = "Brasil", shape = "Brasil"),size=2)+
  geom_line( aes(Year,  gdp.CHN, color ="China") ,size=1)+
  geom_point( aes(Year, gdp.CHN, color ="China", shape ="China") ,size=2)+
  geom_line( aes(Year, gdp.USA, color="EUA"),size=1)+
  geom_point( aes(Year,  gdp.USA, color="EUA", shape ="EUA"),size=2)+
  geom_line( aes(Year,  gdp.KOR, color="Coreia"),size=1)+ 
  geom_point( aes(Year,  gdp.KOR, color="Coreia", shape ="Coreia"),size=2)+
  geom_line( aes(Year,  gdp.ARG, color="Argentina"),size=1)+
  geom_point( aes(Year,  gdp.ARG, color="Argentina", shape ="Argentina"),size=2)+
  geom_line( aes(Year,  gdp.CHL, color="Chile"),size=1)+
  geom_point(aes(Year,  gdp.CHL, color="Chile", shape ="Chile"),size=2)+
  #Definindo os títulos e os números dos eixos
  labs(title = "Produtividade",
       subtitle = "US$/pessoa empregada - 1980-2010",
       x = NULL,
       y = NULL)+
  scale_x_continuous(breaks = seq(1980,2011, by=5),
                     expand = expand_scale(mult = c(0.03,0.02)))+
  scale_y_continuous(position = "right",
                     breaks = seq(0,70000,70000/10))+
  #Associando uma cor para cada país
  scale_color_manual('',values = c('Brasil' = 'firebrick' ,
                                   'China' = 'palevioletred4', 'EUA'='palegreen4', 'Coreia'='steelblue1', 'Argentina' ='paleturquoise4' , 'Chile'='dodgerblue4')) +
  #Associando um formato de ponto para cada país
   scale_shape_manual('',values = c('Brasil' = 17,'China' = 3, 'EUA'= 18, 'Coreia'= 15, 'Argentina'= 8, 'Chile'=19)) 

#Versão final do gráfico
#Definições estéticas como funco, margem, tamanho e localização da legenda, fontes
paises <-paises+ 
  theme(aspect.ratio = 3.2/7,
        text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "darkgrey"),
        legend.position = "top",
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        legend.key.width = unit(25,"pt"),
        legend.key.height = unit(15, "pt"),
        axis.text = element_text(size = rel(1), color = "gray8"),
        axis.line.x  = element_line(color = "gray8"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9),
        line=element_blank())

paises
ggsave("mundo_paises.png")




