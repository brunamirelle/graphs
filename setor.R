#Autor: Bruna Mirelle

# install.packages("tidyverse")
# install.packages("magick")
# install.packages("ggthemes")
# install.packages("deb")

library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)
library(magick)
library(ggplot2)

#Base de dados de setores para o Brasil
#Produtividade: 
#https://databank.worldbank.org/source/wdi-database-archives-(beta)#
#GDP per person employed (constant 2011 PPP $)

#Reorganizando a base de dados
base = read.dta("10SD_jan15.dta")
base = base[(base$Variable%in%c("VA_Q05", "EMP")),]
base_wide = reshape(base, idvar = c("Country", "Year") , v.names = c("AGR","MIN","MAN","PU","CON","WRT","TRA","FIRE","GOV","OTH","SUM"), timevar = "Variable", direction = "wide")
base_wide = base_wide[!(base_wide$Year%in%c(1947,1948, 1949)),]

#Criando a variável de produtividade pra cada setor
base_wide$prod_agri = base_wide$"AGR.VA_Q05"/base_wide$"AGR.EMP" 
base_wide$prod_manu = base_wide$"MAN.VA_Q05"/base_wide$"MAN.EMP" 
base_wide$prod_wrt = base_wide$"WRT.VA_Q05"/base_wide$"WRT.EMP" 
base_wide$prod_tra = base_wide$"TRA.VA_Q05"/base_wide$"TRA.EMP" 
base_wide$prod_tot = base_wide$"SUM.VA_Q05"/base_wide$"SUM.EMP" 


#Selecionando apenas Brasil e as colunas de cada setor
brasil = base_wide[base_wide$Country=="BRA", c(4,27,28,29,30,31)]



########################## Gráfico ########################################

#Definindo as linhas para cada setor
p <- ggplot(brasil)+
  geom_line( aes(Year, prod_tra,  color = "Transportes, armazenamento e comunicação") ,size=1)+
  geom_line( aes(Year, prod_manu, color ="Manufatura") ,size=1)+
  geom_line( aes(Year, prod_wrt, color="Comércio, restaurantes e hotéis"),size=1)+
  geom_line( aes(Year, prod_agri, color="Agricultura"),size=1)+ 
  #Definindo títulos 
  labs(title = "Produtividade por setor",
       subtitle = "'Brasil: 1950-2011",
       x = NULL,
       y = NULL)+
  #Definindo números dos eixos
  scale_x_continuous(breaks = seq(1950,2011, by=5),
                     expand = expand_scale(mult = c(0.03,0.02)))+
  scale_y_continuous(position = "right",
                     breaks = seq(0,40,40/8))+
  #Definindo uma cor para cada setor
  scale_color_manual(values = c('Transportes, armazenamento e comunicação' = 'dodgerblue4',
                                'Manufatura' = 'firebrick', 'Comércio, restaurantes e hotéis'='steelblue1', 'Agricultura'='goldenrod3')) +
  labs(color = ' ')

# Definindo as margens, tamanho e localização das legendas, fundo do gráfico e linhas 
p <- p+ 
  theme(aspect.ratio = 3.2/7,
        text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "darkgrey"),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        legend.position = c(0.197,0.83),
        legend.key.width = unit(25,"pt"),
        legend.key.height = unit(15, "pt"),
        axis.text = element_text(size = rel(1), color = "gray8"),
        axis.line.x  = element_line(color = "gray8"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9),
        line=element_blank())
p
ggsave("setor.png")