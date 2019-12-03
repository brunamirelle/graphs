#Autor: Bruna Mirelle

#Pacotes
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



#Reshape data
base = read.dta("10SD_jan15.dta")
base = base[(base$Variable%in%c("VA_Q05", "EMP")),]
base_wide = reshape(base, idvar = c("Country", "Year") , v.names = c("AGR","MIN","MAN","PU","CON","WRT","TRA","FIRE","GOV","OTH","SUM"), timevar = "Variable", direction = "wide")
base_wide = base_wide[!(base_wide$Year%in%c(1947,1948, 1949)),]

#Criando a variável de produtividade
base_wide$prod_tot = base_wide$"SUM.VA_Q05"/base_wide$"SUM.EMP" 


##########Gráfico de barras em pé 

#Selecionando apenas alguns países do meu interesse e reorganizando
mundo_paises_taxas =  base_wide[base_wide$Country%in%c("BRA","CHN","KOR","JPN","USA","ARG","MEX","CHL")&base_wide$Year%in%c(1970:2010),c(1,4,31)]
mundo_paises_taxas = reshape(mundo_paises_taxas,  idvar = "Country" , v.names= "prod_tot" , timevar = "Year", direction = "wide")

######################## Preparação da Base

####### Criando a taxa de crescimento anual média da produtividade
base_resultados = c()
for(ano in 1971:2010)
{
  y = which(grepl(ano,colnames(mundo_paises_taxas),))
  yminus = which(grepl(ano-1,colnames(mundo_paises_taxas),))
  growth_y  = 100*(mundo_paises_taxas[,y] - mundo_paises_taxas[,yminus])/mundo_paises_taxas[,yminus]
  base_resultados = cbind(base_resultados, growth_y)
}

#Definindo nomes de colunas e linhas
colnames(base_resultados) = paste("growth", 1971:2010, sep="_")
rownames(base_resultados) = mundo_paises_taxas$Country
y2010 =  which(grepl(2010,colnames(mundo_paises_taxas),))
y1970 =   yminus = which(grepl(1970,colnames(mundo_paises_taxas),))
growth_y  = 100*((mundo_paises_taxas[,y2010]/mundo_paises_taxas[,y1970])^(1/(2010-1970)) - 1)

#Base final
base_resultados = cbind(base_resultados, "avg_growth" = growth_y)
base_resultados =data.frame(base_resultados)
base_resultados = cbind(mundo_paises_taxas[1], base_resultados)


#Substituindo sigla por nome completo
base_resultados$Country=gsub("CHN", "China", base_resultados$Country)
base_resultados$Country=gsub("USA", "Estados Unidos", base_resultados$Country)
base_resultados$Country=gsub("BRA", "Brasil", base_resultados$Country)
base_resultados$Country=gsub("CHL", "Chile", base_resultados$Country)
base_resultados$Country=gsub("ARG", "Argentina", base_resultados$Country)
base_resultados$Country=gsub("JPN", "Japão", base_resultados$Country)
base_resultados$Country=gsub("MEX", "México", base_resultados$Country)
base_resultados$Country=gsub("KOR", "Coreia", base_resultados$Country)



############################ Gráfico  
#Define coluna, linha vermelha no eixo 0, modifica as legendas, muda a coordenada do gráfico para as barras ficarem deitadas
ggplot(base_resultados, aes(x =  reorder(Country, avg_growth), y = (avg_growth), fill = Country))+
  geom_col(width = 0.7, color = "dodgerblue4", fill= "dodgerblue4")+
  geom_hline(yintercept = 0, color="red")+ 
  scale_color_manual(" ", labels = c("CHN" ="China", "KOR"="Coreia","JPN"="Japão","CHL"="Chile","USA"="Estados Unidos", "ARG"="Argentina","BRA"="Brasil","MEX"="México"))+
  coord_flip()+
  #Define títulos
  labs(title = "Taxa de crescimento anual média da produtividade (%)",
       subtitle = "1970-2010",
       x = NULL,
       y = "Taxa de crescimento anual média (%)")+
  #Mudança do fundo do gráfico e estética
  theme_bw()+
  theme(aspect.ratio = 3.2/7,
        text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(0,15,0,15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.border = element_blank(), 
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



ggsave("mundo_barra1970a2010.png")