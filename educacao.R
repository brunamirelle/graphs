#Autor: Bruna Mirelle

#Educação: 
#https://databank.worldbank.org/source/education-statistics-%5E-all-indicators
#Barro-Lee: Average years of total schooling, age 25+, total

#Produtividade: 
#https://databank.worldbank.org/source/wdi-database-archives-(beta)#
#GDP per person employed (constant 2011 PPP $)
 
#Países: Argentina, Brasil, China, Chile, Coreia do Sul, Índia
#1980 a 2010

#Pacotes
library(foreign)
library(reshape2)
library(tidyverse)
library(magrittr)
library(ggthemes)
library(scales)
library(magick)
library(ggplot2)


#Tratamento das bases
#base de produtividade
base_gdp = read.csv("6dee4fa3-023c-455a-ba38-c4a2ea625a33_Data.csv", stringsAsFactors = FALSE)
base_gdp = base_gdp[,c(2,7:37)]
colnames(base_gdp) = c("Country",paste("gdp",c(1980:2010), sep="."))
base_gdp = reshape(data = base_gdp,  direction = "long", varying = 2:32)
names(base_gdp)[2] = "Year"
base_gdp$id = NULL
base_gdp = base_gdp[base_gdp$Year%in%c(1980,2010)&base_gdp$Country%in%c("ARG","BRA","CHN","CHL","KOR","IND"),]
base_gdp$gdp=as.numeric(base_gdp$gdp)

#base educação
educacao = read.csv("9033fc31-ca43-49bd-8ab4-eda8537470ef_Data.csv", stringsAsFactors = FALSE)
educacao = educacao[1:6,c(1,2,15,45)]
colnames(educacao) = c("name", "Country", "mean.1980", "mean.2010")
educacao = reshape(data = educacao,  direction = "long", varying = 3:4)
educacao = educacao[,1:4]
names(educacao)[3] = "Year"
educacao$name <- NULL


#juntando as bases
educacao = merge(educacao, base_gdp)

#Substituindo as siglas pelo nome inteiro
educacao$Country=gsub("CHN", "China", educacao$Country)
educacao$Country=gsub("BRA", "Brasil", educacao$Country)
educacao$Country=gsub("CHL", "Chile", educacao$Country)
educacao$Country=gsub("ARG", "Argentina", educacao$Country)
educacao$Country=gsub("IND", "Índia", educacao$Country)
educacao$Country=gsub("KOR", "Coreia do Sul", educacao$Country)

#Organizando os dados para fazer a legenda dos anos em cada ponto (x e y) 
KORy1980 = educacao[educacao$Country=="Coreia do Sul"&educacao$Year=="1980","gdp"]
KORy2010 = educacao[educacao$Country=="Coreia do Sul"&educacao$Year=="2010","gdp"]
KORx1980 = educacao[educacao$Country=="Coreia do Sul"&educacao$Year=="1980","mean"]
KORx2010 = educacao[educacao$Country=="Coreia do Sul"&educacao$Year=="2010","mean"]


ARGy1980 = educacao[educacao$Country=="Argentina"&educacao$Year=="1980","gdp"]
ARGy2010 = educacao[educacao$Country=="Argentina"&educacao$Year=="2010","gdp"]
ARGx1980 = educacao[educacao$Country=="Argentina"&educacao$Year=="1980","mean"]
ARGx2010 = educacao[educacao$Country=="Argentina"&educacao$Year=="2010","mean"]


BRAy1980 = educacao[educacao$Country=="Brasil"&educacao$Year=="1980","gdp"]
BRAy2010 = educacao[educacao$Country=="Brasil"&educacao$Year=="2010","gdp"]
BRAx1980 = educacao[educacao$Country=="Brasil"&educacao$Year=="1980","mean"]
BRAx2010 = educacao[educacao$Country=="Brasil"&educacao$Year=="2010","mean"]

CHLy1980 = educacao[educacao$Country=="Chile"&educacao$Year=="1980","gdp"]
CHLy2010 = educacao[educacao$Country=="Chile"&educacao$Year=="2010","gdp"]
CHLx1980 = educacao[educacao$Country=="Chile"&educacao$Year=="1980","mean"]
CHLx2010 = educacao[educacao$Country=="Chile"&educacao$Year=="2010","mean"]

CHNy1980 = educacao[educacao$Country=="China"&educacao$Year=="1980","gdp"]
CHNy2010 = educacao[educacao$Country=="China"&educacao$Year=="2010","gdp"]
CHNx1980 = educacao[educacao$Country=="China"&educacao$Year=="1980","mean"]
CHNx2010 = educacao[educacao$Country=="China"&educacao$Year=="2010","mean"]

INDy1980 = educacao[educacao$Country=="Índia"&educacao$Year=="1980","gdp"]
INDy2010 = educacao[educacao$Country=="Índia"&educacao$Year=="2010","gdp"]
INDx1980 = educacao[educacao$Country=="Índia"&educacao$Year=="1980","mean"]
INDx2010 = educacao[educacao$Country=="Índia"&educacao$Year=="2010","mean"]


#########################################  Gráfico    ###########################################################

#ggplot: define a area do gráfico
#Para cada país: 
#geom_line e geom_point definem as linhas e os pontos respectivamente
#size = tamanho e alpha = transparênciada
#Definir color como o nome do país é importante para associar as cores posteriormente 

educ <- ggplot(educacao, aes(mean, gdp))+
  geom_line(data=educacao[educacao$Country=="Brasil",], aes(mean, gdp,  color = "Brasil") ,size=1,alpha=0.7)+
  geom_point(data=educacao[educacao$Country=="Brasil",], aes(mean, gdp,  color = "Brasil") ,size=3,alpha=0.7)+
  geom_line(data=educacao[educacao$Country=="Coreia do Sul",], aes(mean, gdp,  color = "Coreia do Sul") ,size=1,alpha=0.7)+
  geom_point(data=educacao[educacao$Country=="Coreia do Sul",], aes(mean, gdp,  color = "Coreia do Sul") ,size=3,alpha=0.7)+
  geom_line(data=educacao[educacao$Country=="Argentina",], aes(mean, gdp,  color = "Argentina") ,size=1,alpha=0.7)+
  geom_point(data=educacao[educacao$Country=="Argentina",], aes(mean, gdp,  color = "Argentina") ,size=3,alpha=0.7)+
  geom_line(data=educacao[educacao$Country=="Chile",], aes(mean, gdp,  color = "Chile") ,size=1,alpha=0.7)+
  geom_point(data=educacao[educacao$Country=="Chile",], aes(mean, gdp,  color = "Chile") ,size=3,alpha=0.7)+
  geom_line(data=educacao[educacao$Country=="China",], aes(mean, gdp,  color = "China") ,size=1,alpha=0.7)+
  geom_point(data=educacao[educacao$Country=="China",], aes(mean, gdp,  color = "China") ,size=3,alpha=0.7)+
  geom_line(data=educacao[educacao$Country=="Índia",], aes(mean, gdp,  color = "Índia") ,size=1,alpha=0.7)+
  geom_point(data=educacao[educacao$Country=="Índia",], aes(mean, gdp,  color = "Índia") ,size=3,alpha=0.7)


#Definição de títulos
  educ = educ +
      labs(title = "Evolução da escolaridade e da produtividade",
       subtitle = "1980-2010",
       x = "Escolaridade média da população adulta",
       y = "Produtividade média do trabalho (PIB por trabalhador empregado US$)",
       color = ' ')+
  #definingo os números e limites dos eixos
    scale_x_continuous(breaks = seq(0,15, by=2.5),
                     limits=c(NA,12.5) )+
  scale_y_continuous(breaks = seq(0,50000,10000))+
    # Associando uma cor para cada país 
  scale_color_manual('',
                     values = c('Argentina' = 'steelblue1','Brasil' = 'firebrick', 'Chile'='forestgreen', 'China'='gold', 'Índia'='darkorange1','Coreia do Sul'='dodgerblue4'))+
    #Adicionando as legendas em cada ponto, a localização de cada legenda é feita a mão       
     annotate("text", y = KORy1980-1500, x=KORx1980-0.3,
             label = "1980",
             color = "gray8", size=3.7)+
    annotate("text", y = KORy2010+1500, x=KORx2010+0.1,
             label = "2010",
             color = "gray8", size=3.7)+
    annotate("text", y = INDy1980-200, x=INDx1980-0.35,
             label = "1980",
             color = "gray8", size=3.7)+
    annotate("text", y = INDy2010+1500, x=INDx2010-0.2,
             label = "2010",
             color = "gray8", size=3.7)+
    annotate("text", y = ARGy1980-200, x=ARGx1980-0.35,
             label = "1980",
             color = "gray8", size=3.7)+
    annotate("text", y = ARGy2010+400, x=ARGx2010+0.4,
             label = "2010",
             color = "gray8", size=3.7)+
  annotate("text", y = CHLy1980-200, x=CHLx1980-0.35,
           label = "1980",
           color = "gray8", size=3.7)+
    annotate("text", y = CHLy2010+1500, x=CHLx2010-0.1,
             label = "2010",
             color = "gray8", size=3.7)+
    annotate("text", y = BRAy1980-1500, x=BRAx1980,
             label = "1980",
             color = "gray8", size=3.7)+
    annotate("text", y = BRAy2010+1700, x=BRAx2010+0.15,
             label = "2010",
             color = "gray8", size=3.7)+
    annotate("text", y = CHNy1980+1500, x=CHNx1980-0.2,
             label = "1980",
             color = "gray8", size=3.7)+
    annotate("text", y = CHNy2010-1600, x=CHNx2010,
             label = "2010",
             color = "gray8", size=3.7)


############################# Últimos detalhes do gráfico #######################
  ### inserir fundo branco, alterar fonte, decidir cores das linhas de fundo, borda, margem, tamanho e localização da legenda, cores e tamanhos dos títulos 
   
educ <-    educ+
  theme_bw()+
   theme(#aspect.ratio = 4/7,
        text = element_text(family = "Roboto Condensed"),
       plot.margin = margin(0,15,0,15),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x =element_line(color = "gray88"),
        panel.grid.major.y = element_line(color = "gray88"),
       panel.border = element_blank(),
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
       legend.position = c(0.197,0.73),
        legend.key.width = unit(25,"pt"),
        legend.key.height = unit(15, "pt"),
       legend.key=element_blank(),
        axis.text = element_text(size = rel(1), color = "gray8"),
        axis.line.x  = element_line(color = "gray8"),
      axis.line.y  = element_line(color = "gray8"),
      axis.ticks.y = element_blank(),
        plot.title = element_text(size = rel(1.5), hjust = 0, vjust = 0.3 , face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9),
        line=element_blank())
educ

#Salvar
ggsave("educacao.png")





