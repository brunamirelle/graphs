#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf"))
old.loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","en_US.UTF-8")
Sys.setlocale(old.loc)
library(scales)
library(ggplot2)
library(data.table)
library(stringr)
setwd("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

dataset_indice = read.csv2("dataset_indice.csv", sep=";", stringsAsFactors = F)

painel_medidas_final = read.csv2("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXv",stringsAsFactors = FALSE)

painel_medidas_final =  painel_medidas_final[,c(4:25)]
painel_medidas_final = data.table(painel_medidas_final)
indices_dup = which(duplicated(painel_medidas_final[,c("Localidade","date")])) 

cols = c(colnames(painel_medidas_final[,8:22]))

#Função para aplicar a soma das linhas por data e estado em cada coluna 
fct_those_columns <- function(DT, cols_in, cols_new, fct) {
  DT[, (cols_new) := lapply(.SD, fct), .SDcols = cols_in, by=c("Localidade","date")]
}
fct_those_columns(painel_medidas_final, cols, cols, sum)
painel_medidas_final = painel_medidas_final[-indices_dup,]

#Colocando todas as colunas em uma só, poderia ter feito apenas isso no passo anterior
painel_medidas_final[, medidas:=sum(.SD),.SDcols =cols, by=c("Localidade","date")]
#Tem medida 
painel_medidas_final$medidas=1 

dataset_indice$date = gsub("mar","-03-",dataset_indice$date)
dataset_indice$date = gsub("apr","-04-",dataset_indice$date)
dataset_indice$date = gsub("feb","-02-",dataset_indice$date)

split_date = (str_split(dataset_indice$date,"-", simplify = TRUE))


dataset_indice$date = paste(split_date[,3],split_date[,2],split_date[,1],sep="-")
colnames(painel_medidas_final)[2] = "name_state"
painel_covid = merge(dataset_indice,painel_medidas_final,by=c("date","name_state"), all = T)

estados = unique(c(painel_covid$name_state))

for (estado in estados){
  
  df = painel_covid[painel_covid$name_state==estado,]
  df$indice_isolamento = as.numeric(df$indice_isolamento)
  df= df[complete.cases(df$indice_isolamento),]
  df= df[complete.cases(df$pesotot),]
  
  
  df = data.table(df)
  df[, indice_isolamento := weighted.mean(indice_isolamento, pesotot), by=c("name_state","date")]
  df[, confirmed := sum(confirmed), by=c("name_state","date")]
  df[, deaths := sum(deaths), by=c("name_state","date")]
  df = as.data.frame(df)
  
  df[,c(53:68)][is.na(df[,c(53:68)])] <- 0
  
  
  #A partir daqui mantenho apenas as colunas de interesse
  cols_agregate = c("name_state","date", "confirmed","deaths")
  
  col_agregar = c("indice_isolamento","medidas")
  col_agregar = which(colnames(df)==col_agregar)
  
  lista_agg = list()
  for(name in cols_agregate)
    lista_agg[[name]] = df[,name]
  
  df =aggregate(df[,col_agregar],lista_agg, mean)
  
  
  df = df[order(as.Date(df$date, format="%Y-%m-%d")),]
  df$date = as.Date(df$date, format="%Y-%m-%d")
  
  
  #Data da primeira medida
  data_primeira_caso = "2020-02-25"
  #df[df$confirmed>0,2][1]
  #data_primeira_morte = df[df$deaths>0,2][1]
  
  
  pcasoy =df[df$date==data_primeira_caso ,"indice_isolamento"]
  pcasox = as.Date(df[df$date==data_primeira_caso ,"date"])
  #pmortey =df[df$date==data_primeira_morte ,"indice_isolamento"]
  #pmortex = as.Date(df[df$date==data_primeira_morte ,"date"])
  
  #Tratando as medidas 
  
  medidas = which((df$medidas>0))
  
  datas_med = c()
  for (med in c(1:length(medidas))){
    data_tmp =  df$date[medidas[med]]
    datas_med = c(datas_med, data_tmp)
  }
  
  datas_med =  as.Date(datas_med, origin="1970-01-01")
  
  pontos = data.frame(c())
  for (dat in  c(1:length(datas_med))){
    pmedida_tmpy = df[df$date==datas_med[dat],"indice_isolamento"]
    pmedida_tmpx = datas_med[dat]
    #assign(paste("pmedida", dat,"x",sep=""), pmedida_tmpx)
    # assign(paste("pmedida", dat,"y",sep=""), pmedida_tmpy)
    pontos = rbind(pontos,c(pmedida_tmpx, pmedida_tmpy))
  }
  colnames(pontos) = c("pontox","pontoy")
  pontos$pontox = as.Date(pontos$pontox, origin = "1970-01-01") 
  
  list_geom_point = list()
  for(ponto in 1:nrow(pontos)){
    list_geom_point[[ponto]] =  geom_point(y =pontos[ponto,2] , x=pontos[ponto,1],color = "darkred", shape=19, size=2)
  }
  
  
  camadas <- list(geom_line(color = "#01344A", size = 1.5)) 
  #camadas <- list(camadas, list_geom_point[[1]])
  camadas <- list(camadas, geom_point(y =pcasoy , x=pcasox ,color = "darkred", shape=19, size=2))
  camadas <- list(camadas,
                  geom_segment(y =pcasoy   , x=pcasox , xend=pcasox, yend=pcasoy + 0.05,   alpha = 1, color = "darkred"))
  #   geom_segment(y =pmortey   , x=pmortex , xend=pmortex+2, yend=pmortey - 0.04,   alpha = 1, color = "darkred"),
  #   geom_segment(y =pontos[1,2]   , x=pontos[1,1], xend=pontos[1,1], yend=pontos[1,2] - 0.05,   alpha = 1, color = "darkred"))
  
  camadas <- list(camadas,
                  annotate("text", y = pcasoy+0.065, x=pcasox, 
                           label = "First case",
                           color = "black"))
  #   annotate("text", y = pmortey - 0.0515, x=pmortex+2.5, 
  #            label = "First Death",
  #            color = "black"), 
  #   annotate("text", y = pontos[1,2]- 0.06 , x=pontos[1,1], 
  #     label = "First Measure",
  #     color = "black")) 
  
  
  
  g <-ggplot(data = df, aes(x = date, y = indice_isolamento,group = 1)) + 
    camadas+
    labs(title = "Social Distancing Index per day",
         subtitle =estado ,
         x = "Days",
         y ="Social Distancing Index")  +
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
      axis.ticks.y = element_line(),
      axis.ticks.x = element_line()
      #axis.text.x =  element_blank(),
      # axis.text.y =  element_blank()
    )
  assign(paste("graph",estado,sep=""),g)
  ggsave(paste("Graphs/graph_timeseries_",estado,".png",sep = ""))
}

###################### Sâo Paulo e Rio de Janeiro ###################################################

estado = "São Paulo"

df = painel_covid[painel_covid$name_state==estado,]
df$indice_isolamento = as.numeric(df$indice_isolamento)
df= df[complete.cases(df$indice_isolamento),]
df= df[complete.cases(df$pesotot),]


df = data.table(df)
df[, indice_isolamento := weighted.mean(indice_isolamento, pesotot), by=c("name_state","date")]
df[, confirmed := sum(confirmed), by=c("name_state","date")]
df[, deaths := sum(deaths), by=c("name_state","date")]
df = as.data.frame(df)

df[,c(53:68)][is.na(df[,c(53:68)])] <- 0


#A partir daqui mantenho apenas as colunas de interesse
cols_agregate = c("name_state","date", "confirmed","deaths")

col_agregar = c("indice_isolamento","medidas")
col_agregar = which(colnames(df)==col_agregar)

lista_agg = list()
for(name in cols_agregate)
  lista_agg[[name]] = df[,name]

df =aggregate(df[,col_agregar],lista_agg, mean)


df = df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date = as.Date(df$date, format="%Y-%m-%d")


#Data da primeira medida
data_primeira_caso = "2020-02-25"
#df[df$confirmed>0,2][1]
#data_primeira_morte = df[df$deaths>0,2][1]


pcasoy =df[df$date==data_primeira_caso ,"indice_isolamento"]
pcasox = as.Date(df[df$date==data_primeira_caso ,"date"])
#pmortey =df[df$date==data_primeira_morte ,"indice_isolamento"]
#pmortex = as.Date(df[df$date==data_primeira_morte ,"date"])

#Tratando as medidas 

medidas = which((df$medidas>0))

datas_med = c()
for (med in c(1:length(medidas))){
  data_tmp =  df$date[medidas[med]]
  datas_med = c(datas_med, data_tmp)
}

datas_med =  as.Date(datas_med, origin="1970-01-01")

pontos = data.frame(c())
for (dat in  c(1:length(datas_med))){
  pmedida_tmpy = df[df$date==datas_med[dat],"indice_isolamento"]
  pmedida_tmpx = datas_med[dat]
  #assign(paste("pmedida", dat,"x",sep=""), pmedida_tmpx)
  # assign(paste("pmedida", dat,"y",sep=""), pmedida_tmpy)
  pontos = rbind(pontos,c(pmedida_tmpx, pmedida_tmpy))
}
colnames(pontos) = c("pontox","pontoy")
pontos$pontox = as.Date(pontos$pontox, origin = "1970-01-01") 

list_geom_point = list()
for(ponto in 1:nrow(pontos)){
  list_geom_point[[ponto]] =  geom_point(y =pontos[ponto,2] , x=pontos[ponto,1],color = "darkred", shape=19, size=2)
}


camadas <- list(geom_line(color = "#01344A", size = 1.5)) 
camadas <- list(camadas, list_geom_point[[1]])
camadas <- list(camadas, geom_point(y =pcasoy , x=pcasox ,color = "darkred", shape=19, size=2))


camadas <- list(camadas,
                geom_segment(y =pcasoy   , x=pcasox , xend=pcasox, yend=pcasoy + 0.05,   alpha = 1, color = "darkred"))
#   geom_segment(y =pmortey   , x=pmortex , xend=pmortex+2, yend=pmortey - 0.04,   alpha = 1, color = "darkred"),
#  geom_segment(y =pontos[1,2]   , x=pontos[1,1], xend=pontos[1,1], yend=pontos[1,2] - 0.05,   alpha = 1, color = "darkred"))

camadas <- list(camadas,
                annotate("text", y = pcasoy+0.065, x=pcasox, 
                         label = "First case",
                         color = "black"),
                #   annotate("text", y = pmortey - 0.0515, x=pmortex+2.5, 
                #            label = "First Death",
                #            color = "black"), 
                annotate("text", y = pontos[1,2] , x=pontos[1,1] + 5.4, 
                         label = "First Measure", 
                         color = "black")) 



g <-ggplot(data = df, aes(x = date, y = indice_isolamento,group = 1)) + 
  camadas+
  labs(title = "Social Distancing Index per day",
       subtitle =estado ,
       x = "Days",
       y ="Social Distancing Index")  +
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
    axis.ticks.y = element_line(),
    axis.ticks.x = element_line(),
    axis.text.x=element_text(angle=45, hjust=1)
    
    #axis.text.x =  element_blank(),
    # axis.text.y =  element_blank()
  )
assign(paste("graph",estado,sep=""),g)


g+ geom_segment(y =pcasoy   , x=pcasox , xend=pcasox, yend=0,   alpha = 1, color = "darkred",linetype =2 )+
  geom_segment(y =pontos[1,2]  , x=pontos[1,1] , xend=pontos[1,1] , yend=0,   alpha = 1, color = "darkred",linetype =2 )+
  scale_x_date(breaks = c(seq(as.Date("2020-02-01"), as.Date("2020-04-12"), by="1 month"),pcasox, pontos[1,1]),labels=c(("Feb"),("Mar"),("Apr"),"25-Feb","13-Mar")) 


ggsave(paste("Graphs/graph_timeseries_",estado,".png",sep = ""))


############ Rio de janeiro



estado = "Rio de Janeiro"

df = painel_covid[painel_covid$name_state==estado,]
df$indice_isolamento = as.numeric(df$indice_isolamento)
df= df[complete.cases(df$indice_isolamento),]
df= df[complete.cases(df$pesotot),]


df = data.table(df)
df[, indice_isolamento := weighted.mean(indice_isolamento, pesotot), by=c("name_state","date")]
df[, confirmed := sum(confirmed), by=c("name_state","date")]
df[, deaths := sum(deaths), by=c("name_state","date")]
df = as.data.frame(df)

df[,c(53:68)][is.na(df[,c(53:68)])] <- 0


#A partir daqui mantenho apenas as colunas de interesse
cols_agregate = c("name_state","date", "confirmed","deaths")

col_agregar = c("indice_isolamento","medidas")
col_agregar = which(colnames(df)==col_agregar)

lista_agg = list()
for(name in cols_agregate)
  lista_agg[[name]] = df[,name]

df =aggregate(df[,col_agregar],lista_agg, mean)


df = df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date = as.Date(df$date, format="%Y-%m-%d")


#Data da primeira medida
#data_primeira_caso = "2020-02-25"
data_primeira_caso = df[df$confirmed>0,2][1]
#data_primeira_morte = df[df$deaths>0,2][1]


pcasoy =df[df$date==data_primeira_caso ,"indice_isolamento"]
pcasox = as.Date(df[df$date==data_primeira_caso ,"date"])
#pmortey =df[df$date==data_primeira_morte ,"indice_isolamento"]
#pmortex = as.Date(df[df$date==data_primeira_morte ,"date"])

#Tratando as medidas 

medidas = which((df$medidas>0))

datas_med = c()
for (med in c(1:length(medidas))){
  data_tmp =  df$date[medidas[med]]
  datas_med = c(datas_med, data_tmp)
}

datas_med =  as.Date(datas_med, origin="1970-01-01")

pontos = data.frame(c())
for (dat in  c(1:length(datas_med))){
  pmedida_tmpy = df[df$date==datas_med[dat],"indice_isolamento"]
  pmedida_tmpx = datas_med[dat]
  #assign(paste("pmedida", dat,"x",sep=""), pmedida_tmpx)
  # assign(paste("pmedida", dat,"y",sep=""), pmedida_tmpy)
  pontos = rbind(pontos,c(pmedida_tmpx, pmedida_tmpy))
}
colnames(pontos) = c("pontox","pontoy")
pontos$pontox = as.Date(pontos$pontox, origin = "1970-01-01") 

list_geom_point = list()
for(ponto in 1:nrow(pontos)){
  list_geom_point[[ponto]] =  geom_point(y =pontos[ponto,2] , x=pontos[ponto,1],color = "darkred", shape=19, size=2)
}


camadas <- list(geom_line(color = "#01344A", size = 1.5)) 
camadas <- list(camadas, list_geom_point[[1]])
camadas <- list(camadas, geom_point(y =pcasoy , x=pcasox ,color = "darkred", shape=19, size=2))


camadas <- list(camadas,
                #     geom_segment(y =pcasoy   , x=pcasox , xend=pcasox, yend=pcasoy + 0.05,   alpha = 1, color = "darkred"),
                #   geom_segment(y =pmortey   , x=pmortex , xend=pmortex+2, yend=pmortey - 0.04,   alpha = 1, color = "darkred"),
                geom_segment(y =pontos[1,2]   , x=pontos[1,1], xend=pontos[1,1], yend=pontos[1,2] + 0.05,   alpha = 1, color = "darkred"))

camadas <- list(camadas,
                annotate("text", y = pcasoy -0.015, x=pcasox -4, 
                         label = "First case",
                         color = "black"),
                #   annotate("text", y = pmortey - 0.0515, x=pmortex+2.5, 
                #            label = "First Death",
                #            color = "black"), 
                annotate("text", y = pontos[1,2]+ 0.065 , x=pontos[1,1], 
                         label = "First Measure",
                         color = "black")) 



g <-ggplot(data = df, aes(x = date, y = indice_isolamento,group = 1)) + 
  camadas+
  labs(title = "Social Distancing Index per day",
       subtitle =estado ,
       x = "Days",
       y ="Social Distancing Index")  +
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
    axis.ticks.y = element_line(),
    axis.ticks.x = element_line(),
    #axis.text.x =  element_blank(),
    # axis.text.y =  element_blank()
    axis.text.x=element_text(angle=45, hjust=1)
  )
assign(paste("graph",estado,sep=""),g)


g+  geom_segment(y =pcasoy   , x=pcasox , xend=pcasox, yend=0,   alpha = 1, color = "darkred",linetype =2 )+
  geom_segment(y =pontos[1,2]  , x=pontos[1,1] , xend=pontos[1,1] , yend=0,   alpha = 1, color = "darkred",linetype =2 )+
  scale_x_date(breaks = c(seq(as.Date("2020-02-01"), as.Date("2020-04-12"), by="1 month"),pcasox, pontos[1,1]),labels=c(("Feb"),("Mar"),("Apr"),"05-Mar","08-Mar")) 

ggsave(paste("Graphs/graph_timeseries_",estado,".png",sep = ""))


