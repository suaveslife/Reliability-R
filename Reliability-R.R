#fun??o minimo rely com escolha entre diferentes curvas
Fit_best<-function(dados){
  #getlibrary
  library(fitdistrplus)
  library(EnvStats)
  if(length(dados)>=8){
    #fixing data
    dados<- as.numeric(dados)
    
    #criando lista com todas as distribui??es possiveis
    #distr<- c("lnorm","weibull","gamma","logis","norm")
    distr<- c( "exp","lnorm","weibull","gamma","logis","norm")
    
    #Criando lista com os fit de acordo com a distribui??o
    expG<- fitdist(dados,"exp")
    lnormG<- fitdist(dados,"lnorm")
    weibullG<- fitdist(dados,"weibull")
    gammaG<-fitdist(dados,"gamma",lower = c(0, 0)) 
    logisG<-fitdist(dados,"logis")
    normG<- fitdist(dados,"norm")
    
    #Lista de fit
    #lista_f<-list( lnormG,weibullG,gammaG,logisG,normG)
    lista_f<-list(expG, lnormG,weibullG,gammaG,logisG,normG)
    
    #Realizadno testes de goodness
    test<- gofstat(lista_f)
    
    #transformando valores do teste em uma lista
    lista_T<- as.numeric(test$ks)
    
    #Pegando o menor valor na lista
    menor<- Reduce(min,lista_T)
    
    #Pegando index deste valor
    index <- match(menor,lista_T)
    
    #pegando dist na lista
    resultado<-distr[index]
    
    
    
    return(c(resultado,lista_f[index]))}
  else{print("Erro: A amostra deve ter no minimo 8 valores" )}
}


# Criando fun??o onde a entrada ser?o os dados de vidas, a confiabilidade desejada
#ou os dias do ciclo do plano.

Reliability_dias<- function(dados,conf){
  #getlibrary
  library(fitdistrplus)
  library(EnvStats)
  
  #trocando confiabilidade para taxa de falha
  if(conf<0 || conf>1){print("Erro: confiabilidade deve estar entre 0 e 1")}
  pfalha<- (1-conf)
  #Pegando resultado de FiT_best
  aux<- Fit_best(dados)
  
  #Aplicando quantile para cada tipo de distribui??o
  ifelse( aux[[1]]=="exp", resultado <- qexp(pfalha,rate=as.list(aux[[2]]$estimate)$rate),
          ifelse( aux[[1]]=="lnorm",resultado <- qlnorm(pfalha, meanlog = as.list(aux[[2]]$estimate)$meanlog,sdlog =as.list(aux[[2]]$estimate)$sdlog ),
                  ifelse(aux[[1]]=="weibull",resultado<- qweibull(pfalha, shape =as.list(aux[[2]]$estimate)$shape, scale=as.list(aux[[2]]$estimate)$scale ),
                         ifelse(aux[[1]]=="gamma", resultado<- qgamma(pfalha,shape =as.list(aux[[2]]$estimate)$shape, rate = as.list(aux[[2]]$estimate)$rate ),
                                ifelse(aux[[1]]=="logis", resultado<- qlogis(pfalha, location = as.list(aux[[2]]$estimate)$location , scale = as.list(aux[[2]]$estimate)$scale ),
                                       ifelse(aux[[1]]=="norm", resultado<- qnorm(pfalha,mean =as.list(aux[[2]]$estimate)$mean , sd=as.list(aux[[2]]$estimate)$sd )
                                              
                                       ))))))
  #aproximando
  resultado<- round(resultado,digits=1)
  return(resultado) 
  
}

# Criando fun??o onde a entrada ser?o os dados de vidas, a confiabilidade desejada
#ou os dias do ciclo do plano.
Reliability_conf<- function(dados,dias){
  #getlibrary
  library(fitdistrplus)
  library(EnvStats)
  
  #Pegando resultado de FiT_best
  aux<- Fit_best(dados)
  
  #Aplicando quantile para cada tipo de distribui??o
  ifelse( aux[[1]]=="exp", resultado <- pexp(dias,rate=as.numeric(aux[2])),
          ifelse( aux[[1]]=="lnorm",resultado <- plnorm(dias, meanlog = as.list(aux[[2]]$estimate)$meanlog,sdlog =as.list(aux[[2]]$estimate)$sdlog ),
                  ifelse(aux[[1]]=="weibull",resultado<- pweibull(dias, shape =as.list(aux[[2]]$estimate)$shape, scale=as.list(aux[[2]]$estimate)$scale ),
                         ifelse(aux[[1]]=="gamma", resultado<- pgamma(dias,shape =as.list(aux[[2]]$estimate)$shape, rate = as.list(aux[[2]]$estimate)$rate ),
                                ifelse(aux[[1]]=="logis", resultado<- plogis(dias, location = as.list(aux[[2]]$estimate)$location , scale = as.list(aux[[2]]$estimate)$scale ),
                                       ifelse(aux[[1]]=="norm", resultado<- pnorm(dias,mean =as.list(aux[[2]]$estimate)$mean , sd=as.list(aux[[2]]$estimate)$sd )
                                              
                                       ))))))
  
  #pasando de pflaha pra conf
  conf <- 1-resultado
  
  return(conf) 
  
}


#Fun??o para plot comparativo das distribui??o
plot_comp<- function(dados,Equi, unidade = "Dias"){
  #getlibrary
  library(fitdistrplus)
  library(EnvStats)
  library(ggplot2)
  library(cowplot)
  #criando lista com todas as distribui??es possiveis
  distr<- c( "exp","lnorm","weibull","gamma","logis","norm")
  
  #Criando lista com os fit de acordo com a distribui??o
  expG<- fitdist(dados,"exp")
  lnormG<- fitdist(dados,"lnorm")
  weibullG<- fitdist(dados,"weibull")
  gammaG<-fitdist(dados,"gamma") 
  logisG<-fitdist(dados,"logis")
  normG<- fitdist(dados,"norm")
  
  #Lista de fit
  lista_f<-list(expG, lnormG,weibullG,gammaG,logisG,normG)
  
  #Realizadno testes de goodness
  teste<- gofstat(lista_f)
  
  #pegando s? as distribui??es que passaram no Ks test
  A<-as.data.frame(teste$kstest)
  B<-setNames(A,"teste")
  
  #pegando index dos testes rejeitados
  index<- match("rejected",B$teste)
  
  #retirando distribui??o da lista de plot e da lista de legenda
  if(is.na(index)){}else{lista_f<- lista_f[-index]
  distr<- distr[-index]
  }
  
  
  # plot o comarativo de curvas
  plot.legend <- distr
  plotd<-denscomp(lista_f, legendtext = plot.legend, plotstyle = "ggplot",xlab = unidade)
  plotc<-cdfcomp(lista_f, legendtext = plot.legend,plotstyle = "ggplot",xlab = unidade)
  
  p<-plot_grid(plotd, plotc, 
               labels = c("A", "B"),
               ncol = 2, nrow = 1) 
  
  #adicionando titulo na imagem
  title <- ggdraw() + draw_label(Equi, fontface='bold')
  plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
}

#Fun??o que plot dados de analise
#falta confiabilidade no grafico e o ponto de analise
#falta summary com table to show
plot_rely<- function(dados,Equi,unidade="Dias"){
  library(gridExtra)
  library(cowplot)
  library(EnvStats)
  
  aux<- Fit_best(dados)
  name<- aux[[2]]$distname
  
  print(name)
  
  if(name=="weibull"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-pweibull(x, shape=aux[[2]]$estimate[1], scale = aux[[2]]$estimate[2]),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) pweibull(x, shape=aux[[2]]$estimate[1], scale = aux[[2]]$estimate[2]),aes(colour = "Prob. de Falha"))}
  
  
  if(name=="exp"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-pexp(x, shape=aux[[2]]$estimate[1], scale = aux[[2]]$estimate[2]),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) pexp(x, shape=aux[[2]]$estimate[1], scale = aux[[2]]$estimate[2]),aes(colour = "Prob. de Falha"))}
  
  if(name=="exp"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-pexp(x, rate =aux[[2]]$estimate ),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) pexp(x,  rate =aux[[2]]$estimate ),aes(colour = "Prob. de Falha"))}
  
  if(name=="norm"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-pnorm(x, mean = aux[[2]]$estimate[1], sd = aux[[2]]$estimate[2]),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) pnorm(x, mean=aux[[2]]$estimate[1], sd = aux[[2]]$estimate[2]),aes(colour = "Prob. de Falha"))}
  
  if(name=="lnorm"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-plnorm(x, meanlog = aux[[2]]$estimate[1], sdlog = aux[[2]]$estimate[2]),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) plnorm(x, meanlog=aux[[2]]$estimate[1], sdlog = aux[[2]]$estimate[2]),aes(colour = "Prob. de Falha"))}
  
  if(name=="gamma"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-pgamma(x, shape  = aux[[2]]$estimate[1], rate = aux[[2]]$estimate[2]),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) pgamma(x, shape =aux[[2]]$estimate[1], rate = aux[[2]]$estimate[2]),aes(colour = "Prob. de Falha"))}
  
  if(name=="logis"){plota<- ggplot(data.frame(x=c(0, max(dados))), aes(x))+
    stat_function(fun=function(x) 1-pgamma(x, location  = aux[[2]]$estimate[1], scale = aux[[2]]$estimate[2]),aes(colour = "Confiabilidade")) +
    stat_function(fun=function(x) pgamma(x, location =aux[[2]]$estimate[1], scale = aux[[2]]$estimate[2]),aes(colour = "Prob. de Falha"))}
  
  
  plota<- plota + scale_x_continuous(name = "Dias",
                                     limits=c(0, max(dados))) +
    scale_y_continuous(name = "Probabilidade") +
    ggtitle("") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "legenda")
  
  
  #Printando graficos
  plotd<-denscomp(aux[2], legendtext = aux[1], plotstyle = "ggplot",xlab = unidade, main= " Aprox. Func densidade")
  plotc<-cdfcomp(aux[2], legendtext = "Prob falha" ,plotstyle = "ggplot",xlab = unidade,do.points = FALSE,ylab = "", main = "Aprox. Dist. cumulativa")
  
  ''#printando resultado do fit dist
  print(aux[[2]])
  
  #instaciando o plot grid
  p<-plot_grid(plotd,plotc,plota,ncol = 2, nrow =2  )
  #grid.arrange(plotc,plotd,tabl)
  
  #adicionando titulo na imagem
  title <- ggdraw() + draw_label(Equi, fontface='bold')
  plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
  
  
  #
}
