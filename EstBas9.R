
################################################################################
##################          PACOTES UTILIZADOS          ########################
################################################################################

install.packages("akima")
install.packages("bootstrap")
install.packages("colorRamps")
install.packages("diagram")
install.packages("ggExtra")
install.packages("colorRamps")
install.packages("gmodels")
install.packages("mvtnorm")
install.packages("scatterplot3d")
install.packages("tables")

library(akima)
library(boot)
library(bootstrap)
library(colorRamps)
library(diagram)
library(MASS)
library(ggExtra)
library(ggplot2)
library(gmodels)
library(mvtnorm)
library(reshape2)
library(scales)
library(scatterplot3d)
library(stats)
library(stats4)
library(tables)

################################################################################
###########################     FUNCOES     ####################################
################################################################################
varp<-function(x, pop=TRUE, na.rm=TRUE,...){
  if (pop==TRUE & na.rm==TRUE){ # calcula var_n desconsiderando os missings
    n<-sum(!is.na(x))
    return(var(x,na.rm=na.rm,...)*(n-1)/n)
  }
  else{
    if(na.rm==FALSE){
      n<-length(x)
      return(var(x,na.rm=na.rm,...)*(n-1)/n)
    }
  }
  return(var(x,na.rm=na.rm,...))
}

dm<-function(x,...){
  return(mean(abs(x[!is.na(x)]-mean(x,...))))
}

summary2<-function(x,limtrim=c(.01,.99), pop=FALSE, na.rm=TRUE, ...){
  # x : argumento numerico para o qual calcularemos as estat?sticas descritivas
  # limtrim: Limites para a m?dia truncada
  # pop: se esta vari?vel ? TRUE, entao utiliza-se 'n' como denominador da estimativa da vari?ncia. caso contr?rio, 'n-1'.
  # na.rm: a??o para os outliers
  
  newsumm<-matrix(NA,nrow=dim(as.matrix(x))[2],ncol=11)
  x<-as.matrix(x)
  newsumm[,1]<-apply(x,2,function(...){return(dim(x)[1])})
  newsumm[,2:7]<-t(apply(as.data.frame(x),2,summary))
  newsumm[,8]<-apply(x,2,function(...){mean(x[x>quantile(x,probs=0.01) & 
                                                x<quantile(x,probs=0.99)])})
  if (pop==TRUE & na.rm==TRUE){ # calcula var_n desconsiderando os missings
    n<-sum(!is.na(x))
    newsumm[,9]<-var(x,na.rm=na.rm)*(n-1)/n
  }
  else
  {
    if(pop==TRUE & na.rm==FALSE ){
      n<-length(x)
      newsumm[,9]<-var(x)*(n-1)/n
    }
    else {
      if(pop==FALSE & na.rm==TRUE){
        n<-sum(!is.na(x))
        newsumm[,9]<-var(x,na.rm=TRUE)
      }
      else
      {
        n<-length(x)
        newsumm[,9]<-var(x)
      }
    }
  }
  newsumm[,1]<-n
  newsumm[,10]<-sqrt(newsumm[,9])
  newsumm[,11]<-sqrt(newsumm[,10]/newsumm[,1])
  colnames(newsumm)<-c("N", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","Tr Mean","Var","StDev","SE Mean")
  return(t(newsumm))
}

scdf<-function(x){
  f<-t<-table(x)
  st<-sum(t[1:length(x)])
  for(i in 1:length(x))
    f[i]<-sum(t[1:i])/st - (t[i]/st)/2
  return(f)
}

scatter.marginal <- function(x, y, type="histogram",breaks=NA,col="darkred",pch=20,method="jitter",...){
  #  method="jitter" define o tipo de dispers?o que teremos na marginal quando type for "dispersion".
  #         Outras op??es s?o "stack", para empilhar os pontos e "overplot", para sobrep?-los.
  
  if(type=="histogram"){
    m=matrix(c(2,0,1,3), ncol=2, byrow=TRUE) # define as regi?es a serem utilizadas no gr?fico.
    layout(mat=m, widths=c(0.82,0.18), heights=c(0.18,0.82)) 
    if(is.na(breaks)){
      xhist = hist(x, plot=FALSE,breaks=min(max(5,floor(length(x)/7)),10)) # marginal x
      yhist = hist(y, plot=FALSE,breaks=min(max(5,floor(length(x)/7)),10)) # marginal y
    }
    else{
      xhist = hist(x, plot=FALSE,breaks=breaks) # marginal x
      yhist = hist(y, plot=FALSE,breaks=breaks) # marginal y
    }
    top = max(c(xhist$counts, yhist$counts)) # maximo entre os dois eixos
    par(mar=c(5,5,1,1)) 
    plot(x,y,pch=pch,col=col,...) # Grafico principal
    par(mar=c(0,5,1,1))
    barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, col="lightgray",border="white") # imprime o grafico da marginal x
    par(mar=c(5,0,1,1))
    barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE,  col="lightgray",border="white") # imprime o grafico da marginal y
  }
  
  if(type=="boxplot"){
    m=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
    layout(mat=m, widths=c(0.8,0.2), heights=c(0.18,0.82))
    par(mar=c(5,5,1,1))
    plot(x,y,pch=pch,col=col,...) # Grafico principal
    par(mar=c(0,5,1,1))
    boxplot(x, axes=FALSE, col="lightgray",pch=20, horizontal=TRUE) # marginal x
    par(mar=c(5,0,1,1))
    boxplot(y, axes=FALSE, col="lightgray",pch=20) # marginal y
  }
  
  if(type=="dispersion"){
    m=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
    layout(mat=m, widths=c(0.85,0.15), heights=c(0.18,0.82))
    par(mar=c(5,5,1,1))
    plot(x,y,pch=pch,col=col,...) # gr?fico principal
    par(mar=c(0,5,1,1))
    stripchart(x, method = method, pch = 20, col="gray",vertical = FALSE,yaxt="n",xaxt="n") # marginal x
    par(mar=c(5,0,1,1))
    stripchart(y, method = method, pch = 20, col="gray",vertical = TRUE,yaxt="n",xaxt="n") # marginal y
  }
}

moda<-function(x,breaks,...){
  h<-hist(x,breaks=breaks,plot=F,...)
  h$mids[h$counts==max(h$counts)]
}

moda2<-function(x){return(names(sort(-table(as.vector(x))))[1])}

fdamvnorm<-function(upper,lower=c(-Inf,-Inf),mu=rep(0,times=length(upper)),Sigma=diag(length(upper))){
  return(pmvnorm(lower=c(-Inf,-Inf),upper=upper,mean=mu,sigma=Sigma)[[1]]) # Calcula a integral dupla da fdp da normal multivariada entre no retangulo `lower X upper`.
}

rbernoulli<-function(n=1,p=0.5){
  u<-runif(n,0,1) # simula n uniformes
  x<-1*(u<=p) #categoriza as uniformes simuladas de acordo com o valor de p desejado
  return(x) 
}

MV <- function(rate) {
  R = dnorm(x_sim, rate)
  return(-sum(log(R))) # devolvendo '-log(verossimilhanca)'
}

IC<-function(n, media, desvpad, gama){
  left_tail<-(1-gama)/2
  gamma_adj<-left_tail+gama # ajuste para somar a cauda inferior da distribui??o somente para encontrar o quantil gama usando a fun??o dist. acumulada
  llim<-media-qnorm(gamma_adj)*desvpad/sqrt(n) # calcula o limite inferior
  ulim<-media+qnorm(gamma_adj)*desvpad/sqrt(n) # calcula o limite inferior
  writeLines(paste("IC(",gama,"):[",round(llim,3),";",round(ulim,3),"]\n",sep=""))
  return(ic_calc=c(inf.lim=llim,upper.lim=ulim))
}

err_pad<- function(x){sqrt(var(x)/length(x))}

RC<-function(tipo="bilateral",x_min=NA,x_max=NA, media, desvpad, alpha, grafico=TRUE){
  # 'x_min' e 'x_max' s?o os valores maximo e m?nimo do grafico
  # 'tipo' ? o tipo de teste. Pode ser 'bilateral', 'superior' ou 'inferior'
  # 'media' e 'desvpad' s?o a m?dia e desvio padrao sob H0
  # alpha ? o n?vel de significancia
  # Se grafico == TRUE plota o grafico, caso contrario apenas retorna o valor de Xc.
  if (grafico == TRUE){
    x<-seq(x_min,x_max,0.1)
    
    if (tipo == "bilateral"){
      regiao1=seq(x_min,qnorm(p=alpha/2, mean=media, sd=desvpad),0.001)
      regiao2=seq(qnorm(p=1-alpha/2, mean=media, sd=desvpad),x_max,0.001)
      cord.x1 <- c(min(regiao1),regiao1,max(regiao1))
      cord.x2 <- c(min(regiao2),regiao2,max(regiao2))
      cord.y1 <- c(0,dnorm(regiao1,mean=media,sd=desvpad),0) 
      cord.y2 <- c(0,dnorm(regiao2,mean=media,sd=desvpad),0) 
      curve(dnorm(x,mean=media,sd=desvpad),xlim=c(x_min,x_max),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
      polygon(cord.x1,cord.y1,col='lightblue3')
      polygon(cord.x2,cord.y2,col='lightblue3')
      lines(x=c(media,media),y=c(0,dnorm(media,media,desvpad)),col="darkgray",lty=2)
      axis(side=1,at = c(qnorm(p=1-alpha/2, mean=media, sd=desvpad), media, qnorm(p=alpha/2, mean=media, sd=desvpad)), 
           labels = round(c(qnorm(p=1-alpha/2, mean=media, sd=desvpad), media, qnorm(p=alpha/2, mean=media, sd=desvpad)),3))
      writeLines(paste("RC:(-infinito; ",round(qnorm(p=alpha/2, mean=media, sd=desvpad),3),"] + [",round(qnorm(p=1-alpha/2, mean=media, sd=desvpad),3),"; +infinito)",sep=""))
      return(c(qnorm(p=alpha/2, mean=media, sd=desvpad),qnorm(p=1-alpha/2, mean=media, sd=desvpad)))
    }
    else if (tipo == "superior"){
      regiao2=seq(qnorm(p=1-alpha, mean=media, sd=desvpad),x_max,0.001)
      cord.x2 <- c(min(regiao2),regiao2,max(regiao2))
      cord.y2 <- c(0,dnorm(regiao2,mean=media,sd=desvpad),0) 
      curve(dnorm(x,mean=media,sd=desvpad),xlim=c(x_min,x_max),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
      polygon(cord.x2,cord.y2,col='lightblue3')
      lines(x=c(media,media),y=c(0,dnorm(media,media,desvpad)),col="darkgray",lty=2)
      axis(side=1,at = c(qnorm(p=1-alpha, mean=media, sd=desvpad),media), 
           labels = round(c(qnorm(p=1-alpha, mean=media, sd=desvpad),media),3))
      writeLines(paste("RC:[",round(qnorm(p=1-alpha, mean=media, sd=desvpad),3),"; +infinito)",sep=""))
      return(qnorm(p=1-alpha, mean=media, sd=desvpad))
    }
    else if (tipo == "inferior"){
      regiao1=seq(x_min,qnorm(p=alpha, mean=media, sd=desvpad),0.001)
      cord.x1 <- c(min(regiao1),regiao1,max(regiao1))
      cord.y1 <- c(0,dnorm(regiao1,mean=media,sd=desvpad),0) 
      curve(dnorm(x,mean=media,sd=desvpad),xlim=c(x_min,x_max),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
      polygon(cord.x1,cord.y1,col='lightblue3')
      lines(x=c(media,media),y=c(0,dnorm(media,media,desvpad)),col="darkgray",lty=2)
      axis(side=1,at = c(media,qnorm(p=alpha, mean=media, sd=desvpad)), 
           labels = round(c(media,qnorm(p=alpha, mean=media, sd=desvpad)),3))
      writeLines(paste("RC:(-infinito; ",round(qnorm(p=alpha, mean=media, sd=desvpad),3),"]",sep=""))
      return(qnorm(p=alpha, mean=media, sd=desvpad))
    }
    else {
      writeLines("'tipo' deve ser 'bilateral', 'superior' ou 'inferior'.")
      return(0)
    }
  }
  else{
    if (tipo == "bilateral"){
      writeLines(paste("RC:(-infinito; ",round(qnorm(p=alpha/2, mean=media, sd=desvpad),3),"] + [",round(qnorm(p=1-alpha/2, mean=media, sd=desvpad),3),"; +infinito)",sep=""))
      return(c(qnorm(p=alpha/2, mean=media, sd=desvpad),qnorm(p=1-alpha/2, mean=media, sd=desvpad)))
    }
    else if (tipo == "superior"){
      writeLines(paste("RC:[",round(qnorm(p=1-alpha, mean=media, sd=desvpad),3),"; +infinito)",sep=""))
      return(qnorm(p=1-alpha, mean=media, sd=desvpad))
    }
    else if (tipo == "inferior"){
      writeLines(paste("RC:(-infinito; ",round(qnorm(p=alpha, mean=media, sd=desvpad),3),"]",sep=""))
      return(qnorm(p=alpha, mean=media, sd=desvpad))
    }
    else {
      writeLines("'tipo' deve ser 'bilateral', 'superior' ou 'inferior'.")
      return(0)
    }
  }
}

RC_<-function(tipo="bilateral",x_min=NA,x_max=NA, media, desvpad, alpha,media_){
  x<-seq(x_min,x_max,0.1)
  if (tipo == "bilateral"){
    regiao1=seq(x_min,qnorm(p=alpha/2, mean=media_, sd=desvpad),0.001)
    regiao2=seq(qnorm(p=1-alpha/2, mean=media_, sd=desvpad),x_max,0.001)
    cord.x1 <- c(min(regiao1),regiao1,max(regiao1))
    cord.x2 <- c(min(regiao2),regiao2,max(regiao2))
    cord.y1 <- c(0,dnorm(regiao1,mean=media,sd=desvpad),0) 
    cord.y2 <- c(0,dnorm(regiao2,mean=media,sd=desvpad),0) 
    curve(dnorm(x,mean=media,sd=desvpad),xlim=c(x_min,x_max),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
    polygon(cord.x1,cord.y1,col='lightblue3')
    polygon(cord.x2,cord.y2,col='lightblue3')
    lines(x=c(media,media),y=c(0,dnorm(media,media,desvpad)),col="darkgray",lty=2)
    axis(side=1,at = c(qnorm(p=1-alpha/2, mean=media_, sd=desvpad), media_, qnorm(p=alpha/2, mean=media_, sd=desvpad)), 
         labels = round(c(qnorm(p=1-alpha/2, mean=media_, sd=desvpad), media_, qnorm(p=alpha/2, mean=media_, sd=desvpad)),3))
  }
  else if (tipo == "superior"){
    regiao2=seq(qnorm(p=1-alpha, mean=media_, sd=desvpad),x_max,0.001)
    cord.x2 <- c(min(regiao2),regiao2,max(regiao2))
    cord.y2 <- c(0,dnorm(regiao2,mean=media,sd=desvpad),0) 
    curve(dnorm(x,mean=media,sd=desvpad),xlim=c(x_min,x_max),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
    polygon(cord.x2,cord.y2,col='lightblue3')
    lines(x=c(media,media),y=c(0,dnorm(media,media,desvpad)),col="darkgray",lty=2)
    axis(side=1,at = c(qnorm(p=1-alpha, mean=media_, sd=desvpad),media), 
         labels = round(c(qnorm(p=1-alpha, mean=media_, sd=desvpad),media),3))
  }
  else if (tipo == "inferior"){
    regiao1=seq(x_min,qnorm(p=alpha, mean=media_, sd=desvpad),0.001)
    cord.x1 <- c(min(regiao1),regiao1,max(regiao1))
    cord.y1 <- c(0,dnorm(regiao1,mean=media,sd=desvpad),0) 
    curve(dnorm(x,mean=media,sd=desvpad),xlim=c(x_min,x_max),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
    polygon(cord.x1,cord.y1,col='lightblue3')
    lines(x=c(media,media),y=c(0,dnorm(media,media,desvpad)),col="darkgray",lty=2)
    axis(side=1,at = c(media,qnorm(p=alpha, mean=media_, sd=desvpad)), 
         labels = round(c(media,qnorm(p=alpha, mean=media_, sd=desvpad)),3))
  }
}

teste_var<- function(s1,s2,n1,n2,alpha){
  #s1: Variancia amostral da primeira amostra
  #s2: Variancia amostral da primeira amostra
  #n1: tamanho da primeira amostra
  #n2: tamanho da seunda amostra
  #alpha: Nivel de signific?ncia
  
  f_c<-qf(p = alpha/2,n1,n2) # Calculando o quantil da distribruicao F
  print("Teste para igualdade de vari?ncias")
  print("H0:Sigma_A/Sigma_B=1")
  if(f_c*s1/s2>1 || 1/f_c*s1/s2<1)
    print("Rejeito H0: Vari?ncias iguais")
  else print("Aceito H0: Vari?ncias iguais")
  print(paste("Intervalo de aceita??o: (",round(f_c*s1/s2,5)," ; ",round(1/f_c*s1/s2,5),")",sep=""))
}

teste_t<-function(x_barra1,x_barra2,s1,s2,n1,n2,alpha=0.05,H_1="A!=B",var_iguais=TRUE){
  
  #x_barra1,x_barra2 : m?dias amostrais das amostras 1 e 2 respectivamente
  #s1,s2             : variancias amostrais das amostras 1 e 2 respectivamente
  #n1,n2             : Tamanhos das amostras 1 e 2 respectivamente
  #alpha             : Nivel de Signific?ncia
  #H_1               : Hip?tese Alternativa
  #var_iguais        : Se TRUE calcula as estatisticas supondo vari?ncias iguais e desconhecidas.
  #                    Caso contr?rio, sup?e vari?ncias desiguais e desconhecidas
  
  if(var_iguais==TRUE){ # Caso com vari?ncias iguais e desconhecidas
    
    s_p=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
    t_obs=(x_barra2-x_barra1)/sqrt(s_p*(1/n1+1/n2)) # Calculando estat?stica do teste
    print("Teste para igualdade de m?dias: Vari?ncias iguais e desconhecidas")
    
    if (H_1=="A!=B"){
      t_c=qt(p = 1-alpha/2, df = n1 + n2 - 2) # Calculando o valor para a RC
      if(t_obs < -t_c || t_obs > t_c) # Comparando a estatistica do teste com a RC
        print("Rejeito H0: Mu_A = Mu_B")
      else 
        print("Aceito H0: Mu_A = Mu_B")
      print(paste("Intervalo de aceita??o: (",round(-t_c,4)," ;",
                  round(t_c,4),")",sep=""))
    }
    else if (H_1=="A<B"){
      t_c=qt(p = 1-alpha, df = n1 + n2 - 2) # Calculando o valor para a RC
      if(t_obs > t_c) # Comparando a estatistica do teste com a RC
        print("Rejeito H0: Mu_A = Mu_B")
      else 
        print("Aceito H0: Mu_A = Mu_B")
      print(paste("Intervalo de aceita??o: (-infinito ;", round(t_c,4),")",sep=""))
    }
    else if (H_1=="A>B"){
      t_c=qt(p = alpha, df = n1 + n2 - 2) # Calculando o valor para a RC
      if(t_obs < t_c) # Comparando a estatistica do teste com a RC
        print("Rejeito H0: Mu_A = Mu_B")
      else 
        print("Aceito H0: Mu_A = Mu_B")
      print(paste("Intervalo de aceita??o: (", round(t_c,4),";+infinito)",sep=""))
    }
    else {
      print("'H_1'deve ser 'A!=B', 'A>B' ou 'A<B'")
      return()
    }
    print(paste("Estat?stica do teste:" , round(t_obs,4)))
  }
  else{ # Caso com vari?ncias desiguais e desconhecidas
    t_obs=(x_barra2-x_barra1)/sqrt(s1/n1+s2/n2) # Calculando estat?stica do teste
    gl=round((s1/n1+s2/n2)^2/((s1/n1)^2/(n1-1)+(s2/n2)^2/(n2-1)),0)
    print("Teste para igualdade de m?dias: Vari?ncias desiguais e desconhecidas")
    
    if (H_1=="A!=B"){
      t_c=qt(p = 1-alpha/2, df = gl) # Calculando o valor para a RC
      if(t_obs < -t_c || t_obs > t_c) # Comparando a estatistica do teste com a RC
        print("Rejeito H0: Mu_A = Mu_B")
      else 
        print("Aceito H0: Mu_A = Mu_B")
      print(paste("Intervalo de aceita??o: (",round(-t_c,4)," ;",
                  round(t_c,4),")",sep=""))
    }
    else if (H_1=="A<B"){
      t_c=qt(p = 1-alpha, df = gl) # Calculando o valor para a RC
      if(t_obs > t_c) # Comparando a estatistica do teste com a RC
        print("Rejeito H0: Mu_A = Mu_B")
      else 
        print("Aceito H0: Mu_A = Mu_B")
      print(paste("Intervalo de aceita??o: (-infinito ;", round(t_c,4),")",sep=""))
    }
    else if (H_1=="A>B"){
      t_c=qt(p = alpha, df = gl) # Calculando o valor para a RC
      if(t_obs < t_c) # Comparando a estatistica do teste com a RC
        print("Rejeito H0: Mu_A = Mu_B")
      else 
        print("Aceito H0: Mu_A = Mu_B")
      print(paste("Intervalo de aceita??o: (", round(t_c,4),";+infinito)",sep=""))
    }
    else {
      print("'H_1'deve ser 'A!=B', 'A>B' ou 'A<B'")
      return()
    }
    print(paste("Estat?stica do teste:" , round(t_obs,4)))
  }
}

IC_diff_medias<-function(x_barra1,x_barra2,s1,s2,n1,n2,gama=0.95){
  s_p=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
  t_c_ic=-qt(p = (1-gama)/2, df = n1 + n2 - 2) # calculando t com a confianca desejada
  t_obs=(x_barra2-x_barra1)/sqrt(s_p*(1/n1+1/n2)) # Calculando estat?stica do teste
  icl=(x_barra2-x_barra1)-t_c_ic*sqrt(s_p*(1/n1+1/n2)) # limite inferior do intervalo
  icu=(x_barra2-x_barra1)+t_c_ic*sqrt(s_p*(1/n1+1/n2))# limite superior do intervalo
  print(paste("IC(",gama,"):]",round(icl,4),";",round(icu,4),"[",sep=""))
}

p.test<-function(p1,p2,n1,n2,alpha=0.05, H1="p1!=p2"){
  p_c=(n1*p1+n2*p2)/(n1+n2)
  z_obs=(p1-p2)/sqrt(p_c*(1-p_c)*(1/n1+1/n2))
  print("Teste de Compara??o de Propor??es")
  if(H1=="p1!=p2"){
    z_c=-qnorm(alpha/2)
    if(z_obs< -z_c || z_obs>z_c){
      print("Rejeito H_0!")
      print(paste("Regi?o de Aceita??o: ]",round(-z_c,4),";",round(z_c,4),"[",sep=""))
    }
  }
  else if(H1=="p1>p2"){
    z_c=qnorm(1-alpha)
    if(z_obs>z_c){
      print("Rejeito H_0!")
      print(paste("Regi?o de Aceita??o: ]-infinito;",round(z_c,4),"[",sep=""))
    }
  }
  else if(H1=="p1<p2"){
    z_c=qnorm(alpha)
    if(z_obs<z_c){
      print("Rejeito H_0!")
      print(paste("Regi?o de Aceita??o: ]",round(z_c,4),";+infinito[",sep=""))
    }
  }else {
    print("Erro: H1 deve ser 'p1!=p2', 'p1>p2' ou 'p1<p2' ")
    return()
  }
  print(paste("Z observado:",round(z_obs,4)))
}

IC_proporcoes<-function(p1,p2,n1,n2,gama=0.95){
  z_c=-qnorm((1-gama)/2)
  icl=(p1-p2)-z_c*sqrt((p1*(1-p1))/n1+(p2*(1-p2))/n2)
  icu=(p1-p2)+z_c*sqrt((p1*(1-p1))/n1+(p2*(1-p2))/n2)
  print(paste("IC(",gama,"):]",round(icl,4),";",round(icu,4),"[",sep=""))
}

teste_quiquad<-function(f_obs,f_esp,alpha,gl){
  q_obs=sum((f_obs-f_esp)^2/f_esp)
  q_c=qchisq(1-alpha,df=gl)
  p_val=pchisq(q_obs,df=gl,lower.tail = FALSE)
  writeLines("                  *** Teste de Qui-quadrado ***  ")
  writeLines(paste(" Estat?stica observada: ",round(q_obs,2),"  ",sep=""))
  writeLines(paste("    Graus de liberdade: ", gl,"  ",sep=""))
  writeLines(paste("         Valor cr?tico: ",round(q_c,2),"  ",sep=""))
  writeLines(paste("               Valor-p: ",round(p_val,4),"  ",sep=""))
  writeLines(paste("N?vel de signific?ncia: ",alpha,"  ",sep=""))
  if(q_obs>q_c){
    writeLines("        **Rejeito H0**")
  } else writeLines("         **Aceito H0**")
}

IC_t<-function(x_barra1,x_barra2,s1,s2,n1,n2,gama=0.95){
  left_tail<-(1-gama)/2
  gamma_adj<-left_tail+gama # ajuste para somar a cauda inferior da distribui??o somente para encontrar o quantil gama usando a fun??o dist. acumulada
  Se=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
  llim1<-x_barra1-qt(gamma_adj,df = (n1+n2-2))*sqrt(Se)/sqrt(n1) # calcula o limite inferior
  ulim1<-x_barra1+qt(gamma_adj,df = (n1+n2-2))*sqrt(Se)/sqrt(n1) # calcula o limite inferior
  llim2<-x_barra2-qt(gamma_adj,df = (n1+n2-2))*sqrt(Se)/sqrt(n2) # calcula o limite inferior
  ulim2<-x_barra2+qt(gamma_adj,df = (n1+n2-2))*sqrt(Se)/sqrt(n2) # calcula o limite inferior
  
  #writeLines(paste("IC_1(",gama,"):[",round(llim1,3),";",round(ulim1,3),"]\n",sep=""))
  #writeLines(paste("IC_2(",gama,"):[",round(llim2,3),";",round(ulim2,3),"]\n",sep=""))
  return(list(ic_calc1=c(inf.lim=llim1,upper.lim=ulim1),ic_calc2=c(inf.lim=llim2,upper.lim=ulim2)))
}

IC_t_diff<-function(x_barra1,x_barra2,SQ,n,n1,n2,gama=0.95){
  left_tail<-(1-gama)/2
  gamma_adj<-left_tail+gama # ajuste para somar a cauda inferior da distribui??o somente para encontrar o quantil gama usando a fun??o dist. acumulada
  Se=SQ/(n-2)
  llim1<-(x_barra1-x_barra2)-qt(gamma_adj,df = (n-2))*sqrt(Se)*sqrt(1/n1+1/n2) # calcula o limite inferior
  ulim1<-(x_barra1-x_barra2)+qt(gamma_adj,df = (n-2))*sqrt(Se)*sqrt(1/n1+1/n2) # calcula o limite inferior
  #writeLines(paste("t_c:",qt(gamma_adj,df = (n-2))))
  #writeLines(paste("IC_1(",gama,"):[",round(llim1,3),";",round(ulim1,3),"]\n",sep=""))
  #writeLines(paste("IC_2(",gama,"):[",round(llim2,3),";",round(ulim2,3),"]\n",sep=""))
  return(ic_calc=c(inf.lim=llim1,upper.lim=ulim1))
}

reg_resist<-function(tab,plot=TRUE){
  # tab ? um data.frame contendo as colunas: x e y.
  
  tab<-tab[order(tab$x,tab$y),]
  g_E<-tab[1:ceiling(length(tab$x)/3),]
  g_C<-tab[(ceiling(length(tab$x)/3)+1):(floor(length(tab$x)/3*2)),]
  g_D<-tab[(floor(length(tab$x)/3*2)+1):(length(tab$x)),]
  
  md_y_E<-median(g_E[,2])
  md_x_E<-median(g_E[,1])
  
  md_y_C<-median(g_C[,2])
  md_x_C<-median(g_C[,1])
  
  md_y_D<-median(g_D[,2])
  md_x_D<-median(g_D[,1])
  
  tab_resist=data.frame(x=c(md_x_E,md_x_C,md_x_D),y=c(md_y_E,md_y_C,md_y_D))
  fit<-lm(tab_resist$y~tab_resist$x)
  
  if (plot == TRUE){
    plot(tab$x,tab$y, pch=20, xlab="x", ylab="y", col="darkblue")
    abline(lm(tab$y~tab$x), lwd=2, col="red")
    abline(lm(tab_resist$y~tab_resist$x), lwd=2, col="blue", lty=2)
    legend("topleft", legend =c("Min.Quad","Resistente") ,lty=c(1,2),col=c("red","blue"),lwd=c(2,2))
  }
  return(fit)
}

################################################################################
##################          IMPORTACAO DOS DADOS        ########################
################################################################################

cd_poluicao<-read.table("cd-poluicao.csv",header=TRUE,skip=8,sep=";",dec=",")
cd_brasil<-read.table("cd-brasil.csv",header=TRUE,skip=7,sep=";")
cd_temperaturas<-read.table("cd-temperaturas.csv",header=TRUE,skip=4,sep=";",dec=",") # Leitura dos dados
cd_notas<-read.table("cd-notas.csv",header=TRUE,skip=4,sep=";",dec=",") # Leitura dos dados
cd_mercado <- read.table("cd-mercado.csv",header=TRUE,skip=4,sep=";",dec=",") # Leitura dos dados
cd_veiculos <- read.table("cd-veiculos.csv",header=TRUE,skip=4,sep=";",dec=",") # Leitura dos dados
cd_municipios<-read.table("cd-municipios.csv",header=TRUE,skip=4,sep=";",dec=",")
tab2_1<-read.table("tabela2_1.csv", dec=",", sep=";",header=TRUE)
attach(tab2_1)
dureza<-c(53,70.2,84.3,69.5,77.8,87.5,53.4,82.5,67.3,54.1,
          70.5,71.4,95.4,51.1,74.4,55.7,63.5,85.8,53.5,64.3,
          82.7,78.5,55.7,69.1,72.3,59.5,55.3,73  ,52.4,50.7)
munic<-cd_municipios[order(cd_municipios$populacao,decreasing = TRUE),]
dados.tab4_7<-data.frame(rbind(
  matrix(rep(c("1.Masculino","1.F?sica"),times=100),ncol=2,byrow=T),
  matrix(rep(c("2.Feminino","1.F?sica"),times=20),ncol=2,byrow=T),
  matrix(rep(c("1.Masculino","2.Ci?ncias Sociais"),times=40),ncol=2,byrow=T),
  matrix(rep(c("2.Feminino","2.Ci?ncias Sociais"),times=40),ncol=2,byrow=T)))
colnames(dados.tab4_7)<-c("sexo","curso")
dados.tab4_8<-data.frame(rbind(
  matrix(rep(c("1.Consumidor","1.S?o Paulo"),times=214),ncol=2,byrow=T),
  matrix(rep(c("1.Consumidor","2.Paran?"),times=51),ncol=2,byrow=T),
  matrix(rep(c("1.Consumidor","3.Rio G. do Sul"),times=111),ncol=2,byrow=T),
  matrix(rep(c("2.Produtor","1.S?o Paulo"),times=237),ncol=2,byrow=T),
  matrix(rep(c("2.Produtor","2.Paran?"),times=102),ncol=2,byrow=T),
  matrix(rep(c("2.Produtor","3.Rio G. do Sul"),times=304),ncol=2,byrow=T),
  matrix(rep(c("3.Escola","1.S?o Paulo"),times=78),ncol=2,byrow=T),
  matrix(rep(c("3.Escola","2.Paran?"),times=126),ncol=2,byrow=T),
  matrix(rep(c("3.Escola","3.Rio G. do Sul"),times=139),ncol=2,byrow=T),
  matrix(rep(c("4.Outras","1.S?o Paulo"),times=119),ncol=2,byrow=T),
  matrix(rep(c("4.Outras","2.Paran?"),times=22),ncol=2,byrow=T),
  matrix(rep(c("4.Outras","3.Rio G. do Sul"),times=48),ncol=2,byrow=T)))
colnames(dados.tab4_8)<-c("tipo_de_cooperativa","estado")
dados.ex4_4<-data.frame(agente=c("A","B","C","D","E","F","G","H","I","J"),
                       anos_servico=c(2,3,4,5,4,6,7,8,8,10),
                       n_clientes=c(48,50,56,52,43,60,62,58,64,72))
dados.ex4_6<-data.frame(
  familia=c("A","B","C","D","E","F","G","H","I","J"),
  renda_bruta=c(12,16,18,20,28,30,40,48,50,54),
  gasto_saude=c(7.2,7.4,7,6.5,6.6,6.7,6,5.6,6,5.5))
dados.tab4_14<-data.frame(
  individuo=c("A","B","C","D","E","F","G","H"),
  resultado=c(45,52,61,70,74,76,80,90),
  tempo=c(343,368,355,334,337,381,345,375))

tab4_18<-data.frame(aluno=1:20,prova1=c(8.5,3.5,7.2,5.5,9.5,7,4.8,
                                        6.6,2.5,7,7.4,5.6,6.3,3,8.1,
                                        3.8,6.8,10,4.5,5.9),
                    prova2=c(8,2.8,6.5,6.2,9,7.5,5.2,7.2,4,6.8,6.5,
                             5,6.5,3,9,4,5.5,10,5.5,5))

x<-0:10
n_k<- c(57,203,383,525,532,408,273,139,45,27,16)
np_k<-c(54.399,210.523,407.361,525.496,508.418,393.515,253.817,140.325,67.882,29.189,17.075)
tab6_13<-cbind(x,n_k,np_k)

c_hum<-c(15,20,30,20,15)
c_bio<-c( 8,23,18,34,17)
tab14_2<-rbind(c_hum,c_bio)  
test_tab14_2=chisq.test(tab14_2)

tab14_8=rbind(test_tab14_2$expected, total=apply(test_tab14_2$expected,2,sum))

acidentes<-c(32,40,20,25,33)
test_tab14_5<-chisq.test(acidentes)
q_i<-(acidentes-test_tab14_5$expected)^2/test_tab14_5$expected
tab14_5<- rbind(acidentes, e_i=test_tab14_5$expected, q_i)
tab14_5<-cbind(tab14_5,c(sum(tab14_5[1,]),sum(tab14_5[2,]),sum(tab14_5[3,])))

dados.ex14_6<-c( 1.04,   1.73,   3.93,   4.44,   6.37,   6.51,
                 7.61,   7.64,   8.18,   8.48,   8.57,   8.65,
                 9.71,   9.87,   9.95,  10.01,  10.52,   10.69, 
                 11.72,  12.17,  12.61,  12.98,  13.03,  13.16,
                 14.11,  14.60,  14.64,  14.75,  16.68,  22.14)

tab14_10<-rbind(P_1T=c(29,60,9,2),P_2C=c(37,44,13,6))

anos_exp<-c(2,4,5,6,8)
n_clientes<-c(48,56,64,60,72)

tab15_1<-data.frame(
  tempo_Y=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117),
  sexo_W=factor(c("H","M","H","M","M","H","H","M","M","H","H","M","M","M","H","H","M","M","H","H")),
  idade_X=factor(c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)),
  acuidade_Z=c(90,100,80,90,100,90,80,90,70,90,90,80,90,80,70,90,90,90,60,80))
attach(tab15_1)
mean_idade<-rep(0,20)
for(age in levels(idade_X)){
  mean_idade[mean_idade==0]<-((idade_X==age)*mean(tempo_Y[idade_X==age]))[mean_idade==0]
}
mean_sexo<-rep(0,length(sexo_W))
for(sex in levels(sexo_W)){
  mean_sexo[mean_sexo==0]<-((sexo_W==sex)*mean(tempo_Y[sexo_W==sex]))[mean_sexo==0]
}
e1<-tempo_Y-mean(tempo_Y)
e2<-tempo_Y-mean_sexo
e3<-tempo_Y-mean_idade
tab15_4<-cbind(tab15_1[,1:4],e1,e2,e3)

tab16_5<-data.frame(n=1:25,
                    xi=c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,46.4,28.9,28.1,39.1,46.8,48.5,59.3,70,70,74.5,72.1,58.1,44.6,33.4,28.6),
                    yi=c(10.98,11.13,12.51,8.4,9.27,8.73,6.36,8.5,7.82,9.14,8.24,12.19,11.88,9.57,10.94,9.58,10.09,8.11,6.83,8.88,7.68,8.47,8.86,10.36,11.08))

ano=seq(1961,1979,2)
inflacao_Y=c(9,24,72,128,192,277,373,613,1236,2639)

tab16_6<-data.frame(
  ano=ano,
  t=ano-mean(ano),
  inflacao_Y=inflacao_Y,
  log_Y=log(inflacao_Y))

tab16_8<-data.frame(t=1:15, vt=c(22.2,61.1,13,27.8,22.2,7.4,7.4,7.4,20.4,20.4,20.4,11.1,13,7.4,14.8))

tab16_9A<-data.frame(x=c(119,155,174,190,196,233,272,253,276),
                     y=c(112,152,172,183,192,228,263,239,263))

##########################################
####     TERMINO DO SCRIPT   ######
##########################################

?rea de anexos