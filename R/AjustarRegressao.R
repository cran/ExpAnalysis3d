#' Ajuste de modelos de regressao multipla
#'
#' @description
#\cr
#\if{html}{\figure{logo2.png}{options: align='right'  height="1\%" alt="Figure: logo.png"}}
#
#\cr
#' Esta funcao realiza o ajuste de 12 modelos de regressao multipla
#'   considerando 2 variaveis independentes (explicativas) e uma variavel
#'   dependente (resposta). E possivel analisar dados de experimentos avaliados
#'   sem delineamento (repeticoes) e com delineamento estatistico (DIC e DBC)
#'
#'
#' @usage AjustarRegressao(Dados,design,verbose=TRUE)
#' @param Dados   Matriz contendo 3 colunas obrigatoriamente caso o design seja
#'   1 (experimento sem repeticoes), sendo as duas primeiras as variaveis
#'   explicativas e a terceira a variavel resposta. Se houver repeticoes (Design
#'   2 ou 3) a matriz deve conter obrigatoriamente 4 colunas, as duas primeiras
#'   com as variaveis explicativas, a terceira com a identificacao das
#'   repeticoes/blocos e a quarta coluna com a variavel resposta.
#' @param design Indica o delineamento utilizado na pesquisa:
#' \itemize{
#' \item design 1 -> Experimento  sem repeticao.
#' \item design 2 -> Experimento no delineamento inteiramente casualizado (Dic).
#' \item design 3 -> Experimento no delineamento em blocos casualizados (Dbc).
#'   }
#' @param verbose Valor logico (TRUE ou FALSE) indicando se os resultados devem
#' ou nao serem apresentados no console.
#' @return A funcao retorna o resultado do ajuste de 12 modelos de regressao.
#' Estes resultados podem ser apresentados no console, e alem disso, estao
#' carregados em um objeto do tipo list.
#  \cr
# \if{html}{\figure{Table.jpg}{options: align='left'  height="1\%" alt="Figure: Table.jpg"}}
#  \cr
#' @seealso \code{\link{plot2D}}, \code{\link{plot3D}}, \code{\link{predict3D}}
#' @references Tutoriais onlines:
#' https://www.youtube.com/playlist?list=PLvth1ZcREyK6OUnWVs-hnyVdCB1xuxbVs
#' @examples
# \donttest{
#' #Exemplo 1: Experimento sem delineamento
#' data("Dados1")
#' res=AjustarRegressao(Dados = Dados1, design=1,verbose=FALSE)
#' plot2D(res,niveis = 3)
#' plot2D(res,niveis = 3,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'        Metodo = "simple")
#' plot2D(res,niveis = 5,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'        Metodo="edge",col.contour = "blue")
#' plot3D(res)
#'
#'##########################
#'#Criando paleta de cores
#'col0 = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))
#'col1 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
#'                          'cyan', '#007FFF', 'blue','#00007F'))
#'col2 = colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
#'                          '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
#'                          '#4393C3', '#2166AC', '#053061'))
#'col3 = colorRampPalette(c('red', 'white', 'blue'))
#'col4 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
#'                          'cyan', '#007FFF', 'blue', '#00007F'))
#'
#'plot2D(res,niveis = 5,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'       Metodo="edge",contour = TRUE,cor=col0(200),box=FALSE)
#'
#'
#'plot2D(res,niveis = 10,xlab="Comprimento (cm)",ylab="Largura (cm)",zlab=FALSE,
#'       contour = TRUE,cor=col1(200),box=TRUE,col.contour = "black",
#'       main="Superficie Resposta")
#'
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 2: Experimento sem delineamento
#'data("Dados2")
#'res=AjustarRegressao(Dados = Dados2, design=1,verbose=TRUE)
#'plot2D(res,niveis = 10,xlab="Acucar (%)",ylab="Banana (%)",
#'zlab="Aceitabilidade",
#'       contour = TRUE,cor=col1(200),box=TRUE,col.contour = "black",
#'       main="Superficie Resposta")
#' plot3D(res)
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 3: Experimento com delineamento (DIC)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=2,verbose=TRUE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = FALSE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = TRUE,col.contour = "black")
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 4: Experimento com delineamento (DBC)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=3,verbose=TRUE)
#'plot2D(res,niveis = 20,xlab="N (K/ha)",ylab="K (Kg/ha)",
#'       Metodo="edge",contour = TRUE,cor=col1(200),box=TRUE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = TRUE,col.contour = "black")
#' plot3D(res)
#'
# }
#' @importFrom grDevices colorRampPalette hcl.colors
#' @importFrom graphics layout par image
#'@importFrom stats AIC BIC anova aov coefficients lm model.matrix pf predict pt
#' @importFrom crayon green
#' @importFrom magrittr `%>%`
#' @export



AjustarRegressao=function(Dados,design,verbose=TRUE){
  if(verbose==TRUE){
  cat("_____________________________________________________________________","\n")
  cat("Obrigado por utilizar o ExpAnalysis3D","\n")
  cat("Veja tutoriais sobre este e outros pacotes no canal: ","\n")
  cat("https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ","\n")
  cat("Duvidas e sugestoes podem nos ser enviados por essa midia.","\n")
  cat("Se inscreva no canal e compartilhem os videos para ajudar nosso canal a crescer.","\n")
  cat("Alcinei Mistico Azevedo (ICA-UFMG).","\n")
  cat("_____________________________________________________________________","\n")
  cat("","\n")
}
  ########### Medias
  if(design==1){
   res= AjustarRegressaoMED(Dados,verbose)
  }
  ########### DIC
  if(design==2){
   res= AjustarRegressaoDIC(Dados,verbose)
  }
  ############ DBC
  if(design==3){
   res= AjustarRegressaoDBC(Dados,verbose)
  }
  return(res)
}

























################################################################
############################################################
#########################################################
#
AjustarRegressaoMED=function(D,verbose){

  ########################################################################################
  modelos=list(
    m1  =Z~   1 + X  + Y,
    m2  =Z~	 1 + X  + I(X^2)  + Y,
    m3	=Z~	 1 + X  + Y       + I(Y^2),
    m4	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
    m5	=Z~	 1 + X  + Y       + X:Y,
    m6	=Z~	 1 + X  + I(X^2)  + Y  + X:Y,
    m7	=Z~	 1 + X  + Y       + I(Y^2)  + X:Y,
    m8	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y,
    m9	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y,
    m10	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(Y^2):X,
    m11	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X,
    m12	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X + I(X^2):I(Y^2))
  #########################################################################################
  X=D[,1]
  Y=D[,2]
  Z=D[,3]

  npar=c(2,3,3,4,3,4,4,5,6,6,7,8)

  # print("Estudo de regressao multipla")
  Resultado=list()
  Resultado[["Ajuste"]]=""
  Resultado[["Qualidade"]]=""
  Resultado[["MelhorModelo"]]=""
  Resultado[["Design"]]=""
  Resultado[["multicolinearidade"]]=""
  Resultado[["Dados"]]=""
  Ajuste=list()
  for (m in 1:12){
    model=lm(modelos[[m]])
    Anova=rbind(Regressao=apply(anova(model)[1:4][1:npar[m],],2,sum),anova(model)[1:4][npar[m]+1,])
    pValor=round(1-pf(Anova[1,4],Anova[1,1],Anova[2,1]),5)
    pValor[pValor==0]="<0.0001"
    Anova=cbind(Anova,`Pr(>F)`=c(pValor,""))

    res=summary(model)
    Qualidade=c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
    names(Qualidade)=c("R2","R2aj","AIC","BIC")

    Ajuste[[paste0("Modelo",m)]]=list(Model=modelos[[m]],AnovaGlobal=Anova,
                                      AnovaParcial=anova(model),Coeficientes=summary(model),
                                      Qualidade=Qualidade)

  }
  Resultado$Ajuste=Ajuste



  r=function(Z,m,D){
    X=D[,1]
    Y=D[,2]
    model=lm(modelos[[m]])
    res=summary(model)
    c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
  }
  ########################################################################################
  Resumo=matrix(NA,ncol=4,nrow=12)

  for (m in 1:12){
    Z=D[,3]
    Ajust=r(Z,m,D)
    Resumo[m,1]=100*round(Ajust[1],8) #r2
    Resumo[m,2]=100*round(Ajust[2],8) #r2aj
    Resumo[m,3]=round(Ajust[3],6) #AIC
    Resumo[m,4]=round(Ajust[4],6) #BIC
  }

  colnames(Resumo)=c("R2","R2aj","AIC","BIC")
  rownames(Resumo)=paste("modelo",1:12)

  Resultado$Qualidade=Resumo

  Resumo2=Resumo
  Resumo2[,1:2]=100-Resumo[,1:2]

  Resultado$MelhorModelo=apply(Resumo2,2,function(x) order(x)[1])
  Resultado$Design=1
  Resultado$multicolinearidade=rep(FALSE,12)
  Resultado$Dados=D

  if(verbose==TRUE){
    for(m in 1:12){
      cat("#####################################################################","\n")
      cat("#####################################################################","\n")
      cat(paste("Analise do modelo ",m),"\n")
      a=modelos[[m]]
      cat(paste("modelo->",a[2],a[1],a[3]),"\n")
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia global","\n")
      print(Resultado[[1]][[m]]$AnovaGlobal)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia parcial","\n")
      print(Resultado[[1]][[m]]$AnovaParcial)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Coeficientes","\n")
      print(Resultado[[1]][[m]]$Coeficientes)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Avaliadores da qualidade do ajuste","\n")
      print(Resultado[[1]][[m]]$Qualidade)
      cat("","\n")
      cat("Modelo selecionado","\n")
      print(Resultado$MelhorModelo)
      cat("","\n")
      cat("","\n")
    }
  }


  return(Resultado)
}


#################################################################
######################################################
############################################
#
AjustarRegressaoDIC=function(D,verbose){

  ########################################################################################
  modelos=list(
    m1  =Z~   1 + X  + Y,
    m2  =Z~	 1 + X  + I(X^2)  + Y,
    m3	=Z~	 1 + X  + Y       + I(Y^2),
    m4	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
    m5	=Z~	 1 + X  + Y       + X:Y,
    m6	=Z~	 1 + X  + I(X^2)  + Y  + X:Y,
    m7	=Z~	 1 + X  + Y       + I(Y^2)  + X:Y,
    m8	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y,
    m9	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y,
    m10	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(Y^2):X,
    m11	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X,
    m12	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X + I(X^2):I(Y^2))
  #########################################################################################
  X=D[,1]
  Y=D[,2]
  Z=D[,4]

  npar=c(2,3,3,4,3,4,4,5,6,6,7,8)

  # print("Estudo de regressao multipla")
  Resultado=list()
  Resultado[["Ajuste"]]=""
  Resultado[["Qualidade"]]=""
  Resultado[["MelhorModelo"]]=""
  Resultado[["Design"]]=""
  Resultado[["multicolinearidade"]]=""
  Resultado[["Dados"]]=""
  MULTI=rep(F,12)
  Ajuste=list()
  for (m in 1:12){
    model=lm(modelos[[m]])
    #print(paste ("modelo ajustado ->", c(modelos[[m]])))
    B=coefficients(model)
    anova=anova(aov(Z~paste(X,Y)))
    XX=model.matrix(model)



    Trat=anova[1,]
    Residuo=anova[2,]
    QMR=Residuo$`Mean Sq`
    GLR=Residuo$Df

    Yp=XX%*%B
    n=length(Z)
    sq=t(Yp)%*%Yp-sum(Yp)^2/n
    gl=ncol(XX)-1
    qm=sq/gl
    Fc=qm/QMR
    pV=1-pf(as.numeric(Fc),gl,Residuo$Df)
    #if(pV==0){pV="<2e-16"}

    `Regressao`=round(c(gl,sq,qm,Fc,pV),4)
    `Desv Reg`=Trat-Regressao

    Parc=matrix(0,ncol=5,nrow=ncol(XX)-1)
    rownames(Parc)=colnames(XX[,-1])
    Parc[,1]=1

    for(k in 2:(ncol(XX))){
      XXX=XX[,1:k]
      if(rcond(t(XXX)%*%XXX)> 2.220446e-16){
        B=solve(t(XXX)%*%XXX)%*%t(XXX)%*%Z
        Yp=XXX%*%B
        sq= t(Yp)%*%Yp-sum(Z)^2/n
        if(k==2){Parc[1,2:3]=sq}
        if(k>2){Parc[k-1,2:3]=sq-sum(Parc[,2])}
      }
      if(rcond(t(XXX)%*%XXX)< 2.220446e-16){
        MULTI[m]=T
      }
    }
    colnames(Parc)=c("Sum Sq","DF","Mean Sq","F value","F value")

    Anova=rbind(Trat=c(Trat),`Regressao`,Parc,`Desv Reg`=c(`Desv Reg`),Residuo=c(Residuo))
    Anova[,3]=as.numeric(as.matrix(Anova[,2]))/as.numeric(as.matrix(Anova[,1]) )
    Anova[,4]=as.numeric(as.matrix(Anova[,3]))/QMR


    pV=round(1-pf(as.numeric(as.matrix(Anova[,4])),as.numeric(as.matrix(Anova[,1])),GLR),4)
    pV[pV==0.0000]="<0.0001"
    Anova[,5]=pV
    nfv=nrow(Anova)
    Anova[nfv,4:5]=""

    if(rcond(t(XXX)%*%XXX)> 2.220446e-16){
      VarB=solve(t(XX)%*%XX)*QMR
      Tc=B/sqrt(diag(VarB) )
      PV=round(1-pt(sqrt((Tc)^2),GLR),4)


      ResultCoef=round(cbind(B,sqrt(diag(VarB)),Tc,PV),7)
      ResultCoef[ResultCoef[,4]==0,4]="<0.000001"
      ResultCoef=data.frame(ResultCoef)
      colnames(ResultCoef)=c("Coeficientes","Desv. Padr","estimativa de t","Pr(>F)")


    }

    if(rcond(t(XXX)%*%XXX)< 2.220446e-16){
      #print("Nao e possivel obter a inversa da matriz x'x (multicolinearidade)")

    }


    model2=lm(modelos[[m]])
    res=summary(model2)
    Qualidade=c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
    names(Qualidade)=c("R2","R2aj","AIC","BIC")

    if(MULTI[m]==TRUE){Qualidade=("Nao e possivel obter a inversa da matriz x'x (multicolinearidade)")}

    Ajuste[[paste0("Modelo",m)]]=list(Model=modelos[[m]],AnovaGlobal=Anova[c(1:2,(nfv-1):nfv),],
                                      AnovaParcial=Anova,Coeficientes=ResultCoef,
                                      Qualidade=Qualidade)
  }
  Resultado$Ajuste=Ajuste



  r=function(Z,m,D){
    X=D[,1]
    Y=D[,2]
    model=lm(modelos[[m]])
    res=summary(model)
    c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
  }
  ########################################################################################
  Resumo=matrix(NA,ncol=4,nrow=12)

  for (m in 1:12){
    Z=D[,4]
    Ajust=r(Z,m,D)
    Resumo[m,1]=100*round(Ajust[1],8) #r2
    Resumo[m,2]=100*round(Ajust[2],8) #r2aj
    Resumo[m,3]=round(Ajust[3],6) #AIC
    Resumo[m,4]=round(Ajust[4],6) #BIC
  }

  colnames(Resumo)=c("R2","R2aj","AIC","BIC")
  rownames(Resumo)=paste("modelo",1:12)
  Resumo[MULTI,]=NA
  Resultado$Qualidade=Resumo

  Resumo2=Resumo
  Resumo2[,1:2]=100-Resumo[,1:2]

  Resultado$MelhorModelo=apply(Resumo2,2,function(x) order(x)[1])
  Resultado$Design=2
  Resultado$Dados=D

  id=MULTI*1:length(MULTI)
  id=id[id!=0]
  Resultado$Ajuste[[id]]$AnovaGlobal="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"
  Resultado$Ajuste[[id]]$AnovaParcial="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"
  Resultado$Ajuste[[id]]$Coeficientes="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"

  Resultado$multicolinearidade=MULTI
  if(verbose==TRUE){
    for(m in 1:12){
      cat("#####################################################################","\n")
      cat("#####################################################################","\n")
      cat(paste("Analise do modelo ",m),"\n")
      a=modelos[[m]]
      cat(paste("modelo->",a[2],a[1],a[3]),"\n")
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia global","\n")
      print(Resultado[[1]][[m]]$AnovaGlobal)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia parcial","\n")
      print(Resultado[[1]][[m]]$AnovaParcial)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Coeficientes","\n")
      print(Resultado[[1]][[m]]$Coeficientes)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Avaliadores da qualidade do ajuste","\n")
      print(Resultado[[1]][[m]]$Qualidade)
      cat("","\n")
      cat("","\n")
      cat("","\n")
    }
    cat("Resumo dos avaliadores da qualidade do ajuste","\n")
    print(Resultado$Qualidade)
    cat("Modelo selecionado","\n")
    print(Resultado$MelhorModelo)
  }




  (Resultado)

}


########################################################
###############################
#############
#
AjustarRegressaoDBC=function(D,verbose){

  ########################################################################################
  modelos=list(
    m1  =Z~   1 + X  + Y,
    m2  =Z~	 1 + X  + I(X^2)  + Y,
    m3	=Z~	 1 + X  + Y       + I(Y^2),
    m4	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
    m5	=Z~	 1 + X  + Y       + X:Y,
    m6	=Z~	 1 + X  + I(X^2)  + Y  + X:Y,
    m7	=Z~	 1 + X  + Y       + I(Y^2)  + X:Y,
    m8	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y,
    m9	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y,
    m10	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(Y^2):X,
    m11	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X,
    m12	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X + I(X^2):I(Y^2))
  #########################################################################################
  X=D[,1]
  Y=D[,2]
  Z=D[,4]

  npar=c(2,3,3,4,3,4,4,5,6,6,7,8)

  # print("Estudo de regressao multipla")
  Resultado=list()
  Resultado[["Ajuste"]]=""
  Resultado[["Qualidade"]]=""
  Resultado[["MelhorModelo"]]=""
  Resultado[["Design"]]=""
  Resultado[["multicolinearidade"]]=""
  Resultado[["Dados"]]=""
  Ajuste=list()
  MULTI=rep(F,12)
  for (m in 1:12){
    model=lm(modelos[[m]])
    #print(paste ("modelo ajustado ->", c(modelos[[m]])))
    B=coefficients(model)
    Bloco=as.factor(D[,3])
    anova=anova(aov(Z~paste(X,Y)+Bloco))
    XX=model.matrix(model)



    Trat=anova[1,]
    Bloco=anova[2,]
    Residuo=anova[3,]
    QMR=Residuo$`Mean Sq`
    GLR=Residuo$Df

    Yp=XX%*%B
    n=length(Z)
    sq=t(Yp)%*%Yp-sum(Yp)^2/n
    gl=ncol(XX)-1
    qm=sq/gl
    Fc=qm/QMR
    pV=1-pf(as.numeric(Fc),gl,Residuo$Df)
    #if(pV==0){pV="<2e-16"}

    `Regressao`=round(c(gl,sq,qm,Fc,pV),4)
    `Desv Reg`=Trat-Regressao

    Parc=matrix(0,ncol=5,nrow=ncol(XX)-1)
    rownames(Parc)=colnames(XX[,-1])
    Parc[,1]=1

    for(k in 2:(ncol(XX))){
      XXX=XX[,1:k]
      if(rcond(t(XXX)%*%XXX)> 2.220446e-16){
        B=solve(t(XXX)%*%XXX)%*%t(XXX)%*%Z
        Yp=XXX%*%B
        sq= t(Yp)%*%Yp-sum(Z)^2/n
        if(k==2){Parc[1,2:3]=sq}
        if(k>2){Parc[k-1,2:3]=sq-sum(Parc[,2])}
      }

      if(rcond(t(XXX)%*%XXX)< 2.220446e-16){
        MULTI[m]=T
      }


    }
    colnames(Parc)=c("Sum Sq","DF","Mean Sq","F value","F value")

    Anova=rbind(Trat=c(Trat),`Regressao`,Parc,`Desv Reg`=c(`Desv Reg`),Bloco=c(Bloco),Residuo=c(Residuo))
    Anova[,3]=as.numeric(as.matrix(Anova[,2]))/as.numeric(as.matrix(Anova[,1]) )
    Anova[,4]=as.numeric(as.matrix(Anova[,3]))/QMR


    pV=round(1-pf(as.numeric(as.matrix(Anova[,4])),as.numeric(as.matrix(Anova[,1])),GLR),4)
    pV[pV==0.0000]="<0.0001"
    Anova[,5]=pV
    nfv=nrow(Anova)

    if(rcond(t(XXX)%*%XXX)> 2.220446e-16){
      #XX=as.matrix(aggregate(XXX,by=list(paste(X,Y)),mean)[,-1])
      VarB=solve(t(XX)%*%XX)*QMR
      Tc=B/sqrt(diag(VarB) )
      PV=round(1-pt(sqrt((Tc)^2),GLR),4)


      ResultCoef=round(cbind(B,sqrt(diag(VarB)),Tc,PV),7)
      ResultCoef[ResultCoef[,4]==0,4]="<0.000001"
      ResultCoef=data.frame(ResultCoef)
      colnames(ResultCoef)=c("Coeficientes","Desv. Padr","estimativa de t","Pr(>F)")


    }

    if(rcond(t(XXX)%*%XXX)< 2.220446e-16){
      ResultCoef=("Nao e possivel obter a inversa da matriz x'x (multicolinearidade)")
    }


    model2=lm(modelos[[m]])
    res=summary(model2)
    #names(Qualidade)=c("R2","R2aj","AIC","BIC")
    Qualidade=c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
    names(Qualidade)=c("R2","R2aj","AIC","BIC")
    if(MULTI[m]==TRUE){Qualidade=("Nao e possivel obter a inversa da matriz x'x (multicolinearidade)")}

    Ajuste[[paste0("Modelo",m)]]=list(Model=modelos[[m]],AnovaGlobal=Anova[c(1:2,(nfv-1):nfv),],
                                      AnovaParcial=Anova,Coeficientes=ResultCoef,
                                      Qualidade=Qualidade)
  }
  Resultado$Ajuste=Ajuste



  r=function(Z,m,D){
    X=D[,1]
    Y=D[,2]
    model=lm(modelos[[m]])
    res=summary(model)
    c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
  }
  ########################################################################################
  Resumo=matrix(NA,ncol=4,nrow=12)

  for (m in 1:12){
    Z=D[,4]
    Ajust=r(Z,m,D)
    Resumo[m,1]=100*round(Ajust[1],8) #r2
    Resumo[m,2]=100*round(Ajust[2],8) #r2aj
    Resumo[m,3]=round(Ajust[3],6) #AIC
    Resumo[m,4]=round(Ajust[4],6) #BIC
  }

  colnames(Resumo)=c("R2","R2aj","AIC","BIC")
  rownames(Resumo)=paste("modelo",1:12)
  Resumo[MULTI,]=NA
  Resultado$Qualidade=Resumo
  id=MULTI*1:length(MULTI)
  id=id[id!=0]
  Resultado$Ajuste[[id]]$AnovaGlobal="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"
  Resultado$Ajuste[[id]]$AnovaParcial="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"



  Resumo2=Resumo
  Resumo2[,1:2]=100-Resumo[,1:2]

  Resultado$MelhorModelo=apply(Resumo2,2,function(x) order(x)[1])
  Resultado$Design=3
  Resultado$multicolinearidade=MULTI
  Resultado$Dados=D

  if(verbose==TRUE){
    for(m in 1:12){
      cat("#####################################################################","\n")
      cat("#####################################################################","\n")
      cat(paste("Analise do modelo ",m),"\n")
      a=modelos[[m]]
      cat(paste("modelo->",a[2],a[1],a[3]),"\n")
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia global","\n")
      print(Resultado[[1]][[m]]$AnovaGlobal)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia parcial","\n")
      print(Resultado[[1]][[m]]$AnovaParcial)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Coeficientes","\n")
      print(Resultado[[1]][[m]]$Coeficientes)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Avaliadores da qualidade do ajuste","\n")
      print(Resultado[[1]][[m]]$Qualidade)
      cat("","\n")
      cat("","\n")
      cat("","\n")
    }
    cat("Resumo dos avaliadores da qualidade do ajuste","\n")
    print(Resultado$Qualidade)
    cat("Modelo selecionado","\n")
    print(Resultado$MelhorModelo)
  }




  return(Resultado)

}

