

################################################################
############################################################
#########################################################
#
AjustarRegressaoMED=function(D,modelos){


  X=D[,1]
  Y=D[,2]
  Z=D[,3]

DD=data.frame(X=X,Y=Y,Z=Z)
  npar=unlist(lapply(modelos, function(x)  length(unlist(strsplit(as.character(x)[3],"[+]")))-1))

  # print("Estudo de regressao multipla")
  Resultado=list()
  Resultado[["Ajuste"]]=""
  Resultado[["Qualidade"]]=""
  Resultado[["MelhorModelo"]]=""
  Resultado[["Design"]]=""
  Resultado[["multicolinearidade"]]=""
  Resultado[["Dados"]]=""
  Ajuste=list()

  for (m in 1:length(npar)){

    model=lm(modelos[[m]],data=DD)

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
    model=lm(modelos[[m]],data=DD)
    res=summary(model)
    c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
  }
  ########################################################################################
  Resumo=matrix(NA,ncol=4,nrow=length(npar))

  for (m in 1:length(npar)){
    Z=D[,3]
    Ajust=r(Z,m,D)
    Resumo[m,1]=100*round(Ajust[1],8) #r2
    Resumo[m,2]=100*round(Ajust[2],8) #r2aj
    Resumo[m,3]=round(Ajust[3],6) #AIC
    Resumo[m,4]=round(Ajust[4],6) #BIC
  }

  colnames(Resumo)=c("R2","R2aj","AIC","BIC")
  rownames(Resumo)=paste("modelo",1:length(npar))

  Resultado$Qualidade=Resumo

  Resumo2=Resumo
  Resumo2[,1:2]=100-Resumo[,1:2]

  Resultado$MelhorModelo=apply(Resumo2,2,function(x) order(x)[1])
  Resultado$Design=1
  Resultado$multicolinearidade=rep(FALSE,length(npar))
  Resultado$Dados=D

  return(Resultado)
}




