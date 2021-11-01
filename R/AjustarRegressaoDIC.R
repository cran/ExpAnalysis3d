
#################################################################
######################################################
############################################
#
AjustarRegressaoDIC=function(D,modelos){

 modelos=modelos
   #########################################################################################
  X=D[,1]
  Y=D[,2]
  Z=D[,4]

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
  MULTI=rep(F,length(npar))
  Ajuste=list()
  for (m in 1:length(npar)){

    model=lm(modelos[[m]],data=DD)
    #print(paste ("modelo ajustado ->", c(modelos[[m]])))
    B=coefficients(model)
    anova=anova(aov(Z~paste(X,Y),data=DD))
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


    model2=lm(modelos[[m]],data=DD)
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
    model=lm(modelos[[m]],data=DD)
    res=summary(model)
    c(res$r.squared,res$adj.r.squared,AIC(model),BIC(model))
  }
  ########################################################################################
  Resumo=matrix(NA,ncol=4,nrow=length(npar))

  for (m in 1:length(npar)){
    Z=D[,4]
    Ajust=r(Z,m,D)
    Resumo[m,1]=100*round(Ajust[1],8) #r2
    Resumo[m,2]=100*round(Ajust[2],8) #r2aj
    Resumo[m,3]=round(Ajust[3],6) #AIC
    Resumo[m,4]=round(Ajust[4],6) #BIC
  }

  colnames(Resumo)=c("R2","R2aj","AIC","BIC")
  rownames(Resumo)=paste("modelo",1:length(npar))
  Resumo[MULTI,]=NA
  Resultado$Qualidade=Resumo

  Resumo2=Resumo
  Resumo2[,1:2]=100-Resumo[,1:2]

  Resultado$MelhorModelo=apply(Resumo2,2,function(x) order(x)[1])
  Resultado$Design=2
  Resultado$Dados=D

  id=MULTI*1:length(MULTI)
  id=id[id!=0]
  if(sum(id)!=0){
  Resultado$Ajuste[[id]]$AnovaGlobal="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"
  Resultado$Ajuste[[id]]$AnovaParcial="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"
  Resultado$Ajuste[[id]]$Coeficientes="Nao e possivel obter a inversa da matriz x'x (multicolinearidade)"
}
  Resultado$multicolinearidade=MULTI

  return(Resultado)

}
