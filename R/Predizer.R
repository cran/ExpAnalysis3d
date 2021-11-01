
Predizer=function(D,Modelo,xx=seq(min(D[,1]),max(D[,1]),l=30),yy=seq(min(D[,2]),max(D[,2]),l=30),quadrada=F){

DD=data.frame(X=D[,1],Y=D[,2],Z=D[,3])

  model=lm(Modelo,data=DD)


  XY=expand.grid(X=xx,Y=yy)
  XYZ=cbind(XY,Yp=predict(model,newdata = XY))
  z=matrix(XYZ$Yp,ncol=length(unique(xx)))
  if(quadrada==F){Resultado=XYZ}
  if(quadrada==T){Resultado=z}

  return(Resultado)
 }
