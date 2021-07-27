
Predizer=function(D,m=2,xx=seq(min(D[,1]),max(D[,1]),l=30),yy=seq(min(D[,2]),max(D[,2]),l=30),quadrada=F){
  modelos=list(
    m1  =y~   1 + A  + B,
    m2  =y~	 1 + A  + I(A^2)  + B,
    m3	=y~	 1 + A  + B       + I(B^2),
    m4	=y~	 1 + A  + I(A^2)  + B  + I(B^2),
    m5	=y~	 1 + A  + B       + A:B,
    m6	=y~	 1 + A  + I(A^2)  + B  + A:B,
    m7	=y~	 1 + A  + B       + I(B^2)  + A:B,
    m8	=y~	 1 + A  + I(A^2)  + B  + I(B^2)  + A:B,
    m9	=y~	 1 + A  + I(A^2)  + B  + I(B^2)  + A:B  + I(A^2):B,
    m10	=y~	 1 + A  + I(A^2)  + B  + I(B^2)  + A:B  + I(B^2):A,
    m11	=y~	 1 + A  + I(A^2)  + B  + I(B^2)  + A:B  + I(A^2):B  +  I(B^2):A,
    m12	=y~	 1 + A  + I(A^2)  + B  + I(B^2)  + A:B  + I(A^2):B  +  I(B^2):A + I(A^2):I(B^2))
  #########################################################################################
  A=D[,1]
  B=D[,2]
  y=D[,3]


  model=lm(modelos[[m]])


  XY=expand.grid(A=xx,B=yy)
  XYZ=cbind(XY,Yp=predict(model,newdata = XY))
  z=matrix(XYZ$Yp,ncol=length(unique(xx)))
  if(quadrada==F){Resultado=XYZ}
  if(quadrada==T){Resultado=z}

  return(Resultado)
 }
