
Plotar2D=function(D,m=2,niveis,n,xlab=NULL,ylab=NULL,zlab=NULL,Metodo,
                  contour,col.contour,cor,box,main){

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


  XY=expand.grid(A=seq(min(A),max(A),l=n),B=seq(min(B),max(B),l=n))
  XYZ=cbind(XY,Yp=predict(model,newdata = XY))

  z=matrix(XYZ$Yp,ncol=n)

  jet.colors <- colorRampPalette( c("blue", "red") )

  # Generate the desired number of colors from this palette
  nbcol <- 100
  color <- jet.colors(nbcol)

  nrz <- nrow(z)
  ncz <- ncol(z)

  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]

  # Recode facet z-values into color indices
  facetcol <- cut(zfacet, nbcol)

  x=unique(XYZ$A)
  y=unique(XYZ$B)

  if(is.null(xlab)){xlab=colnames(D)[1]}
   if(is.null(ylab)){ylab=colnames(D)[2]}
  if(is.null(zlab)){zlab=colnames(D)[3]}
  if(isFALSE(zlab)){zlab=NULL}
  obj<- list( x=x,y=y,z=z)
  image.plot(obj, legend.lab=zlab, ylim=c(min(y),max(y)), zlim=c(min(z),max(z)),
        xlab=xlab,ylab=ylab,col=cor,axes =box,main=main)
  if(contour==TRUE){
    contour(x = x,y = y,z=z,add=T,xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), zlim=c(min(z),max(z)),
            nlevels =niveis, labcex = 1, drawlabels = TRUE, method = Metodo,
            axes = TRUE, xlab=xlab,ylab=ylab,
            col = col.contour, lty = par("lty"), lwd = par("lwd"))
  }


}
