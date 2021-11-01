
Plotar2D=function(D,m,niveis,n,xlab=NULL,ylab=NULL,zlab=NULL,Metodo,
                  contour,col.contour,cor,box,main=NULL){






  X=D[,1]
  Y=D[,2]
  Z=D[,3]

  DD=data.frame(X=X,Y=Y,Z=Z)
  model=lm(m,data=DD)


  XY=expand.grid(X=seq(min(X),max(X),l=n),Y=seq(min(Y),max(Y),l=n))
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

  x=unique(XYZ$X)
  y=unique(XYZ$Y)

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
