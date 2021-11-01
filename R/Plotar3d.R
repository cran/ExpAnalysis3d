
Plotar3D=function(D,m,cor,xlab=NULL,ylab=NULL,zlab=NULL,main=NULL){

X=D[,1]
  Y=D[,2]
  Z=D[,3]

  DD=data.frame(X=X,Y=Y,Z=Z)
  model=lm(m,data=DD)
n=30

  XY=expand.grid(X=seq(min(X),max(X),l=n),Y=seq(min(Y),max(Y),l=n))
  XYZ=cbind(XY,Yp=predict(model,newdata = XY))
#write.table(XYZ,file="res.txt",sep="\t",col.names=F,row.names=F)
  z=matrix(XYZ$Yp,ncol=n,byrow = TRUE)
  if(is.null(xlab)){xlab=colnames(D)[1]}
  if(is.null(ylab)){ylab=colnames(D)[2]}
  if(is.null(zlab)){zlab=colnames(D)[3]}
  fig <-plot_ly(x = seq(min(X),max(X),l=n), y = seq(min(Y),max(Y),l=n),
                z = z,type = "surface",colors = cor)

  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  fig<- fig  %>%   plotly::layout(title=main,
                                  scene=list(xaxis = list(title = xlab,titlefont = f),
                                             yaxis = list(title = ylab,titlefont = f),
                                             zaxis = list(title = zlab,titlefont = f))
                        )

fig

}
