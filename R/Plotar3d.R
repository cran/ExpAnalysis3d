
Plotar3D=function(D,m,cor,xlab=NULL,ylab=NULL,zlab=NULL,main=NULL){

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
n=30

  XY=expand.grid(A=seq(min(A),max(A),l=n),B=seq(min(B),max(B),l=n))
  XYZ=cbind(XY,Yp=predict(model,newdata = XY))

  z=matrix(XYZ$Yp,ncol=n)
  if(is.null(xlab)){xlab=colnames(D)[1]}
  if(is.null(ylab)){ylab=colnames(D)[2]}
  if(is.null(zlab)){zlab=colnames(D)[3]}
  fig <-plot_ly(x = seq(min(A),max(A),l=n), y = seq(min(B),max(B),l=n),
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
