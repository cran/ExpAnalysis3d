#' Funcao para obter valores para construcao de graficos de superficie resposta
#'
#' @description
#\cr
# \if{html}{\figure{logo2.png}{options: align='right'  height="1\%" alt="Figure: logo.png"}}
#' Esta funcao possibilita gerar valores para construcao de
#' graficos de superficie resposta (3D).
#' @usage predict3D(Resultado, modelo=NULL, type=1,n=30)
#' @param Resultado    :Objeto do tipo list referente ao output da funcao
#' AjustarRegressao.

#' @param modelo Valor numerico indicando o modelo considerado na confeccao do
#' grafico. Pode ser
#' NULL (default) ou um valor numerico indicando o modelo a ser considerado.
#' \itemize{
#' \item NULL -> Considera o melhor dos  modelos testados de acordo com o
#' Criterio de informatividade de Akaike (AIC)
#' \item n -> considera o n-esimo modelo para plotar o grafico.
#'
#' }
#'
#' @param type    :Valor de 1 a 3 indicando o output desejado pela funcao.
#'  \itemize{
#' \item 1: Matriz XYZ
#' \item 2: Matriz Z quadrada
#' \item 3: Matriz XY + Z quadrada
#'  }
#'
#' @param n    :Numeros de valores equidistantes entre o menor e maior valor de
#' cada variavel explicativa. O numero final de valores preditos sera n x n.
#' @return A funcao apresenta  valores para a obtencao de grafico de superificie
#'  resposta 3D.
#' @seealso \code{\link{plot2D}}, \code{\link{plot3D}},
#'  \code{\link{AjustarRegressao}}
#' @examples
# \donttest{
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 1: Experimento sem delineamento
#'  data("Dados1")
#'  res=AjustarRegressao(Dados = Dados1, design=1)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#'
#'
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 2: Experimento sem delineamento
#'  data("Dados2")
#'  res=AjustarRegressao(Dados = Dados2, design=1)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#'
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 3: Experimento com delineamento (DIC)
#'  data("Dados3")
#'  res=AjustarRegressao(Dados = Dados3, design=2)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#'
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 4: Experimento com delineamento (DBC)
#'  data("Dados3")
#'  res=AjustarRegressao(Dados = Dados3, design=3)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#  }
#' @export
predict3D=function(Resultado, modelo=NULL, type=1,n=30){
res=Resultado
  if(is.null(modelo)){Modelo=res$Ajuste[[res$MelhorModelo[3]]]$Model  }
  if(is.null(modelo)==F){Modelo=res$Ajuste[[modelo]]$Model  }

  if(Resultado$Design==1){D=Resultado$Dados}
  if((Resultado$Design==2)|(Resultado$Design==3)){D=Resultado$Dados[,-3]}

  if(type==1){Quadrado=F}
  if((type==2)|(type==3)){Quadrado=T}

  MAT=Predizer(D=D,Modelo=Modelo,xx=seq(min(D[,1]),max(D[,1]),l=n),yy=seq(min(D[,2]),max(D[,2]),l=n),
               quadrada=Quadrado)

  if((type==2)|(type==1)){return(MAT)}

  if((type==3)){
    MAT2=list()
    MAT2$XY=cbind(x=seq(min(D[,1]),max(D[,1]),l=n),y=seq(min(D[,2]),max(D[,2]),l=n))
    MAT2$Z=MAT
    return(MAT2)

  }
}
