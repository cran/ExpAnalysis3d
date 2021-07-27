#' Funcao para obter valores para construcao de graficos de superficie resposta
#'
#' @description
#\cr
# \if{html}{\figure{logo2.png}{options: align='right'  height="1\%" alt="Figure: logo.png"}}
#' Esta funcao possibilita gerar valores para construcao de
#' graficos de superficie resposta (3D).
#' @usage predict3D(Resultado, Modelo=NULL, type=1,n=30)
#' @param Resultado    :Objeto do tipo list referente ao output da funcao
#' AjustarRegressao.
#' @param Modelo    :Valor numerico (inteiro) de 1 a 12 indicando o modelo de
#'  regressao multipla selecionado:
#  \cr
# \if{html}{\figure{Table.jpg}{options: align='left'  height="1\%" alt="Figure: Table.jpg"}}
#  \cr
#' \itemize{
#' \item NULL -> Considera o melhor dos 12 modelos testados de acordo com o
#' Criterio de informatividade de Akaike (AIC)
#' \item 1 -> O grafico e plotado considerando o modelo:
#'  Z~1+X+Y
#' \item 2 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y
#' \item 3 -> O grafico e plotado considerando o modelo:
#'Z~1+X+Y+I(Y^2)
#' \item 4 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)
#' \item 5 -> O grafico e plotado considerando o modelo:
#'Z~1+X+Y+X:Y
#' \item 6 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+X:Y
#' \item 7 -> O grafico e plotado considerando o modelo:
#'Z~1+X+Y+I(Y^2)+X:Y
#' \item 8 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y
#' \item 9 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(X^2):Y
#' \item 10 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(Y^2):X
#' \item 11 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(X^2):Y+I(Y^2):X
#' \item 12 -> O grafico e plotado considerando o modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(X^2):Y+I(Y^2):X+I(X^2):I(Y^2))
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
#'  res=AjustarRegressao(Dados = Dados1, design=1,verbose=FALSE)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#'
#'
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 2: Experimento sem delineamento
#'  data("Dados2")
#'  res=AjustarRegressao(Dados = Dados2, design=1,verbose=TRUE)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#'
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 3: Experimento com delineamento (DIC)
#'  data("Dados3")
#'  res=AjustarRegressao(Dados = Dados3, design=2,verbose=TRUE)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#'
#'  ############################################################################
#'  ############################################################################
#'  #Exemplo 4: Experimento com delineamento (DBC)
#'  data("Dados3")
#'  res=AjustarRegressao(Dados = Dados3, design=3,verbose=TRUE)
#'  predict3D(Resultado = res,type =1) #matriz XYZ
#'  predict3D(Resultado = res,type =2) #matriz Z quadrada
#'  predict3D(Resultado = res,type =3) #matriz XY + Z quadrada
#  }
#' @export
predict3D=function(Resultado, Modelo=NULL, type=1,n=30){
  if(is.null(Modelo)){Modelo=Resultado$MelhorModelo[3]}
  if(Resultado$Design==1){D=Resultado$Dados}
  if((Resultado$Design==2)|(Resultado$Design==3)){D=Resultado$Dados[,-3]}

  if(type==1){Quadrado=F}
  if((type==2)|(type==3)){Quadrado=T}

  MAT=Predizer(D=D,m=Modelo,xx=seq(min(D[,1]),max(D[,1]),l=n),yy=seq(min(D[,2]),max(D[,2]),l=n),
               quadrada=Quadrado)

  if((type==2)|(type==1)){return(MAT)}

  if((type==3)){
    MAT2=list()
    MAT2$XY=cbind(x=seq(min(D[,1]),max(D[,1]),l=n),y=seq(min(D[,2]),max(D[,2]),l=n))
    MAT2$Z=MAT
    return(MAT2)

  }
}
