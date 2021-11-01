#' Plotar graficos 2D
#'
#' @description
#\cr
# \if{html}{\figure{logo2.png}{options: align='right'  height="1\%" alt="Figure: logo.png"}}
#' Esta funcao proporciona a criacao de um grafico de contorno (2D).
#'  Para isso, deve-se ter como entrada o output da funcao
#' 'AjustarRegressao'.
#' @usage plot2D(Resultado,
#'               modelo=NULL,
#'               niveis=10,
#'               xlab=NULL,
#'               ylab=NULL,
#'               zlab=NULL,
#'               Metodo="flattest",
#'               main=NULL,
#'               contour=TRUE,
#'               col.contour="red",
#'               cor=NULL,box=TRUE)
#' @param Resultado   Objeto do tipo list contendo a saida da funcao
#' 'AjustarRegressao'
#' @param modelo Valor numerico indicando o modelo considerado na confeccao do
#' grafico. Pode ser
#' NULL (defaul) ou um valor numerico indicando o modelo a ser considerado.
#' \itemize{
#' \item NULL -> Considera o melhor dos  modelos testados de acordo com o
#' Criterio de informatividade de Akaike (AIC)
#' \item n -> considera o n esimo modelo para plotar o grafico.
#'
#' }
#' @param niveis indica o numero de niveis (curvas) se deseja apresentar
#' no grafico de contorno
#' @param xlab Texto indicando o nome do eixo x.
#' @param ylab Texto indicando o nome do eixo y.
#' @param zlab Texto indicando o nome do eixo z
#' @param Metodo character string ("simple", "edge" ou "flattest") indicando o
#' metodo a ser utilizando para a obtencao dos contornos.
#' @param main Texto indicando o nome do grafico.
#' @param contour indica se e desejavel a apresentacao dos contornos no grafico.
#' @param col.contour indica a cor das linhas de contorno no grafico.
#' @param cor Refere-se a paleta de cores para a construcao do grafico. Se for
#' NULL (defaulTRUE) sera utilizado uma paleta de cores padrao. Se for desejavel
#' utilizar outras cores veja como criar a paleta de cores no exemplo dessa
#' funcao.
#' @param box Valor logico (TRUE ou FALSE) indicando se e desejavel a
#' apresentacao dos valores numericos nos eixos.
#' @return A funcao retorna um grafico 2D.
#' @seealso \code{\link{plot3D}}, \code{\link{predict3D}},
#'  \code{\link{AjustarRegressao}}
#' @references Tutoriais onlines:
#' https://www.youtube.com/playlist?list=PLvth1ZcREyK6OUnWVs-hnyVdCB1xuxbVs
#' @examples
# \donttest{
#'##############################################################################
#'##############################################################################
#' #Exemplo 1: Experimento sem delineamento
#' data("Dados1")
#' res=AjustarRegressao(Dados = Dados1, design=1)
#' plot2D(res,niveis = 3)
#' plot2D(res,niveis = 3,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'        Metodo = "simple")
#' plot2D(res,niveis = 5,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'        Metodo="edge",col.contour = "blue")
#'
#'
#'##########################
#'#Criando paleta de cores
#'col0 = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))
#'col1 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
#'                          'cyan', '#007FFF', 'blue','#00007F'))
#'col2 = colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
#'                          '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
#'                          '#4393C3', '#2166AC', '#053061'))
#'col3 = colorRampPalette(c('red', 'white', 'blue'))
#'col4 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
#'                          'cyan', '#007FFF', 'blue', '#00007F'))
#'
#'plot2D(res,niveis = 5,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'       Metodo="edge",contour = TRUE,cor=col0(200),box=FALSE)
#'
#'
#'plot2D(res,niveis = 10,xlab="Comprimento (cm)",ylab="Largura (cm)",zlab=FALSE,
#'       contour =TRUE,cor=col1(200),box=TRUE,col.contour = "black",
#'       main="Superficie Resposta")
#'
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 2: Experimento sem delineamento
#'data("Dados2")
#'res=AjustarRegressao(Dados = Dados2, design=1)
#'plot2D(res,niveis = 10,xlab="Acucar (%)",ylab="Banana (%)",
#'zlab="Aceitabilidade",
#'       contour =TRUE,cor=col1(200),box=TRUE,col.contour = "black",
#'       main="Superficie Resposta")
#'
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 3: Experimento com delineamento (DIC)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=2)
#'plot2D(res,niveis = 5, Metodo="edge",contour = FALSE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = TRUE,col.contour = "black")
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 4: Experimento com delineamento (DBC)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=3)
#'plot2D(res,niveis = 20,xlab="N (K/ha)",ylab="K (Kg/ha)",
#'       Metodo="edge",contour = TRUE,cor=col1(200),box=TRUE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = TRUE,col.contour = "black")
#'
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 4: Experimento com delineamento (DBC) com modelo personalizado
#'   Mod=list(
#'   m1  =Z~   1 + X  + Y,
#'   m2	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
#'   m3	=Z~	 1 + X  + Y       + X:Y)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=3,Modelos=Mod)
#'plot2D(res,niveis = 20,xlab="N (K/ha)",ylab="K (Kg/ha)",
#'       Metodo="edge",contour = TRUE,cor=col1(200),box=TRUE)
#'plot2D(res,niveis = 5,modelo=3, Metodo="edge",contour = TRUE,col.contour = "black")
#'
#'
#'
#}
#' @importFrom plotly plot_ly
#' @importFrom fields image.plot
#' @export

plot2D=function(Resultado,modelo=NULL,niveis=10,xlab=NULL,ylab=NULL,zlab=NULL,
                Metodo="flattest",main=NULL,
                contour=TRUE,col.contour="red",cor=NULL,box=TRUE){
  res=Resultado

  if(is.null(cor)){cor=hcl.colors(12, "YlOrRd", rev = TRUE)}

  if(is.null(modelo)){Modelo=res$Ajuste[[res$MelhorModelo[3]]]$Model  }
  if(is.null(modelo)==F){Modelo=res$Ajuste[[modelo]]$Model  }


  if(res$Design==1){
    Plotar2D(D=res$Dados,m =Modelo,niveis = niveis, n=200,xlab=xlab,ylab=ylab,zlab=zlab,
             Metodo,contour,col.contour,cor,box,main)
  }

  if((res$Design==2)|(res$Design==3) ){
    Plotar2D(D=res$Dados[,-3],m =Modelo,niveis = niveis, n=200,xlab=xlab,ylab=ylab,zlab=zlab,
             Metodo,contour,col.contour,cor,box,main)
  }



}
