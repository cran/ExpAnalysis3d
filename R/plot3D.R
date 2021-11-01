#' Plotar graficos 3D
#'
#' @description
#\cr
# \if{html}{\figure{logo2.png}{options: align='right'  height="1\%" alt="Figure: logo.png"}}
# \if{latex}{\figure{logo2.png}{options:  align='right' height=1in alt="Figure: logo.png"}}
#' Esta funcao proporciona a criacao de um grafico de superficie
#' resposta (3D). Para isso, deve-se ter como entrada o output da funcao
#' 'AjustarRegressao'.
#' @usage plot3D(Resultado,
#'               modelo=NULL,
#'               cor=NULL,
#'               xlab=NULL,
#'               ylab=NULL,
#'               zlab=NULL,
#'               main=NULL)
#' @param Resultado   Objeto do tipo list contendo a saida da funcao
#' 'AjustarRegressao'

#' @param modelo Valor numerico indicando o modelo considerado na confeccao do
#' grafico. Pode ser
#' NULL (default) ou um valor numerico indicando o modelo a ser considerado.
#' \itemize{
#' \item NULL -> Considera o melhor dos  modelos testados de acordo com o
#' Criterio de informatividade de Akaike (AIC)
#' \item n -> considera o n-esimo modelo para plotar o grafico.
#'}
#'
#' @param cor Refere-se a paleta de cores para a construcao do grafico. Se for
#' NULL (default) sera utilizado uma paleta de cores padrao. Se for desejavel
#' utilizar outras cores veja como criar a paleta de cores no exemplo dessa
#' funcao.
#' @param xlab Texto indicando o nome do eixo x.
#' @param ylab Texto indicando o nome do eixo y.
#' @param zlab Texto indicando o nome do eixo z.
#' @param main Texto indicando o nome do grafico.
#'
#' @return A funcao retorna um grafico 3D.
#' @seealso \code{\link{plot2D}}, \code{\link{predict3D}},
#' \code{\link{AjustarRegressao}}
#' @references Tutoriais onlines:
#' https://www.youtube.com/playlist?list=PLvth1ZcREyK6OUnWVs-hnyVdCB1xuxbVs
#' @examples
# \donttest{
#' #'  ########################################################################
#'  ########################################################################
#'   #Exemplo 1: Experimento sem delineamento
#'   data("Dados1")
#'   res=AjustarRegressao(Dados = Dados1,
#'   design=1)
#'   plot3D(res)
#'   ##########################
#'   #Criando paleta de cores
#'   col0 = colorRampPalette(c('white', 'cyan', '#007FFF', 'blue','#00007F'))
#'   col1 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', 'white',
#'                             'cyan', '#007FFF', 'blue','#00007F'))
#'   col2 = colorRampPalette(c('#67001F', '#B2182B', '#D6604D', '#F4A582',
#'                             '#FDDBC7', '#FFFFFF', '#D1E5F0', '#92C5DE',
#'                             '#4393C3', '#2166AC', '#053061'))
#'   col3 = colorRampPalette(c('red', 'white', 'blue'))
#'   col4 = colorRampPalette(c('#7F0000', 'red', '#FF7F00', 'yellow', '#7FFF7F',
#'                             'cyan', '#007FFF', 'blue', '#00007F'))
#'
#'
#'   plot3D(res,cor=col4(200),xlab="Comprimento (cm)",ylab="Largura (cm)",
#'   zlab="Area (cm2)")
#'
#'
#'  ########################################################################
#'  ########################################################################
#'  #Exemplo 2: Experimento sem delineamento
#'   data("Dados2")
#'   res=AjustarRegressao(Dados = Dados2, design=1)
#'   plot3D(res,cor=col1(200),xlab="Acucar (%)",ylab="Banana (%)",
#'   zlab="Aceitabilidade")
#'
#'
#'
#'
#'   #Exemplo 3: Experimento com delineamento (DIC)
#'   data("Dados3")
#'   res=AjustarRegressao(Dados = Dados3, design=2)
#'   plot3D(res,cor=col1(200),xlab="N (K/ha)",ylab="K (Kg/ha)")
#'
#'
#'   #Exemplo 4: Experimento com delineamento (DBC)
#'   data("Dados3")
#'   res=AjustarRegressao(Dados = Dados3, design=3)
#'   plot3D(res,cor=col1(200),modelo = 10,xlab="N (K/ha)",ylab="K (Kg/ha)")
#'
#'   #Exemplo 5: Experimento com delineamento (DBC) e modelos personalizados
#'   data("Dados3")
#'   Mod=list(
#'   m1  =Z~   1 + X  + Y,
#'   m2	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
#'   m3	=Z~	 1 + X  + Y       + X:Y)
#'   res=AjustarRegressao(Dados = Dados3, design=3,Modelos=Mod)
#'   plot3D(res,cor=col1(200),modelo = 3,xlab="N (K/ha)",ylab="K (Kg/ha)")
#'
#'
# }
#' @export

plot3D=function(Resultado,modelo=NULL,cor=NULL,
                xlab=NULL,ylab=NULL,zlab=NULL,main=NULL){
res=Resultado
  if(is.null(cor)){cor=hcl.colors(12, "YlOrRd", rev = TRUE)}

  if(is.null(modelo)){Modelo=res$Ajuste[[res$MelhorModelo[3]]]$Model  }
if(is.null(modelo)==FALSE){Modelo=res$Ajuste[[modelo]]$Model  }
  if(res$Design==1){
   D=res$Dados

  }

  if((res$Design==2)|(res$Design==3) ){
   D=res$Dados[,-3]
  }

Plotar3D(D,m=Modelo,cor=cor,xlab=xlab,ylab=ylab,zlab=zlab,main)

}
