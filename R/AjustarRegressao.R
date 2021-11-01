#' Ajuste de modelos de regressao multipla
#'
#' @description
#\cr
#\if{html}{\figure{logo2.png}{options: align='right'  height="1\%" alt="Figure: logo.png"}}
#
#\cr
#' Esta funcao realiza o ajuste de  modelos de regressao multipla
#'   considerando 2 variaveis independentes (explicativas) e uma variavel
#'   dependente (resposta). E possivel analisar dados de experimentos avaliados
#'   sem delineamento (repeticoes) e com delineamento estatistico (DIC e DBC)
#'
#'
#' @usage AjustarRegressao(Dados, design,Modelos=NULL)
#' @param Dados   Matriz contendo 3 colunas obrigatoriamente caso o design seja
#'   1 (experimento sem repeticoes), sendo as duas primeiras as variaveis
#'   explicativas e a terceira a variavel resposta. Se houver repeticoes (Design
#'   2 ou 3) a matriz deve conter obrigatoriamente 4 colunas, as duas primeiras
#'   com as variaveis explicativas, a terceira com a identificacao das
#'   repeticoes/blocos e a quarta coluna com a variavel resposta.
#' @param design Indica o delineamento utilizado na pesquisa:
#' \itemize{
#' \item design 1 -> Experimento  sem repeticao.
#' \item design 2 -> Experimento no delineamento inteiramente casualizado (Dic).
#' \item design 3 -> Experimento no delineamento em blocos casualizados (Dbc).
#'   }
#' @param Modelos Objeto do tipo list com os objetos a serem testados. Se NULL
#' (default) sao testados 12 modelos de regressao.

#' @return A funcao retorna o resultado do ajuste de modelos de regressao.
#' Estes resultados podem ser apresentados no console, e alem disso, estao
#' carregados em um objeto do tipo list.
#  \cr
# \if{html}{\figure{Table.jpg}{options: align='left'  height="1\%" alt="Figure: Table.jpg"}}
#  \cr
#' @seealso \code{\link{plot2D}}, \code{\link{plot3D}}, \code{\link{predict3D}}
#' @references Tutoriais onlines:
#' https://www.youtube.com/playlist?list=PLvth1ZcREyK6OUnWVs-hnyVdCB1xuxbVs
#' @examples
# \donttest{
#' #Exemplo 1: Experimento sem delineamento
#' data("Dados1")
#' res=AjustarRegressao(Dados = Dados1, design=1)
#' res
#' plot2D(res,niveis = 3)
#' plot2D(res,niveis = 3,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'        Metodo = "simple")
#' plot2D(res,niveis = 5,xlab="Comprimento (cm)",ylab="Largura (cm)",
#'        Metodo="edge",col.contour = "blue")
#' plot3D(res)
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
#'       contour = TRUE,cor=col1(200),box=TRUE,col.contour = "black",
#'       main="Superficie Resposta")
#'
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 2: Experimento sem delineamento
#'data("Dados2")
#'res=AjustarRegressao(Dados = Dados2, design=1)
#'res
#'plot2D(res,niveis = 10,xlab="Acucar (%)",ylab="Banana (%)",
#'zlab="Aceitabilidade",
#'       contour = TRUE,cor=col1(200),box=TRUE,col.contour = "black",
#'       main="Superficie Resposta")
#' plot3D(res)
#'
#'##############################################################################
#'##############################################################################
#'#Exemplo 3: Experimento com delineamento (DIC)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=2)
#'res
#'plot2D(res,niveis = 5, Metodo="edge",contour = FALSE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = TRUE,col.contour = "black")
#'plot3D(res)
#'##############################################################################
#'##############################################################################
#'#Exemplo 4: Experimento com delineamento (DBC)
#'data("Dados3")
#'res=AjustarRegressao(Dados = Dados3, design=3)
#'res
#'plot2D(res,niveis = 20,xlab="N (K/ha)",ylab="K (Kg/ha)",
#'       Metodo="edge",contour = TRUE,cor=col1(200),box=TRUE)
#'plot2D(res,niveis = 5, Metodo="edge",contour = TRUE,col.contour = "black")
#' plot3D(res)
#'
# }
#' @importFrom grDevices colorRampPalette hcl.colors
#' @importFrom graphics layout par image
#' @importFrom stats AIC BIC anova aov coefficients lm model.matrix pf predict pt
#' @importFrom crayon green
#' @importFrom magrittr `%>%`
#' @export
#' @exportS3Method print AjustarRegressao



AjustarRegressao=function(Dados,design,Modelos=NULL){
  modelos=Modelos
  if(is.null(Modelos)){
    ########################################################################################
    modelos=list(
      m1  =Z~   1 + X  + Y,
      m2  =Z~	 1 + X  + I(X^2)  + Y,
      m3	=Z~	 1 + X  + Y       + I(Y^2),
      m4	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2),
      m5	=Z~	 1 + X  + Y       + X:Y,
      m6	=Z~	 1 + X  + I(X^2)  + Y  + X:Y,
      m7	=Z~	 1 + X  + Y       + I(Y^2)  + X:Y,
      m8	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y,
      m9	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y,
      m10	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(Y^2):X,
      m11	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X,
      m12	=Z~	 1 + X  + I(X^2)  + Y  + I(Y^2)  + X:Y  + I(X^2):Y  +  I(Y^2):X + I(X^2):I(Y^2))
    #########################################################################################

  }



  ########### Medias
  if(design==1){
   res= AjustarRegressaoMED(Dados,modelos)
  }
  ########### DIC
  if(design==2){
   res= AjustarRegressaoDIC(Dados,modelos)
  }
  ############ DBC
  if(design==3){
   res= AjustarRegressaoDBC(Dados,modelos)
  }
  class(res)="AjustarRegressao"
  return(res)
}



print.AjustarRegressao=function(x,...){
Resultado=x
npar=nrow(Resultado$Qualidade)
    for(m in 1:npar){
      cat("#####################################################################","\n")
      cat("#####################################################################","\n")
      cat(paste("Analise do modelo ",m),"\n")

      a=Resultado[[1]][[m]]$Model
      cat(paste("modelo->",a[2],a[1],a[3]),"\n")
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia global","\n")
      print(Resultado[[1]][[m]]$AnovaGlobal)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Analise de variancia parcial","\n")
      print(Resultado[[1]][[m]]$AnovaParcial)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Coeficientes","\n")
      print(Resultado[[1]][[m]]$Coeficientes)
      cat("_____________________________________________________________________","\n")
      cat("","\n")
      cat("Avaliadores da qualidade do ajuste","\n")
      print(Resultado[[1]][[m]]$Qualidade)
    }

    cat("Resumo dos avaliadores da qualidade do ajuste","\n")
    print(Resultado$Qualidade)
    cat("Modelo selecionado","\n")
    print(Resultado$MelhorModelo)
  }
















