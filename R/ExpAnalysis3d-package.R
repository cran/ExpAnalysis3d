#' Pacote Para Analise De Experimentos Por Regressao Multipla
#' e Grafico 3D
#'  \cr
#' \if{html}{\figure{graph2.jpg}{options:   height="4\%" alt="Figure: graph2.jpg"}}

#' @description
#' Este pacote realiza a analise de experimentos que tenham
#' duas variaveis explicativas quantitativas e uma variavel resposta
#' quantiativa.
#' O experimento pode ter sido conduzido sem repeticoes, no delineamento
#' inteiramente casualizado ou em bloco casualizado.
#' @details
#' Os modelos podem ser alocados em um objeto do tipo list. Outra alternativa e
#' considerar os 12 modelos ajustados de regressao default:
#' \itemize{
#' \item 1 -> Modelo:
#'  Z~1+X+Y
#' \item 2 -> Modelo:
#'Z~1+X+I(X^2)+Y
#' \item 3 -> Modelo:
#'Z~1+X+Y+I(Y^2)
#' \item 4 -> Modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)
#' \item 5 -> Modelo:
#''Z~1+X+Y+X:Y
#' \item 6 -> Modelo:
#'Z~1+X+I(X^2)+Y+X:Y
#' \item 7 -> Modelo:
#'Z~1+X+Y+I(Y^2)+X:Y
#'\item 8 -> Modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y
#' \item 9 -> Modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(X^2):Y
#' \item 10 -> Modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(Y^2):X
#' \item 11 -> Modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(X^2):Y+I(Y^2):X
#' \item 12 -> Modelo:
#'Z~1+X+I(X^2)+Y+I(Y^2)+X:Y+I(X^2):Y+I(Y^2):X+I(X^2):I(Y^2))
#' }
#' @docType package
#' @name ExpAnalysis3d package
#' @aliases ExpAnalysis3d
#' @author Alcinei Mistico Azevedo: <alcineimistico@@hotmail.com>
#' @references
#' PlayList "Package R: ExpAnalysis3D":
#' <https://www.youtube.com/playlist?list=PLvth1ZcREyK6OUnWVs-hnyVdCB1xuxbVs>
#'
#' Cecon,P.R.;Silva, A.R;  Nascimento, M; Ferreira, A.
#'Metodos Estatisticos - Serie Didatica. Editora UFV. (2012). 229p.
#'(ISBN: 9788572694421)
#'
#'  Hair, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978-0138132637)
NULL
