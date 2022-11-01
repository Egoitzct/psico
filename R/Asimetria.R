#' Asimetria
#'
#' Una funcion que nos permite sacar el valor de la asimetria, la curtosis y el error estandar de
#' ambas usando funciones que podemos encontrar en los paquetes \code{\link{psych}} y \code{sur}.
#'
#' @param x Grupo de datos numericos.
#'
#' @return El valor de la asimetria y su error estandar
#' @export
#'
#' @importFrom psych skew
#' @importFrom psych kurtosi
#' @importFrom sur se.skew
#' @examples
#' asimetry(c(10,2,35,43), 500)
asimetry <- function(x, n){
  a <- psych::skew(x)
  d <- psych::kurtosi(x)
  b <- sur::se.skew(x)
  c <- se.curt <- 2*b*sqrt(((n^2)-1)/((n-3)*(n+5)))
  print(paste0("La asimetria y la curtosis es ", a," y ", d, ", el error estandar de la asimetria es  ", b, " y el error estandar de la curtosis es ", c))
}
