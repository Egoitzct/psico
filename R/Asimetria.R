#' Asimetria
#'
#' Una funcion que nos permite sacar el valor de la asimetria y su error estandar
#' usando funciones que podemos encontrar en los paquetes \code{\link{psych}} y \code{sur}.
#'
#' @param x Grupo de datos numericos.
#'
#' @return El valor de la asimetria y su error estandar
#' @export
#'
#' @importFrom psych skew
#' @importFrom sur se.skew
#' @examples
#' asimetry(c(10,2,35,43))
asimetry <- function(x){
  a <- psych::skew(x)
  b <- sur::se.skew(x)
  print(paste0("La asimetria es ", a, " y su error estandar es  ", b, "."))
}
