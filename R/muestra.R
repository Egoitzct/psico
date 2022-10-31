#' Tamano muestral
#'
#' @param c si la variable es cuantitativa o cualitativa (asegurarse de escribir en minusculas)
#' @param n el numero de personas en la poblacion
#' @param t error relacionado con alfa
#' @param s desviacion tipica
#' @param d error maximo
#' @param l puntuacion de la escala lickert (si es del uno al diez, sera 10)
#' @param p relacionado con la varianza en variables cualitativas
#' @param q relacionado con la varianza en variables cualitativas
#' @param g estimacion de las respuestas que se van a obtener (en decimales, si es 65%, sera 0.65)
#'
#' @return El numero de personas que seran necesarias para la investigacion (teniendo en cuenta el primer ajuste (si supera
#' al 5% de la poblacion) y el segundo (teniendo en cuenta el porcentaje de respuestas))
#' @export
#'
#' @examples
#' muestra("cuantitativa", 5000, 1.96, 2, 0.03, 7, 0, 0, 0.7)
muestra <- function(c, n, t, s, d, l, p, q, g){
  if(c == "cuantitativa"){
    resultado1 <- ((((t^2)*(s^2))/((d*l)^2)))

    if(resultado1 < (n*0.05)){
      if(g==0){
        cat("El tamaño de muestra requerido es:", resultado1)
      }
      else{
        gente1 <- (resultado1/g)
        cat("El tamaño de muestra requerido es (teniendo en cuenta la gente que vas a perder:D):", gente1)
      }
    }
    else{
      ajust1 <- ((resultado1)/(1+(resultado1/n)))
      if(g==0){
        cat("El tamaño de muestra requerido es (con el primer ajuste):", ajust1)
      }
      else{
        gente3 <- (ajust1/g)
        cat("El tamaño de muestra requerido es (teniendo en cuenta la gente que vas a perder y el primer ajuste :D):", gente3)
      }
    }
  }
  else{
    resultado2 <- (((t^2)*(p*q))/(d^2))

    if(resultado2 < (n*0.05)){
      if(g==0){
        cat("El tamaño de muestra requerido es:", resultado2)
      }
      else{
        gente2 <- (resultado2/g)
        cat("El tamaño de muestra requerido es (teniendo en cuenta la gente que vas a perder:D)", gente2)
      }
    }
    else{
      ajust2 <- ((resultado2)/(1+(resultado2/n)))
      if(g==0){
        cat("El tamaño de muestra requerido es (con el primer ajuste):", ajust2)
      }
      else{
        gente4 <- (ajust2/g)
        cat("El tamaño de muestra requerido es (teniendo en cuenta la gente que vas a perder y el primer ajuste:D):", gente4)
      }
    }
  }
}
