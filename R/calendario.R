calendarioBR <- function(dataInicio, dataFim){
#' Dias úteis Brasil
#'
#' Constroi um calendário de dias úteis.São necessárias a data inicial
#' (AAAA-MM-DD) e a data final (AAAA-MM-DD)
#' Retorna um vetor com as datas.
#' @param dataInicio data no formato "AAAA-MM-DD"
#' @param dataFim data no formato "AAAA-MM-DD"
#' @return vetor de datas no formato "AAAA-MM-DD"
#' @examples
#' calendarioBR("2019-01-01", "2019-12,31")
#' calendarioBR("2020-06-10", "2020-06.25")

  library(bizdays)

  create.calendar(name = "ANBIMA",
                  holidays = holidaysANBIMA,
                  weekdays = c("saturday", "sunday"))

  datas <- bizseq(from = dataInicio, to = dataFim, cal = "ANBIMA")

  return(datas)

}
