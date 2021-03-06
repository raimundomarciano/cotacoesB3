% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{lerCotacoesB3}
\alias{lerCotacoesB3}
\title{Cotações da bolsa de valores brasileira - B3 SA}
\usage{
lerCotacoesB3(dt, out = "./Downloads", per = "anual")
}
\arguments{
\item{dt}{Ano. Informar um ano na forma AAAA. Aspas opcionais.}

\item{out}{Pasta onde os arquivos baixados serão salvos. Não incluir uma "/" ao final.}

\item{per}{Periodicidade. Pode ser informado "anual" ou "diario".}
}
\description{
Faz o download das cotações referentes a um ou mais dias. Usa o FTP da B3.
Dados anuais (per = "anual") estão disponíveis desde 1986
A partir de 2014, também há dados diários (usar per = "2016")
Observar que as ações são negociadas apenas em dias úteis. Para gerar
um calendário de dias úteis, pode ser usada a função calendarioBR.
}
\examples{
lerCotacoesB3("2019", per = "anual")
lerCotacoesB3("2020-07-10", per = "diario")
}
