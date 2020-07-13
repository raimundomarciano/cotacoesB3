#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

lerCotacoesB3 <- function(dt, out = "./Downloads", per = "anual"){
#' Cotações da bolsa de valores brasileira - B3 SA
#'
#' Faz o download das cotações referentes a um ou mais dias. Usa o FTP da B3.
#' Dados anuais (per = "anual") estão disponíveis desde 1986
#' A partir de 2014, também há dados diários (usar per = "2016")
#' Observar que as ações são negociadas apenas em dias úteis. Para gerar
#' um calendário de dias úteis, pode ser usada a função calendarioBR.
#' @param dt Data. Informar um dia útil na forma "AAAA-MM-DD".
#' @param dt Ano. Informar um ano na forma AAAA. Aspas opcionais.
#' @param out Pasta onde os arquivos baixados serão salvos. Não incluir uma "/" ao final.
#' @param per Periodicidade. Pode ser informado "anual" ou "diario".
#' @examples
#' lerCotacoesB3("2019", per = "anual")
#' lerCotacoesB3("2020-07-10", per = "diario")

  if (per == 'anual'){
    dt <- as.character(dt)
    fileList <- list.files(out)

    url <- paste0('http://bvmf.bmfbovespa.com.br/InstDados/SerHist/COTAHIST_A',
                  dt,'.ZIP')
    filename <- paste0(out,'/COTAHIST_A',dt,'.ZIP')
    zipExists <- paste0('COTAHIST_A',dt,'.ZIP') %in% fileList
    txtExists <- paste0('COTAHIST_A',dt,'.TXT') %in% fileList
    oldExists <- paste0('COTAHIST.A',dt) %in% fileList
  }

  if (per == 'diario'){
    url <- format(as.Date(dt, format = "%Y-%m-%d"),
                  "http://bvmf.bmfbovespa.com.br/InstDados/SerHist/COTAHIST_D%d%m%Y.ZIP")
    filename <- paste0(out, '/COTAHIST_D', filename,'.ZIP')

    dtFormatted <- format(as.Date(dt, format = "%Y-%m-%d"), "%d%m%Y")
    zipExists <- paste0('COTAHIST_D',dtFormatted,'.ZIP') %in% fileList
    txtExists <- paste0('COTAHIST_D',dtFormatted,'.TXT') %in% fileList
    oldExists <- FALSE
  }

  if (!(zipExists | txtExists | oldExists)){
    download.file(url = url, destfile = filename, mode = "wb")
    files <- unzip(zipfile = filename, exdir = out)

  } else if (zipExists & !txtExists & !oldExists){

    files <- unzip(zipfile = filename, exdir = out)

  } else if (txtExists) {

      if (per == "anual"){ files <- files <- paste0(out, '/COTAHIST_A', dt,'.TXT') }
      if (per == "diario") {files <- paste0(out, '/COTAHIST_D', dtFormatted,'.TXT') }

  } else if (per == "anual" & oldExists){

      files <- paste0(out, '/COTAHIST.A', dt)

  } else {

      print("Formato não reconhecido. Por favor, reportar o erro.")
      return

  }

  layoutCotacoes <- read.csv2("layoutCotacoesHistoricas.csv", stringsAsFactors = FALSE)
  cotacao <- read.fwf(file = files, widths = layoutCotacoes$tamanho,
                      col.names = layoutCotacoes$campo, stringsAsFactors = FALSE,
                      strip.white = TRUE, skip = 1)
  cotacao <- cotacao[-nrow(cotacao),]
  cotacao$dtPregao <- as.Date(cotacao$dtPregao, "%Y%m%d")
  cotacao$dtVencimento <- as.Date(as.character(cotacao$dtVencimento), "%Y%m%d")

  cols = c("precoAbertura","precoMax","precoMinimo",
           "precoMedio","precoUltimo", "precoOFC",
           "precoOFV")

  for (coluna in cols){
    cotacao[,coluna] <- cotacao[,coluna] / 100
  }

  return(cotacao)
}
