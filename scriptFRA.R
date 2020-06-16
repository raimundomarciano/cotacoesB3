lerCotacoesB3 <- function(dt){
  dt <- as.Date(dt)
  url <- format(dt, "http://bvmf.bmfbovespa.com.br/InstDados/SerHist/COTAHIST_D%d%m%Y.ZIP")
  filename <- format(dt, "Downloads/COTAHIST_D%d%m%Y.ZIP")
  download.file(url = url, destfile = filename, mode = "wb")
  files <- unzip(zipfile = filename, exdir = "Downloads")
  
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
