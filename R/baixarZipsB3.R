baixarZipsB3 <- function(ano, out = "./Downloads", integrity = TRUE){
#' Download dos zips com as cotações diárias de um ano.
#'
#' Faz o download dos arquivos zip, sem processá-los. Para extrair e carregar o conteúdo
#' para o R, use a função lerCotacoesB3. O FTP da B3 inclui cotações a partir de 1986.
#' Este pacote ainda não inclui um teste de integridade. É provável que arquivos com menos de 5mb não
#' foram baixados corretamente.
#'
#' @param ano Um inteiro representando o ano.
#' @param out o caminho onde serão baixados os arquivos
#'
#' @examples
#' baixarZipsB3(2018)
#' for (ano in 1990:1999) { baixarZipsB3(ano) }
  dt <- as.character(ano)
  fileList <- list.files(out)

  url <- paste0('http://bvmf.bmfbovespa.com.br/InstDados/SerHist/COTAHIST_A',
                dt,'.ZIP')
  filename <- paste0(out,'/COTAHIST_A',dt,'.ZIP')
  zipExists <- paste0('COTAHIST_A',dt,'.ZIP') %in% fileList

  if (!zipExists){
    download.file(url = url, destfile = filename, mode = "wb")
  }
  if (zipExists){
    print(paste0('Cotações do ano ', ano, ' já foram baixadas anteriormente.'))
  }

  if(integrity == TRUE){
    outzip <- paste0(out, '/COTAHIST_A', ano, '.ZIP')
    works <- unzip(zipfile = outzip, exdir = out)

    if (length(works) == 0){
      print(paste0('Arquivo referente ao ano ', ano,
                   ' está corrompido. Delete e baixe novamente.'))
    } else {
      fileTXT <- paste0(out, '/COTAHIST_A', ano, '.TXT')
      fileOLD <- paste0(out, '/COTAHIST.A', ano)
      if (file.exists(fileTXT)) {
        file.remove(fileTXT)
        #print(paste0('Arquivo', fileTXT, ' removido.'))
      }

      if (file.exists(fileOLD)) {
        file.remove(fileOLD)
        #print(paste0('Arquivo', fileOLD, ' removido.'))
        }
    }

  }


}
