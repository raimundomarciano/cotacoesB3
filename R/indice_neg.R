# LIQUIDEZ
library(dplyr)
cot19a = cot19 %>% filter(!grepl("^D", especifPapel)) %>%
  filter(!grepl("^CI", especifPapel)) %>%
  filter(!grepl("^CPA", especifPapel)) %>%
  filter(!grepl("^LFT", especifPapel)) %>%
  filter(!grepl("^M1", especifPapel)) %>%
  filter(!grepl("^PCD", especifPapel)) %>%
  filter(!grepl("^WRT", especifPapel)) %>%
  filter(!grepl("^B", especifPapel)) %>%
  filter(!grepl("^IBO", especifPapel)) %>%
  filter(!grepl("^REC", especifPapel)) %>%
  filter(!grepl("^TPR", especifPapel)) %>%
  filter(codBDI %in% c(2,96))

dias = unique(cot19a$dtPregao)
dias= length(dias)

cot19_IN = cot19a %>% filter(codBDI == 2) %>%
  group_by(codNegociacao, nomeResumido) %>%
  summarise(totalTransacoes = sum(qtdeTitulosNegoc),
            totalVolume = sum(volumeNegociado)) %>%
  ungroup() %>%
  mutate(propTransacoes = totalTransacoes / sum(totalTransacoes),
         propVolume = totalVolume / sum(totalVolume)) %>%
  mutate(indice_negoc = ((propTransacoes * (propVolume^2))^(1/3)) / dias)



cot = unique(cot19a$especifPapel)


cot19_IN = cot19a %>% filter(codBDI == 2) %>%
  group_by(dtPregao) %>%
  mutate(propVolume = volumeNegociado / sum(volumeNegociado),
         propTransacoes = qtdeTitulosNegoc / sum(qtdeTitulosNegoc)) %>%
  ungroup() %>%
  group_by(codNegociacao, nomeResumido) %>%
  summarise(indice_negoc = sum((propTransacoes * (propVolume^2))^(1/3))
         ) %>%
  ungroup() %>%
  mutate(IN_perc = round(indice_negoc / sum(indice_negoc)* 100,4)) %>%
  arrange(desc(IN_perc)) %>%
  mutate(IN_perc_cum = cumsum(IN_perc))
