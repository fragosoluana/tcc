#!/usr/bin/env Rscript

#################################################################################################################################
# - Lê arquivo com todas as rotas e sua média de tempo de viagem (esse arquivo deve estar na mesma raiz do script)              #
# - Lê CSVs (que estão em uma pasta - deve estar na mesma raiz do script) que contêm as regras geradas para cada aeroporto      #
# - Verifica se as rotas geradas nas regras realmente existem                                                                   #
# - Verifica se o tempo de atraso é maior do que o tempo de viagem                                                              #
# - Escreve o resultado num CSV. Nele contem o aeroporto de origem e destino e o tempo de atraso que o primeiro influencia no   #
#   segundo                                                                                                                     #
#################################################################################################################################

require(sqldf)
require(stringr)

#Lê o arquivo com a média do tempo das rotas
route <- read.csv("route-time-avg.csv",
                       header = TRUE,
                       quote="\"",
                       stringsAsFactors= TRUE,
                       strip.white = TRUE)

files <- list.files("Sheets", pattern="*.csv", full.names=TRUE)

for (i in seq_along(files)) {
  
  #Lê o csv com as regras de cada aeroporto
  rules <- read.csv(files[i],
                       header = TRUE,
                       quote="\"",
                       stringsAsFactors= TRUE,
                       strip.white = TRUE)
  
  rules <- str_match(rules$rules, ".(SB..)_.*(SB..)_(.).*")
  rules <- data.frame(departure = rules[,2], arrival = rules[,3], delay_time = rules[,4])
  
  route <- data.frame(departure = route$departure, arrival = route$arrival, 
                           travel_time_avg = as.integer(substring(route$travel_time_avg, 1, 2)))
  
  #Compara se o tempo de atraso é maior do que o tempo de viagem e verifica se as rotas geradas nas regras realmente existem
  result <- sqldf('SELECT rules.* 
                     FROM rules 
                     INNER JOIN route ON route.departure = rules.departure 
                     AND route.arrival = rules.arrival 
                     AND route.travel_time_avg < rules.delay_time')
  
  #Ecreve a tabela resultante em um csv
  subdir <- "results"
  
  if (!file.exists(subdir)){
    dir.create(file.path(getwd(), subdir))
  }
  
  out_file <- paste(paste(subdir, "/", sep = ""), substr(files[i], 8, 11), sep = "")
  write.csv(result, file = paste(out_file, ".csv", sep = ""), row.names=FALSE, na="")
}