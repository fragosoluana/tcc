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
files <- list.files("rules", pattern="*.csv", full.names=TRUE)

route <- read.csv("route-time-avg.csv",
                  header = TRUE,
                  quote="\"",
                  stringsAsFactors= TRUE,
                  strip.white = TRUE)

route <- data.frame(departure = route$departure, arrival = route$arrival, 
                    travel_time_avg = as.integer(substring(route$travel_time_avg, 1, 2)))

for (i in seq_along(files)) {
  #Lê o csv com as regras de cada aeroporto
  d_rules <- read.csv(files[i],
                      header = TRUE,
                      quote="\"",
                      stringsAsFactors= TRUE,
                      strip.white = TRUE)
  
  re <- str_match(d_rules$rules, ".(SB..)_.=(.*)......(SB..)_(.)=(.*).")
  
  d_rules <- data.frame(departure = re[,2], d_intensity = re[,3], arrival = re[,4], a_intensity = re[,6], 
                        delay_time = re[,5], lift = d_rules$lift)
  
  d_rules <- sqldf('SELECT d_rules.* 
                  FROM d_rules 
                  INNER JOIN route 
                  ON route.departure = d_rules.departure 
                  AND route.arrival = d_rules.arrival 
                  AND route.travel_time_avg < d_rules.delay_time
                  AND d_rules.lift > 1')
  
  result <- data.frame(departure = character(), d_intensity = character(), arrival = character(), a_intensity = character(), 
                       delay_time = character(), lift = character())
  
  arrivals <- unique(d_rules$arrival)
  if(length(arrivals) != 0) {
    for (i in seq_along(arrivals)) {
      a_rules <- read.csv(paste("rules/", paste(arrivals[i], ".csv", sep=""), sep=""),
                          header = TRUE,
                          quote="\"",
                          stringsAsFactors= TRUE,
                          strip.white = TRUE)
      
      re <- str_match(a_rules$rules, ".(SB..)_.=(.*)......(SB..)_(.)=(.*).")
      
      a_rules <- data.frame(departure = re[,2], d_intensity = re[,3], arrival = re[,4], a_intensity = re[,6], 
                            delay_time = re[,5], lift = a_rules$lift)
      
      autolift <- sqldf('SELECT a_rules.* 
                      FROM a_rules 
                      WHERE arrival = departure
                      AND d_intensity = "high"
                      AND a_intensity = "high"
                      AND lift > 1')
      
      diff <- setdiff(c(1, 2, 3, 4), autolift$delay_time)
      if(length(diff) != 0) {
        for (i in seq_along(diff)) {
          autolift <- rbind(autolift, data.frame(departure = a_rules[1,1], d_intensity = "high", arrival = a_rules[1,1], 
                                                 a_intensity = "high", delay_time = diff[i], lift = 0))
        }
      }
      
      result <- rbind(result, sqldf('SELECT d_rules.* 
                    FROM d_rules 
                    INNER JOIN autolift
                    ON d_rules.arrival = autolift.arrival
                    AND d_rules.delay_time = autolift.delay_time
                    AND d_rules.lift > autolift.lift'))
    }	
  }
  
  if(length(result$departure) != 0) {
    #Ecreve a tabela resultante em um csv
    subdir <- "results"
    
    if (!file.exists(subdir)){
      dir.create(file.path(getwd(), subdir))
    }
    
    out_file <- paste(paste(subdir, "/", sep = ""), result[1,1], sep = "")
    write.csv(result, file = paste(out_file, ".csv", sep = ""), row.names=FALSE, na="")
  }
}