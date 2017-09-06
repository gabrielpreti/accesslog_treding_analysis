require(plyr);

measureByPeriod = function(.data, .field, .periodField="date", .intervalInSeconds, .function){
  #identifica o intervalo em que cada registro se encontra.
  cutingPoints <- seq(min(.data[, .periodField]), max(.data[, .periodField])+.intervalInSeconds, by=.intervalInSeconds);
  .data$interval=cut(.data[, .periodField], breaks=cutingPoints);
  
  #Identifica o índice do campo do tipo data, pois a função ddply não trabalha bem com campos do tipo data.
  periodFieldIndex=which(names(.data)==.periodField);
  
  #Gera um dataframe temporário, agregando os resultados. O formato gerado será:
  #dominio1  intervalo1  valor
  #dominio1  intervalo2  valor
  #dominio2  intervalo1  valor
  #dominio2  intervalo2  valor
  temp=ddply(.data[, -c(periodFieldIndex)], c(.field, "interval"), .function, .drop=FALSE, .parallel=TRUE);
  return(temp)
}

meanResponseTime = function(x){
  if(length(x[,"response_time"])>0){
    return(mean(x[,"response_time"]));
  }else{
    return(0);
  }
}