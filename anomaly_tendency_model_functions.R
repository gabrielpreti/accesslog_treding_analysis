
identifyHistoricalIndexes <- function(data, currentPosition) {
  historicalIndexes = (currentPosition-HISTORY_SIZE-DELAY):(currentPosition-DELAY-1)
  notAlarmingIndexes = which(!data[1:(currentPosition-1), "alarming"]) ;
  return(historicalIndexes[historicalIndexes %in% notAlarmingIndexes])
}
getScore <- function(data, field, currentPosition){  
  analisedIndexes = which(!data[1:(currentPosition-1), "alarming"]) ; #Os índices analisados deveriam ser somente os do histórico. Porém, se eu não colocar o conjunto completo (desde o índice 1) não funciona bem
  return(max(data[analisedIndexes, field], na.rm=TRUE));
  #   return(quantile(scores, probs=c(0.99), names=FALSE, na.rm=TRUE));
}

analiseMetricRegister <- function(metricData, registerIndex) {
  historicalMean      = 0;
  historicalSd        = 0;
  currentMean         = 0;
  currentSd           = 0;
  scoreHistoricalSd   = 0;
  scoreCurrentSd      = 0;
  alarming            = FALSE;
  
  historicalIndexes = identifyHistoricalIndexes(metricData, registerIndex);
  
  historicalMean            = mean(metricData$V1[historicalIndexes]);
  historicalSd              = sd(metricData$V1[historicalIndexes]);
  if(historicalMean==0 && historicalSd==0){
    return(list(historicalMean=historicalMean, historicalSd=historicalSd, currentMean=currentMean, currentSd=currentSd, scoreHistoricalSd=scoreHistoricalSd, scoreCurrentSd=scoreCurrentSd, alarming=alarming));
  }
  
  currentIndexes = (registerIndex-DELAY):registerIndex;
  currentMean             = mean(metricData$V1[currentIndexes]);
  currentSd               = sd(metricData$V1[currentIndexes])
  
  difference        = abs(currentMean - historicalMean);
  scoreHistoricalSd = getScore(metricData, "scoreHistoricalSd", registerIndex);
  scoreCurrentSd    = getScore(metricData, "scoreCurrentSd", registerIndex);
  
  isInTrainingPhase                       = registerIndex <= (HISTORY_SIZE + DELAY + TRAINING_SIZE);        #Só analiso depois que terminar a fase de treinamento
  isHistoricalVariationSmallEnough        = historicalSd/historicalMean < THRESOLD_REL_SD_MEAN; #Se os dados históricos tiverem uma variação muito grande, não tem como analisar.
  isDifferenceGreatherThanCurrentScore    = difference > scoreCurrentSd*currentSd;              #TODO: pensar se esse cara realmente faz sentido.
  isDifferenceGreatherThanHistoricalScore = difference > scoreHistoricalSd*historicalSd;
  
  if(!isInTrainingPhase && isHistoricalVariationSmallEnough && isDifferenceGreatherThanCurrentScore && isDifferenceGreatherThanHistoricalScore){
    if(!metricData[registerIndex-1, "alarming"]) {
      print(paste(metricData[registerIndex, "interval"], metricData$V1[registerIndex], currentMean, currentSd, historicalMean, historicalSd, difference, scoreHistoricalSd, scoreCurrentSd, sep=" | "));
    }
    alarming = TRUE;
  }
  
  scoreHistoricalSd  = difference/historicalSd;
  scoreCurrentSd     = difference/currentSd;
  
  return(list(historicalMean=historicalMean, historicalSd=historicalSd, currentMean=currentMean, currentSd=currentSd, scoreHistoricalSd=scoreHistoricalSd, scoreCurrentSd=scoreCurrentSd, alarming=alarming));
}

analiseMetric = function(metricValue, data){
  metricData  = subset(data, url_return_code==metricValue);
  metricData$alarming = FALSE;
  
  alarms = data.frame();
  for(i in (HISTORY_SIZE+DELAY+1):nrow(metricData))  { 
    analysis = analiseMetricRegister(metricData, i);
    metricData[i, "historicalMean"]     = analysis$historicalMean;
    metricData[i, "historicalSd"]       = analysis$historicalSd;
    metricData[i, "currentMean"]        = analysis$currentMean;
    metricData[i, "currentSd"]          = analysis$currentSd;
    metricData[i, "alarming"]           = analysis$alarming;
    metricData[i, "scoreHistoricalSd"]  = analysis$scoreHistoricalSd
    metricData[i, "scoreCurrentSd"]     = analysis$scoreCurrentSd;
  }
  
  if(length(which(metricData$alarming)) > 0){
    return(metricData);
  }else{
    return(data.frame())
  }
}

generateReport <- function(metricName, alarms, fileName){
  pdf(file=fileName)  
  par(mfrow=c(2, 1))
  par(mar=c(6, 2, 3, 1))
  
  for(alarm in unique(alarms[, metricName])){
    alarmValues = alarms[which(alarms[, metricName]==alarm), ];
    
    plot(rep(1, nrow(alarmValues)), type="n", xlim=c(0, nrow(alarmValues)), ylim=c(0, max(alarmValues$V1)), main=alarm,  xaxt="n", xlab="", ylab="");
    legend("topleft", legend=c("Média hist.", "Desv. padrão hist.", "Média atual", "Lim. sup. alarme", "Lim. inf. alarme"), fill=c("blue", "red", "green", "orange", "yellow"), bty="n", cex=0.5);
    labels=seq(1, nrow(alarmValues), by=nrow(alarmValues)/30);
    axis(1, at=labels, lab=strftime(alarmValues$interval[labels], format="%d/%m %H:%M"), las=2);
    
    for(i in (HISTORY_SIZE+DELAY+1):nrow(alarmValues)){        
      historicalMean  = alarmValues[i, "historicalMean"];
      historicalSd    = alarmValues[i, "historicalSd"];
      currentMean     = alarmValues[i, "currentMean"];
      
      difference = abs(currentMean-historicalMean);
      
      points(i, historicalMean, cex=0.2, col="blue", pch=1);
      lines(x=c(i-1, i), y=c(alarmValues[i-1, "historicalMean"], historicalMean), cex=0.1, col="blue")
      
      points(i, historicalSd, cex=0.2, col="red", pch=1);
      lines(x=c(i-1, i), y=c(alarmValues[i-1, "historicalSd"], historicalSd), cex=0.1, col="red")
      
      if(alarmValues[i, "alarming"]) {
        points(i, currentMean, cex=0.7, col="green", pch=16)
      }else{
        points(i, currentMean, cex=0.2, col="green", pch=1)
      }
      lines(x=c(i-1, i), y=c(alarmValues[i-1, "currentMean"], currentMean), cex=0.1, col="green")
      
      previousSupLimit  = alarmValues[i-1, "historicalMean"]  + (alarmValues[i-2, "scoreHistoricalSd"] * alarmValues[i-1, "historicalSd"]);
      supLimit          = historicalMean                      + (alarmValues[i-1, "scoreHistoricalSd"] * historicalSd);
      previousInfLimit  = alarmValues[i-1, "historicalMean"]  - (alarmValues[i-2, "scoreHistoricalSd"] * alarmValues[i-1, "historicalSd"]);
      infLimit          = historicalMean                      - (alarmValues[i-1, "scoreHistoricalSd"] * historicalSd);
      
      points(i, supLimit, cex=0.05, col="orange", pch=1)
      lines(x=c(i-1, i), y=c(previousSupLimit, supLimit), cex=0.01, col="orange")
      
      points(i, infLimit, cex=0.05, col="black", pch=1)
      lines(x=c(i-1, i), y=c(previousInfLimit, infLimit), cex=0.01, col="yellow")
    }
  }
  dev.off();
}