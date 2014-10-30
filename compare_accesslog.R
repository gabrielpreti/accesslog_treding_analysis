library(plyr)
library(doMC)
library(stringr)

source("accesslog_process_functions.R");

logger = function(...){
  print(paste(Sys.time(), ": ", ...))
}

echoCommands = FALSE;

registerDoMC(detectCores());
Sys.setlocale(category="LC_TIME", "en_US.UTF-8")
options(echo=echoCommands) # if you want see commands in output file

#############################################
#Leitura dos parâmetros de entrada
############################################
logger("Reading input parameters ...")
args <- commandArgs(trailingOnly = TRUE)
if(length(args)<3){
  stop("Missing parameters");
}

accesslogFile = args[1];
initialTime=strptime(x=args[2], format="%d/%m/%Y %H:%M:%S")
if(is.na(initialTime)){
  stop("Invalid initial time: ", args[2]);
}
finalTime=strptime(x=args[3], format="%d/%m/%Y %H:%M:%S")
if(is.na(finalTime)){
  stop("Invalid final time: ", args[3]);
}

logger("Input parameters: accesslogFile=", accesslogFile, " initialTime=", initialTime, " finalTime=", finalTime)


#############################################
#Para execução pelo RStudio
############################################
baseAccesslogFile = "tests/access_pagseguro.uol.com.br_443.log.201409040200"
actualAccesslogFile = "tests/access_pagseguro.uol.com.br_443.log.201409120200"
initialTime = strptime(x="10/09/2014 20:30:00", format="%d/%m/%Y %H:%M:%S")
finalTime = strptime(x="10/09/2014 21:00:00", format="%d/%m/%Y %H:%M:%S")

inicio = Sys.time();

#############################################
#Leitura dos acess logs
############################################
#setwd("~/Dropbox/projects/analise_accesslog_R/")
logger("Cleaning access logs ...")
system(paste("perl ./cleanlog.pl", baseAccesslogFile, ">> accesslog_443_base_cleaned"));
system(paste("perl ./cleanlog.pl", actualAccesslogFile, ">> accesslog_443_actual_cleaned"));

logger("Access logs cleaned. Reading results ...")
baseData = read.table(file="accesslog_443_base_cleaned", sep="\t", allowEscapes=TRUE);
actualData = read.table(file="accesslog_443_actual_cleaned", sep="\t", allowEscapes=TRUE);
logger("Read ", length(baseData[, 1]), " and ", length(actualData[, 1]), " lines for base and actual log respectively");

#############################################
#Pré-processamento do acess log
############################################
logger("Pré-processing data ...")
#parseia data
baseData$date = strptime(baseData[, 3], format="%d/%b/%Y:%H:%M:%S")
baseData = baseData[, -c(3)]
actualData$date = strptime(actualData[, 3], format="%d/%b/%Y:%H:%M:%S")
actualData = actualData[, -c(3)]

#nomeia as colunas
names(baseData) = c("ip", "port", "method", "url", "domain", "return_code", "referrer", "user_agent", "response_time", "date")
names(actualData) = c("ip", "port", "method", "url", "domain", "return_code", "referrer", "user_agent", "response_time", "date")

#Filtra um período de tempo
baseData = baseData[baseData$date>=initialTime & baseData$date<=finalTime, ];
actualData = actualData[actualData$date>=initialTime & actualData$date<=finalTime, ];


#identifica o dominio do referrer
baseData$referrer_domain = str_match(baseData[, "referrer"], "(http|https)\\://(.*?)/")[, 3] 
actualData$referrer_domain = str_match(actualData[, "referrer"], "(http|https)\\://(.*?)/")[, 3] 


#############################################
#Identifica as métricas a serem geradas
############################################
logger("Identifing metrics ...")
# input             = list(c("url", "return_code"), c("referrer_domain", "return_code")) # a segunda métrica é muito demorada, não termina nunca
input             = list(c("url", "return_code"))
generatedMetricNames  = c();
for(m in input){
  generatedMetricNames[length(generatedMetricNames)+1] = paste(m[1], m[2], sep="_")
  baseData[, paste(m[1], m[2], sep="_")] = paste(baseData[, m[1]], baseData[, m[2]])
  actualData[, paste(m[1], m[2], sep="_")] = paste(actualData[, m[1]], actualData[, m[2]])
}
logger("Metrics identified: ", generatedMetricNames)


#############################################
#Levanta as métricas do accesslog, período a período
############################################
logger("Generating metrics ...")
baseMetricValues = list();
actualMetricValues = list();
baseGeneratedMetrics = list();
actualGeneratedMetrics = list();
for(m in generatedMetricNames){
  logger("Generating metric", m)
  baseMetricValues[[m]] = unique(baseData[!is.na(baseData[, m]), m])
  actualMetricValues[[m]] = unique(actualData[!is.na(actualData[, m]), m])
  
  baseGeneratedMetrics[[paste(m, "responseTime", sep="_")]] = measureByPeriod(.data=baseData[!is.na(baseData[, m]), ], .field=m, .periodField="date", .intervalInSeconds=60, .function=meanResponseTime)
  baseGeneratedMetrics[[paste(m, "hitsQty", sep="_")]] = measureByPeriod(.data=baseData[!is.na(baseData[, m]), ], .field=m, .periodField="date", .intervalInSeconds=60, .function=nrow)
  
  actualGeneratedMetrics[[paste(m, "responseTime", sep="_")]] = measureByPeriod(.data=actualData[!is.na(actualData[, m]), ], .field=m, .periodField="date", .intervalInSeconds=60, .function=meanResponseTime)
  actualGeneratedMetrics[[paste(m, "hitsQty", sep="_")]] = measureByPeriod(.data=actualData[!is.na(actualData[, m]), ], .field=m, .periodField="date", .intervalInSeconds=60, .function=nrow)
  
  logger("finished with metric", m)
}
logger("Metrics generated.")


#############################################
#Analisa as métricas geradas
############################################
for(metricName in generatedMetricNames){
  
  #primeiro analisa as métricas comuns, ou seja, que aparecem em ambos os conjuntos de dados
  uniqueValues=intersect(baseMetricValues[[metricName]], actualMetricValues[[metricName]])
  
  ldply(.data=uniqueValues, .parallel=FALSE, .fun=analiseDifference, metricName, baseGeneratedMetrics[[paste(metricName, "responseTime", sep="_")]], actualGeneratedMetrics[[paste(metricName, "responseTime", sep="_")]])
  
  #Próximos passos
    # - Baixar o accesslog de 0209
    # - Fixar a url pagseguro.uol.com.br/transaction/search.jhtml (uniqueValues=c("pagseguro.uol.com.br/transaction/search.jhtml 200"))
    # - Plotar gráficos com as diferenças 
}

analiseDifference = function(metricValue, metricName, baseData, actualData){
  baseMetrics  =   baseData[which(baseData[, metricName]==metricValue), ];
  actualMetrics  = actualData[which(actualData[, metricName]==metricValue), ];
  
  for(i in 1:nrow(actualMetrics))  { 
    print(actualMetrics[i, "V1"] - baseMetrics[i, "V1"])
  }
}