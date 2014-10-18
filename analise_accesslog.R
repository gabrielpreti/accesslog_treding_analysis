library(stringr)
library(data.table)
library(plyr)
library(doMC)
library(foreach)
library(boot)
library(manipulate)

source("accesslog_process_functions.R");
source("anomaly_tendency_model_functions.R");

log = function(...){
  print(paste(Sys.time(), ": ", ...))
}

echoCommands = FALSE;

registerDoMC(detectCores());
Sys.setlocale(category="LC_TIME", "en_US.UTF-8")
options(echo=echoCommands) # if you want see commands in output file

#############################################
#Leitura dos parâmetros de entrada
############################################
log("Reading input parameters ...")
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

log("Input parameters: accesslogFile=", accesslogFile, " initialTime=", initialTime, " finalTime=", finalTime)


#############################################
#Para execução pelo RStudio
############################################
# accesslogFile = "tests/access_pagseguro.uol.com.br_443.log.201409120200"
# initialTime = strptime(x="10/09/2014 17:30:00", format="%d/%m/%Y %H:%M:%S")
# finalTime = strptime(x="10/09/2014 21:00:00", format="%d/%m/%Y %H:%M:%S")

inicio = Sys.time();

#############################################
#Leitura do acess log
############################################
#setwd("~/Dropbox/projects/analise_accesslog_R/")
log("Cleaning access log ...")
system(paste("perl ./cleanlog.pl", accesslogFile, ">> accesslog_443_cleaned"));

log("Access log cleaned. Reading results ...")
data = read.table(file="accesslog_443_cleaned", sep="\t", allowEscapes=TRUE);
log("Read ", length(data[, 1]), " lines");

#############################################
#Pré-processamento do acess log
############################################
log("Pré-processing data ...")
#parseia data
data$date = strptime(data[, 3], format="%d/%b/%Y:%H:%M:%S")
data = data[, -c(3)]

#nomeia as colunas
names(data) = c("ip", "port", "method", "url", "domain", "return_code", "referrer", "user_agent", "response_time", "date")

#Filtra um período de tempo
# initialTime=strptime(x="10/09/2014 17:30:00", format="%d/%m/%Y %H:%M:%S")
# finalTime=strptime(x="10/09/2014 21:00:00", format="%d/%m/%Y %H:%M:%S")
data = data[data$date>=initialTime & data$date<=finalTime, ];


#identifica o dominio do referrer
data$referrer_domain = str_match(data[, "referrer"], "(http|https)\\://(.*?)/")[, 3] 


#############################################
#Identifica as métricas a serem geradas
############################################
log("Identifing metrics ...")
# input             = list(c("url", "return_code"), c("referrer_domain", "return_code")) # a segunda métrica é muito demorada, não termina nunca
input             = list(c("url", "return_code"))
generatedMetricNames  = c();
for(m in input){
    generatedMetricNames[length(generatedMetricNames)+1] = paste(m[1], m[2], sep="_")
    data[, paste(m[1], m[2], sep="_")] = paste(data[, m[1]], data[, m[2]])
}
log("Metrics identified: ", generatedMetricNames)


#############################################
#Levanta as métricas do accesslog, período a período
############################################
log("Generating metrics ...")
generatedMetrics = list();
uniqueValues = list();
for(m in generatedMetricNames){
  log("Generating metric", m)
  uniqueValues[[m]] = unique(data[!is.na(data[, m]), m])
  generatedMetrics[[paste(m, "responseTime", sep="_")]] = measureByPeriod(.data=data[!is.na(data[, m]), ], .field=m, .periodField="date", .intervalInSeconds=60, .function=meanResponseTime)
  generatedMetrics[[paste(m, "hitsQty", sep="_")]] = measureByPeriod(.data=data[!is.na(data[, m]), ], .field=m, .periodField="date", .intervalInSeconds=60, .function=nrow)
  log("finished with metric", m)
}
log("Metrics generated.")

#############################################
#Analisa as métricas geradas (aqui entra a fase de machine learning, é a inteligência do processo)
############################################
log("Analysing metrics ...")

options(echo=TRUE);
HISTORY_SIZE = 15;
DELAY = 5;
TRAINING_SIZE = 30;
THRESOLD_REL_SD_MEAN = 0.2;
options(echo=echoCommands);

for(metricName in generatedMetricNames){
  log("Analising", metricName, "...");
  
  log("Generating alarms for response time")
  alarms = ldply(.data=uniqueValues[[metricName]], .parallel=TRUE, .fun=analiseMetric, generatedMetrics[[paste(metricName, "responseTime", sep="_")]])
  log("Generating report for response time")
  generateReport(metricName, alarms, fileName=paste("/tmp/report_", metricName, "_responseTime.pdf", sep=""))
  rm(alarms)#pra economizar memória
  
  log("Generating alarms for hits quantity")
  alarms = ldply(.data=uniqueValues[[metricName]], .parallel=TRUE, .fun=analiseMetric, generatedMetrics[[paste(metricName, "hitsQty", sep="_")]])
  log("Generating report for hits quantity")
  generateReport(metricName, alarms, fileName=paste("/tmp/report_", metricName, "_hitsQty.pdf", sep=""))
  
  log("Done with", metricName);
}

fim = Sys.time();
print(paste("Inicio: ", inicio))
print(paste("Fim: ", fim))