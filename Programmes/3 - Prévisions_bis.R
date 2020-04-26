library(forecast)
library(patchwork)
data <- readRDS(file = "Rapport/data/bis_donnees.RDS")
data_complet <- readRDS(file = "Rapport/data/bis_donnees_completes.RDS")
x <- data[, "ipi_cl1"]
x_complet <- data_complet[, "ipi_cl1"]
ordres_retenus <- readRDS(file = "Rapport/data/bis_ordres_retenus.RDS")
model_estime <- Arima(x, order = ordres_retenus)
model_estime
prev <- forecast(model_estime, h = 2)
prev
# Graphiques un peu moches à refaire : 
plot(prev)
lines(window(x_complet,start = 2020))

plot(prev, xlim = c(2018,2020.5)) 
lines(window(x_complet,start = c(2019,12)), col = "red")

ts2df <- function(data){
	time <- time(data)
	freq <- frequency(data)
	dataGraph <- data.frame(cbind(time, data))
	colnames(dataGraph) <- c("dates", "x")
	dataGraph
}
data_etud <- window(x, start = c(2016,12))
data_fin <- window(x_complet, start = c(2019,12))
prevs <- ts(c(tail(prev$x,1), prev$mean),
			start = end(prev$x),
			frequency = 12)
time <- time(data)
freq <- frequency(data)
dataGraph <- data.frame(cbind(time, data))
prevs_df <- ts2df(prevs)


