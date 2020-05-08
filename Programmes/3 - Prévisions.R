library(forecast)
library(patchwork)
data <- readRDS(file = "Rapport/data/donnees.RDS")
data_complet <- readRDS(file = "Rapport/data/donnees_completes.RDS")
x <- data[, "ipi_cl1"]
x_complet <- data_complet[, "ipi_cl1"]
ordres_retenus <- readRDS(file = "Rapport/data/ordres_retenus.RDS")
model_estime <- Arima(x, order = ordres_retenus, include.constant = FALSE)
prev <- forecast(model_estime, h = 2)
prev

#Retrouver les IC : 
sum((res - mean(res))^2) / (length(res) - 2) # sigma2
res <- residuals(model_estime)
prev$mean[1]+sqrt(model_estime$sigma2)*qnorm(1-0.05/2)
prev$mean[2]+sqrt(model_estime$sigma2*(1+(1+model_estime$coef[1])^2))*qnorm(1-0.05/2)
# Graphiques un peu moches à refaire : 
plot(prev)
lines(window(x_complet,start = 2020))

plot(prev, xlim = c(2018,2020.5)) 
lines(window(x_complet,start = c(2019,12)), col = "red")

prev <- forecast(model_estime, h = 2)
prev
# Graphiques un peu moches à refaire : 
plot(prev)
lines(window(x_complet,start = 202))

plot(prev, xlim = c(2018,2021)) 
lines(window(x_complet,start = c(2019,12)), col = "red")

ts2df <- function(data){
	time <- time(data)
	freq <- frequency(data)
	dataGraph <- data.frame(cbind(time, data))
	colnames(dataGraph) <- c("date", "value")
	dataGraph
}
data_etud <- window(x, start = c(2016,12))
data_fin <- window(x_complet, start = c(2019,12))
prevs <- ts(c(tail(prev$x,1), prev$mean),
			start = end(prev$x),
			frequency = 12)
dataGraph <- ts2df(data_etud)
prevs_df <- ts2df(prevs)
data_fin_df <- ts2df(data_fin)

ic_df <- ts2df(ts.union(prev$mean,prev$lower[,"95%"],prev$upper[,"95%"]))
colnames(ic_df) <- c("x","level", "ymin", "ymax")

(c(96.13005,111.1733) - 102.66)*100/102.66 
summary(AQLTools::ev(x))
dataGraph
prevs_df
graph_prev(x, x_complet, prev, n_xlabel = 12)

plot(window(x_complet,start = 2000))
n_x_labels <- 12
library(ggplot2)
p <- ggplot(data = dataGraph, aes(x = date, y = value))+
	geom_line(size=0.70) +
	geom_ribbon(mapping = aes(x = x, ymin = ymin, ymax = ymax), 
				data = ic_df, inherit.aes = FALSE, alpha = 0.2)+
	geom_point(data = data_fin_df, show.legend = TRUE)+
	geom_line(data = data_fin_df, size=0.7,
			  linetype = "longdash", show.legend = TRUE)+
	geom_point(data = prevs_df, color = "red", show.legend = TRUE)+
	geom_line(data = prevs_df, size=0.7, color = "red",
			  linetype = "longdash", show.legend = TRUE)+
	scale_x_continuous(breaks = scales::pretty_breaks(n = n_x_labels),
					   labels = function(x) AQLTools:::creation_x_label(x, x_lab_month = TRUE))+
	AQLTools:::theme_aqltools() +
	labs(title = NULL,
		 x = NULL, y = NULL) 
p

p <- ggplot(data = dataGraph, aes(x = date, y = value))+
	geom_line(size=0.70, aes(color = "ipi")) +
	geom_ribbon(mapping = aes(x = x, ymin = ymin, ymax = ymax), 
				data = ic_df, inherit.aes = FALSE, alpha = 0.2, fill = "black")+
	geom_point(data = data_fin_df, aes(color = "ipireel"))+
	geom_line(data = data_fin_df, size=0.7,
			  linetype = "longdash", aes(color = "ipireel"))+
	geom_point(data = prevs_df, aes(color = "prev"))+
	geom_line(data = prevs_df, size=0.7,
			  linetype = "longdash", aes(color = "prev"))+
	scale_x_continuous(breaks = scales::pretty_breaks(n = n_x_labels),
					   labels = function(x) AQLTools:::creation_x_label(x, x_lab_month = TRUE))+
	AQLTools:::theme_aqltools() +
	labs(title = NULL,
		 x = NULL, y = NULL)  +
	geom_point(aes(color = "ic", shape = NA, fill = "ic"), alpha = 0.2) +
	scale_colour_manual(name = "Légende",
						breaks = c("ipi", "ipireel", "prev", "ic"),
						labels = c("IPI", "IPI observé", "IPI prévu ARIMA(0,1,1)", "IC95%"),
						values = c("black", "black", "red", "black")) +   
	guides(color = guide_legend(#keywidth = 2, keyheight = 1,
								override.aes = list(shape = c(NA,19,19,NA),
													linetype = c(1,6,6,NA),
													fill = c("white","white","red","red"),
													alpha = 1)))
p
?guide_legend
# marche mais avec legende différente



graph_prev <- function(x, x_complet, prev, n_x_label = 12){
	
	ts2df <- function(data){
		time <- time(data)
		freq <- frequency(data)
		dataGraph <- data.frame(cbind(time, data))
		colnames(dataGraph) <- c("date", "value")
		dataGraph
	}
	data_etud <- window(x, start = c(2016,12))
	data_fin <- window(x_complet, start = c(2019,12))
	prevs <- ts(c(tail(prev$x,1), prev$mean),
				start = end(prev$x),
				frequency = 12)
	dataGraph <- ts2df(data_etud)
	prevs_df <- ts2df(prevs)
	data_fin_df <- ts2df(data_fin)
	
	ic_df <- ts2df(ts.union(prev$mean,prev$lower[,"95%"],prev$upper[,"95%"]))
	colnames(ic_df) <- c("x","level", "ymin", "ymax")
	
	
	p <- ggplot(data = dataGraph, aes(x = date, y = value))+
		geom_line(size=0.70, aes(color = "ipi")) +
		geom_ribbon(mapping = aes(x = x, ymin = ymin, ymax = ymax, fill = "ic"),
					data = ic_df, inherit.aes = FALSE, alpha = 0.2)+
		geom_point(data = data_fin_df, aes(color = "ipireel"))+
		geom_line(data = data_fin_df, size=0.7,
				  linetype = "longdash", aes(color = "ipireel"))+
		geom_point(data = prevs_df, aes(color = "prev"))+
		geom_line(data = prevs_df, size=0.7,
				  linetype = "longdash", aes(color = "prev"))+
		scale_x_continuous(breaks = scales::pretty_breaks(n = n_x_label),
						   labels = function(x) AQLTools:::creation_x_label(x, x_lab_month = TRUE))+
		AQLTools:::theme_aqltools() +
		labs(title = NULL,
			 x = NULL, y = NULL)  +
		scale_colour_manual(name = "Légende",
							breaks = c("ipi", "ipireel", "prev"),
							labels = c("IPI", "IPI observé", "IPI prévu ARIMA(0,1,1)"),
							values = c("black", "black", "red")) +
		scale_fill_manual(name = "Légende",
						  breaks = "ic",
						  labels = c("IC(95%)"),
						  values = c("black")) +
		guides(color = guide_legend(keywidth = 2, keyheight = 1,
									override.aes = list(shape = c(NA,19,19),
														linetype = c(1,6,6))))
	p
	
}

