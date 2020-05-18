library(urca)
library(fUnitRoots)
# devtools::install_github("aqlt/AQLTools")
library(AQLTools) # utilisé pour tracer les séries
library(patchwork) # pour mettre à coté deux graphiques ggplot2

data <- readRDS(file = "data/donnees.RDS")
# data <- ts(read.csv("data/donnees.csv")[,-1],
# 		   start = 2010, frequency = 12)


x <- data[, "ipi_cl1"]
p1 <- AQLTools::graph_ts(window(x,
								start = c(2009,10),
								end = c(2020,2),
								extend = TRUE), x_lab = "Dates", y_lab = NULL,
						 titre = "IPI-CL1 (sans traitement)", n_xlabel = 6)
p1

summary(lm(x ~ time(x)))

# Même si on observe une tendance dans la régression de la série
# par rapport au temps, étant donné la rupture de tendance, nous 
# considérons, comme dans les TD, qu'il y a ici pas de tendance et
# une moyenne non nulle. 
# => On fait le test ADF AVEC constante et SANS tendance
# Pour que le test soit valide il faut rajouter des retards :
# On fait donc le test jusqu'à ce que les résidus du modèles de "ADF" soient bons :
# que les résidus soient indépendants (on ne veut plus d'endogénéité dû aux variables omises)
lb_test <- function(x, lag_max = 24, fitdf = 0){
	t(sapply(seq_len(lag_max),function(l){
		if(l <= fitdf){
			b <- list(statistic = NA, p.value = NA)
		}else{
			b <- Box.test(x,"Ljung-Box",lag = l,
						  fitdf = fitdf
			)
		}
		data.frame(lag = l,
				   b$statistic,
				   b$p.value
		)
	}))
}# ces deux fonctions sont équivalentes : en haut la fonction AQLT, en bas TD
Qtests <- function(series, k = 24, fitdf=0) {
	pvals <- apply(matrix(1:k), 1, FUN=function(l) {
		pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value 
		return(c("lag"=l,"pval"=pval))
	})
	return(t(pvals))
}
adfTest_valid <- function(series, kmax,type){
	# tests ADF jusqu’à ce que les résidus ne soient pas autocorrélés
	k <- 0
	noautocorr <- 0
	while (noautocorr==0){
		cat(paste0("ADF with ",k, " lags: residuals OK? "))
		adf <- adfTest(series,lags=k,type=type)
		pvals <- Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2] 
		if (sum(pvals<0.05,na.rm=T) == 0) {
			noautocorr <- 1; cat("OK \n")
		} else cat("nope \n")
		k <- k + 1
	}
	return(adf)
}

# On trouve un lag de deux :
adfTest_valid(x, kmax = 20, type = "c") #juste constante

adf <- adfTest(x, type = "c",lags = 2) # juste constante et pas de tendance
adf # on ne rejette pas : série non stationnaire avec une racine unitaire

# vérification tests indépendance
lb_test(adf@test$lm$residuals, fitdf=length(adf@test$lm$coefficients)) 

# PP et kpss donnent des résultats similaires
PP.test(x) # il y a une racine unitaire
tseries::kpss.test(x) # série non stationnaire


# On différentie la série pour la stationnariser :
x_st <- diff(x, 1)
AQLTools::graph_ts(window(x_st,
						  start = c(2009,10),
						  end = c(2020,2),
						  extend = TRUE), x_lab = "Dates", y_lab = NULL,
				   titre = "IPI-CL1 différenciée", n_xlabel = 12)
summary(lm(x_st ~ time(x_st)))

# Série qui parait stationnaire, sans tendance ni constante
# On le vérifie avec un test ADF
adfTest_valid(x_st, kmax = 24, type = "nc")

# Il faut donc utiliser un retard
adf <- adfTest(x_st, type = "nc",lags = 1)
adf # on rejette : série stationnaire.

PP.test(x_st) # vérifié avec test de Phillips-Perron
tseries::kpss.test(x_st) # vérifié avec KPSS

series_a_tracer <- ts.union(x, x_st)
p2 <- AQLTools::graph_ts(window(x_st,
								start = c(2009,10),
								end = c(2020,2),
								extend = TRUE), x_lab = "Dates", y_lab = NULL,
						 titre = "IPI-CL1 (série différenciée)", n_xlabel = 6)
p1 + p2

saveRDS(x_st, file = "data/x_st.RDS")

