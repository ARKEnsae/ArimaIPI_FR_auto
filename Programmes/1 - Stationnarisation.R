library(urca)
library(fUnitRoots)
# devtools::install_github("aqlt/AQLTools")
library(AQLTools) # utiliser pour tracer les séries
library(patchwork)

data <- readRDS(file = "data/donnees.RDS")
# data <- ts(read.csv("data/donnees.csv")[,-1],
# 		   start = 2010, frequency = 12)


x <- data[,"ipi_cl1"]
p1 <- AQLTools::graph_ts(window(x,
								start = c(2009,10),
								end = c(2020,2),
								extend = TRUE), x_lab = "Dates", y_lab = NULL,
						 titre = "IPI-CL1 (sans traitement)", n_xlabel = 6)
p1
t1 <- time(x)*(time(x)<2013)
t2 <- time(x)*(time(x)>=2013)
summary(lm(x ~ time(x)+t2))

# Il y a une tendance assez nette et pas de moyenne : 
# on peut faire le test ADF avec constante et tendance
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
}# ces deux fonctions sont équivalents : en haut la fonction AQLT, en bas Trinh
Qtests <- function(series, k = 24, fitdf=0) {
	pvals <- apply(matrix(1:k), 1, FUN=function(l) {
		pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value 
		return(c("lag"=l,"pval"=pval))
	})
	return(t(pvals))
}
adfTest_valid <- function(series, kmax,type){ #tests ADF jusqu’`a des r ́esidus non autocorr ́el ́
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

# On trouve un lag de trois :
adfTest_valid(x, kmax = 20, type = "ct")
adfTest_valid(x, kmax = 20, type = "c")#juste constante

adf <- adfTest(x, type = "ct",lags = 2) #constante et tendance 
adf
adf <- adfTest(x, type = "c",lags = 2)#juste constante
adf # on ne rejette pas : série non stationnaire.
# A priori c'est donc plutôt une marchine aléatoire qu'une tendance déterministe.
lb_test(adf@test$lm$residuals, fitdf=length(adf@test$lm$coefficients)) # vérification tests indépendance

# PP et kpss donnent des résultats similaires
PP.test(x) 
tseries::kpss.test(x)


# On différentie la série pour la stationnariser :
x_st <- diff(x, 1)
AQLTools::graph_ts(window(x_st,
						  start = c(2009,10),
						  end = c(2020,2),
						  extend = TRUE), x_lab = "Dates", y_lab = NULL,
				   titre = "IPI-CL1 différenciée", n_xlabel = 12)
summary(lm(x_st ~ time(x_st)))

# Série qui parait stationnaire même si l'amplitude parait plus importante depuis 2016
adfTest_valid(x_st, kmax = 24, type = "nc") # lag2

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
p1 + p2 # pas de tendance et a-priori séries stationnaire

saveRDS(x_st, file = "data/x_st.RDS")

