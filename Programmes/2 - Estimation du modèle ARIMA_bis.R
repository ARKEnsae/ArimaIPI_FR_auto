library(forecast)
library(patchwork)
data <- readRDS(file = "Rapport/data/bis_donnees.RDS")
x <- data[,"ipi_cl1"]
x_st <- readRDS(file = "Rapport/data/bis_x_st.RDS")
acf(x_st) # q_max = 4
pacf(x_st) # p_max = 3

# Fonctions identiques du package forecast mais où on enlève lag = 0
# Permet d'éviter les confusions pour l'acf
Acf(x_st)
Pacf(x_st)

ggAcf(x_st) + labs(title = "ACF") + 
	ggPacf(x_st) + labs(title = "PACF")

# On va tester tous les modèles pour q <= 4, p <= 3

evaluation_model <- function(order, x, lags = 24){
	# ici on utilise Arima plutôt que arima pour la fonction accuracy
	model <- forecast::Arima(x, order = order)
	residus <- residuals(model)
	lbtest <- t(sapply(1:lags,function(l){
		if(l <= sum(model$arma[1:2])){
			b <- list(statistic = NA, p.value = NA)
		}else{
			b <- Box.test(residus,"Ljung-Box",lag = l,
						  fitdf = sum(model$arma[1:2])
			)
		}
		data.frame(lag = l,
				   b$statistic,
				   b$p.value
		)
	}))
	lb2test <- t(sapply(1:lags,function(l){
		if(l <= sum(model$arma[1:2])){
			b <- list(statistic = NA, p.value = NA)
		}else{
			b <- Box.test(residus^2,"Ljung-Box",lag = l,
						  fitdf = sum(model$arma[1:2])
			)
		}
		data.frame(lag = l,
				   b$statistic,
				   b$p.value
		)
	}))
	jbtest <- tseries::jarque.bera.test(residus)
	ttest <- lmtest::coeftest(model)
	qualite <- c(AIC(model), BIC(model), accuracy(model))
	names(qualite) <- c("AIC", "BIC", colnames(accuracy(model)))
	list(model = model,
		 ttest = ttest,
		 lbtest = lbtest, lb2test = lb2test,
		 jbtest = jbtest,
		 qualite = qualite)
	
}

models_possibles <- expand.grid(p = 0:1, d = 0, q = 0:1)
models_evalues <- apply(models_possibles,1, evaluation_model, x = x_st)
names(models_evalues) <- sprintf("ARIMA(%i,%i,%i)", models_possibles[,"p"],
								 models_possibles[,"d"], models_possibles[,"q"])
## Pour éviter de tout écrire à la main :
#cat(paste(sprintf("models_evalues$`%s`",names(models_evalues)),collapse = "\n"))

models_evalues$`ARIMA(0,0,0)`
# Il n'y a pas indépendance des résidus
models_evalues$`ARIMA(1,0,0)`
# Il n'y a pas indépendance des résidus
models_evalues$`ARIMA(0,0,1)`
# Modele bon
models_evalues$`ARIMA(1,0,1)`
# coef AR1 non significatif
models_evalues$`ARIMA(1,0,0)`
# Il n'y a pas indépendance des résidus

# Bilan : seul modèle vaile : ARIMA(0,1,1)

ordres_retenus <- c(0,1,1)
saveRDS(ordres_retenus, file = "Rapport/data/bis_ordres_retenus.RDS")

model_estime <- arima(x, order = ordres_retenus)
model_estime
lmtest::coeftest(model_estime) # coefficients significatifs
residus <- residuals(model_estime)
lbtest <- t(sapply(1:24,function(l){
	if(l <= sum(model_estime$arma[1:2])){
		b <- list(statistic = NA, p.value = NA)
	}else{
		b <- Box.test(residus,"Ljung-Box",lag = l,
					  fitdf = sum(model_estime$arma[1:2])
		)
	}
	data.frame(lag = l,
			   b$statistic,
			   b$p.value
	)
}))
lbtest # résidus biens valides
# On remarque que rien ne reste dans ACF/PACF
ggAcf(residus) + labs(title = "ACF") + 
	ggPacf(residus) + labs(title = "PACF")
#Modèle bien valide

tseries::jarque.bera.test(residus) # résidus normaux : on peut bien faire les ic

#Remarquons que le modèle que le même modèle serait determiné automatiquement
m <- auto.arima(x, allowdrift = FALSE)
m
