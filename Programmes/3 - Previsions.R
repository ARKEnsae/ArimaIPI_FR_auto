library(forecast)
library(patchwork) # pour mettre à coté deux graphiques ggplot2
library(conics) # pour tracer une ellipse

data <- readRDS(file = "data/donnees.RDS")
# data <- ts(read.csv("data/donnees.csv")[,-1],
# 		   start = 2010, frequency = 12)

data_complet <- readRDS(file = "data/donnees_completes.RDS")
x <- data[, "ipi_cl1"]
x_complet <- data_complet[, "ipi_cl1"]
ordres_retenus <- readRDS(file = "data/ordres_retenus.RDS")
model_estime <- Arima(x, order = ordres_retenus, include.constant = FALSE)
# Réalise les prévisions sur 2 périodes du modèle retenu
prev <- forecast(model_estime, h = 2)
prev
# On les représente sur un graphique. 
plot(prev)
# Attention, ce sont les intervalles de confiance à chaque période 
# (et non la région de confiance calculée ci-dessous) qui sont représentés automatiquement. 

#Retrouver les IC du graphique : 
res <- residuals(model_estime)
sum((res - mean(res))^2) / (length(res) - 2) # sigma2
prev$mean[1]+sqrt(model_estime$sigma2)*qnorm(1-0.05/2)
prev$mean[2]+sqrt(model_estime$sigma2*(1+(1+model_estime$coef[1])^2))*qnorm(1-0.05/2)

# Plutôt que de tracer les IC on veut une région de confiance :
sigma2 <- model_estime$sigma2
theta <- coef(model_estime)

sigma_m1 = matrix(c(1+(1+theta)^2, -(1+theta),
					-(1+theta),1),ncol = 2)/sigma2
# Pour vérifier qu'on a bien fait l'inversion de la matrice :
# matlib::Inverse(sigma2 * matrix(c(1, (1+theta),
# 				 (1+theta),1+(1+theta)^2),ncol = 2))

alpha = 0.05

sigma_sur_quantile <- sigma_m1/(qchisq(1-alpha, 2))
prevs <- prev$mean

a = sigma_sur_quantile[1,1]
b = sigma_sur_quantile[1,2]
d = sigma_sur_quantile[2,2]
x_p = prevs[1]
y_p = prevs[2]
# coefficients de l'ellipse : a_1 à a_6
# a_1 * x^2 + a_2 * x * y + a_3 * y^2 + a_4 * x + a_5 * y + a_6 = 1
a_1 <- a
a_2 <- 2*b
a_3 <- d
a_4 <- -2*(a*x_p+b*y_p)
a_5 <- -2*(b*x_p+d*y_p)
a_6 <- a*x_p*x_p+2*b*x_p*y_p+d*y_p*y_p

ellispe_eq <- c(a_1, a_2, a_3, a_4,
				a_5, a_6)

eq <- paste(round(ellispe_eq, 3),c("x^2","* x * y","y^2","x","y",1),
			collapse = " + ")
eq <- paste(gsub("+ -","- ", eq,fixed = TRUE),"= 1")
eq <- gsub(" 1 ", " ", eq,fixed = TRUE)
eq
#equation latex :
cat(gsub(".", ",",
		 gsub("*","\\times", eq, fixed = TRUE),
		 fixed = TRUE))

# Pour tracer l'ellispe :
# equation sous la forme :
# a_1 * x^2 + a_2 * x * y + a_3 * y^2 + a_4 * x + a_5 * y + a_6 = 0

# On trace la prévision avec la région de confiance (ellipse) autour
ellipse <- conicPlot(ellispe_eq - c(0,0,0,0,0,1))
ellipse
points(prevs[1], prevs[2])


