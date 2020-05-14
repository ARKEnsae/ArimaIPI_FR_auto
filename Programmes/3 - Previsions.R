library(forecast)
library(patchwork)
library(conics)

data <- readRDS(file = "data/donnees.RDS")
# data <- ts(read.csv("data/donnees.csv")[,-1],
# 		   start = 2010, frequency = 12)

data_complet <- readRDS(file = "data/donnees_completes.RDS")
x <- data[, "ipi_cl1"]
x_complet <- data_complet[, "ipi_cl1"]
ordres_retenus <- readRDS(file = "data/ordres_retenus.RDS")
model_estime <- Arima(x, order = ordres_retenus, include.constant = FALSE)
prev <- forecast(model_estime, h = 2)
prev
plot(prev)

#Retrouver les IC : 
res <- residuals(model_estime)
sum((res - mean(res))^2) / (length(res) - 2) # sigma2
prev$mean[1]+sqrt(model_estime$sigma2)*qnorm(1-0.05/2)
prev$mean[2]+sqrt(model_estime$sigma2*(1+(1+model_estime$coef[1])^2))*qnorm(1-0.05/2)

# Plutôt que de tracer les IC on veut une région de confiance :
sigma2 <- model_estime$sigma2
theta <- coef(model_estime)

# matlib::Inverse(matrix(c(1, (1+theta),
# 				 (1+theta),1+(1+theta)^2),ncol = 2))
sigma_m1 = matrix(c(1+(1+theta)^2, -(1+theta),
					-(1+theta),1),ncol = 2)
alpha = 0.05
sigma_m1 <- sigma_m1/(sigma2*qchisq(1-alpha, 2))
prevs <- prev$mean

a = sigma_m1[1,1]
b = sigma_m1[1,2]
d = sigma_m1[2,2]
x_p = prevs[1]
y_p = prevs[2]
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

# Pour obtenir les plus grandes prévisions possibles pour T+1
ell <- function(x = 103.6517, y = 103.6517, 
				ellispe_eq = c(0.0157341205774816, -0.0141250037159479, 0.0113328509767077, 
							   -1.79765568338535, -0.885257713887019, 139.044239392918)){
	sum(ellispe_eq *c(x^2, x * y, y^2, x, y, 1)) - 1
}

(c(uniroot(ell,interval = c(95,97))$root,
  uniroot(ell,interval = c(111,113))$root) /102.66 - 1)*100



