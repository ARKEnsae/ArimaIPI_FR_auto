library(ggplot2)
library(knitr)
library(urca)
library(fUnitRoots)
library(AQLTools)
library(patchwork)
library(kableExtra)
library(tseries)
library(forecast)

##################################
### tableau test stationnarité ###
##################################

footnote_stars <- paste0("\\\\hspace{-0.4cm}\\\\textbf{Signif. codes: }",
						 "0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1")
add_stars <- function(x, p_value = ncol(x),
					  decimal.mark = getOption("OutDec"),
					  digits = 3){
	stars <- cut(x[, p_value],
				 breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
				 include.lowest = TRUE,
				 labels = c("***","**","*",".",""))
	table <- cbind(formatC(x, digits = digits, decimal.mark = decimal.mark,
						   format = "f"),
				   as.character(stars))
	colnames(table) <- c(colnames(x), "")
	table
}
test_stationnarite <- function(adf, pp, kpss, titre = "",
							   decimal.mark = getOption("OutDec"),
							   digits = 3){
	stat <- c(adf@test$statistic, pp$statistic, kpss$statistic)
	p_val <- c(adf@test$p.value, pp$p.value, kpss$p.value)
	data_test <- as.matrix(data.frame(x = stat, y = p_val))
	data_test <- add_stars(data_test, decimal.mark = decimal.mark, digits = digits)
	data_test <- cbind(c("Dickey-Fuller augmenté \\textsuperscript{a}",
						 "Phillips-Perron", "KPSS"),data_test)
	colnames(data_test)[1:3] <- c("Test", "Statistique", "p-valeur")
	footnoteadf <- paste0("Le test ADF a été fait en rajoutant ", adf@call$lags,
						  " retard", ifelse(adf@call$lags>1, "s", ""),
						  ". De cette façon les résidus utilisés dans ce test sont bien indépendants ",
						  "et le test ADF est bien interprétable")
	data_test %>% 
		kable(digits = digits, format = "latex", escape = FALSE,
			  caption = titre,
			  format.args = list(decimal.mark = decimal.mark),
			  booktabs = TRUE,
			  align = c("l","c", "c", "c")) %>% 
		kable_styling(latex_options = c("hold_position")) %>% 
		footnote(alphabet = footnoteadf,
				 general = footnote_stars,
				 general_title = "",
				 escape = FALSE,
				 threeparttable = TRUE)
}

###########################
### Graphiques ACF/PACF ###
###########################

reformat_graph_acf <- function(p, ny = 6, nx = 12){
	p  + AQLTools:::theme_aqltools()  +
		labs(title = NULL) +
		scale_y_continuous(breaks = scales::pretty_breaks(n = ny),
						   label = function(x) format(x, decimal.mark = ","),
						   limits = c(-.5, .5)) +
		scale_x_continuous(breaks = scales::pretty_breaks(n = nx),
						   label = function(x) format(x, decimal.mark = ",")) 
}

###########################
### Formatage des tests ###
###########################

format_testlb <- function(models_evalues, var = "lbtest", titre = "", 
						digits = 3, decimal.mark = getOption("OutDec")){
	lb <- do.call(cbind, lapply(models_evalues, function(x){
		add_stars(as.matrix(data.frame(unlist(x[[var]][,2]), 
									   unlist(x[[var]][,3]))),
				  decimal.mark = decimal.mark, digits = digits)
	}))
	lb <- apply(lb,2, function(x) gsub(" *NA", "", x))
	lb <- cbind(1:nrow(lb), lb)
	colnames(lb) <- c("Retards", 
					  rep(c("Statistique", "p-valeur", ""),
					  	length(models_evalues)))
	# header <- c(1,rep(3, length(models_evalues)))
	# names(header) <- c(" ", names(models_evalues))
	header <- c(1,rep(c(2,1), length(models_evalues)))
	names(header) <- c(" ", unlist(lapply(names(models_evalues),
										  function(x) c(x," "))))
	names(header) <- gsub(",0,",",1,", names(header))
	
	lb %>% 
		kable(digits = digits, format = "latex", escape = FALSE,
			  caption = titre,
			  format.args = list(decimal.mark = decimal.mark),
			  booktabs = TRUE,
			  align = c("c"))  %>% 
		add_header_above(header) %>% 
		kable_styling(latex_options = c("hold_position",  "scale_down")) %>% 
		footnote(general = c(footnote_stars,
							 "L’hypothèse (H0) d’homoscedasticité des résidus n’est pas rejetée à 5 \\\\% sur les 24 périodes pour l’ensemble des modèles et en particulier pour le modèle retenu ARIMA(0,1,1)."),
				 general_title = "",
				 escape = FALSE,
				 threeparttable = TRUE)
}
format_jbtest <- function(models_evalues, titre = "", 
						  digits = 3, decimal.mark = getOption("OutDec")){
	stat <- sapply(models_evalues, function(x){
		x$jbtest$statistic
	})
	p_val <- sapply(models_evalues, function(x){
		x$jbtest$p.value
	})
	data_test <- as.matrix(data.frame(x = stat, y = p_val))
	data_test <- add_stars(data_test, decimal.mark = decimal.mark, digits = digits)
	data_test
	colnames(data_test)[1:2] <- c("Statistique", "p-valeur")
	rownames(data_test) <- gsub(",0,",",1,", names(models_evalues))
	
	footnotejb <- "Le test de Jarque-Bera suppose que les résidus soient indépendants et homoscédastiques."
	data_test %>% 
		kable(digits = digits, format = "latex", escape = FALSE,
			  caption = titre,
			  format.args = list(decimal.mark = decimal.mark),
			  booktabs = TRUE,
			  align = c("c"))  %>% 
		kable_styling(latex_options = c("hold_position", "repeat_header")) %>% 
		footnote(general = c(footnote_stars, footnotejb, 
							 "L’hypothèse (H0) de normalité des résidus n’est pas rejetée à 5 \\\\% pour l’ensemble des modèles et en particulier pour le modèle retenu ARIMA(0,1,1)."),
				 general_title = "",
				 escape = FALSE,
				 threeparttable = TRUE)
}

format_tab_coef <- function(models_evalues, titre = "", 
							digits = 3, decimal.mark = getOption("OutDec")){
	ar <-  t(sapply(models_evalues, function(x){
		if(identical(x$ttest, 0)){
			valar <- valma <-
				pvalar <- pvalma <-
				sdar <- sdma <- NA
		}else{
			idar <- grep("ar",rownames(x$ttest))
			idma <- grep("ma",rownames(x$ttest))
			valar <- ifelse(length(idar) == 0, NA, x$ttest[idar, 1])
			valma <- ifelse(length(idma) == 0, NA, x$ttest[idma, 1])
			
			sdar <- ifelse(length(idar) == 0, NA, x$ttest[idar, 2])
			sdma <- ifelse(length(idma) == 0, NA, x$ttest[idma, 2])
			
			pvalar <- ifelse(length(idar) == 0, NA, x$ttest[idar, 4])
			pvalma <- ifelse(length(idma) == 0, NA, x$ttest[idma, 4])
		}
		c(valar, sdar, pvalar)
	}))
	ma <- t(sapply(models_evalues, function(x){
		if(identical(x$ttest, 0)){
			valar <- valma <-
				pvalar <- pvalma <-
				sdar <- sdma <- NA
		}else{
			idar <- grep("ar",rownames(x$ttest))
			idma <- grep("ma",rownames(x$ttest))
			valar <- ifelse(length(idar) == 0, NA, x$ttest[idar, 1])
			valma <- ifelse(length(idma) == 0, NA, x$ttest[idma, 1])
			
			sdar <- ifelse(length(idar) == 0, NA, x$ttest[idar, 2])
			sdma <- ifelse(length(idma) == 0, NA, x$ttest[idma, 2])
			
			pvalar <- ifelse(length(idar) == 0, NA, x$ttest[idar, 4])
			pvalma <- ifelse(length(idma) == 0, NA, x$ttest[idma, 4])
		}
		c(valma, sdma, pvalma)
	}))
	colnames(ar) <- colnames(ma) <- c("Coefficient", "Écart-type", "p-valeur")
	
	tab_coef <- cbind(add_stars(ar,
								decimal.mark = decimal.mark, digits = digits),
					  add_stars(ma,
					  		  decimal.mark = decimal.mark, digits = digits)
	)
	tab_coef <- apply(tab_coef,2, function(x) gsub(" *NA", "", x))
	rownames(tab_coef) <- gsub(",0,",",1,", names(models_evalues))
	# header <- c(1, 3, 3)
	# names(header) <- c(" ", "AR(1)","MA(1)")
	
	header <- c(1, rep(c(3, 1), 2))
	names(header) <- c(" ", "AR(1)", " ", "MA(1)", " ")
	names(header) <- gsub(",0,",",1,", names(header))
	
	tab_coef %>% 
		kable(digits = digits, format = "latex", escape = FALSE,
			  caption = titre,
			  format.args = list(decimal.mark = decimal.mark),
			  booktabs = TRUE,
			  align = c("c"))  %>% 
		add_header_above(header) %>% 
		kable_styling(latex_options = c("hold_position")) %>% 
		footnote(general = footnote_stars,
				 general_title = "",
				 escape = FALSE,
				 threeparttable = TRUE)
}
format_ic <- function(models_evalues, titre = "", 
					  digits = 3, decimal.mark = getOption("OutDec")){
	qualite_modeles <- sapply(models_evalues, function(x) x$qualite)
	colnames(qualite_modeles) <- gsub(",0,",",1,", colnames(qualite_modeles))
	qualite_modeles[c("AIC", "BIC"),] %>% 
		kable(digits = digits, format = "latex", escape = FALSE,
			  caption = titre,
			  format.args = list(decimal.mark = decimal.mark),
			  booktabs = TRUE,
			  align = c("c"))  %>% 
		kable_styling(latex_options = c("hold_position")) 
}

############################
### Graphique prévisions ###
############################

graph_prev <- function(x, x_complet, prev, n_xlabel = 12){
	
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
		scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
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
