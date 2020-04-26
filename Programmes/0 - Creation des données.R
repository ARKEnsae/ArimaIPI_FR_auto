ipi_ce <- AQLTools::lectureBDM("010537924")
ipi_ce_brut <- AQLTools::lectureBDM("010537923")
data <- ts.union(ipi_ce, ipi_ce_brut)

saveRDS(data,
		file = "Rapport/data/donnees_completes.RDS")
saveRDS(window(data,
			   start = c(2010, 1),
			   end = c(2019,12)),
		file = "Rapport/data/donnees.RDS")


library(AQLTools)
AQLTools::graph_ts(window(data,start = 2005))
AQLTools::graph_ts(window(data,start = 2012))

# Deuxième série : CL1 = automobile

ipi_cl1 <- AQLTools::lectureBDM("010537940")
ipi_cl1_brut <- AQLTools::lectureBDM("010537939")
data_b <- ts.union(ipi_cl1, ipi_cl1_brut)

saveRDS(data_b,
		file = "Rapport/data/bis_donnees_completes.RDS")
saveRDS(window(data_b,
			   start = c(2010, 1),
			   end = c(2019,12)),
		file = "Rapport/data/bis_donnees.RDS")


# Ici test sur C4 : secteur non retenu

ipi_c4 <- AQLTools::lectureBDM("010537912")
ipi_c4_brut <- AQLTools::lectureBDM("010537911")
data_b <- ts.union(ipi_c4, ipi_c4_brut)
plot(diff(window(ipi_ce,
				 start = c(2010, 1),
				 end = c(2019,12)),1))
plot(diff(window(ipi_c4,
				 start = c(2010, 1),
				 end = c(2019,12)),1)) # peut-être mieux d'utiliser cette série, pas de problème heteroscedasticité
plot(ipi_c4)
