ipi_ce <- AQLTools::lectureBDM("010537924")
ipi_ce_brut <- AQLTools::lectureBDM("010537923")
data <- ts.union(ipi_ce, ipi_ce_brut)

saveRDS(ts.union(ipi_ce, ipi_ce_brut),
		file = "Rapport/data/donnees_completes.RDS")
saveRDS(window(data,
			   start = c(2010, 1),
			   end = c(2019,12)),
		file = "Rapport/data/donnees.RDS")


library(AQLTools)
AQLTools::graph_ts(window(data,start = 2005))
AQLTools::graph_ts(window(data,start = 2012))

RJDemetra::regarima_x13(ipi_ce)

