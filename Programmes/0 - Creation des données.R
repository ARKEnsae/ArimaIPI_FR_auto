# devtools::install_github("aqlt/AQLTools")
 library(AQLTools)
 library(zoo)
 
# CL1 = automobile

ipi_cl1 <- AQLTools::lectureBDM("010537940")
ipi_cl1_brut <- AQLTools::lectureBDM("010537939")
data_b <- ts.union(ipi_cl1, ipi_cl1_brut)
data_2010 <- window(data_b,
				  start = c(2010, 1),
				  end = c(2019,12))



saveRDS(data_b,
		file = "data/donnees_completes.RDS")
saveRDS(data_2010,
		file = "data/donnees.RDS")

write.csv(data.frame(date = format(as.yearmon(time(data_2010)), "%m/%Y"),
							  data_2010),
				   row.names = FALSE,
				   file = "data/donnees.csv")


#AQLTools::graph_ts(window(data,start = 2005))
