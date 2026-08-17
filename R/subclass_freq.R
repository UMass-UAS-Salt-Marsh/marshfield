# subclass frequency
# pulls draft database from UMass UAS 2026 field plots and summarizes subclass counts


library(dplyr)

data <- read.csv('C:/Work/saltmarsh/data/uas2026/field/plots/vegetation_records.csv')
data <- data[!data$site_name %in% c('beech hill', 'Beech Hill Road', 'blue test', 'hoop', 'Office', 'photos', 'pink test', 'test', 'test Aug 2', 'yard', 'yard2', 'yard03'), ]
table(data$site_name)

data$date[data$date == ''] <- '2026-08-05'   # Fix missing date for Andrew & Ava

x <- distinct(data, session_id, plot_number, subclass)

z <- as.data.frame(table(x$subclass))
names(z) <- c('subclass', 'freq')
z$subclass <- as.character(z$subclass)
z$subclass[z$subclass == ''] <- '0 - missing'

y <- strsplit(z$subclass, ' - ')
z$subclass_n <- as.numeric(unlist(lapply(y, function(x) x[[1]])))
z$subclass <- unlist(lapply(y, function(x) x[[2]]))
                         
z <- z[order(z$subclass_n), c('subclass_n', 'subclass', 'freq')]                     

z <- rbind(z, c(NA, 'TOTAL', sum(z$freq)))


cat('Plots by date')
d <- as.data.frame(table(distinct(data, session_id, date, plot_number)$date))
names(d) <- c('Date', 'Count')
d$Date <- as.character(d$Date)
d <- rbind(d, c('Total', sum(as.numeric(d$Count))))
d

cat('Subclass count')
z
