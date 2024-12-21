#install.packages("RSQLite")
library("RSQLite")
connection <- dbConnect(RSQLite::SQLite(), "~/TrainingLog/traininglog.db")
dbListTables(connection)
getLifts <- "SELECT * FROM lifts"
getMeta <- "SELECT * FROM metadata"
liftDf <- dbGetQuery(connection, getLifts)
metaDf <- dbGetQuery(connection, getMeta)

# max weight for all lifts
maxCol <- apply(liftDf[,c(-1,-2)], 2, max, na.rm=TRUE)
maxCol
# frequency of reps
# frequency of lifts
# what day has the most lifts
# relationship between place and lift
# order number and weight/reps


dbDisconnect(connection)