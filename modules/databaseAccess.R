library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), "test.db")
dbListTables(con)


###UPLOAD SIMPOL RESULTS
#### -- main table for different simpol run types
### Quality
### Events/Spills
uploadSimResults<- function(connection,table,type,runName){

  ##Refactor , if else txt ,else read csv in SIMPOL format
  if(type==1){
    dbWriteTable(connection, "Standard", stats,append = TRUE)

  }






}
