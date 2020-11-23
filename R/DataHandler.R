#' @import R6 
#' @export
DataHandler <- R6Class("DataHandler",
  private = list(
    .csvTable = NULL,
    .translog = NULL,
    .translogClean = NULL
  ),
  active = list(
    csvTable = function() {
      private$.csvTable
    }, 
    translog = function(){
      private$.translog
    }, 
    translogClean = function(){
      private$.translogClean
    }
    
  ),
  public = list(
    initialize = function( ) {
      private$.csvTable <- as.data.table(read.csv("data/retail_relay2.csv"))
    }, 
    preprocessTransactionLog = function(){
      private$.translogClean <- PreprocessRawTransactionLog(
        data = private$.csvTable,
        columns = list("custId" = "customerId",
                       "amountSpent" = "amountSpent",
                       "orderTimestamp" = "orderTimestamp")
      )
    },
    createCohortStructure = function(){
      private$.translog <- CreateCohortCols(data = private$.translogClean, cohortType = "Monthly Cohorts")
    }
  )
)