#' Finds common column names in two datasets
#'
#' @param dt1 
#' @param dt2 
#'
#' @return c(String) of column names
FindCommonColumns <- function(dt1, dt2){
  colnames1 <- names(dt1)
  colnames2 <- names(dt2)
  
  commonColumns <- sapply(colnames1, function(colname){
    if (colname %in% colnames2) {
      return(colname)
    } else {
      return(NA)
    }
  })
  
  if (all(is.na(commonColumns))) {
    return(NA)
  } else {
    commonColumns <- commonColumns[!is.na(commonColumns)]
    return(commonColumns)
  }
}

JoinTranslogWithData <- function(translog, data, by){
  out <- merge(x = translog, y = data, by = by, all.x = T)
}

GetCSVFromUserInput <- function(input){
  csvFile <- input
  if (is.null(csvFile)) {
    return(NULL)
  } else if (nrow(csvFile) == 1) {
    data <- fread(csvFile$datapath)
    return(data)
  } else {
    datasets <- lapply(csvFile$datapath, fread)
    return(datasets)
  }
}

