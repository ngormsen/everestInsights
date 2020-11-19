
# Computations ------------------------------------------------------------
PreprocessRawTransactionLog <- function(data, columns){
  # only select relevant columns
  dtOut <- data[, c(columns[["custId"]],
                    columns[["amountSpent"]],
                    columns[["orderTimestamp"]]), with=F]
  
  # rename columns
  setnames(dtOut, new = names(columns))
  
  # Fix datatypes
  dtOut[, orderTimestamp := as.Date(orderTimestamp)]
  return(dtOut)
}

CreateCohortCols <- function(data, cohortType){
  dt <- copy(data)
  
  dt[, acqTimestamp := min(orderTimestamp), by = .(custId)]
  
  dt[, acqMonth := lubridate::month(acqTimestamp)]
  dt[, acqQuarter := lubridate::quarter(acqTimestamp)]
  dt[, acqYear := lubridate::year(acqTimestamp)]
  
  minAcqMonth <- min(dt$acqMonth)
  minAcqQuarter <- min(dt$acqQuarter)
  minAcqYear <- min(dt$acqYear)
  
  dt[, orderMonth := lubridate::month(orderTimestamp)]
  dt[, orderQuarter := lubridate::quarter(orderTimestamp)]
  dt[, orderYear := lubridate::year(orderTimestamp)]
  dt[, orderYearMonth := format(orderTimestamp, "%Y-%m")]
  
  if (cohortType == "Monthly Cohorts"){
    dt[, cohort := format(acqTimestamp, "%Y-%m")]
    dt[, cohortAge := (orderYear - acqYear) * 12 + (orderMonth - acqMonth)]
    dt[, orderPeriod := format(orderTimestamp, "%Y-%m")]
  }
  if (cohortType == "Quarterly Cohorts"){
    dt[, cohort := lubridate::quarter(acqTimestamp, with_year = T)]
    dt[, cohortAge := (orderYear - acqYear) * 4 + (orderQuarter - acqQuarter)]
    dt[, orderPeriod := lubridate::quarter(orderTimestamp, with_year = T)]
  }
  if (cohortType == "Yearly Cohorts"){
    dt[, cohort := format(acqTimestamp, "%Y")]
    dt[, cohortAge := orderYear - acqYear]
    dt[, orderPeriod := lubridate::year(orderTimestamp)]
  }
  
  out <- dt[, c("custId", "amountSpent", "orderTimestamp", "orderPeriod", "orderYearMonth", "cohort", "cohortAge")]
  return(out)
}

ComputeCLV <- function(transLog) {
  dt <- copy(transLog)
  dt[, avgAmountSpentPerOrder := mean(amountSpent), by = custId]
  dt[, numOrdersPerMonth := .N, by = .(custId, orderYearMonth)]
  dt[, avgNumOrdersPerMonth := mean(numOrdersPerMonth), by = custId]
  dt[, avgRevenuePerMonth := avgAmountSpentPerOrder * avgNumOrdersPerMonth]
  dt[, retentionRate := 0.8]
  dt[, discRateYearly := 0.1]
  dt[, discRateMonthly := discRateYearly^(1/12)]
  dt[, marginMultiplier := 1 / (1 - (retentionRate/(1 + discRateMonthly)))]
  
  dataPerCustomer <- unique(dt, by = "custId")
  dataPerCustomer[, clv := avgRevenuePerMonth * marginMultiplier]
  
  return(dataPerCustomer[, c("custId", "clv")])
}

#' Computes for each customer
#'     (1) Recency := timestamp of last order
#'     (2) Frequency := average number of purchases per month
#'
#' @return data.table with columns: "custId", "frequency", "recency"
ComputeRecencyFrequency <- function(translog){
  dt <- copy(translog)
  dt[, recentPurchaseTimestamp := max(orderTimestamp), by = custId]
  
  frequency <- dt[, .N, by = .(custId, orderPeriod)]
  frequency[, avgNumPurchasesPerMonth := mean(N), by = custId]
  frequency <- unique(frequency, by = "custId")
  frequency[, c("custId", "avgNumPurchasesPerMonth")]
  
  # join
  out <- merge(dt, frequency, all.x = T, by = "custId")
  out <- out[, c("custId", "avgNumPurchasesPerMonth", "recentPurchaseTimestamp")]
  out <- unique(out, by = "custId")
  setnames(out,
           old = c("avgNumPurchasesPerMonth", "recentPurchaseTimestamp"),
           new = c("frequency", "recency"))
  return(out)
}

GetDataCohortTableOfNumPurchases <- function(translog, x){
  data <- translog %>% 
    group_by(cohort, get(x)) %>% 
    count() %>% 
    rename(period = `get(x)`) %>% 
    mutate(cohort = as.factor(cohort)) %>% 
    mutate(cohort = factor(cohort, levels = rev(levels(cohort))))
  return(data)
}

GetDataCohortTableCustom <- function(translog, x, var, fun, relativeTo = NULL){
  data <- translog %>% 
    group_by(cohort, get(x)) %>% 
    summarise_at(.vars = var, .funs = fun) %>% 
    rename(period = `get(x)`) %>%
    mutate(cohort = as.factor(cohort)) %>% 
    mutate(cohort = factor(cohort, levels = rev(levels(cohort)))) %>% 
    setDT()
  
  setnames(data, old = var, new = "var")
  data[, var := round(var)]
  
  if (relativeTo == "acq"){
    acq <- data[period == cohort]
    acq[, acqValue := var]
    acq <- acq[, c("cohort", "acqValue")]
    
    data <- merge(data, acq, by = "cohort", all.x = T)
    data[, var := round((var / acqValue) * 100)]
    data[, varLabel := paste0(var, "%")]
    return(data)
  } else if (relativeTo == "prev"){
    #TODO    
  } 
  return(data)
}

computeDataPerMonth <- function(transLog) {
  dt <- copy(transLog)
  dt[, numOrdersPerMonth := .N, by = .(custId, orderPeriod)]
  dt[, amountSpentPerOrderPeriod := sum(amountSpent), by = orderPeriod]
  customerPerMonth <- unique(dt, by = "orderPeriod")
  revenuePerMonth <- unique(dt, by = "orderPeriod")
  return(list(customerPerMonth=customerPerMonth, revenuePerMonth=revenuePerMonth))
}

computeUniqueCustomerPerMonth <- function(transLog){
  dt <- transLog %>%
    group_by(orderPeriod) %>%
    summarise(n = n_distinct(custId))
}

# Plots -------------------------------------------------------------------
PlotC3 <- function(data, cohortType){
  dtPlt <- data[, .N, by = .(cohort, orderPeriod)]
  
  if (cohortType == "Monthly Cohorts"){
    dtPlt[, period := as.Date(paste0(orderPeriod, "-01"))]
  } else if (cohortType == "Yearly Cohorts"){
    dtPlt[, period := orderPeriod]
  } else if (cohortType == "Quarterly Cohorts"){
    dtPlt[, period := orderPeriod]
  }
  
  # We need to add an additional row for each cohort with the dependent variable
  # equal to zero, such that the chart doesn't have these weird-looking "steps"
  # whenever a new cohort joins.
  cohorts <- unique(dtPlt$cohort)
  
  # Compute period before acquisition
  if (cohortType == "Monthly Cohorts"){
    # Monthly Cohorts: string "yyyy-mm"
    cohortsTemp <- as.Date(paste0(cohorts, "-01"))
    periodBeforeAcquisition <- cohortsTemp %m-% months(1)
  } else if (cohortType == "Quarterly Cohorts"){
    # Quarterly Cohorts: string "yyyy.q"
    periodBeforeAcquisition <- sapply(cohorts, function(cohort) {
      year <- as.numeric(str_extract(cohort, "[0-9]+"))
      qrtr <- as.numeric(str_sub(cohort, -1, -1))
      if (qrtr == 1) {
        periodBeforeAcquisition <- as.numeric(paste0(year-1, ".", 4))
      } else {
        periodBeforeAcquisition <- as.numeric(paste0(year, ".", qrtr-1))
      }
      return(periodBeforeAcquisition)
    })
  } else if (cohortType == "Yearly Cohorts"){
    # Yearly Cohorts: string "yyyy"
    periodBeforeAcquisition <- as.character(as.numeric(cohorts) - 1)
  }
  
  tempRows <- data.table(
    cohort = cohorts,
    period = periodBeforeAcquisition,
    N = 0 # dependent variable
  )
  dtPlt <- rbind(dtPlt[, c("cohort", "period", "N")], tempRows)
  
  if (cohortType == "Yearly Cohorts") {
    dtPlt$period <- as.numeric(dtPlt$period)
  }
  if (cohortType == "Quarterly Cohorts") {
    xLabels <- QuarterToPrettyString(sort(unique(dtPlt$period)))
    quarters <- lapply(dtPlt$period, function(x) QuarterToTimestamp(x))
    quarters <- purrr::flatten_chr(quarters)
    dtPlt$period <- as.Date(quarters)
    xBreaks <- sort(unique(dtPlt$period))
  }
  dtPlt <- dtPlt %>% 
    mutate(cohort = as.factor(cohort)) %>% 
    mutate(cohort = fct_reorder(cohort, desc(cohort)))
  
  colors_layer_cust <- colorRampPalette(brewer.pal(n = 9, name = "Blues"))(length(cohorts))
  plt <- ggplot(dtPlt, aes(x = period, y = N, fill = cohort)) +
    geom_area(position = "stack") +
    geom_line(position = "stack", alpha=0.2) +
    theme_classic() +
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 90),
      axis.line = element_line(color = "grey50"),
      legend.position = "top",
      legend.text = element_text(color = "grey50")
    ) +
    labs(fill = "Cohort") +
    ylab("Number of Customers") +
    xlab("Period") + 
    scale_fill_manual(values = colors_layer_cust)
  
  if (cohortType == "Quarterly Cohorts"){
    plt <- plt + 
      scale_x_date(breaks = xBreaks, labels = xLabels)
  }
  return(plt)
}

PlotCohortAgeLinechart <- function(data){
  dtPlt <- data[, .N, by = .(cohort, cohortAge)]
  dtPlt[, cohort := as.character(cohort)]
  #        cohort cohortAge N
  # 1: 2009-01-11         0 2
  # 2: 2009-01-11         1 5
  dtPlt %>% 
    mutate(cohort = fct_reorder(cohort, desc(cohort)))
  
  xBreaks <- sort(unique(dtPlt$cohortAge))
  
  colors_layer_cust <- colorRampPalette(brewer.pal(n = 9, name = "Blues"))(length(xBreaks))
  ggplot(dtPlt, aes(x = cohortAge, y = N, color = cohort)) +
    geom_line() +
    geom_point(size = 2, alpha = 0.5) +
    theme_classic() +
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 90),
      axis.line = element_line(color = "grey50"),
      legend.position = "top",
      legend.text = element_text(color = "grey50")
    ) +
    scale_x_continuous(breaks = xBreaks) +
    labs(color = "Cohort") +
    xlab("Age") +
    ylab("Number of Customers") +
    scale_colour_manual(values = colors_layer_cust)
}

PlotCohortTableOfNumPurchases <- function(data){
  ggplot(data, aes(x = period, y = cohort, fill = n)) +
    geom_raster() +
    geom_text(aes(label = n), color = "black") +
    scale_fill_continuous(high = "#239af6", low = "#e7f4fe") +
    theme_classic() +
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 0),
      axis.line = element_line(color = "grey50"), 
      legend.position = "none"
    )
}

PlotCohortTableCustom <- function(data, perc = F){
  plt <- ggplot(data, aes(x = period, y = cohort, fill = var)) +
    geom_raster() +
    scale_fill_continuous(high = "#239af6", low = "#e7f4fe") +
    theme_classic() +
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 0),
      axis.line = element_line(color = "grey50"), 
      legend.position = "none"
    )
  
  if (perc == F){
    plt <- plt + geom_text(
      data = data, 
      mapping = aes(x = period, y = cohort, label = var), color = "black"
    )
  } else if (perc == T){
    plt <- plt + geom_text(
      data = data, 
      mapping = aes(x = period, y = cohort, label = varLabel), 
      color = "black"
    )
  }
  return(plt)
}

plotUniqueCustomerPerMonth <- function(data){
  ggplotly(ggplot(data = data, aes(x = orderPeriod, y = n)) +
             geom_bar(stat = "identity", fill = "steelblue", color = "white", width = 0.5) +
             theme_minimal() +
             theme(axis.text.x = element_text(angle = 50, size = 8)) +
             ggtitle(paste("Unique Customers per Month")) +
             ylab("Number of Unique Customers")) %>% 
    config(displayModeBar = F)
}

PlotCLVDensity <- function(dataCLV) {
  ggplot(dataCLV, aes(x = clv)) +
    geom_density(fill = "lightblue",
                 color = "white",
                 alpha = 0.5) +
    geom_rug() +
    ggtitle("Distribution of CLVs \n") +
    theme_classic() + 
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 0),
      axis.line = element_line(color = "grey50"),
      legend.position = "top",
      legend.text = element_text(color = "grey50")
    ) +
    xlab("CLV ($)") +
    ylab("Density")
}

PlotRecencyFrequency <- function(data) {
  ggplot(data, aes(x = recency, y = frequency)) +
    geom_point() +
    theme_classic() +
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 0),
      axis.line = element_line(color = "grey50"),
      legend.position = "top",
      legend.text = element_text(color = "grey50")
    ) +
    xlab("Recency (last purchase date)") +
    ylab("Frequency \n (avg. number of \n purchases \n per period)")
}

PlotTranslog <- function(translog) {
  DT::datatable(
    translog,
    options = list(
      pageLength = 10, # number of rows to display
      searching = F,   # disable search field
      paging = F,      # disable "show x entries" button
      scroller = T,
      scrollY = 300
    )
  ) %>% 
    # Reduce font size of the table entries
    DT::formatStyle(
      columns = names(translog),
      fontSize = '85%'
    )
}


# Linear Regression -------------------------------------------------------------------

#' Fits linear regression where APC Variables are numeric
#'
#' @param data output from 'GetDataCohortTableCustom'
#' @param predictors c(character) e.g. c("age", "period")
#' @return named list: (1) the data used to fit the model and (2) the model 
#' object returned by lm()
ComputeLinearModel <- function(data, predictors){
  # convert cohort & period from factor to integer (to get a linear trend)
  dt <- copy(data)
  dt[, ':='(cohort = as.numeric(factor(cohort, levels = rev(levels(cohort)))),
            period = as.numeric(as.factor(period)))]
  dt[, age := period - cohort]
  setnames(x = dt, old = "var", new = "Y")
  
  predictors <- paste(predictors, collapse = "+") # > "x1 + x2 + ..."
  
  model <- lm(
    formula = as.formula(paste0("Y ~ ", predictors)),
    data = dt
  )
  
  return(list(data = dt, model = model))
}

SetLabelForDepVar <- function(summariseVar, summariseFunc, relativeTo){
  if (summariseFunc == "n_distinct"){
    depVarLabel <- "Count"
  } else {
    depVarLabel <- paste0(summariseFunc, "(", summariseVar, ")")
  }
  
  if (relativeTo == "acq") {
    depVarLabel <- paste0(depVarLabel, " relative to acq. period")
  } else if (relativeTo == "prev") {
    depVarLabel <- paste0(depVarLabel, " relative to prev. period")
  }
  return(depVarLabel)
}

CreateRegressionHTMLTable <- function(model, depVarLabel) {
  regTbl <- stargazer::stargazer(
    model,
    type = "html",
    report = "vc*", # removes standard errors
    dep.var.labels = depVarLabel
  )
  regTbl <- paste(regTbl, collapse = "")
  return(regTbl)
}

InterpretCoefficients <- function(model){
  coefs <- as.matrix(round(coef(model), 2))
  coefNames <- rownames(coefs)
  nCoefs <- nrow(coefs)
  
  interpretations <- sapply(seq_along(coefs), function(i){
    predictor <- coefNames[i]
    coef <- coefs[i]
    
    out <- ""
    if (predictor == "age"){
      if (coef < 0) {
        out <- glue("Every additional age {Bold('decreases')} the dependent variable by {Bold(coef)}.")
      } else {
        out <- glue("Every additional age {Bold('increases')} the dependent variable by {Bold(coef)}")
      }
    } else if (predictor == "cohort") {
      if (coef < 0) {
        out <- glue("Every subsequent cohort {Bold('decreases')} the dependent variable by {Bold(coef)}")
      } else {
        out <- glue("Every subsequent cohort {Bold('decreases')} the dependent variable by {Bold(coef)}")
      }
    } else if (predictor == "period") {
      if (coef < 0) {
        out <- glue("Every additional period is expected to {Bold('decreases')} the dependent variable by {Bold(coef)}.")
      } else {
        out <- glue("Every additional period is expected to {Bold('increases')} the dependent variable by {Bold(coef)}")
      }
    } else if (predictor == "(Intercept)"){
      out <- glue("The average value of the dependent variable when all predictors are 0 is {Bold(coef)}")
    }
    out <- glue("<p>{out}</p>")
    return(out)
  })
  return(paste(interpretations, collapse = ""))
}

CreateDataForLinearModelFit <- function(model, data){
  preds <- predict.lm(object = model, newdata = data)
  
  dtActual <- copy(data)
  dtActual[, label := "actual"]
  
  dtModel <- copy(data)
  dtModel[, Y := preds]
  dtModel[, label := "model"]
  
  dtPlt <- rbind(dtActual, dtModel)
  return(dtPlt)
}

PlotLinearModelFit <- function(dtPlt) {
  dtActual <- dtPlt[label == "actual"]
  dtModel <- dtPlt[label == "model"]
  
  ggplot() +
    geom_line(
      data = dtActual,
      mapping = aes(x = age, y = Y, color = as.factor(cohort))
    ) +
    geom_line(
      data = dtModel,
      mapping = aes(x = age, y = Y, color = as.factor(cohort)),
      linetype = "dashed",
      size = 1,
      alpha = 0.5
    ) +
    ThemeGG()
}

# Small Help Functions ---------------------------------------------------------------
QuarterToTimestamp <- function(yearDotQuarter){
  year <- as.numeric(str_extract(yearDotQuarter, "[0-9]+"))
  qrtr <- as.numeric(str_sub(yearDotQuarter, -1, -1))
  if (qrtr == 1) monthDay <- "01-01" # 1.Jan - 31. März
  if (qrtr == 2) monthDay <- "04-01" # 1.April - 30.Juni
  if (qrtr == 3) monthDay <- "07-01" # 1. Juli - 30. Sept
  if (qrtr == 4) monthDay <- "10-01" # 1. Okt - 31.Dez
  return(paste0(year, "-", monthDay))
}

QuarterToPrettyString <- function(yearDotQuarter){
  year <- as.numeric(str_extract(yearDotQuarter, "[0-9]+"))
  qrtr <- as.numeric(str_sub(yearDotQuarter, -1, -1))
  return(paste0(year, " Q", qrtr))
}

Bold <- function(x){
  spanOpen <- "<span style='font-weight: bold'>"
  spanClose <- "</span>"
  return(paste0(spanOpen, x, spanClose))
}

MarginTopBottom <- function(x){
  openTag <- "<div style='margin: 2rem 0'>"
  closeTag <- "</div>"
  return(paste0(openTag, x, closeTag))
}

ThemeGG <- function(){
  theme_classic() +
    theme(
      axis.text = element_text(color = "grey50", size = 12),
      axis.text.x = element_text(angle = 60, hjust = .5, vjust = .5, face = "plain"),
      axis.title = element_text(color = "grey30", size = 12, face = "bold"),
      axis.title.y = element_text(angle = 0),
      axis.line = element_line(color = "grey50"),
      legend.position = "top",
      legend.text = element_text(color = "grey50")
    )
}
