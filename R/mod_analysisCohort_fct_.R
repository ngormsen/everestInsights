library(glue)

# 1_data ------------------------------------------------------------------

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


# 2_cohorts ---------------------------------------------------------------

AddAcquisitionTimestamp <- function(data, orderTimestampCol, customerIdCol){
  data[, acqTimestamp := min(base::get(orderTimestampCol)), by = customerIdCol]
}

LubridateQuarterToPrettyString <- function(yearDotQuarter){
  year <- as.numeric(str_extract(yearDotQuarter, "[0-9]+"))
  qrtr <- as.numeric(str_sub(yearDotQuarter, -1, -1))
  return(paste0(year, " Q", qrtr))
}

AddMonthlyCohorts <- function(data, orderTimestampCol, acqTimestampCol, custIdCol){
  data[, cohortName := format(base::get(acqTimestampCol), "%Y-%m")]
  data[, periodName := format(base::get(orderTimestampCol), "%Y-%m")]

  data[, age :=
         (year(base::get(orderTimestampCol)) - year(base::get(acqTimestampCol))) * 12 +
         (month(base::get(orderTimestampCol)) - month(base::get(acqTimestampCol)))]

  data[, period :=
         (year(base::get(orderTimestampCol)) - year(min(base::get(orderTimestampCol)))) * 12 +
         month(base::get(orderTimestampCol)) - month(min(base::get(orderTimestampCol))) + 1]

  # data[, cohort := as.numeric(as.factor(cohortName))]
  data[, cohort := period - age]
}

AddQuarterlyCohorts <- function(data, orderTimestampCol, acqTimestampCol, custIdCol){
  data[, cohortName := LubridateQuarterToPrettyString(
    quarter(base::get(acqTimestampCol), with_year = T)
  )]

  data[, periodName := LubridateQuarterToPrettyString(
    quarter(base::get(orderTimestampCol), with_year = T)
  )]

  data[, age :=
         (year(base::get(orderTimestampCol)) - year(base::get(acqTimestampCol))) * 4 +
         (quarter(base::get(orderTimestampCol)) - quarter(base::get(acqTimestampCol)))]

  data[, period :=
         (year(base::get(orderTimestampCol)) - year(min(base::get(orderTimestampCol)))) * 4 +
         quarter(base::get(orderTimestampCol)) - quarter(min(base::get(orderTimestampCol))) +1]

  # data[, cohort := as.numeric(as.factor(cohortName))]
  data[, cohort := period - age]
}

AddYearlyCohorts <- function(data, orderTimestampCol, acqTimestampCol, custIdCol){
  data[, cohortName := as.character(year(base::get(acqTimestampCol)))]
  data[, periodName := as.character(year(base::get(orderTimestampCol)))]

  data[, age := year(base::get(orderTimestampCol)) - year(base::get(acqTimestampCol))]

  data[, period := year(base::get(orderTimestampCol)) - year(min(base::get(orderTimestampCol))) + 1]

  data[, cohort := period - age]
}

SegmentCohortData <- function(data, filterVariable){

  values <- unique(data[, base::get(filterVariable)])
  datasets <- lapply(values, function(value) {
    if (is.character(data[[filterVariable]])){
      subset <- data[eval(parse(text = glue("{filterVariable} == '{value}'")))]
    } else {
      subset <- data[eval(parse(text = glue("{filterVariable} == {value}")))]
    }
  })
  names(datasets) <- values
  return(datasets)
}


# 3_customer metrics ------------------------------------------------------

CohortPurchases <- function(data){
  out <- data %>%
    group_by(age, cohortName, cohort, period) %>%
    count() %>%
    rename(y = n)
  return(out)
}

CohortRevenue <- function(data){
  out <- data %>%
    group_by(age, period, cohort, cohortName) %>%
    summarise(y = sum(amountSpent))
  return(out)
}

CohortCustomers <- function(data){
  out <- data %>%
    group_by(age, period, cohort, cohortName) %>%
    summarise(y = n_distinct(customerId))
  return(out)
}

CustomerLTV <- function(data, days = 30){
  result <- data %>%
    mutate(
      interval = lubridate::interval(
        start = acqTimestamp,
        end = acqTimestamp + lubridate::days(days)
      )
    ) %>%
    mutate(isWithinInterval = ifelse(orderTimestamp %within% interval, T, F)) %>%
    filter(isWithinInterval == T) %>%
    group_by(customerId) %>%
    summarise(LTV = sum(amountSpent))
  #summarise(!! paste0("LTV", days) := sum(amountSpent)) %>%
  return(result)
}

CohortLTV <- function(data, days = 30, summarise = "mean"){
  result <- data %>%
    mutate(
      interval = lubridate::interval(
        start = acqTimestamp,
        end = acqTimestamp + lubridate::days(days)
      )
    ) %>%
    mutate(isWithinInterval = ifelse(orderTimestamp %within% interval, T, F)) %>%
    filter(isWithinInterval == T) %>%
    group_by(customerId, cohortName, cohort) %>%
    summarise(LTV = sum(amountSpent)) %>%
    ungroup() %>%
    group_by(cohortName, cohort) %>%
    summarise(LTV = mean(LTV))
  return(result)
}

CustomerCAC <- function(data){
  out <- unique(data[, c("customerId", "adSpent")])
  return(out %>% rename(CAC = adSpent))
}

CohortCAC <- function(data, summarise = "mean"){
  result <- data %>%
    group_by(cohortName, cohort) %>%
    summarise(CAC = mean(adSpent))
  return(result)
}

CustomerLTVCAC <- function(data, days = 30){
  LTV <- LTVperCustomer(data, days = days)
  CAC <- CACperCustomer(data)

  # inner join = return all rows from x where there are matching values in y,
  # and all columns from x and y
  return(LTV %>% inner_join(CAC) %>% mutate(LTVCAC = LTV / CAC))
}

CohortLTVCAC <- function(data, days = 30){
  LTV <- LTVperCohort(data, days = days)
  CAC <- CACperCohort(data)

  # inner join = return all rows from x where there are matching values in y,
  # and all columns from x and y
  return(LTV %>% inner_join(CAC) %>% mutate(LTVCAC = LTV / CAC))
}

# 4_visualisation ---------------------------------------------------------

PlotCohort <- function(data, plotType = "table", view = "CA"){
  # Args:
  #   plotType %in% c("table", "linechart")
  #   view %in% c("CA", "CP")
  if (plotType == "table"){
    PlotCohortTable(data = data, view = view)
  } else if (plotType == "linechart"){
    PlotCohortLinechart(data = data, view = view)
  } else {
    message("Wrong plotType or view.")
    return(NULL)
  }
}

PlotCohortTable <- function(data, view = "CA"){
  if (view == "CA"){
    plt <- ggplot(
      data,
      aes(x = age, y = reorder(cohortName, desc(cohortName)), fill = y)
    )
  } else if (view == "CP") {
    plt <- ggplot(
      data,
      aes(x = period, y = reorder(cohortName, desc(cohortName)), fill = y)
    )
  } else {
    message("Choose one of 'CA' or 'CP' for view.")
    return(NULL)
  }

  plt <- plt +
    geom_raster() +
    scale_fill_continuous(high = "#239af6", low = "#e7f4fe") +
    theme_everest() +
    geom_text(mapping = aes(label = y), color = "black") +
    ylab(label = "Cohort")

  return(plt)
}

PlotCohortLinechart <- function(data, view = "CA"){
  if (view == "CA"){
    x <- "age"
  } else if (view == "CP"){
    x <- "period"
  } else {
    message("Choose one of 'CA' or 'CP' for view.")
    return(NULL)
  }
  ggplot(data, aes_string(x = x, y = "y", color = "cohortName")) +
    geom_line() +
    theme_gg()
}

theme_everest <- function(){
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
