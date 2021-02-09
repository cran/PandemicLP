## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PandemicLP)

## -----------------------------------------------------------------------------
regions <- c("PR", "SC","RS")
last_date <- "2020-10-01"
case_type <- "deaths"

## -----------------------------------------------------------------------------
data <- list()
outputs <- list()
preds <- list()
states <- state_list()
# Load precomputed MCMC
download.file("http://github.com/CovidLP/PandemicLP/raw/master/temp/PandemicLP_SumRegions.rda","./PandemicLP_SumRegions.rda")
load("./PandemicLP_SumRegions.rda")
for(i in 1:length(regions)) { 
  if (is.na(match(regions[i],states$state_abb))){
    data[[i]] <- load_covid(country_name=regions[i],last_date=last_date)
  } else {
    data[[i]] <- load_covid(country_name="Brazil", state_name = regions[i],last_date=last_date)
  }
  # Commenting to reduce vignette runtime
  # outputs[[i]] <- pandemic_model(data[[i]],case_type = case_type, covidLPconfig = TRUE)
  preds[[i]] <- posterior_predict(outputs[[i]])
}
# Load precomputed MCMC
download.file("http://github.com/CovidLP/PandemicLP/raw/master/temp/PandemicLP_SumRegions.rda","./PandemicLP_SumRegions.rda")
load("./PandemicLP_SumRegions.rda")

## -----------------------------------------------------------------------------
# consider the file for the first region (to save the right format)
data_base <- preds[[1]]
bind_regions <- regions[1]
for (i in 2:length(regions)) {
  bind_regions <- paste(bind_regions, "and", regions[i])
}
data_base$location <- bind_regions

## -----------------------------------------------------------------------------
# get the mean sample and set it to be dates x mcmc sample
mu_t <- t(data_base$pastMu)

# include the dates in the data frame
mu_final <- data.frame(data = data_base$data$date,mu_t)
names_mu <- names(data_base$pastMu)

# get hidden objects (necessary for the pandemic_stats function)
hidden_short_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandShortPred 
hidden_long_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandLongPred 
hidden_mu_total <- methods::slot(data_base$fit,"sim")$fullPred$thousandMus

# loop for each region - starting with the second one 
for (u in 2:length(regions)) {
  
  # preds for the selected region
  data_region <- preds[[u]]
  
  # sum the variables predictive_Long, predictive_Short and futMu 
  data_base$predictive_Long <- data_base$predictive_Long +
    data_region$predictive_Long
  data_base$predictive_Short <- data_base$predictive_Short +
    data_region$predictive_Short
  data_base$futMu <- data_base$futMu + data_region$futMu
  
  # create a large data frame by concatenating samples for current state in the mean data frame
  mu_t <- t(data_region$pastMu)
  mu_2 <- data.frame(data = data_region$data$date,mu_t)
  names_mu <- c(names_mu,names(data_region$pastMu))
  mu_final <- rbind(mu_final,mu_2)
  
  # merge datasets by date since they can differ on start
  data_base$data <- merge(data_base$data,data_region$data, 
                          by = "date", all = TRUE)
  data_base$data[is.na(data_base$data)] = 0
  data_base$data$cases.x = data_base$data$cases.x +
    data_base$data$cases.y
  data_base$data$deaths.x = data_base$data$deaths.x +
    data_base$data$deaths.y
  data_base$data$new_cases.x = data_base$data$new_cases.x +
    data_base$data$new_cases.y
  data_base$data$new_deaths.x = data_base$data$new_deaths.x +
    data_base$data$new_deaths.y
  data_base$data <- data_base$data[,-c(6:9)]
  names(data_base$data) <- c("date","cases","deaths",
                             "new_cases","new_deaths")
  
  # sum hidden objects (necessary for the pandemic_stats function) 
  hidden_short_region <- methods::slot(data_region$fit,"sim")$fullPred$thousandShortPred 
  hidden_short_total <- hidden_short_total + hidden_short_region
  hidden_long_region <- methods::slot(data_region$fit,"sim")$fullPred$thousandLongPred 
  hidden_long_total <- hidden_long_total + hidden_long_region
  hidden_mu_region <- methods::slot(data_region$fit,"sim")$fullPred$thousandMus
  hidden_mu_total <- hidden_mu_total + hidden_mu_region
  
}

# create hidden object (necessary for the pandemic_stats function)
methods::slot(data_base$fit,"sim")$fullPred$thousandShortPred <- hidden_short_total
methods::slot(data_base$fit,"sim")$fullPred$thousandLongPred <- hidden_long_total
methods::slot(data_base$fit,"sim")$fullPred$thousandMus <- hidden_mu_total

# aggregate the mean samples
mu_final <- aggregate(. ~ data, data=mu_final, FUN=sum)
mu_final <- mu_final[,-1]
mu_final <- t(mu_final)
names_mu <- unique(names_mu)
colnames(mu_final) <- names_mu
data_base$pastMu <- mu_final

## ---- fig.asp=2/(sqrt(5)+1), fig.width=8, fig.align='center', warnings = FALSE----
plots <- plot(data_base,term = "both")
plots$long
plots$short

