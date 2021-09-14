## ----set_options, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(PandemicLP)

## ----load_data, fig.asp=2/(sqrt(5)+1), fig.width=8, fig.align='center', warnings = FALSE----
MGdata <- load_covid("Brazil","MG", last_date = "2020-11-09")
plot(MGdata)$new

## ----load_estimated_from_internet---------------------------------------------
#MGestimated <- pandemic_model(MGdata, case_type = "deaths", covidLPconfig = TRUE)
## Using pre-generated results for speed
temp <- tempfile(fileext = ".rda")
d <- download.file("https://drive.google.com/u/2/uc?id=165mXm5DbtPENGJlVLVJvzvVfFFWxT_dr&export=download",
  temp, mode = "wb", quiet = TRUE)

# Try to use downloaded version. If not available, run the model
if (!d) load(temp) else {
  warning("Data failed to download from drive. Please check internet connection and try again.")
  knitr::knit_exit()
}

## ----print_estimation---------------------------------------------------------
MGestimated

## ----traceplot_density, fig.asp=2/(sqrt(5)+1), fig.align='center', fig.width=4----
traceplot(MGestimated)+theme(legend.position = "")
density(MGestimated)

## ----make_predictions---------------------------------------------------------
MGpredicted <- posterior_predict(MGestimated, horizonLong=200)
MGpredicted

## ----check_pandemic_stats-----------------------------------------------------
pandemic_stats(MGpredicted)

## ----plot_predictions, fig.asp=2/(sqrt(5)+1), fig.width=8, fig.align='center', warnings = FALSE----
MGplots <- plot(MGpredicted,term="both")
MGplots$long
MGplots$short

