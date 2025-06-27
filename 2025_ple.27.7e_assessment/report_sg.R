### ------------------------------------------------------------------------ ###
### create ICES standard graphs for advice sheet ####
### ------------------------------------------------------------------------ ###

## Before: method/advice.rds
##         method/advice_history.rds
## After:  report/standard_graphs/ple.27.7e_YYYY.xml

### load packages
suppressPackageStartupMessages(library(icesTAF))
taf.libPaths()
suppressPackageStartupMessages(library(icesSAG))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(cat3advice))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

mkdir("report/standard_graphs")

if (!exists("verbose")) verbose <- FALSE

### ------------------------------------------------------------------------ ###
### load data ####
### ------------------------------------------------------------------------ ###
advice <- readRDS("method/advice.rds")
catch <- read.taf("data/advice_history.csv")

### ------------------------------------------------------------------------ ###
### create SAG objects ####
### ------------------------------------------------------------------------ ###
### assessment year
ass_yr <- 2025

### list of possible elements:
### https://datsu.ices.dk/web/selRep.aspx?Dataset=126
### allowed units:
### https://vocab.ices.dk/?ref=155

### set up stock info
if (isTRUE(verbose)) {
  ContactPerson <- readline(prompt = "Enter ContactPerson/email: ")
} else {
  ContactPerson <- ""
}
stk_info <- stockInfo(
  StockCode = "ple.27.7e",
  AssessmentYear = ass_yr,
  ContactPerson = ContactPerson,
  Purpose = "Advice",
  StockCategory = "3.22", # chr rule, see https://vocab.ices.dk/?ref=1526
  ModelType = "None", # https://vocab.ices.dk/?ref=1524
  ModelName = "None"
)

### add some more data manually
stk_info$CustomLimitName1 <- "I_{trigger}" ### for biomass index plot
stk_info$CustomLimitValue1 <- advice@b@Itrigger
stk_info$CustomLimitName2 <- "HR_{MSY proxy}" ### for length-based indicator
stk_info$CustomLimitValue2 <- advice@F@value
stk_info$StockSizeDescription <- "Biomass Index"
#stk_info$StockSizeUnits <- "kg hr-1 m beam-1" ### units: https://vocab.ices.dk/?ref=155
stk_info$CatchesLandingsUnits <- "t" ### t for tonnes
# stk_info$CustomSeriesUnits1 <- "Relative harvest rate in tonnes / kg hr-1 m beam-1"
# stk_info$CustomSeriesName1 <- "Relative harvest rate"
#stk_info$FishingPressureDescription <- "Relative harvest rate"
#stk_info$FishingPressureUnits <- "tonnes"

### get and format data
df_catch <- catch %>%
  select(Year = year,
         Landings = ICES_landings_stock,
         Discards = ICES_discards_stock) %>%
  filter(!is.na(Landings))
df_idx <- advice@I@idx %>%
  select(Year = year, StockSize = index)
df_hr <- advice@F@HR@data %>%
  filter(!is.na(landings)) %>%
  select(Year = year, FishingPressure = harvest_rate)
df <- Reduce(full_join, list(df_catch, df_idx, df_hr))

### set up data
# https://datsu.ices.dk/web/selRep.aspx?Dataset=126  # Record: AF - Fish Data
stk_data <- stockFishdata(
  Year = df$Year,
  Landings = df$Landings,
  Discards = df$Discards,
  StockSize = df$StockSize,
  FishingPressure = df$FishingPressure
)

### save as XML file
xmlfile <- createSAGxml(stk_info, stk_data)
writeSAGxml(info = stk_info, fishdata = stk_data, 
            file = "report/standard_graphs/ple.27.7e_SAG.xml")
### this file can be manually uploaded at
### https://standardgraphs.ices.dk/manage/index.aspx
### Alternatively: do it all from R (see below)


### ------------------------------------------------------------------------ ###
### automatic upload of data and configuration of plots/data ####
### ------------------------------------------------------------------------ ###

### ICES standard graphs
### create token for authentication
### go to https://standardgraphs.ices.dk/manage/index.aspx
### login
### click on create token or go directly to
### https://standardgraphs.ices.dk/manage/CreateToken.aspx
### create new token, save in file
# file.edit("~/.Renviron")
### in the format
### SG_PAT=some_token_......
### save and restart R

if (isTRUE(verbose)) {

  ### load token
  Sys.getenv("SG_PAT")
  options(icesSAG.use_token = TRUE)
  ### may need setting username and password despite token...
  # icesConnect::set_username(readline())
  
  ## check assessments keys
  key <- findAssessmentKey("ple.27.7e", year = ass_yr)
  key_last <- findAssessmentKey("ple.27.7e", year = ass_yr - 2) ### biennial

  ### last year's graphs
  # plot(getSAGGraphs(key_last))
  ### doesn't work ... error 404

  ### last year's graphs
  # settings_last <- getSAGSettingsForAStock(key_last)

  ### upload
  key_new <- uploadStock(info = stk_info, fishdata = stk_data, verbose = TRUE)
  ### key_new <- 19051
  # findAssessmentKey('ple.27.7e', ass_yr, full = TRUE)$AssessmentKey

  ### plots and settings not working properly...
  #icesSAG:::plot.ices_standardgraph_list(getSpawningStockBiomassGraph(key_new))
  ### return a plot with text "is not published yet"...

  # ### check upload
  # windows() ### RStudio's interal plot pane causes RStudio to crash...
  # plot(getSAGGraphs(key_new))
  #
  # ### get chart settings
  # ### should be automatically copied from last year
  # chart_settings <- getSAGSettingsForAStock(key_new)
  #
  # plot(getLandingsGraph(key_new))
  #
  # ### compare with last years settings
  # settings_last <- getSAGSettingsForAStock(key_last)
  # all.equal(chart_settings, settings_last)
  # ### yes, identical (only new assessment key)

  ### modify chart settings
  ### possible options listed here:
  ### https://standardgraphs.ices.dk/manage/ListSettings.aspx

  # ### check again
  # getSAGSettingsForAStock(key_new)
  # windows()
  # plot(getSAGGraphs(key_new))
  #

}

