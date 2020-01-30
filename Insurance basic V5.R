# Clean existing variables
rm(list=ls())
library(plyr)
library(tidyr)
library(dplyr)
library(reshape2)
library(frm)
library(httr)
library(lmtest)
library(sandwich)
library(broom)
library(margins)
library(ggplot2)
library(RColorBrewer)
library(pastecs)

getRMA <- function(type, years) {
  # this function downloads individual yearly policy and claim files from the RMA website  
  # and provides a data frame.
  # inputs:
  #   type: "policy" or "claim"
  #   years: year of the data to be downloaded
  # output:
  #   dataframe
  
  if (type != "policy" & type != "claim") {
    return ("type should be 'policy' or 'claim'")
  }
  dfs <- list()
  for (y in 1:length(years)) {
    year <- years[y]
    if (type == "policy") {
      myurl <- paste(paste("https://www.rma.usda.gov/-/media/RMAweb/SCC-SOB/State-County-Crop-Coverage/sobcov_", year, sep = ""), ".ashx?la=en", sep = "")
      cols <- c("year", "stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", 
                "insplancode","insplanname", "covcateg", "delivtype", "covlevel", "polsold", "polprem", "polindemn", "unitssold", 
                "unitsindemn", "quanttype", "acres", "endorsedacres",  
                "liab", "totalpremium", "subsidies", "indemnityamount", "lossratio")
    }
    if (type == "claim") {
      myurl <- paste(paste("https://www.rma.usda.gov/-/media/RMAweb/Cause-Of-Loss/Summary-of-Business-with-Month-of-Loss/colsom_", year, sep = ""), ".ashx?la=en", sep = "")
      cols  <- c("year", "stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", 
                 "insplancode","insplanname", "covcateg", "stagecode", "damagecausecode", "damagecausedesc", "monthloss", "monthname", "yearloss", "polprem", 
                 "polindemn",  "acres", "endorsedacres",  
                 "liab", "totalpremium", "producerpremuim", "subsidies", "stateprivatesubs", "addsubs", "EFApremdiscount", "lostacres", "indemnityamount", "lossratio")
    }
    response <- GET(myurl)
    writeBin(content(response, as = "raw"), "~/Downloads/temp.zip")
    fName <- unzip("~/Downloads/temp.zip", list = TRUE)$Name
    unzip("~/Downloads/temp.zip", exdir = "temp")
    pathtemp <- paste0(getwd(), "/temp/")
    ## add skipNul = TRUE, strip.white = TRUE
    my_data <- read.table(paste0(pathtemp, fName), sep ="|", header = FALSE, dec =".", quote = "", fill=TRUE, skipNul = TRUE, strip.white = TRUE)
    names(my_data) <- cols
    #my_data$covcateg <- trimws(my_data$covcateg, which = "right") 
    #if (type == "policy") {
    #  my_data$quanttype <- trimws(my_data$quanttype, which = "right")
    #}
    dfs[[y]] <- my_data
  }
  data <- do.call("rbind", dfs)
  
  return(data)
}

polacres <- getRMA(type='policy', years=c(2017, 2018))
print(dim(polacres))

claims <- getRMA(type='claim', years=c(2017, 2018))
print(dim(claims))

format_policies <- function(df) {
  # TODO: add docstring
  
  df <- reshape(df, idvar = c("year", "stfips", "stabb", "cntyfips", "cntyname", 
                              "commoditycode", "commodityname", "insplancode", 
                              "insplanname", "covcateg", "claim"), 
                timevar = "covlevel", v.names=c("acres", "unitssold"), direction = "wide")
  df[is.na(df)] <- 0
  # calculate the mean coverage level and the number of units sold by year, stfips, 
  # cntyfips, commoditycode, insplancode, covcateg, claim combination
  df$acres.total <- df$acres.0.95 + df$acres.0.9 + df$acres.0.85 + df$acres.0.8 + df$acres.0.75 + 
    df$acres.0.7 + df$acres.0.65 + df$acres.0.6 + df$acres.0.55 + df$acres.0.5 
  df$meancov <- (0.95*df$acres.0.95 + 0.9*df$acres.0.9 + 0.85*df$acres.0.85 + 0.8*df$acres.0.8 + 
                   0.75*df$acres.0.75 + 0.7*df$acres.0.7 + 0.65*df$acres.0.65 + 0.6*df$acres.0.6 + 
                   0.55*df$acres.0.55 + 0.5*df$acres.0.5) / df$acres.total
  df$unitssold <- df$unitssold.0.95 + df$unitssold.0.9 + df$unitssold.0.85 + df$unitssold.0.8 + 
    df$unitssold.0.75 + df$unitssold.0.7 + df$unitssold.0.65 + df$unitssold.0.6 + 
    df$unitssold.0.55 + df$unitssold.0.5
  myvars <- c("year","stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", 
              "insplancode", "insplanname", "covcateg", "claim", "acres.total", "unitssold", "meancov")
  df <- df[myvars]
  
  return(df)
}

# only keep policies with premium expressed in acres and known commodity code
polacres <- polacres[polacres$polprem > 0 &  
                       polacres$quanttype == "Acres" & polacres$acres > 0 & polacres$commoditycode != 9999, ]
# claim = dummy equal to 1 if at least one claim
polacres$claim <- ifelse(polacres$unitsindemn > 0, 1, 0)
myvars <- c("year","stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", "insplancode", "insplanname", "covcateg", "covlevel", "unitssold", "claim", "acres")
polacres <- polacres[myvars]

polacres <- format_policies(polacres)

# we split the policy file into polacresclaim for which we have claims
polacresclaim <- polacres[polacres$claim == 1,]
# and polacresnoclaim for which we have no claims. We save this file for later.
polacresnoclaim <- polacres[polacres$claim == 0,]
polacresnoclaim$lostacres <- 0


# only keep wildlife claims with insured and lost acres and known commodity code
claimswild <- claims[claims$acres > 0 & 
                       claims$commoditycode != 9999 & claims$lostacres > 0 & claims$damagecausecode == 93, ]
claimswild <- claims[claims$acres > 0 & 
                       claims$lostacres > 0 & claims$damagecausecode == 93, ]

claimswild<-aggregate(cbind(lostacres) ~ year + stfips + stabb + cntyfips + cntyname + commoditycode + 
                        commodityname + insplancode + insplanname + covcateg, 
                      data = claimswild, FUN = function(x) sum(x), na.action=NULL )
claimspol<-join(polacresclaim, claimswild, by=c("year", "stfips", "stabb", "cntyfips", "cntyname", 
                                                "commoditycode", "commodityname", "insplancode", 
                                                "insplanname", "covcateg"), type='left')

# we can now append the 2 files
claimpoltot <- bind_rows(claimspol, polacresnoclaim)
claimpoltot$lostacres[is.na(claimpoltot$lostacres)] <- 0
claimpoltot$ratio <- claimpoltot$lostacres/claimpoltot$acres.total
claimpoltot$ratio[claimpoltot$ratio > 1] <- 1
claimpoltot$lostacres <- claimpoltot$ratio*claimpoltot$acres
claimpoltot$fullfips <- claimpoltot$stfips*1000 + claimpoltot$cntyfips
claimpoltot$logacres <- log(claimpoltot$acres.total)
claimpoltot$logunitssold <- log(claimpoltot$unitssold)
claimpoltot$cat <- ifelse(claimpoltot$covcateg=="C", 1, 0)
claimpoltot$y2017 <- ifelse(claimpoltot$year==2017, 1, 0)

setwd ( "~/Dropbox/APHIS/Insurance/Data")

write.csv(claimpoltot, file = "claimspoltot1.csv",row.names=FALSE)

# create string variables to be treated as categorical variables
claimpoltot$fullfips.ch <- as.character(claimpoltot$fullfips)
claimpoltot$commoditycode.ch <- as.character(claimpoltot$commoditycode)
claimpoltot$insplancode.ch <- as.character(claimpoltot$insplancode)

model_glm = glm(
  ratio ~  meancov + logacres + logunitssold + cat + y2017 + insplancode.ch + commoditycode.ch + fullfips.ch,
  epsilon = 1e-6,
  data = claimpoltot,
  family = quasibinomial
)

# get robust standard errors
tidy(coeftest(model_glm, vcov = vcovHC(model_glm, type="HC")))

summary(model_glm)

stat.desc(claimpoltot$ratio) 
stat.desc(claimpoltot$y2017) 
stat.desc(claimpoltot$cat) 
stat.desc(claimpoltot$meancov) 
stat.desc(claimpoltot$logacres) 
stat.desc(claimpoltot$logunitssold) 

effects_fracreg <- margins(model_glm, variables = c("meancov", "logacres", "logunitssold", "y2017", "cat"))
summary(effects_fracreg)

effects_fracreg_tot <- margins(model_glm)

m <- marginal_effects(model_glm, variables = c("meancov", "logacres", "logunitssold", "y2017", "cat"))

marginal_effects_table <- matrix(NA, ncol(m), 5)

summary(m)
for (i in 1:ncol(m)) {
  marginal_effects_table[i,1] <- mean(m[,i])
  marginal_effects_table[i,2] <- sd(m[,i]) / sqrt(length(m[,i]))
  marginal_effects_table[i,3] <- marginal_effects_table[i,1] / marginal_effects_table[i,2]
  marginal_effects_table[i,4] <- 2*pt(-abs(marginal_effects_table[i,3]),df=length(m[,i])-1)
  marginal_effects_table[i,5] <- length(m[,i])
}
rownames(marginal_effects_table) <- colnames(m)
me_names <- c("Mean", "SE", "tstat", "p-value", "n")
colnames(marginal_effects_table) <- me_names
marginal_effects_table
format(marginal_effects_table[,4], scientific = TRUE)

