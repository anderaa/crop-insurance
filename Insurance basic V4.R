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
library(fastDummies)
library(margins)

getRMA <- function(type, year) {
  # this function downloads individual yearly policy and claim files from the RMA website  
  # and provides a data frame.
  # inputs:
  #   type: "policy" or "claim "
  #   year: year of the data to be downloaded
  # output:
  #   dataframe
  if (type != "policy" & type != "claim") return ("type should be 'policy' or 'claim'")
  if (type == "policy")  
    myurl <- paste(paste("https://www.rma.usda.gov/-/media/RMAweb/SCC-SOB/State-County-Crop-Coverage/sobcov_", year, sep = ""), ".ashx?la=en", sep = "")
  if (type == "claim") 
    myurl <- paste(paste("https://www.rma.usda.gov/-/media/RMAweb/Cause-Of-Loss/Summary-of-Business-with-Month-of-Loss/colsom_", year, sep = ""), ".ashx?la=en", sep = "")
  
  response <- GET(myurl)
  writeBin(content(response, as = "raw"), "~/Downloads/temp.zip")
  fName <- unzip("~/Downloads/temp.zip", list = TRUE)$Name
  unzip("~/Downloads/temp.zip", exdir = "temp")
  pathtemp <- paste0(getwd(), "/temp/")
  my_data <- read.table(paste0(pathtemp, fName), sep ="|", header = FALSE, dec =".", quote = "", fill=TRUE)
  if (type == "policy")  
  {
    names(my_data) <- c("year", "stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", 
                        "insplancode","insplanname", "covcateg", "delivtype", "covlevel", "polsold", "polprem", "polindemn", "unitssold", 
                        "unitsindemn", "quanttype", "acres", "endorsedacres",  
                        "liab", "totalpremium", "subsidies", "indemnityamount", "lossratio")
    my_data$covcateg <- trimws(my_data$covcateg, which = "right") 
    my_data$quanttype <- trimws(my_data$quanttype, which = "right")
  }
  if (type == "claim")  
  {
    names(my_data) <- c("year", "stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", 
                        "insplancode","insplanname", "covcateg", "stagecode", "damagecausecode", "damagecausedesc", "monthloss", "monthname", "polprem", 
                        "polindemn",  "acres", "endorsedacres",  
                        "liab", "totalpremium", "subsidies", "lostacres", "indemnityamount", "lossratio")
    my_data$covcateg <- trimws(my_data$covcateg, which = "right")
  }
  
  return(my_data)
}

format_policies <- function(policyfile) {
  # this function downloads and saves individual yearly policy and claim files from the RMA website under 
  # a subdirectory "Data"
  # inputs:
  #   type: "policy" or "claim "
  #   year: year of the data to be downloaded
  policyfile<-reshape(policyfile, idvar = c("year", "stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", "insplancode", "insplanname", "covcateg", "claim"), timevar = "covlevel", v.names=c("acres", "unitssold"), direction = "wide")
  policyfile[is.na(policyfile)] <- 0
  # We want to calculate the mean coverage level and the number of units sold by "year", "stfips", "cntyfips", "commoditycode", "insplancode", "covcateg", "claim" combination
  attach(policyfile)
  policyfile$acres.total <- acres.0.95 + acres.0.9 + acres.0.85 + acres.0.8 + acres.0.75 + acres.0.7 + acres.0.65 + acres.0.6 + acres.0.55 + acres.0.5 
  policyfile$meancov <- (0.95*acres.0.95 + 0.9*acres.0.9 + 0.85*acres.0.85 + 0.8*acres.0.8 + 0.75*acres.0.75 + 0.7*acres.0.7 + 0.65*acres.0.65 + 0.6*acres.0.6 + 0.55*acres.0.55 + 0.5*acres.0.5) / policyfile$acres.total
  policyfile$unitssold <- unitssold.0.95 + unitssold.0.9 + unitssold.0.85 + unitssold.0.8 + unitssold.0.75 + unitssold.0.7 + unitssold.0.65 + unitssold.0.6 + unitssold.0.55 + unitssold.0.5 
  myvars <- c("year","stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", "insplancode", "insplanname", "covcateg", "claim", "acres.total", "unitssold", "meancov")
  policyfile <- policyfile[myvars]
  detach(policyfile)
  return(policyfile)
}

# Downloads policy files for 2017 and 2018
polacres <- getRMA(year=2017, type="policy")

temp <- getRMA(year=2018, type="policy")
polacres <- rbind(polacres, temp)
rm(temp)

# Only keep policies with premium expressed in acres and known county fips and commodity code
attach(polacres)
polacres <- polacres[polprem > 0 & cntyfips < 999 & quanttype == "Acres" & acres > 0 & commoditycode != 9999, ]
# claim = dummy equal to 1 if at least one claim
polacres$claim <- ifelse(polacres$unitsindemn >0, 1, 0)
myvars <- c("year","stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", "insplancode", "insplanname", "covcateg", "covlevel", "unitssold", "claim", "acres")
polacres <- polacres[myvars]
detach(polacres)

polacres <- format_policies(polacres)

# we split the policy file into polacresclaim for which we have claims
polacresclaim <- polacres[polacres$claim == 1,]
# and polacresnoclaim for which we have no claims. We save this file for later.
polacresnoclaim <- polacres[polacres$claim == 0,]
polacresnoclaim$lostacres <- 0

# Claim files: load only 2017 and 2018

claims <- getRMA(year=2017, type="claim")
temp <- getRMA(year=2018, type="claim")
claims <- rbind(claims, temp)
rm(temp)

# Only keep wildlife claims with insured and lost acres and known county fips and commodity code
attach(claims)
claimswild <- claims[cntyfips < 999 & acres > 0 & commoditycode != 9999 & lostacres > 0 & damagecausecode == 93, ]
detach(claims)

claimswild<-aggregate(cbind(lostacres) ~ year + stfips + stabb + cntyfips + cntyname + commoditycode + commodityname + insplancode + insplanname + covcateg, data = claimswild, FUN = function(x) sum(x), na.action=NULL )
claimspol<-join(polacresclaim, claimswild,by=c("year", "stfips", "stabb", "cntyfips", "cntyname", "commoditycode", "commodityname", "insplancode", "insplanname", "covcateg"), type='left')

# We can now append the 2 files
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

#write.csv(claimpoltot, file = "claimspoltot1.csv",row.names=FALSE)
# Rest in Stata (estimate lost acres.do)

# create string variables to be treated as categorical variables
claimpoltot$fullfips.ch<-as.character(claimpoltot$fullfips)
claimpoltot$commoditycode.ch<-as.character(claimpoltot$commoditycode)
claimpoltot$insplancode.ch<-as.character(claimpoltot$insplancode)

model_glm = glm(
  ratio ~  meancov + logacres + logunitssold + cat + y2017 + insplancode.ch + commoditycode.ch + fullfips.ch,
  epsilon = 1e-6, ## Match Stat's convergence threshold
  data = claimpoltot,
  family = quasibinomial
)

# To get robust standard errors
tidy(coeftest(model_glm, vcov = vcovHC(model_glm, type="HC")))

summary(model_glm)
(m <- margins(model_glm))
summary(m)

# Estimated lost acres if no low coverage
new<-claimpoltot[claimpoltot$year == 2018,]
new$meancov<-1 # 100%
new$insplancode.ch<-"1"
new$cat<-0
#ratio.hat<-as.vector(predict(model_glm, new, se.fit = FALSE))
new$ratio.hat<-predict(model_glm, new, se.fit = FALSE, type = "response")

summary(new$ratio.hat)

new$lostacres.hat <- new$acres.total * new$ratio.hat
new<-aggregate(cbind(acres.total, lostacres, lostacres.hat) ~ year + fullfips + stfips + stabb + cntyfips + cntyname + commoditycode + commodityname, data = new, FUN = function(x) sum(x), na.action=NULL )

new$ratio.county = new$lostacres/new$acres.total
new$ratio.county.hat = new$lostacres.hat/new$acres.total
summary(new$ratio.county)
summary(new$ratio.county.hat)

write.csv(new, file = "/Users/sophiemckee/Dropbox/APHIS/Insurance/Paper on Wildlife Claims/predictions_ratio_allfips_1.csv",row.names=FALSE)

############
# Code below is not used
##############
y <- claimpoltot$ratio

x <- claimpoltot[,c("fullfips", "commoditycode", "y2017", "insplancode", "meancov", "logacres", "logunitssold", "cat" )]

x  <- dummy_cols(x, select_columns = "fullfips")
x  <- dummy_cols(x, select_columns = "commoditycode")
x  <- dummy_cols(x, select_columns = "insplancode")
x$fullfips <- NULL
x$commoditycode <- NULL
x$insplancode <- NULL

x <- claimpoltot[,c("fullfips", "commoditycode", "y2017", "insplancode", "meancov", "logacres", "logunitssold", "cat" )]
#x <- claimpoltot[,c("logacres", "logunitssold")]

summary(x)
myfrm <- frm(y, x, linkfrac = 'logit', epsilon = 1e-4)

# GLMs are fit via Fisher scoring which, as Dimitriy V. Masterov notes, is Newton-Raphson with the expected Hessian instead 
# (i.e. we use an estimate of the Fisher information instead of the observed information). If we are using the canonical 
# link function it turns out that the observed Hessian equals the expected Hessian so NR and Fisher scoring are the same 
# in that case. Either way, we'll see that Fisher scoring is actually fitting a weighted least squares linear model, 
# and the coefficient estimates from this converge* on a maximum of the logistic regression likelihood. 

claimpoltot <- dummy_cols(claimpoltot, select_columns = c("insplancode"))
claimpoltotfips <- dummy_cols(claimpoltot, select_columns = c("fullfips"))

PredictorVariables <- "insplancode_1"
uniqueCodes <- sort(unique(claimpoltot$insplancode))
for (index in uniqueCodes) { 
  if (index != 1)
  {dummy_var <- paste("insplancode_", index, sep="")
  PredictorVariables <- paste(PredictorVariables, dummy_var, sep=" + ")}
}

PredictorVariables <- "fullfips_1001"
uniqueCodes <- sort(unique(claimpoltot$fullfips))
for (index in uniqueCodes) { 
  if (index != 1001)
  {dummy_var <- paste("fullfips_", index, sep="")
  PredictorVariables <- paste(PredictorVariables, dummy_var, sep=" + ")}
}
Formula <- paste("ratio ~  y2017 + meancov + logacres + ", 
                 PredictorVariables, sep="")

#model_glm = glm(
#  Formula,
#  epsilon = 1e-4,
#  data = claimpoltotfips,
#  family = quasibinomial,
#)

