##### Implementation Begins ####
#This code replicates the FED's paper 

require("BMA")
require("PerformanceAnalytics")

### Load the Data
fedData_raw <- read.csv("~/Upwork/Betasmartz - Black Litterman/FED's Model/Fed_Data_R.csv")

##### Preprocesing the data for making the analysis #####

#Coercing the datatypes from factor to numeric
indx <- sapply(fedData_raw[,2:(dim(fedData_raw)[2])], is.factor)
fedData_raw[indx] <- lapply(fedData_raw[,2:(dim(fedData_raw)[2])][indx], function(x) as.numeric(as.character(x)))
fedData_raw[,1] <- as.Date(as.character(fedData_raw[,1]) , format="%d/%m/%Y")

fedData_raw['Returns_M1MS']        <-0
fedData_raw['Returns_VM1MSUS']     <-0
fedData_raw['Returns_TWUSDIB']     <-0
fedData_raw['Returns_USRPCE']      <-0
fedData_raw['Returns_USRDPI']      <-0
fedData_raw['Returns_USNHPUM']     <-0
fedData_raw['Returns_USTNP']       <-0
fedData_raw['Returns_USICUI']      <-0
fedData_raw['Returns_USMAWH']      <-0
fedData_raw['Returns_USPMI']       <-0
fedData_raw['Returns_SPECCDCI']    <-0
fedData_raw['Returns_USGDP']       <-0
fedData_raw['Returns1_SP500']       <-0
fedData_raw['Returns3_SP500']       <-0
fedData_raw['Returns_USGDP']       <-0
fedData_raw['Cycle_var']           <-0


#Rearange the columns
col_idx <- grep("NBER", names(fedData_raw))
fedData_raw <- fedData_raw[, c((1:ncol(fedData_raw))[-col_idx],col_idx )]
#names(fedData_raw)

#Double Check Data Types
#str(fedData_raw)

### Transforming the Dataframe to xts
fedData <-xts(fedData_raw[,-c(1,dim(fedData_raw)[2])],order.by=fedData_raw[,1])
cycleNBER <- xts(fedData_raw[,dim(fedData_raw)[2]],order.by=fedData_raw[,1])


##Filtrating the data as quarterly
fedData_Q <- aggregate(fedData, as.yearqtr, function(x) tail(x, n=1))
cycleNBER_Q <- aggregate(cycleNBER, as.yearqtr, function(x) names(sort(-table(x)))[1])

## Calculating the Quarterly returns for the variables of interest
fedData_Q[,'Returns_M1MS']        <- unlist(c(NA,diff(log(fedData_Q[,'M1MS']))))
fedData_Q[,'Returns_VM1MSUS']     <- unlist(c(NA,diff(log(fedData_Q[,'VM1MSUS']))))
fedData_Q[,'Returns_TWUSDIB']     <- unlist(c(NA,diff(log(fedData_Q[,'TWUSDIB']))))
fedData_Q[,'Returns_USRPCE']      <- unlist(c(NA,diff(log(fedData_Q[,'USRPCE']))))
fedData_Q[,'Returns_USRDPI']      <- unlist(c(NA,diff(log(fedData_Q[,'USRDPI']))))
fedData_Q[,'Returns_USNHPUM']     <- unlist(c(NA,diff(log(fedData_Q[,'USNHPUM']))))
fedData_Q[,'Returns_USTNP']       <- unlist(c(NA,diff(log(fedData_Q[,'USTNP']))))
fedData_Q[,'Returns_USICUI']      <- unlist(c(NA,diff(log(fedData_Q[,'USICUI']))))
fedData_Q[,'Returns_USMAWH']      <- unlist(c(NA,diff(log(fedData_Q[,'USMAWH']))))
fedData_Q[,'Returns_USPMI']       <- unlist(c(NA,diff(log(fedData_Q[,'USPMI']))))
fedData_Q[,'Returns_SPECCDCI']    <- unlist(c(NA,diff(log(fedData_Q[,'SPECCDCI']))))
fedData_Q[,'Returns_USGDP']       <- unlist(c(NA,diff(log(fedData_Q[,'USGDP']))))

## Creation of new Bussiness Cycle Variable
Threshold = 0.002
fedData_Q[1,'Cycle_var'] <- c("Peak to Equilibrium")

### Filtering the Equilibrium to Peak
index_threshold = fedData_Q[,'Returns_USGDP']> Threshold
index_composed = index_threshold  & !(as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Equilibrium to Peak")

### Filtering Peak to Equilibirum
index_threshold = fedData_Q[,'Returns_USGDP']> Threshold
index_composed = index_threshold  & (as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Peak to Equilibrium")

### Filtering Trough to Equilibrium
index_threshold = fedData_Q[,'Returns_USGDP'] < Threshold
index_composed = index_threshold  & !(as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Trough to Equilibrium")

### Filtering Equilibirum to Trough 
index_threshold = fedData_Q[,'Returns_USGDP'] < Threshold
index_composed = index_threshold  & (as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Equilibirum to Trough ")


##Filter NA rows
fedData_Q_clean <- fedData_Q[complete.cases(fedData_Q)]
working_df<-data.frame(fedData_Q_clean)


# Retransform the data
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
working_df[,'Cycle_var'] <- as.character(working_df[,'Cycle_var'])
working_df<-factorsNumeric(working_df)
working_df[,'Cycle_var'] <- as.factor(working_df[,'Cycle_var'])

## Print the current table of variables
table(working_df[,'Cycle_var'])/(length(working_df[,'Cycle_var']))


fed.bic.glm <- bic.glm (Cycle_var ~ VIX + TEDS + USIR ,
                        data=working_df, glm.family="binomial")

summary (fed.bic.glm,conditional=T,digits=2)
imageplot.bma(fed.bic.glm)


# #Data Review (double check)
#View(fedData_Q_clean)
#View(cycleNBER_clean)


