##### Implementation Begins ####
#This code replicates the FED's paper 

require("BMA")
require("PerformanceAnalytics")
require("ggplot2")


### Load the Data
fedData_raw <- read.csv("~/General Documents/FED Study/FED-master/Fed_Data_R.csv")

##### Preprocesing the data for making the analysis #####

#Coercing the datatypes from factor to numeric
indx <- sapply(fedData_raw[,2:(dim(fedData_raw)[2])], is.factor)
fedData_raw[indx] <- lapply(fedData_raw[,2:(dim(fedData_raw)[2])][indx], function(x) as.numeric(as.character(x)))
fedData_raw[,1] <- as.Date(as.character(fedData_raw[,1]) , format="%d/%m/%Y")


#Transformation of FED Variables
fedData_raw['Returns1_SP500']       <-0
fedData_raw['Returns3_SP500']       <-0
fedData_raw['Returns3_TWUSDIB']     <-0
fedData_raw['Returns3_USRPCE']      <-0
fedData_raw['Returns3_USRDPI']      <-0
fedData_raw['Returns3_USNHPUM']     <-0
fedData_raw['Returns3_USTNP']       <-0
fedData_raw['Returns3_USICUI']      <-0 #4 week MA Watch out!!!
fedData_raw['Returns3_USMAWH']      <-0
fedData_raw['Returns3_USPMI']       <-0
fedData_raw['Returns6_USGDP']       <-0
fedData_raw['Cycle_var']            <-0

## There are 11 FED Variables

#Rearange the columns
col_idx <- grep("NBER", names(fedData_raw))
fedData_raw <- fedData_raw[, c((1:ncol(fedData_raw))[-col_idx],col_idx )]

#names(fedData_raw)

#Double Check Data Types
#str(fedData_raw)

### Transforming the Dataframe to xts
#fedData <-xts(fedData_raw[,-c(1,dim(fedData_raw)[2])],order.by=fedData_raw[,1])
#cycleNBER <- xts(fedData_raw[,dim(fedData_raw)[2]],order.by=fedData_raw[,1])

fedData_Q <- fedData_raw[,-c(1)]
cycleNBER_Q <- fedData_raw[,dim(fedData_raw)[2]]

## Calculating the Quarterly returns for the variables of interest
fedData_Q['Returns1_SP500']       <-c(rep(NA,1),diff(log(fedData_Q[,'SP500NM']),1))
fedData_Q['Returns3_SP500']       <-c(rep(NA,3),diff(log(fedData_Q[,'SP500NM']),3))
fedData_Q['Returns3_TWUSDIB']     <-c(rep(NA,3),diff(log(fedData_Q[,'TWUSDIB']),3))
fedData_Q['Returns3_USRPCE']      <-c(rep(NA,3),diff(log(fedData_Q[,'USRPCE']) ,3))
fedData_Q['Returns3_USRDPI']      <-c(rep(NA,3),diff(log(fedData_Q[,'USRDPI']) ,3))
fedData_Q['Returns3_USNHPUM']     <-c(rep(NA,3),diff(log(fedData_Q[,'USNHPUM']),3))
fedData_Q['Returns3_USTNP']       <-c(rep(NA,3),diff(log(fedData_Q[,'USTNP'])  ,3))
fedData_Q['Returns3_USICUI']      <-c(rep(NA,3),diff(log(fedData_Q[,'USICUI']) ,3))
fedData_Q['Returns3_USMAWH']      <-c(rep(NA,3),diff(log(fedData_Q[,'USMAWH']) ,3))
fedData_Q['Returns3_USPMI']       <-c(rep(NA,3),diff(log(fedData_Q[,'USPMI'])  ,3))
fedData_Q['Returns6_USGDP']       <-c(rep(NA,6),diff(log(fedData_Q[,'USGDP'])  ,6))

## Creation of new Bussiness Cycle Variable
Threshold = 0.002
fedData_Q[1,'Cycle_var'] <- c("Peak to Equilibrium")

### Filtering the Equilibrium to Peak
index_threshold <- fedData_Q[,'Returns6_USGDP']> Threshold
index_threshold[is.na(index_threshold)] <- FALSE   
index_composed = index_threshold  & !(as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Equilibrium to Peak")

### Filtering Peak to Equilibirum
index_threshold = fedData_Q[,'Returns6_USGDP']> Threshold
index_threshold[is.na(index_threshold)] <- FALSE
index_composed = index_threshold  & (as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Peak to Equilibrium")

### Filtering Trough to Equilibrium
index_threshold = fedData_Q[,'Returns6_USGDP'] < Threshold
index_threshold[is.na(index_threshold)] <- FALSE
index_composed = index_threshold  & !(as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Trough to Equilibrium")

### Filtering Equilibirum to Trough 
index_threshold = fedData_Q[,'Returns6_USGDP'] < Threshold
index_threshold[is.na(index_threshold)] <- FALSE
index_composed = index_threshold  & (as.logical(as.numeric(cycleNBER_Q)))
fedData_Q[index_composed,'Cycle_var'] <- c("Equilibirum to Trough ")

##### Plot of the Data without the NA Filter #####

#Extract the first dates and the last dates
aux=1
initial_dates=c(aux,which(diff(fedData_Q[,"NBER"])==1)-1)
last_dates=which(diff(fedData_Q[,"NBER"])==-1)
up=rep(Inf,length(initial_dates))
down=rep(-Inf,length(last_dates))
shade=data.frame(initial_dates,last_dates,up,down)

aux2=dim(fedData_Q)[1]
# Ploting the data
ggplot() + 
  geom_line(aes(x=c(1:aux2), y=SP500NM), color='red',data=fedData_Q)+
  geom_rect(data=shade, 
            mapping=aes(xmin=initial_dates, xmax=last_dates, ymin=up, ymax=down), color='grey', alpha=0.2)


##Filter NA rows
fedData_Q_clean <- fedData_Q[complete.cases(fedData_Q),]
working_df<-data.frame(fedData_Q_clean)


##### Plot of the Data with the NA Filter #####

#Extract the first dates and the last dates

initial_dates=which(diff(fedData_Q_clean[,"NBER"])==1)-1
last_dates=which(diff(fedData_Q_clean[,"NBER"])==-1)
up=rep(Inf,length(initial_dates))
down=rep(-Inf,length(last_dates))
shade=data.frame(initial_dates,last_dates,up,down)

aux2=dim(fedData_Q_clean)[1]
# Ploting the data
ggplot() + 
  geom_line(aes(x=c(1:aux2), y=SP500NM), color='red',data=fedData_Q_clean)+
  geom_rect(data=shade, 
            mapping=aes(xmin=initial_dates, xmax=last_dates, ymin=up, ymax=down), color='grey', alpha=0.2)




# Retransform the data
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))
working_df[,'Cycle_var'] <- as.character(working_df[,'Cycle_var'])
working_df<-factorsNumeric(working_df)
working_df[,'Cycle_var'] <- as.factor(working_df[,'Cycle_var'])

#Transformation 2
#working_df[,'NBER'] <- as.factor(working_df[,'NBER'])



#Iteration over the variables.
var_list = c("Returns3_TWUSDIB",
             "VIX",
             "CURVYCURV",
             "TEDS",
             "Returns3_USRPCE",
             "Returns3_USRDPI",
             "Returns1_SP500",
             "Returns3_SP500",
             "Returns3_USNHPUM", 
             "Returns3_USTNP", 
             "Returns3_USICUI",
             "Returns3_USMAWH",
             "USCIPI", 
             "CURVYCURV",
             "S10Y3MTS",
             "GZSPREAD",
             "USCBBBBR")

working_df_iter<- working_df[,var_list]
working_df_y<- working_df[,"NBER"]
## Variable Selection Iterator
#fed.iter.bic.glm <- iBMA.glm(working_df_iter, working_df_y, thresProbne0 = 5, verbose = TRUE, maxNvar = 30,glm.family=binomial() )


## Variable Selection Iterator
#fed.bic.glm.matrix <- bic.glm(working_df_iter, working_df_y, glm.family=binomial(link="probit") )
#summary (fed.bic.glm.matrix,conditional=T,digits=2)
#imageplot.bma(fed.bic.glm.matrix)
x <-working_df_iter[,c("USCIPI","GZSPREAD","CURVYCURV")]
y <-working_df[,"NBER"]
  
glib.fed<- glib(x,y, error="binomial", link = "logit")
summary(glib.fed)

## Print the current table of variables
fed.bic.glm <- bic.glm (NBER ~  Returns3_TWUSDIB + VIX + CURVYCURV +
                          TEDS + Returns3_USRPCE + Returns3_USRDPI +
                          Returns1_SP500 + Returns3_SP500 +
                          Returns3_USNHPUM + Returns3_USTNP + Returns3_USICUI +
                          Returns3_USMAWH + USCIPI + CURVYCURV + S10Y3MTS + GZSPREAD + USCBBBBR,
                        data=working_df, glm.family=binomial(link="probit"))#, OR=2000000, OR.fix=200000,nBest=450)


summary (fed.bic.glm,conditional=T,digits=2)
imageplot.bma(fed.bic.glm,order="probne0")

table(working_df[,"NBER"],round(predict(fed.bic.glm, newdata = working_df)))

# #Data Review (double check)
#View(fedData_Q_clean)
#View(cycleNBER_clean)
