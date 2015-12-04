############################################################################
#SOLUTION TO HOMEWORK -III
#Author : Tushar Subramaniam
# Date : November 20,2015
#Topic : Feature Selection & PCA
#############################################################################

############################
#Install required libraries.
#############################
library(zoo)
library(plyr)
library(MASS)
library(leaps)
library(DAAG)
library(ggplot2)
require(xlsx)

##############################
# Import the required files.
##############################
bank=read.csv('bank-additional-full.csv',header=TRUE,sep=";",strip.white = TRUE)



#a) Corelation based feature selection.
##########################################

filter.features.by.cor<- function(dataframe)
{
  #All variables except the last column go into the input variable
  input_matrix<-dataframe[,1:ncol(dataframe)-1]
  attribute_names<-data.frame(names(input_matrix))
  
  #The last attribute is the output dimension.
  output_attribute<-dataframe[,ncol(dataframe)]
  
  #Calculate correlation of every attribute in the input matrix with the output attribute.
  output_df<-NULL
  for(i in 1:ncol(input_matrix))
  {
    #NOTE : I'm considering ABSOLUTE VALUES OF correlation here.
    temp_cor<-abs(cor(output_attribute,input_matrix[,i]))
    output_df<-rbind(output_df,temp_cor)
  }
  
  #Append attribute names with correlations
  output_df<-cbind(Attribute=attribute_names,Absolute_Correlation=output_df)
  #Sort dataset by correlations
  output_df<-na.omit(output_df[order(output_df),])
  return(output_df)
}


############################################################################################
#We need to convert the categorical variables into numeric before they can be used further
###########################################################################################
days_map <- read.xlsx("mapping.xlsx", sheetName="days")
months_map <- read.xlsx("mapping.xlsx", sheetName="months")
jobs_map <- read.xlsx("mapping.xlsx", sheetName="jobs")
marital_map <- read.xlsx("mapping.xlsx", sheetName="marital")
education_map <- read.xlsx("mapping.xlsx", sheetName="education")
default_map <- read.xlsx("mapping.xlsx", sheetName="default")
housing_map <- read.xlsx("mapping.xlsx", sheetName="housing")
loan_map <- read.xlsx("mapping.xlsx", sheetName="loan")
contact_map <- read.xlsx("mapping.xlsx", sheetName="contact")
poutcome_map <- read.xlsx("mapping.xlsx", sheetName="poutcome")
y_map <- read.xlsx("mapping.xlsx", sheetName="y")


###########################################################
#Get all the required variables into the dataset
###########################################################

bank_modified<-merge(bank,jobs_map,by="job")
bank_modified<-merge(bank_modified,marital_map,by="marital")
bank_modified<-merge(bank_modified,education_map,by="education")
bank_modified<-merge(bank_modified,default_map,by="default")
bank_modified<-merge(bank_modified,housing_map,by="housing")
bank_modified<-merge(bank_modified,loan_map,by="loan")
bank_modified<-merge(bank_modified,contact_map,by="contact")
bank_modified<-merge(bank_modified,months_map,by="month")
bank_modified<-merge(bank_modified,days_map,by="day_of_week")
bank_modified<-merge(bank_modified,poutcome_map,by="poutcome")
bank_modified<-merge(bank_modified,y_map,by="y")


#####################################
#Retain only numeric columns
####################################
nums<-sapply(bank_modified,is.numeric)
bank_final<-bank_modified[,nums]

#############################################
# Missing value treatment
#############################################

### If no job is given, consider the most frequently occurring job.
bank_final$job_number<-with(bank_final,ifelse(job_number==10,3,job_number))

### If marital-status is not given. All people above 30 years are considered married, else single.
### This assumption taken based on average age of marriage in Portugal.
bank_final$marital_number<-with(bank_final,ifelse(marital_number==4,ifelse(age>=30,2,1),marital_number))

### Default is 'No'
bank_final$default_number<-with(bank_final,ifelse(default_number==3,1,0))

### Housing : If it is unknown, we assume that the person does not have a loan
bank_final$housing_number<-with(bank_final,ifelse(housing_number==3,1,housing_number))

### If month and day information is absent , we assume that it is '0' i.e. never called
bank_final$month_number<-with(bank_final,ifelse(is.na(month_number),0,month_number))
bank_final$day_number<-with(bank_final,ifelse(is.na(day_number),0,day_number))

####################
#Correlation matrix
#####################
corr_matrix<-data.frame(filter.features.by.cor(bank_final))



#(b) FEature selection using leaps package
############################################

feature.select.leaps<-function(dataframe,features)
{
  #All variables except the last column go into the input variable
  input_matrix<-dataframe[,1:ncol(dataframe)-1]
  attribute_names<-data.frame(names(input_matrix))
  
  #The last attribute is the output dimension.
  output_attribute<-dataframe[,ncol(dataframe)]
  
  #USe regsubset function in leaps to get desired output
  output_tmp<-regsubsets(output_attribute~.,input_matrix,nvmax=features,nbest=1)
  
  return(output_tmp)
}

top5<-feature.select.leaps(bank_final,5)
summary(top5)

#########################################################################################
#Top-5 attributes come out to be : duration,nr.employed,pdays,cons.conf,contact_number
#########################################################################################


# Part(c) Backward Step-wise regression
########################################

#All variables except the last column go into the input variable
input_matrix<-bank_final[,1:ncol(bank_final)-1]

#The last attribute is the output dimension.
output_attribute<-bank_final[,ncol(bank_final)]


base_reg_model<-lm(output_attribute~.,input_matrix)
backward_reg<-stepAIC(base_reg_model,direction="backward",k=2)
summary(backward_reg)

#################################################
#The backward AIC has retained 17 variables.
##################################################



####################################################
#         E N D    O F   C o D E
###################################################










