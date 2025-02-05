
library(tidyverse)
library(VIM)
# library(MASS)
library(caret)
library(MLmetrics)
library(knitr)
library(mice)
# install.packages('doParallel');
library(doParallel)
registerDoParallel(cores=5)

#install.packages("yardstick")
library(yardstick)

Train <- read.csv("C:\\Users\\Tushar\\Downloads\\hm7-Train-2023.csv\\hm7-Train-2023.csv", stringsAsFactors = F, na.strings = c("", "NA"))  

Test <- read.csv("C:\\Users\\Tushar\\Downloads\\hm7-Test-2023.csv\\hm7-Test-2023.csv", stringsAsFactors = F, na.strings = c("", "NA")) 


# Generating the Data Report
NumericTableSummaryFinal <- function(df) {
  # Function to generate summary of a numeric column
  numericSummaryFun <- function(x){
    Q1<-function(x,na.rm=TRUE) {
      quantile(x,na.rm=na.rm)[2]
    }
    Q3<-function(x,na.rm=TRUE) {
      quantile(x,na.rm=na.rm)[4]
    }
    
    c(length(x), n_distinct(x),((n_distinct(x)/length(x))*100), sum(is.na(x)),
      ((sum(is.na(x))/length(x))*100),mean(x, na.rm=TRUE),
      min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
      max(x,na.rm=TRUE), sd(x,na.rm=TRUE),var(x,na.rm=TRUE))
  }
  
  # Extracting the Numeric columns and generate the Numeric summary
  numericTable <- df %>% 
    select(where(is.numeric)) %>% 
    as_tibble()
  
  if(length(numericTable) > 0) {
    cols <- colnames(numericTable)
    
    
    NumericTableSummary <- numericTable %>%
      summarize(across(everything(), numericSummaryFun))
    
    NumericTableSummary <-cbind(
      stat=c("n","unique","Unique_percentage","missing","missing_Percentage", "mean","min","Q1","median","Q3","max","sd","var"),
      NumericTableSummary)
    
    NumericTableSummary<-NumericTableSummary %>%
             
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             select(variable, n, missing, missing_Percentage, unique, Unique_percentage, everything())
  
    
    
    options(digits=3)
    options(scipen=99)
    
    #Train Numeric Data report
    return(NumericTableSummary)
    }
  
  return(data.frame(Message="No Num values in data"))
}

DiscreteTableSummaryFinal <- function(df) {
  # Function to generate summary of a non-numeric columns
  DiscreteSummaryFun <- function(x){
    getmodes <- function(v,type=1) {
      tbl <- table(v)
      m1<-which.max(tbl)
      if(length(tbl) < type) { return(NA) }
      
      if (type==1) {
        return (names(m1)) #1st mode
      }
      else if (type==2) {
        return (names(which.max(tbl[-m1]))) #2nd mode
      }
      else if (type==-1) {
        return (names(which.min(tbl))) #least common mode
      }
      else {
        stop("Invalid type selected")
      }
    }
    
    getmodesCnt <- function(v,type=1) {
      tbl <- table(v)
      m1<-which.max(tbl)
      if(length(tbl) < type) { return(NA) }
      
      if (type==1) {
        return (max(tbl)) #1st mode freq
      }
      else if (type==2) {
        return (max(tbl[-m1])) #2nd mode freq
      }
      else if (type==-1) {
        return (min(tbl)) #least common freq
      }
      else {
        stop("Invalid type selected")
      }
    }
    
    freqRatio <- function(x){ getmodesCnt(x, type = 1)/getmodesCnt(x,type = 2) }
    
    c(length(x), n_distinct(x), sum(is.na(x)), getmodes(x,type = 1),
      getmodesCnt(x,type= 1), getmodes(x, type = 2), getmodesCnt(x,type= 2),round(freqRatio(x),digits = 3),
      getmodes(x,type= -1),getmodesCnt(x,type= -1))
  }
  
  # Extracting the Non-Numeric Columns
  DiscreteTable <- df[, sapply(Test, function(x) !is.numeric(x))]
 
  
  if(length(DiscreteTable) > 0) {
    cols <- colnames(DiscreteTable)
    
    result1 <- lapply(DiscreteTable, DiscreteSummaryFun)
    result_matrix <- do.call(cbind, result1)
    
    # Convert the matrix into a dataframe
    DiscreteTableSummary <- as.data.frame(result_matrix)
    
    DiscreteTableSummary <-cbind(
      stat=c("n","unique","missing","1st mode","1st mode freq","2nd mode",
             "2nd mode freq","freqRatio", "least common","least common freq"),
      DiscreteTableSummary)
    
    DiscreteTableSummary<- DiscreteTableSummary %>%
             pivot_longer(cols[1]:cols[length(cols)], names_to = "variable", values_to = "value") %>%
             pivot_wider(names_from = stat, values_from = value) %>%
             mutate(across(c(missing, unique, n, freqRatio, `1st mode freq`,
                                           `2nd mode freq`, `least common freq`),as.numeric)) %>%
             mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
             select(variable, n, missing, missing_pct, unique, unique_pct, everything())
    
    options(digits=3)
    options(scipen=99)
    return(DiscreteTableSummary)
    
    
    
    }
  
  return(data.frame(Message="No Discrete values in data"))
}


view(NumericTableSummaryFinal(Train))
view(DiscreteTableSummaryFinal(Train)) 

view(NumericTableSummaryFinal(Test))
view(DiscreteTableSummaryFinal(Test)) 
# Data Preprocessing

## Handling all Missing Values through imputation 


#Performing imputation for Train

Train.imp<-Train
columns_to_impute <- c("indicator_level","time_in_hospital","num_lab_procedures","indicator_2_level")

for (col_name in columns_to_impute) {
  missing <- is.na(Train[[col_name]])
  if (sum(missing) > 0) {
    Train.imp[missing, col_name] <- mice.impute.pmm(
      Train.imp[[col_name]], 
      !missing, 
      Train.imp$patientID
    )
  }
}
#Performing imputation for Train
Test.imp<-Test

for (col_name in columns_to_impute) {
  missing <- is.na(Test[[col_name]])
  if (sum(missing) > 0) {
    Test.imp[missing, col_name] <- mice.impute.pmm(
      Test.imp[[col_name]], 
      !missing, 
      Test.imp$patientID
    )
  }
}

#Function for getting modes
getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}



Train_preprocess.imp<- Train.imp
col_names<- names(Train_preprocess.imp)
for (col in col_names)
{
  
  missing <- is.na(Train_preprocess.imp[[col]])
  if(is.character(Train_preprocess.imp[[col]])){
    if (sum(missing) < 5000 & sum(missing) > 0) {
      Train_preprocess.imp[[col]][is.na(Train_preprocess.imp[[col]])] <- getmodes(Train_preprocess.imp[[col]])
    }
    else{
      Train_preprocess.imp[[col]][is.na(Train_preprocess.imp[[col]])] <- "Other"
      
    }
  }
}


### Generating reports of Imputed Training dataset to check the overall missing values
view(NumericTableSummaryFinal(Train_preprocess.imp)) 
view(DiscreteTableSummaryFinal(Train_preprocess.imp)) 

# Imputing missing values in Test data
Test_preprocess.imp<- Test.imp
col_names<- names(Test_preprocess.imp)
for (col in col_names)
{
  #print(Train_preprocess.imp[[col]])
  missing <- is.na(Test_preprocess.imp[[col]])
  if(is.character(Test_preprocess.imp[[col]])){
    if (sum(missing) < 3500 & sum(missing) > 0) {
      Test_preprocess.imp[[col]][is.na(Test_preprocess.imp[[col]])] <- getmodes(Train_preprocess.imp[[col]])
    }
    else{
      Test_preprocess.imp[[col]][is.na(Test_preprocess.imp[[col]])] <- "Other"
      
    }
  }
}
#Removing colums with zero Variance
columns_to_remove <- c("examide", "citoglipton", "glimepiride.pioglitazone")
Train_preprocess.imp <- Train_preprocess.imp %>%
  select(-one_of(columns_to_remove))

Test_preprocess.imp <- Test_preprocess.imp %>%
  select(-one_of(columns_to_remove))




# Factor Level Collapsing of Columns

# Column wise collapsing medication information
mspeciality_Other = c("AllergyandImmunology","Anesthesiology","Anesthesiology-Pediatric","Cardiology-Pediatric","DCPTEAM",
                      "Dentistry","Dermatology","Endocrinology","Endocrinology-Metabolism","Gynecology",
                      "Hematology","Hematology/Oncology","Hospitalist","InfectiousDiseases","Neurology","Neurophysiology",
                      "Obsterics&Gynecology-GynecologicOnco","Obstetrics","Oncology","Ophthalmology",
                      "OutreachServices","Pathology","Pediatrics","Pediatrics-AllergyandImmunology",
                      "Pediatrics-CriticalCare","Pediatrics-EmergencyMedicine","Pediatrics-Endocrinology","Pediatrics-Hematology-Oncology",
                      "PhysicianNotFound","Podiatry","Psychiatry-Addictive","Psychiatry-Child/Adolescent",
                      "Pediatrics-InfectiousDiseases","Pediatrics-Neurology","PhysicianNotFound",
                      "Perinatology","PhysicalMedicineandRehabilitation","Psychology","Radiology",
                      "Rheumatology","Speech","SportsMedicine","Surgeon","Surgery-Cardiovascular","Surgery-Colon&Rectal","Surgery-Maxillofacial",
                      "Surgery-Pediatric","Surgery-Plastic","Surgery-Thoracic","SurgicalSpecialty",
                      "Proctology", "Resident", "Surgery-PlasticwithinHeadandNeck")
Train_preprocess.imp$medical_specialty<- fct_collapse(Train_preprocess.imp$medical_specialty, Other = mspeciality_Other)

Test_preprocess.imp$medical_specialty<- fct_collapse(Test_preprocess.imp$medical_specialty, Other = mspeciality_Other)
#train
#performing Factor collapse for diagnosis column 
diagnosisFactorCollapse <- function(x) {
  if(is.na(x)) return(-1)
  
  
  x = as.numeric(x)
  if(1<=x && x<=100) return(1)
  if(101<=x && x<=200) return(2)
  if(201<=x && x<=300) return(3)
  if(301<=x && x<=400) return(4)
  if(401<=x && x<=500) return(5)
  if(501<=x && x<=600) return(6)
  if(601<=x && x<=700) return(7)
  if(701<=x && x<=800) return(8)
  if(801<=x && x<=900) return(9)
  if(901<=x && x<=999) return(10)
  
  
  x = as.character(x)
  if(startsWith(x, "V")) return(11)
  if(startsWith(x, "E")) return(12)
  
  return(-1)
}

#Calling the function to collapse the diagnosis function and converting it to factor
Train_preprocess.imp$diagnosis<-as.numeric(Train_preprocess.imp$diagnosis)
Train_preprocess.imp$diagnosis<-lapply(Train_preprocess.imp$diagnosis, diagnosisFactorCollapse)
Train_preprocess.imp$diagnosis<-as.character(Train_preprocess.imp$diagnosis)
Train_preprocess.imp$diagnosis<-as.factor(Train_preprocess.imp$diagnosis)

#test
Test_preprocess.imp$diagnosis<-as.numeric(Test_preprocess.imp$diagnosis)
Test_preprocess.imp$diagnosis<-lapply(Test_preprocess.imp$diagnosis, diagnosisFactorCollapse)
Test_preprocess.imp$diagnosis<-as.character(Test_preprocess.imp$diagnosis)
Test_preprocess.imp$diagnosis<-as.factor(Test_preprocess.imp$diagnosis)

# Using fct_lump_n Function to collapse Factor levels and change missing values to Other level 
collapse.factor <- function(x) {
  if(is.factor(x)){
    x <- fct_lump_n(x, n=5)
    x <- fct_explicit_na(x, na_level = "Other")
    return(x)}
}

#using the fct_lump_n function to collapse all the discrete columns in the datsets
Traincollapsed <- Train_preprocess.imp %>%
mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), collapse.factor))

Testcollapsed <- Test_preprocess.imp %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), collapse.factor))

Traincollapsed <- Train_preprocess.imp
Testcollapsed <- Test_preprocess.imp




Traincollapsed <- Traincollapsed %>%
  select(-nearZeroVar(Traincollapsed))

Traincollapsed <- Traincollapsed %>%
  mutate(readmitted=ifelse(readmitted==TRUE, "Yes", "No"))

Testcollapsed <- Testcollapsed %>%
  select(-nearZeroVar(Testcollapsed))




char_vars <- sapply(Traincollapsed, is.factor)

#using PCA and LDA to explore important features

lda_result <- lda(readmitted ~ ., data = Traincollapsed)

pcatable <- Traincollapsed %>% 
  select(where(is.numeric)) %>% 
  as_tibble()
pca<-prcomp(pcatable,scale. = TRUE)

library('ggbiplot')
ggbiplot(lda_result,groups =pcatable$num_procedures , circle = TRUE, varname.size = 3) + theme(legend.direction = 'horizontal', legend.position = 'right')


# Building Models to predict/classify the outcome 


#### Logistic Regression 
Ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Training logistic regression model with cross-validation
glm_model <- train(as.factor(readmitted) ~ . - patientID, 
                 data = Traincollapsed, 
                 method = "glm", 
                 family = "binomial",  
                 trControl = Ctrl)

# Print the results
print(glm_model)



glm_model

glm_Train_prediction <- predict(glm_model, newdata = Traincollapsed, type = "prob")
LogLoss(glm_Train_prediction$Yes, ifelse(Traincollapsed$readmitted=="Yes", 1, 0))

gml_test_predictions <- predict(glm_model, newdata = Testcollapsed, type = "prob")
predictions <- data.frame(patientID=Testcollapsed$patientID, 
                         predReadmit=gml_test_predictions$Yes)
View(predictions)
write.csv(predictions, "glm_predictions.csv", row.names = F)

#####Random Forest
#usinhg limited variables as it takes too long to process
rf_model <- train(as.factor(readmitted) ~patientID+A1Cresult+age+gender+race+admission_type+medical_specialty+payer_code+diagnosis+indicator_level,
                data = Traincollapsed, method = "rf", trControl=Ctrl,
                preProc= c("center","scale"))

rf_tune <- tuneRF(X_train, y_train, ntreeTry = c(50, 100, 150), mtryStart = 2, stepFactor = 1.5, improve = 0.05, trace = TRUE)

rf_model

rf_model_preds <- predict(rf_model, newdata = Traincollapsed, type = "prob")
LogLoss(rf_model_preds$Yes, ifelse(Traincollapsed$readmitted=="Yes", 1, 0))

rf_Predictions <- predict(rf_model, newdata = Testcollapsed, type = "prob")
predictions <- data.frame(patientID=Testcollapsed$patientID, 
                         predReadmit=rf_Predictions$Yes)
View(predictions)
write.csv(predictions, "rf.predictions.csv", row.names = F)

####MARS Model
trControl <- trainControl(method = "repeatedcv", number = 10, repeats= 3, metric="Accuracy")
mars_model <- train(as.factor(readmitted) ~.-patientID,
                  data = Traincollapsed, method = "earth", trControl=trControl,
                  preProc= c("center","scale"))
mars_model

MarsTrainPred <- predict(mars_model, newdata = TrainCollapsed, type = "prob")
LogLoss(MarsTrainPred$Yes, ifelse(TrainCollapsed$readmitted=="Yes", 1, 0))

MarsTestPred <- predict(mars_model, newdata = TestCollapsed, type = "prob")
predictions <- data.frame(patientID=TestCollapsed$patientID, 
                         predReadmit=MarsTestPred$Yes)
View(predictions)
write.csv(predictions, "mars.predictions.csv", row.names = F)

#Descision Tree
dt.fit <- train(as.factor(readmitted) ~.-patientID,
                  data = TrainCollapsed, method = "rpart", trControl=trControl)
dt.fit

dt.train.preds <- predict(dt.fit, newdata = TrainCollapsed, type = "prob")
LogLoss(dt.train.preds$Yes, ifelse(TrainCollapsed$readmitted=="Yes", 1, 0))


# Create a training control object with the desired metrics
ctrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)

##### Train the C5.0 model
C50model <- train(as.factor(readmitted) ~.-patientID, data = Traincollapsed, method = "C5.0", trControl = ctrl)

# Print the model summary
print(C50model)

params <- list(
  objective = "binary",
  metric = "binary_logloss",
  boosting_type = "gbdt",
  num_leaves = 31,
  learning_rate = 0.05,
  feature_fraction = 0.9
)
cv_results <- lgb.cv(
  params,
  lgb_data,
  nrounds = 100,
  folds = createFolds(data$target_variable, k = 5, list = TRUE, returnTrain = FALSE),
  early_stopping_rounds = 10,
  stratified = TRUE
)

lgb_data <- lgb.Dataset(data = as.matrix(Traincollapsed[, -c("readmitted")]), label = as.factor(Traincollapsed),
                      
                        final_model <- lgb.train(params, lgb_data, nrounds = cv_results$best_iter))

## Extreme gradient boosting (XGBoost) ===================================
Ctrl <- trainControl(method = "repeatedcv", number = 10, repeats= 3)

XGBGrid <- expand.grid(nrounds = c(400,450,500),
                       max_depth = c(3,4,5),
                       eta = c(0.05,0.1,0.5),
                       gamma = 0.01,
                       colsample_bytree = 0.6,
                       min_child_weight = 0,
                       subsample = 0.75)


XGBoost_Model <- train(as.factor(readmitted) ~.-patientID, 
                       data = Traincollapsed, method = "xgbTree", trControl=Ctrl,verbose = F, verbosity = 0,
                       tuneGrid=XGBGrid, preProc= c("center","scale"))

XGBoost_Model

xgb_Tr_preds<- predict(XGBoost_Model, newdata = Traincollapsed,type="prob")

#first way of finding Logloss thorugh Logloss function
LogLoss(xgb_Tr_preds, ifelse(Traincollapsed$readmitted=="Yes", 1, 0))


#Alternate way:Calculate LogLoss manually
predicted_probs <- xgb_Tr_preds$Yes

actual_labels <- as.numeric(Traincollapsed$readmitted == "Yes")

log_loss <- -mean(actual_labels * log(predicted_probs) + (1 - actual_labels) * log(1 - predicted_probs))
print(log_loss)

#Confusion Matrix
xgb_Tr_preds<- predict(XGBoost_Model, newdata = Traincollapsed,type="raw")
confusionMatrix(xgb_Tr_preds,as.factor(Traincollapsed$readmitted),mode = "everything")

# Sort the predictions in descending order
sorted_predictions <- sort(xgb_Tr_preds, decreasing = TRUE)

# Calculate the cumulative gains at each rank k
cumulative_gains <- sapply(1:length(sorted_predictions), function(k) sum(as.numeric(sorted_predictions[1:k])))

# Normalize the cumulative gains by the maximum possible cumulative gains
normalized_cumulative_gains <- cumulative_gains / max(as.numeric(sorted_predictions))

# Plot the cumulative gains
plot(1:length(sorted_predictions), normalized_cumulative_gains, type = "line", main = "Cumulative Gains")



xgb_predictions <- predict(XGBoost_Model, newdata = Testcollapsed, type = "prob")
predictions <- data.frame(patientID=Testcollapsed$patientID, 
                          predReadmit=xgb_predictions$Yes)

booster <- XGBoost_Model$get_booster()
#plotting The most important Features
xgboostFinal<-XGBoost_Model$finalModel
print(xgboostFinal)
Importance<- varImp(XGBoost_Model)

plot(Importance, main = "The most Important Variables for Predictions", top = 20)

View(predictions)
write.csv(predictions, "xgpredictions.csv", row.names = F)


                  