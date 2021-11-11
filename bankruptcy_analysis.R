install.packages("ggcorrplot")
library("caret")
library("e1071")
library("ggcorrplot")
library("leaps")
library("reshape2")
library("ROSE")
library("summarytools")
library("tidyverse")

#Read data from csv
bankruptcy <- read.csv(file = 'data.csv')

#Check for missing values
cbind(
  lapply(
    lapply(bankruptcy, is.na)
    , sum
  )
)

#Check column types
sapply(bankruptcy, typeof)

#Histogram of Bankruptcy columns
ggplot(bankruptcy, aes(x=as.factor(Bankrupt.))) +
  geom_histogram(stat="count", color="red", fill="red") +
  xlab("Bankrupt.") +
  ylab("Count") +
  ggtitle("Bankrupt Companies")

#Visualization of Liability.Assets.Flag
ggplot(data = bankruptcy, aes(x=as.factor(Liability.Assets.Flag), group=as.factor(Bankrupt.), fill=as.factor(Bankrupt.))) +
  geom_bar(position="dodge") +
  ggtitle("Distribution of Liability.Assets.Flag by Bankrupt.") +
  scale_fill_discrete(name="Bankrupt.",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Liability Assets Flag") +
  ylab("Count")

#Visualization of Net.Income.Flag
ggplot(data = bankruptcy, aes(x=as.factor(Net.Income.Flag), group=as.factor(Bankrupt.), fill=as.factor(Bankrupt.))) +
  geom_bar(position="dodge") +
  ggtitle("Distribution of Net.Income.Flag by Bankrupt.") +
  scale_fill_discrete(name="Bankrupt.",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Liability Assets Flag") +
  ylab("Count")


#Heat Map
bankruptcy_numeric <- bankruptcy %>%
  select(-c("Liability.Assets.Flag", "Net.Income.Flag"))
correlations <- cor(bankruptcy_numeric)
correlations_df <- as.data.frame(as.table(correlations)) %>%
  filter(Var2 == "Bankrupt.") %>%
  arrange(desc(abs(Freq))) %>%
  head(11)

#Rename Columns for Ease of Reading Table
attribute_names <- data.frame(Old = c("Bankrupt.","Net.Income.to.Total.Assets","ROA.A..before.interest.and...after.tax",
                                      "ROA.B..before.interest.and.depreciation.after.tax","ROA.C..before.interest.and.depreciation.before.interest",
                                      "Debt.ratio..","Net.worth.Assets","Persistent.EPS.in.the.Last.Four.Seasons","Retained.Earnings.to.Total.Assets",
                                      "Net.profit.before.tax.Paid.in.capital","Per.Share.Net.profit.before.tax..Yuan.Â.."), New = c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10"))

bankruptcy_numeric_filtered <- bankruptcy %>%
  select(c("Bankrupt.","Net.Income.to.Total.Assets","ROA.A..before.interest.and...after.tax",
           "ROA.B..before.interest.and.depreciation.after.tax","ROA.C..before.interest.and.depreciation.before.interest"),
         "Debt.ratio..","Net.worth.Assets","Persistent.EPS.in.the.Last.Four.Seasons","Retained.Earnings.to.Total.Assets",
         "Net.profit.before.tax.Paid.in.capital","Per.Share.Net.profit.before.tax..Yuan.Â..")

colnames(bankruptcy_numeric_filtered) <- c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")

correlations <- cor(bankruptcy_numeric_filtered)

ggcorrplot(correlations, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of bankruptcy", 
           ggtheme=theme_bw)

#Plots for 10 assets
ggplot(bankruptcy, aes(Net.Income.to.Total.Assets)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Net.Income.to.Total.Assets by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Net.Income.to.Total.Assets") +
  ylab("Count")

ggplot(bankruptcy, aes(ROA.A..before.interest.and...after.tax)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of ROA.A..before.interest.and...after.tax by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("ROA.A..before.interest.and...after.tax") +
  ylab("Count")

ggplot(bankruptcy, aes(ROA.B..before.interest.and.depreciation.after.tax)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of ROA.B..before.interest.and.depreciation.after.tax by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("ROA.B..before.interest.and.depreciation.after.tax") +
  ylab("Count")

ggplot(bankruptcy, aes(ROA.C..before.interest.and.depreciation.before.interest)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of ROA.C..before.interest.and.depreciation.before.interest by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("ROA.C..before.interest.and.depreciation.before.interest") +
  ylab("Count")

ggplot(bankruptcy, aes(Debt.ratio..)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Debt.ratio.. by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Debt.ratio..") +
  ylab("Count")

ggplot(bankruptcy, aes(Net.worth.Assets)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Net.worth.Assets by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Net.worth.Assets") +
  ylab("Count")

ggplot(bankruptcy, aes(Persistent.EPS.in.the.Last.Four.Seasons)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Persistent.EPS.in.the.Last.Four.Seasons by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Persistent.EPS.in.the.Last.Four.Seasons") +
  ylab("Count")

ggplot(bankruptcy, aes(Retained.Earnings.to.Total.Assets)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Retained.Earnings.to.Total.Assets by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Retained.Earnings.to.Total.Assets") +
  ylab("Count")

ggplot(bankruptcy, aes(Net.profit.before.tax.Paid.in.capital)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Net.profit.before.tax.Paid.in.capital by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Net.profit.before.tax.Paid.in.capital") +
  ylab("Count")

ggplot(bankruptcy, aes(Per.Share.Net.profit.before.tax..Yuan.Â..)) +
  geom_histogram(aes(fill=as.factor(Bankrupt.))) +
  ggtitle("Distribution of Per.Share.Net.profit.before.tax..Yuan.Â.. by Bankrupt.") +
  scale_fill_discrete(name="Bankruptcy",
                      labels=c("No Bankruptcy", "Bankruptcy")) +
  xlab("Per.Share.Net.profit.before.tax..Yuan.Â..") +
  ylab("Count")

#Normalize data and recreate table
bankruptcy_numeric <- bankruptcy %>%
  select(-c("Bankrupt.","Liability.Assets.Flag", "Net.Income.Flag"))
bankruptcy_bool <- bankruptcy %>%
  select(c("Bankrupt.","Liability.Assets.Flag"))
norm <- function(x, na.rm=FALSE) (x - mean(x, na.rm = na.rm))/sd(x, na.rm)
bankruptcy_numeric <- bankruptcy_numeric %>%
  mutate_all(norm, na.rm=TRUE)
bankruptcy <- bankruptcy_bool %>%
  cbind(bankruptcy_numeric)

#Create Traning and Test Data
set.seed(999)
trainIndex <- createDataPartition(bankruptcy$Bankrupt., p=0.70, list=FALSE, times=1)

train_data <- bankruptcy[trainIndex,]
test_data <- bankruptcy[-trainIndex,]

freq(train_data$Bankrupt., report.nas = FALSE, totals = FALSE, 
     cumul = FALSE, headings = FALSE)

#Balance data using ROSE
train_data <- ROSE(Bankrupt. ~ ., data=train_data, seed=999)$data

freq(train_data$Bankrupt., report.nas = FALSE, totals = FALSE, 
     cumul = FALSE, headings = FALSE)
test_data <- ROSE(Bankrupt. ~ ., data=test_data, seed=999)$data

freq(test_data$Bankrupt., report.nas = FALSE, totals = FALSE, 
     cumul = FALSE, headings = FALSE)

#Logistic Regression - All Predictors
lmod_bankruptcy <- glm(Bankrupt. ~ ., family=binomial, train_data)
summary(lmod_bankruptcy)

#Prediction - Train
lmod_bankruptcy_prob <- predict(lmod_bankruptcy, train_data, type="response")
bankruptcy_train_pred_lmod <- train_data %>%
  mutate(predict=1*(lmod_bankruptcy_prob > 0.5)) %>%
  mutate(accurate=1*(predict==Bankrupt.))
lmod_acc <- sum(bankruptcy_train_pred_lmod$accurate)/nrow(bankruptcy_train_pred_lmod)

#Prediction - Test
lmod_bankruptcy_prob <- predict(lmod_bankruptcy, test_data, type="response")
bankruptcy_test_pred_lmod <- test_data %>%
  mutate(predict=1*(lmod_bankruptcy_prob > 0.5)) %>%
  mutate(accurate=1*(predict==Bankrupt.))
lmod_acc <- sum(bankruptcy_test_pred_lmod$accurate)/nrow(bankruptcy_test_pred_lmod)

#Analysis
confusion_matrix_lmod <- as.data.frame(table(bankruptcy_test_pred_lmod$Bankrupt.,bankruptcy_test_pred_lmod$predict))
confusion_matrix_lmod$Var1 <- as.character(confusion_matrix_lmod$Var1)
confusion_matrix_lmod$Var2 <- as.character(confusion_matrix_lmod$Var2)
confusion_matrix_lmod$Var1[confusion_matrix_lmod$Var1 == 0] <- "Not Bankrupt"
confusion_matrix_lmod$Var1[confusion_matrix_lmod$Var1 == 1] <- "Bankrupt"
confusion_matrix_lmod$Var2[confusion_matrix_lmod$Var2 == 0] <- "Not Bankrupt"
confusion_matrix_lmod$Var2[confusion_matrix_lmod$Var2 == 1] <- "Bankrupt"

ggplot(data=confusion_matrix_lmod, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - Logistic Regression")

#Support Vector Machine
bankruptcy_svm <- svm(Bankrupt. ~ Net.Income.to.Total.Assets + ROA.A..before.interest.and...after.tax + 
ROA.B..before.interest.and.depreciation.after.tax + ROA.C..before.interest.and.depreciation.before.interest + 
Debt.ratio.. + Net.worth.Assets + Persistent.EPS.in.the.Last.Four.Seasons + Retained.Earnings.to.Total.Assets + 
Net.profit.before.tax.Paid.in.capital + Per.Share.Net.profit.before.tax..Yuan.Â.., cost=1000, gamma=1, type="C-class",kernel="linear", data=train_data)

bankruptcy_train_pred_svm <- predict(bankruptcy_svm, train_data)
bankruptcy_train_pred_svm_df <- train_data %>%
  mutate(accurate=1*(bankruptcy_train_pred_svm==Bankrupt.))
svm_acc <- sum(bankruptcy_train_pred_svm_df$accurate)/nrow(bankruptcy_train_pred_svm_df)

bankruptcy_test_pred_svm <- predict(bankruptcy_svm, test_data)
bankruptcy_test_pred_svm_df <- test_data %>%
  mutate(predict=bankruptcy_test_pred_svm) %>%
  mutate(accurate=1*(bankruptcy_test_pred_svm==Bankrupt.))
svm_acc <- sum(bankruptcy_test_pred_svm_df$accurate)/nrow(bankruptcy_test_pred_svm_df)

confusion_matrix_svm <- as.data.frame(table(bankruptcy_test_pred_svm_df$Bankrupt.,bankruptcy_test_pred_svm_df$predict))
confusion_matrix_svm$Var1 <- as.character(confusion_matrix_svm$Var1)
confusion_matrix_svm$Var2 <- as.character(confusion_matrix_svm$Var2)
confusion_matrix_svm$Var1[confusion_matrix_svm$Var1 == 0] <- "Not Bankrupt"
confusion_matrix_svm$Var1[confusion_matrix_svm$Var1 == 1] <- "Bankrupt"
confusion_matrix_svm$Var2[confusion_matrix_svm$Var2 == 0] <- "Not Bankrupt"
confusion_matrix_svm$Var2[confusion_matrix_svm$Var2 == 1] <- "Bankrupt"

ggplot(data=confusion_matrix_svm, mapping=aes(x=Var1,y=Var2)) +
  geom_tile(aes(fill=Freq), color = "white") +
  geom_text(aes(label=sprintf("%1.0f", Freq)), vjust=1) +
  scale_fill_gradient(low="steelblue", high="red") +
  theme_bw() + theme(legend.position="none") +
  xlab("Predicted") + ylab("Actual") + ggtitle("Predicted versus Actual - SVM")
