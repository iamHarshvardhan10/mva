# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function(names) {
  if (names == "pca"){
    cat('
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")

library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)


df = read.csv("apple_quality.csv")
str(df)

#check for null values

colSums(is.na(df))

# Removing na values

df = na.omit(df)

# check the na values are there are not
colSums(is.na(df))


# numerical to characterize

df$Acidity = as.numeric(df$Acidity)
str(df)

# removing the a_id
df = df[,2:8]

# scaling
data_normalized_1 = scale(df)
head(data_normalized_1)

# Compute the Correlation Matrix

corr_matrix = cor(data_normalized_1)
corr_matrix

# ploting corr Plot

ggcorrplot(corr_matrix)


# Applying PCA

data.pca = princomp(corr_matrix)
summary(data.pca)


# Loading Matrix

data.pca$loadings[,1:4]

# Visualization of principal component

fviz_eig(data.pca , addlabels = TRUE)

# Biplot of Attributes

fviz_pca_var(data.pca , col.var = "black")


# cos2
fviz_cos2(data.pca, choice = "var" , axes = 1.3)

')
  } else if (names == "sem"){
    cat("
library(lavaan)
?lavaan
library(semPlot)

# The Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed   =~ x7 + x8 + x9 '

fit <- lavaan(HS.model, data=HolzingerSwineford1939,
              auto.var=TRUE, auto.fix.first=TRUE,
              auto.cov.lv.x=TRUE)
summary(fit, fit.measures=TRUE)


?semPaths
semPaths(fit,'std', edge.label.cex = 0.5, exoVar = FALSE,
         exoCov = FALSE)

data('PoliticalDemocracy')
head(PoliticalDemocracy)

model <- '
        ind60 =~ x1+x2+x3
        dem60 =~ y1+y2+y3+y4
        dem65 =~ y5+y6+y7+y8

        dem60 ~ ind60
        dem65 ~ ind60 + dem60

        y1 ~~ y5
        y2 ~~ y4+y6
        y3 ~~ y7
        y4 ~~ y8
        y6 ~~ y8
        '

fit <- sem(model , data = PoliticalDemocracy)
summary(fit , standardized = TRUE)
semPaths(fit, what = 'std' , layout = 'tree' , edge.label.cex = 0.9 , curvePivot  = TRUE)
semPaths(fit , whatLabels = 'est'' , style = 'lisrel' , main='SEM Diagram')

        ")
  }else if(names == 'ldaqda'){
    cat('
library(klaR)
library(psych)
library(MASS)

# LDA


# Read the dataset
df <- read.csv("diabetes.csv")

# Train LDA model
model_lda <- lda(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = df)
model_lda
plot(model_lda)
# Summary of the LDA model
summary(model_lda)

# Make predictions on the training set using LDA
predictions_lda <- predict(model_lda)
predictions_lda
# Access class means and priors for LDA
class_means_lda <- model_lda$means
priors_lda <- model_lda$prior

# Access coefficients for linear discriminants
coefficients_lda <- model_lda$scaling
coefficients_lda

# Access the confusion matrix for LDA
conf_matrix_lda <- table(predictions_lda$class, df$Outcome)
print("Confusion Matrix for LDA:")
print(conf_matrix_lda)



# Train QDA model
model_qda <- qda(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = df)
model_qda


# Make predictions on the training set using QDA
predictions_qda <- predict(model_qda)
predictions_qda



# Access the confusion matrix for QDA
conf_matrix_qda <- table(predictions_qda$class, df$Outcome)
print("Confusion Matrix for QDA:")
print(conf_matrix_qda)


# roc curve


# Load necessary libraries
library(MASS)
library(pROC)

# Read the dataset
df <- read.csv("diabetes.csv")

# Specify the formula for the discriminant analysis
formula <- Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age
formula
# Train LDA model
model_lda <- lda(formula, data = df)
model_lda

# Train QDA model
model_qda <- qda(formula, data = df)
model_qda
# Make predictions on the training set
predictions_lda <- as.numeric(predict(model_lda)$class == "1")
predictions_qda <- as.numeric(predict(model_qda)$class == "1")

# Create ROC curves
roc_lda <- roc(df$Outcome, predictions_lda)
roc_qda <- roc(df$Outcome, predictions_qda)

# Plot ROC curves
plot(roc_lda, col = "blue", main = "ROC Curves", lwd = 2, col.main = "black", ylim = c(0, 1), xlim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_qda, col = "red", lwd = 2)

# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)

###################################################################################
library(klaR)
library(psych)
library(MASS)

# LDA


# Read the dataset
df <- read.csv("breastCancer.csv")
df

df <- df[, -c(1, ncol(df))]
df
df$diagnosis <- factor(df$diagnosis)

lda_model <- lda(diagnosis ~ ., data = df)
lda_model


qda_model <- qda(diagnosis ~ ., data = df)
qda_model

# Predictions using LDA
lda_pred <- predict(lda_model)
lda_pred
lda_conf_matrix <- table(Actual = df$diagnosis, Predicted = lda_pred$class)
lda_conf_matrix
# Print the confusion matrix for LDA
print("Confusion Matrix for LDA:")
print(lda_conf_matrix)

# Predictions using QDA
qda_pred <- predict(qda_model)
qda_pred
qda_conf_matrix <- table(Actual = df$diagnosis, Predicted = qda_pred$class)
qda_conf_matrix
# Print the confusion matrix for QDA
print("Confusion Matrix for QDA:")
print(qda_conf_matrix)


# Coefficients of LDA
lda_coefficients <- coef(lda_model)
print("Coefficients of LDA:")
print(lda_coefficients)

# Coefficients of QDA
qda_coefficients <- coef(qda_model)
print("Coefficients of QDA:")
print(qda_coefficients)


str(df$diagnosis)
str(lda_model$posterior)

# Inspect the lda_model object
str(lda_model)

# Check for missing values in the data
any(is.na(df))

class(lda_pred)
str(lda_pred)


# Load the pROC package
library(pROC)

# Create ROC curve for LDA
roc_lda <- roc(df$diagnosis, lda_pred$posterior[, "M"], levels = c("B", "M"))

# Create ROC curve for QDA
roc_qda <- roc(df$diagnosis, qda_pred$posterior[, "M"], levels = c("B", "M"))

# Plot ROC curve for LDA
plot(roc_lda, col = "blue", main = "ROC Curve for LDA")

# Add ROC curve for QDA
plot(roc_qda, col = "red", add = TRUE)

# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lty = 1)

        ')
  } else if(names == 'cla'){
    cat('
install.packages("MVN")
library(MVN)
install.packages("psych")
library(car)

df = data("USArrests")
df



outlier=mvn(USArrests,multivariateOutlierMethod = "quan", showNewData = TRUE)


USArrests <- USArrests[-2,]
USArrests


summary(USArrests)

data("USArrests", package = "datasets")
USArrests$Assault
USArrests$Murder
USArrests$Rape
USArrests$UrbanPop

mult1=vif(lm(USArrests$Murder~USArrests$Assault+USArrests$UrbanPop+USArrests$Rape))
mult1
mult2=vif(lm(USArrests$Assault~USArrests$Murder+USArrests$UrbanPop+USArrests$Rape))
mult2
mult3=vif(lm(USArrests$UrbanPop~USArrests$Murder+USArrests$Assault+USArrests$Rape))
mult3
mult4=vif(lm(USArrests$Rape~USArrests$Murder+USArrests$Assault+USArrests$UrbanPop))
mult4
cbind.data.frame(mult1, mult2, mult3, mult4)



library(factoextra)
fviz_nbclust(USArrests, FUN=hcut, method = "silhouette")

dist_matrix <- dist(USArrests)

#Average Linkage Method
metode_al<-hclust(dist_matrix,"ave")
hc_ave = cophenetic(metode_al)
cor.ave = cor(as.dist(dist_matrix),hc_ave)

#Single Linkage Method
metode_sl<-hclust(dist_matrix,"single")
hc_single = cophenetic(metode_sl)
cor.single = cor(as.dist(dist_matrix),hc_single)

#Complete Linkage Method
metode_cl<-hclust(dist_matrix,"complete")
hc_comp = cophenetic(metode_cl)
cor.comp = cor(as.dist(dist_matrix),hc_single)

# Perform hierarchical clustering
metode_w <- hclust(dist_matrix, "ward.D")
metode_w
hc_w <- cophenetic(metode_w)
hc_w
cor.w <- cor(as.dist(dist_matrix), hc_w)
cor.w

#Centroid Method
metode_cd<-hclust(dist_matrix,"centroid")
hc_cd = cophenetic(metode_cd)
cor.cd = cor(as.dist(dist_matrix),hc_cd)

#Displays the cophenetic correlation value to 1 line
cbind.data.frame(cor.ave, cor.comp, cor.single, cor.w, cor.cd)



plot(metode_al)
rect.hclust(metode_al,2)


anggota<-cutree(metode_al,2)
tabel=data.frame(USArrests,anggota)
View(tabel)

cluster1 <- subset(USArrests, anggota==1)
cluster2 <- subset(USArrests, anggota==2)
cluster_1 <- sapply(cluster1, mean)
cluster_2 <- sapply(cluster2, mean)
mean_total=rbind(cluster_1,cluster_2)
mean_total

#############################################################
install.packages("factoextra" , dependencies = TRUE)
install.packages("NbClust")
install.packages("ggplot2"")
install.packages("cluster")

library(cluster)
library(factoextra)
library(NbClust)


# Install the package

data = read.csv("Sales.csv")
data

str(data)

head(data)

??factor
data$Product_Description  = as.factor(data$Product_Description)
data$Product_Category = as.factor(data$Product_Category)
data$Product_Line = as.factor(data$Product_Line)
data$Raw_Material = as.factor(data$Raw_Material)
data$Region = as.factor(data$Region)

data$Product_Description  = as.numeric(data$Product_Description)
data$Product_Category = as.numeric(data$Product_Category)
data$Product_Line = as.numeric(data$Product_Line)
data$Raw_Material = as.numeric(data$Raw_Material)
data$Region = as.numeric(data$Region)

head(data)

df = scale(data)
df



res.dist <- dist(df, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]
kmeans(df, centers=2, iter.max = 10, nstart = 25)
kmeans(df, centers=3, iter.max = 10, nstart = 25)
kmeans(df, centers=4, iter.max = 10, nstart = 25)
kmeans(df, centers=5, iter.max = 10, nstart = 25)

fviz_nbclust(data, FUN=hcut, method = "silhouette")

set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)
# Print the results
print(km.res)


#To find mean of the each variable
aggregate(df, by=list(cluster=km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster)
head(dd)
table(km.res$cluster)

km.res$size


fviz_cluster(km.res, data = df, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())


# -------------------- BAR PLOT -----------------------------------
# Combine cluster assignment with scaled data
clustered_data <- cbind(data.frame(cluster = km.res$cluster), as.data.frame(df))

# Set up layout for plots
par(mfrow=c(1, max(km.res$cluster))) # Setting up multiple plots in a row

# Plot histograms for each cluster
for (i in 1:max(km.res$cluster)) {
  # Subset data for the current cluster
  cluster_subset <- clustered_data[clustered_data$cluster == i, ]

  # Barplot with Quantity on x-axis and Sales Revenue on y-axis
  barplot(height = cluster_subset$Sales_Revenue,
          names.arg = cluster_subset$Quantity,
          main = paste("Cluster", i),
          xlab = "Quantity",
          ylab = "Sales Revenue",
          col = "skyblue")
}



# Combine cluster assignment with the dataset
library(ggplot2)

# Assuming df is your dataset and km.res contains the cluster assignments
# Rename columns in the scaled data
#colnames(df) <- make.unique(colnames(df), sep = "_")

# Combine scaled data with cluster assignments
clustered_data <- cbind(data.frame(cluster = km.res$cluster), as.data.frame(data))

# Create a 2D histogram for "Sales Revenue" and "Quantity" for each cluster
for (i in unique(clustered_data$cluster)) {
  # Subset data for the current cluster
  cluster_subset <- clustered_data[clustered_data$cluster == i, ]

  # Create a 2D histogram
  print(ggplot(cluster_subset, aes(x = Quantity, y = Sales_Revenue)) +
          geom_bin2d() +
          labs(title = paste("Cluster", i),
               x = "Quantity",
               y = "Sales Revenue"))
}


#Hierarchical clustering
res.hc <- hclust(d = res.dist, method = "ward.D2")
print(res.hc)
#cex: label size
install.packages("factoextra")
library("factoextra")
fviz_dend(res.hc, cex = 0.5)
# Compute cophentic distance
res.coph <- cophenetic(res.hc)
# Correlation between cophenetic distance and
# the original distance
cor(res.dist, res.coph)
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))
grp <- cutree(res.hc, k = 3)
head(grp, n = 3)
table(grp)/nrow(data)
aggregate(data , list(grp), mean)

        ')
  }else if(names == 'con'){
    cat('
install.packages("cautilities")
library(caUtilities)

install.packages("conjoint")
library(conjoint)
conjointAnalyis = read.csv("pizza_data.csv")
str(conjointAnalyis)
head(conjointAnalyis)


# converting into char to numeric

conjointAnalyis$brand = as.numeric(factor(conjointAnalyis$brand))
conjointAnalyis$price = as.numeric(factor(conjointAnalyis$price))
conjointAnalyis$weight = as.numeric(factor(conjointAnalyis$weight))
conjointAnalyis$crust = as.numeric(factor(conjointAnalyis$crust))
conjointAnalyis$cheese = as.numeric(factor(conjointAnalyis$crust))
conjointAnalyis$size = as.numeric(factor(conjointAnalyis$size))
conjointAnalyis$toppings = as.numeric(factor(conjointAnalyis$toppings))
conjointAnalyis$spicy = as.numeric(factor(conjointAnalyis$spicy))

# removing na values

colSums(is.na(conjointAnalyis))
conjointAnalyis = na.omit(conjointAnalyis)
conjointAnalyis

# removing last col from dataset
conjointAnalyis = conjointAnalyis[,-ncol(conjointAnalyis)]
conjointAnalyis

# creating data with 10 observation and 16 profile
tprefm = matrix(sample(0:16 , 10*16 , replace = TRUE) , ncol = 16)

colnames(tprefm) = paste0("profile",1:16)
tprefm = as.data.frame(tprefm)
str(tprefm)

# Extracting columns names from dataset

col_names = colnames(conjointAnalyis)
col_names

caUtilities(y=tprefm[1,], x=conjointAnalyis, z=col_names)

conjoint_model = Conjoint(y=tprefm , x = pizza_data , z = col_names)


        ')
  }else if(names == 'lr'){
    cat('
library(corrplot)
library(ggplot2)
library(psych)
library(DescTools)
library(factoextra)
library(paran)

#Import the training CSV
ap_train<- read.csv("ap_train.csv")

#Convert non-numerical col to numerical col
ap_train_encode <- c("Gender", "Customer.Type", "Type.of.Travel", "Class", "satisfaction")
for (col in ap_train_encode) {
  ap_train[[col]] <- as.numeric(factor(ap_train[[col]]))
}
head(ap_train,2)

# Convert values to 0 and 1
ap_train$satisfaction <- ifelse(ap_train$satisfaction == 2, 0, 1)
head(ap_train,3)

#Check for null value
ap_train_nn <- na.omit(ap_train)
null_count_ap <- colSums(is.na(ap_train_nn))
print(null_count_ap)

# Exclude columns "ID", "X", and "satisfaction"
cols_to_plot <- colnames(ap_train_nn)[!(colnames(ap_train_nn) %in% c("id", "X", "satisfaction"))]
variables <- c("v1", "v2", "v3", "v4")

# Set up the plotting layout
par(mfrow = c(1, length(variables)))

# Create a boxplot for each column
for (col in cols_to_plot) {
  boxplot(ap_train_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Specify the columns to Winsorize
cols_to_winsorize <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")

# Apply Winsorization to the specified columns
for (col in cols_to_winsorize) {
  ap_train_nn[[col]] <- Winsorize(ap_train_nn[[col]], probs = c(0.10, 0.90))  # Trim 5% from both tails
}

variables1 <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
# Set up the plotting layout
par(mfrow = c(1, length(variables1)))

# Create a boxplot for each column
for (col in variables1) {
  boxplot(ap_train_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Convert the correlation matrix to a data frame
cap <- cor(ap_train_nn)
cor_df <- as.data.frame(as.table(cap))

# Create the ggplot2 correlation plot
names(cor_df) <- c("Variable1", "Variable2", "Correlation")
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black") +  # Add text labels
  scale_fill_gradient2(low = "blue", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust=1, size = 8, face = "italic"),
        axis.text.y = element_text(size = 8, face = "italic"))


# Reset the plotting layout
par(mfrow = c(1, 1))

# Filter the correlation matrix for variables with correlation greater than 0.2 with "satisfaction"
satisfaction_correlation <- cap[abs(cap["satisfaction", ]) > 0.2, ]

# Extract variable names
relevant_variables <- rownames(satisfaction_correlation)

# Subset the original dataframe based on relevant variables
relevant_data <- ap_train_nn[, relevant_variables]

# Print the first few rows of the relevant data
head(relevant_data)

print(colnames(relevant_data))

#Factor Analysis
#Create a new Dataframe with all relevant Columns
Fa_cols <- c("Inflight.wifi.service", "Food.and.drink", "Online.boarding",
                   "Seat.comfort", "Inflight.entertainment", "On.board.service",
                   "Leg.room.service", "Baggage.handling", "Checkin.service",
                   "Inflight.service", "Cleanliness")

# Convert the correlation matrix to a data frame
fa_df <- relevant_data[Fa_cols]
capdr <- cor(fa_df)
cor_dfr <- as.data.frame(as.table(capdr))
print(capdr)

# Parallel analysis (using the paran package)
optimal_num_factors_parallel <- fa.parallel(capdr)$nfact

# Factor analysis
model <- fa(capdr , 3, rotate = "promax")
loadings <- model$loadings
fa.diagram(loadings, factors = colnames(loadings), node.size = 2, mar = c(0.1, 0.1, 0.1, 0.1),
           node.color = "lightblue", arrow.color = "darkblue")


# Getting factor scores
factor_names <- colnames(model$loadings)
print(factor_names)
out<-fa(fa_df,3,scores = "regression")
factor_scores=out$scores

# Add factors and thier scores to relevant_data dataframe
relevant_data <- cbind(relevant_data, factor_scores)
head(relevant_data,2)

# Build logistic regression model
logit_model <- glm(satisfaction ~ MR1 + MR2 + MR3 + Type.of.Travel + Class + Flight.Distance, data = relevant_data, family = "binomial")

# Summarize the model
summary(logit_model)


##################################################################################################################3
ap_test<- read.csv("ap_test.csv")

#Testing the model
head(ap_test)
ap_test_encode <- c("Gender", "Customer.Type", "Type.of.Travel", "Class", "satisfaction")
for (col in ap_train_encode) {
  ap_test[[col]] <- as.numeric(factor(ap_test[[col]]))
}
# Convert values to 0 and 1
ap_test$satisfaction <- ifelse(ap_test$satisfaction == 2, 0, 1)
head(ap_test)

#Check for null value
ap_test_nn <- na.omit(ap_test)
null_count_ap <- colSums(is.na(ap_test_nn))
print(null_count_ap)

# Exclude columns "ID", "X", and "satisfaction"
cols_to_plot <- colnames(ap_test_nn)[!(colnames(ap_test_nn) %in% c("id", "X", "satisfaction"))]
variables <- c("v1", "v2", "v3", "v4")

# Set up the plotting layout
par(mfrow = c(1, length(variables)))

# Create a boxplot for each column
for (col in cols_to_plot) {
  boxplot(ap_test_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Specify the columns to Winsorize
cols_to_winsorize <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")

# Apply Winsorization to the specified columns
for (col in cols_to_winsorize) {
  ap_test_nn[[col]] <- Winsorize(ap_test_nn[[col]], probs = c(0.10, 0.90))  # Trim 5% from both tails
}

variables1 <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
# Set up the plotting layout
par(mfrow = c(1, length(variables1)))

# Create a boxplot for each column
for (col in variables1) {
  boxplot(ap_test_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

#Factor Analysis
Fa_colst <- c("Inflight.wifi.service", "Food.and.drink", "Online.boarding",
             "Seat.comfort", "Inflight.entertainment", "On.board.service",
             "Leg.room.service", "Baggage.handling", "Checkin.service",
             "Inflight.service", "Cleanliness")

# Convert the correlation matrix to a data frame
fa_dft <- ap_test_nn[Fa_colst]
capdr <- cor(fa_dft)
cor_dfr <- as.data.frame(as.table(capdr))
print(capdr)

# Parallel analysis (using the paran package)
library(paran)
optimal_num_factors_parallel <- fa.parallel(capdr)$nfact

# Factor analysis
modelt <- fa(capdr , 3, rotate = "promax")
loadings <- modelt$loadings
fa.diagram(loadings, factors = colnames(loadings), node.size = 2, mar = c(0.1, 0.1, 0.1, 0.1),
           node.color = "lightblue", arrow.color = "darkblue")


# Assuming model is your factor analysis model
factor_namest <- colnames(modelt$loadings)
print(factor_namest)
out<-fa(fa_dft,3,scores = "regression")
factor_scores=out$scores

# Add factors to relevant_data dataframe
ap_test_nn <- cbind(ap_test_nn, factor_scores)
head(ap_test_nn)

# Predict on test data
predicted <- predict(logit_model, newdata = ap_test_nn, type = "response")

# calculate accuracy
predicted_class <- ifelse(predicted > 0.5, 1, 0)  # Convert probabilities to classes
accuracy <- mean(predicted_class == ap_test_nn$satisfaction)
print(paste("Accuracy:", accuracy))

# Interpret coefficients
summary(logit_model)

        ')
  }else if(names == 'ma'){
    cat('
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
library(ggplot2)
library(dplyr)

groceries_data1 = read.transactions("groceries.csv" , format = "basket" , sep = "," , rm.duplicates = TRUE)
groceries_data1



itemFrequencyPlot(groceries_data1,
                  type = "absolute",
                  topN = 30,
                  horiz = TRUE,
                  main = "Absolute item frequency")




# Setting the plot configuration option
par(mfrow=c(2,1))

# Plot the relative and absolute item frequency plot
itemFrequencyPlot(groceries_data1,
                  type = "relative",
                  topN = 30,
                  horiz = TRUE,
                  main = "Relative item frequency")

itemFrequencyPlot(groceries_data1,
                  type = "absolute",
                  topN = 30,
                  horiz = TRUE,
                  main = "Absolute item frequency")


# Setting the plot configuration option
par(mar=c(2,30,2,2), mfrow=c(1,1))

# Plot the 10 least popular items
barplot(sort(table(unlist(LIST(groceries_data1))))[1:10],
        horiz = TRUE,
        las = 1,
        main = "Least popular items")

# Inspect the first few transactions
inspect(head(groceries_data1))



# Apriori algorithm to mine association rules
itemsets <- apriori(groceries_data1,
                    parameter = list(support = 0.4,
                                     target = "frequent"))


str(itemsets)
itemsets
# Check the structure of itemsets

rules = apriori(groceries_data1 , parameter = list(support=0.001 , confidence = 0.5))
summary(rules)

inspect(head(rules))

sorted_rules = sort(rules , by= "confidence" , decreasing = TRUE)
top_10_rules = sorted_rules[1:10]
inspect(top_10_rules)
top_3_rules = sorted_rules[1:3]

# Plot rules using arulesViz
plot(top_10_rules, method = "graph", control = list(type = "items"))
plot(top_3_rules, method = "graph", control = list(type = "items"))

        ')
  }else if(names == 'ca'){
    cat("
# Load the lavaan package
install.packages('lavaan')
install.packages('semPlot')
library(lavaan)
# Load the built-in dataset HolzingerSwineford1939
data('HolzingerSwineford1939')
# Define the CFA model
model = 'visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
        speed =~ x7 + x8 + x9
        x1 ~~ x4
        x2 ~~ x5
        x3 ~~ x6
        x4 ~~ x7
        x5 ~~ x8
        x6 ~~ x9'
# Fit the CFA model to the data
fit<-cfa(model, data = HolzingerSwineford1939)
# Summarize the results
summary(fit)

# Visualize the results (optional)
inspect(fit,'std.lv')


library(semPlot)
#plot the standardized factor loadings
semPaths(fit,'std',layout='tree2')
semPaths(fit,whatLabels = 'est',style = 'lisrel',main='cfa diagram')

        ")
  }else if(names == 'ea'){
    cat('
install.packages("corrr")
install.packages("ggcorrplot")
library("corrr")
install.packages("psych" , dependencies = TRUE)
library(psych)
install.packages("GPArotation")
library(GPArotation)
library(ggcorrplot)

df = read.csv("ap_train.csv")
df

colSums(is.na(df))

df = na.omit(df)

colSums(is.na(df))

df

df = df[,9:22]
ploting = cor(df)
ploting

ggcorrplot(ploting)


fa.parallel(ploting, fa="fa")

ploting = fa(c,5 , rotate="promax")
ploting

fa.diagram(ploting$loadings)

        ')
  }else if(names == "kpca"){
    cat('
# Load the required package
library(kernlab)
# Load the built-in iris dataset
data(iris)
# Extract the features from the iris dataset
features <- iris[, 1:4]
# Define a kernel function (e.g., radial basis function kernel)
kernel <- rbfdot(sigma = 0.1)
# Convert features to matrix
features_matrix <- as.matrix(features)
# Perform Kernel PCA
kpca_result <- kpca(features_matrix, kernel = kernel, features = 2)
# Extract the projected data
projected_data <- as.data.frame(predict(kpca_result, features_matrix))
# Plot the projected data
plot(projected_data, col = iris$Species, pch = 20, main = "Kernel PCA on Iris Dataset")
legend("topleft", legend = unique(iris$Species), col = 1:length(unique(iris$Species)), pch = 20)

        ')
  } else {
    cat('
        Prinicipal Component :- pca
        SEM :- sem
        LDAQDA :- ldaqda
        Cluster Analysis :- cla
        Conjoint Analysis :- con
        logistic regression :- lr
        Market Analysis :- ma
        Confirmatory :- ca
        Exploratory :- ea
        Kernal - kpca

        ')
  }
}
