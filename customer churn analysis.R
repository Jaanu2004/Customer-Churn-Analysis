#Loading the libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(plotrix)
library(cowplot)
library(Hmisc)
library(tidyr)
library(scales)
library(MASS)
library(psych)
library(naniar)
library(grid)
library(corrplot)

##Loading the data
churnData=read.csv(file.choose(),header = TRUE)
##Viewing the first and last elements for a overview on data
head(churnData)
tail(churnData)
##dimensions of data
dim(churnData)
##structure of data
str(churnData)
## statistical info
describe(churnData)
summary(churnData)
##check whether the data is having null values or not
colSums(is.na(churnData))
#----------------------------DATA MANUPULATION-------------------------------------

##Here we are not having any null values so,now let us insert null values
#Not to effect the original data let us make a copy of the churnData
churn2Data=churnData
head(churn2Data)
##Now let us insert null values in our copy of data
set.seed(123)
churn2Data[sample(seq(NROW(churn2Data)),round(3 / 100 * nrow(churn2Data))),"Age"]<-NA

set.seed(123)
churn2Data[sample(seq(NROW(churn2Data)),round(5 / 100 * nrow(churn2Data))),"Gender"]<-NA

##Now let us see the nullvalues
colSums(is.na(churn2Data))

#Missing values in graph


#--------------------------------DATA CLEANING-------------------------------
#1.Handling Missing Values

#filling age col with their mean
churn2Data <- churn2Data %>%
  mutate(Age = ifelse(is.na(Age), median(churn2Data$Age,na.rm=TRUE), Age))

##Here gender is categorical so we should fill it with mode but in r 
#We dont have mode function so let let us create mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

churn2Data<-churn2Data %>%
  mutate(Gender=ifelse(is.na(Gender),Mode(Gender),Gender))

colSums(is.na(churn2Data))##We are having 0 null values

#2.Handling duplicated values
##Identifying whether we have duplicate values or not
churn2Data[duplicated(churn2Data),]#No duplicate values

#3.Outliers
# Selecting numerical columns
numericalColumns <- churn2Data[, c("CreditScore", "Age", "Tenure", "Balance", "NumOfProducts", "EstimatedSalary")]
# Creating boxplots for each numerical column
par(mfrow = c(2, 3))  # 2 rows, 3 columns #par-parameter #mfrow-multiple figure row
for (col in colnames(numericalColumns)) {
  boxplot(numericalColumns[[col]], main = col)
}

for (col in colnames(churn2Data)[sapply(churn2Data, is.numeric)]) {
  outliers <- boxplot(churn2Data[[col]], plot = FALSE)$out
  cat("Column:", col, "\n")
  cat("Number of outliers:", length(outliers), "\n")
  cat("Outliers:", outliers, "\n\n")
}
#We are having extreme values but it is not effecting our result

#---------------------------------------DATA TRANSFORMATION-----------------------
#5.Transforming Data for univariate analysis
churn2Data <- churn2Data %>%
  mutate(Exited = ifelse(Exited == 1, "Yes", "No"))

churn2Data <- churn2Data %>%
  mutate(IsActiveMember = ifelse(IsActiveMember == 1, "Yes", "No"))


churn2Data <- churn2Data %>%
  mutate(HasCrCard = ifelse(HasCrCard== 1, "Yes", "No"))
##for tenure data
labels <- sprintf("%d - %d", seq(0, 11, 2), seq(1, 11, 2))

# Cut the 'tenure' column into bins
churn2Data$Tenure_group <- cut(
  churn2Data$Tenure,
  breaks = c(seq(0, 11, 2), Inf),
  right = FALSE,
  labels = labels
)

#6.dropping the columns that are not required
churn2Data<-subset(churn2Data,select = -c(RowNumber,CustomerId,Surname))
head(churn2Data)
#----------------------------------DATA EXPLORATION---------------------------
#1.Univariate analysis
##As our Problem statement is analyse the churn customers let us have a lookon that
custom_colors <- c('#FF5733', '#33FF57', '#5733FF', '#FF5733', '#33FF57', '#5733FF')
#exited
valueCount=table(churn2Data$Exited)
df_value_counts=as.data.frame(valueCount)
colnames(df_value_counts) <- c("Variable", "Count")
df_value_counts
percentageCount=prop.table(valueCount)*100
percentageCount
plot_ly(df_value_counts, x = ~Variable, y = ~Count) %>%
  layout(title = 'No.of customers churned', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count')) %>%
  add_trace(marker = list(color = custom_colors))
#card
valueCountcard=table(churn2Data$HasCrCard)
df_value_countcard=as.data.frame(valueCountcard)
colnames(df_value_countcard) <- c("Variable", "Count")
plot_ly(df_value_countcard, x = ~Variable, y = ~Count, name = 'Bar Plot') %>%
  layout(title = 'No.of Customers based on card', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count'))%>%
  add_trace(marker = list(color = c('yellow','purple')))

#gender
valueCountGender=table(churn2Data$Gender)
df_value_countGender=as.data.frame(valueCountGender)
colnames(df_value_countGender) <- c("Variable", "Count")
plot_ly(df_value_countGender, x = ~Variable, y = ~Count, name = 'Bar Plot') %>%
  layout(title = 'No.of Customers based on card', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count'))%>%
  add_trace(marker = list(color = c('blue','pink')))


#active
valueCountactive=table(churn2Data$IsActiveMember)
df_value_countactive=as.data.frame(valueCountactive)
colnames(df_value_countactive) <- c("Variable", "Count")
plot_ly(df_value_countactive, x = ~Variable, y = ~Count, name = 'Bar Plot') %>%
  layout(title = 'No.of Customers based on active', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count'))%>%
  add_trace(marker = list(color = c('black','grey')))

#products
valueCountproducts=table(churn2Data$NumOfProducts)
df_value_countproducts=as.data.frame(valueCountproducts)
colnames(df_value_countproducts) <- c("Variable", "Count")
plot_ly(df_value_countproducts, x = ~Variable, y = ~Count, name = 'Bar Plot') %>%
  layout(title = 'No.of Customers based on products', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count'))%>%
  add_trace(marker = list(color = c('brown','darkgreen','darkblue','red')))

#age
valueCountage=table(churn2Data$Age)
df_value_countage=as.data.frame(valueCountage)
colnames(df_value_countage) <- c("Variable", "Count")
plot_ly(df_value_countage, x = ~Variable, y = ~Count, name = 'Bar Plot') %>%
  layout(title = 'No.of Customers based on age', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count'))%>%
  add_trace(marker = list(color = 'violet'))
#tenure
valueCounttenure=table(churn2Data$Tenure_group)
df_value_counttenure=as.data.frame(valueCounttenure)
colnames(df_value_counttenure) <- c("Variable", "Count")
plot_ly(df_value_counttenure, x = ~Variable, y = ~Count, name = 'Bar Plot') %>%
  layout(
    title = 'No.of Customers based on age',
    xaxis = list(title = 'Target variable'),
    yaxis = list(title = 'count'),
    showlegend = TRUE,
    bargap = 0.05  # Adjust the gap between bars if needed
  ) %>%
  add_trace(marker = list(color = c('green','skyblue','gold','blue','brown')))


##From the graph we can undersatand that exited ratio is almost 20:30
#Let us analyse this by other categories further

#Univariate analysis

#For gender
plotforgender <- ggplot(churn2Data, aes(x = Gender, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Countplot by Churn based on gender")

print(plotforgender)

#for card holders
plotforcard <- ggplot(churn2Data, aes(x = HasCrCard, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Countplot by Churn based on card holders")

print(plotforcard)

#For active members
plotforactivemembers <- ggplot(churn2Data, aes(x = IsActiveMember, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Countplot by Churn based on Active members")

print(plotforactivemembers)

#Geography
plotforgeography <- ggplot(churn2Data, aes(x = Geography, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Countplot by Churn based on Geography")

print(plotforgeography)

#For tenure
plotfortenure <- ggplot(churn2Data, aes(x = Tenure_group, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Countplot by Churn based on Tenure")

print(plotfortenure)

#for products
plotforproducts <- ggplot(churn2Data, aes(x = NumOfProducts, fill = Exited)) +
  geom_bar(position = "dodge") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Countplot by Churn based on No.of Products")

print(plotforproducts)

#For credit  score
ggplot(churnData, aes(x = CreditScore, fill = factor(Exited))) +
  geom_density(alpha = 0.8) +
  labs(title = "Density Plot of CreditScore by Exited Status",
       x = "Credit Score",
       y = "Density")+
  scale_fill_manual(values = c("green", "purple"), name = "Exited") +theme_minimal()

#For Estimated Salary
ggplot(churnData, aes(x = EstimatedSalary, fill = factor(Exited))) +
  geom_density(alpha = 0.8) +
  labs(title = "Density Plot of EstimatedSalary by Exited Status",
       x = "Estimated Salary",
       y = "Density") +
  scale_fill_manual(values = c("red", "blue"), name = "Exited") +
  theme_minimal()
#For Balance
ggplot(churnData, aes(x = Balance, fill = factor(Exited))) +
  geom_density(alpha = 0.9) +
  labs(title = "Density Plot of Balance by Exited Status",
       x = "Balance",
       y = "Density") +
  scale_fill_manual(values = c("yellow", "purple"), name = "Exited") +
  theme_minimal()
#BAlance alone is not effecting
#for age
  ggplot(churn2Data, aes(x = Age,fill=factor(Exited))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("brown", "skyblue")) +
    labs(fill = "Churn") +
    labs(y = "Density", x = "Age") +
    ggtitle("Age by Churn") +
    theme_minimal()
 
  
#2.Bivariate analysis
#for bivariate analysis dividing it into exited non exited
exited=churn2Data[churn2Data$Exited=='Yes',]
notexited=churn2Data[churn2Data$Exited=='No',]


#for exited
#Age and Gender
ggplot(exited, aes(x=Gender, fill=Geography)) +
  geom_bar(position="dodge", stat="count") +
  labs(title="Exited customers based Gender and Geography",
       x="Gender",
       y="Count") +
  geom_text(
    aes(label = stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = -0.5
  )+
  theme_minimal()

  #card and gender
  ggplot(exited, aes(x=Gender, fill=HasCrCard)) +
    geom_bar(position="dodge", stat="count") +
    labs(title="Exited customers based Gender and card holders",
         x="Gender",
         y="Count") +
    geom_text(
      aes(label = stat(count)),
      stat = "count",
      position = position_dodge(width = 1),
      vjust = -0.5
    )+
    theme_minimal()
  #geography and gender
  ggplot(exited, aes(x=Geography, fill=HasCrCard)) +
    geom_bar(position="dodge", stat="count") +
    labs(title="Exited customers based Gender and card holders",
         x="Gender",
         y="Count") +
    geom_text(
      aes(label = stat(count)),
      stat = "count",
      position = position_dodge(width = 1),
      vjust = -0.5
    )+
    theme_minimal()
#Not exited
  
    #piechart for geography
 chart1= ggplot(notexited, aes(x = "", fill = Geography)) +
    geom_bar(width = 1, stat = "count") +
    coord_polar("y") +
    ggtitle("Distribution of Geography")
  
  # Pie chart for Gender
 chart2=ggplot(notexited, aes(x = "", fill = Gender)) +
    geom_bar(width = 1, stat = "count") +
    coord_polar("y") +
    ggtitle("Distribution of Gender")
  
  # Pie chart for HasCrcard
 chart3=ggplot(notexited, aes(x = "", fill = as.factor(HasCrCard))) +
    geom_bar(width = 1, stat = "count") +
    coord_polar("y") +
    ggtitle("Distribution of HasCrcard")
  
  # Pie chart for Isactive
 chart4=ggplot(notexited, aes(x = "", fill = as.factor(IsActiveMember))) +
    geom_bar(width = 1, stat = "count") +
    coord_polar("y") +
    ggtitle("Distribution of Isactive")
  
 grid.arrange(
   top = textGrob("Combined Distribution of Charts for non-exited customers", gp = gpar(fontsize = 16, fontface = "bold")),
   arrangeGrob(chart1, chart2, chart3, chart4, ncol = 2)
 )
 
  
  
  
#Numerical Analysis
  numeric_data <- select_if(churn2Data, is.numeric)
  correlation_matrix <- cor(numeric_data)
  print(as.data.frame(correlation_matrix))
  correlation_with_churn <- correlation_matrix["Exited",]
  
  # Create a  plot of the correlation with 'Churn'
  corrplot(correlation_matrix, method = "color", col = c("darkblue", "green", "darkred",'purple'), scale = "none")
  