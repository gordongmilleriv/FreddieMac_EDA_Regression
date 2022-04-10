library(ggplot2)
library(Hmisc)
library(corrplot)
library(plyr)
library(dplyr)
library(ggcorrplot)
library(psych)
library(usmap)
library(writexl)
library(moments)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

dfQ1 <- read.delim(file.choose(), sep = "|", header = FALSE)
dfQ2 <- read.delim(file.choose(), sep = "|", header = FALSE)
dfQ3 <- read.delim(file.choose(), sep = "|", header = FALSE)
dfQ4 <- read.delim(file.choose(), sep = "|", header = FALSE)

df <- rbind(dfQ1, dfQ2, dfQ3, dfQ4)

colnames(df) <- c("Credit_Score","First_Payment_Date", "First_Time_Homebuyer", "Maturity_Date","Metropolitan_Statistical_Area","Mortgage_Insurance_Percentage","Number_of_Units","Occupancy_Status","Original_Combined_Loan_to_value","Original_Debt_To_Income","Original_UPB", "Original_Loan_To_Value", "Original_Interest_Rate","Channel","Prepayment_Penalty_Mortgage_Flag","Amortization_Type","Property_State","Property_Type","Postal_Code","Loan_Sequence_Number","Loan_Purpose","Original_Loan_Term","Number_of_Borrowers","Seller_Name","Servicer_Name","Super_Conforming_Flag","Pre_Harp_loan_sequence_Number","Program_Indicator","Harp_Indicator","Property_Valuation_Method","Interest_Only_Indicator")

##replace placeholder values with NA values for numeric variables and categorical variables of interest
df$Credit_Score[df$Credit_Score == 9999] <- NA
df$First_Time_Homebuyer[df$First_Time_Homebuyer == 9] <- NA
df$Mortgage_Insurance_Percentage[df$Mortgage_Insurance_Percentage == 999] <- NA
df$Number_of_Units[df$Number_of_Units == 99] <- NA
df$Original_Combined_Loan_to_value[df$Original_Combined_Loan_to_value == 999] <- NA
df$Original_Debt_To_Income[df$Original_Debt_To_Income == 999] <- NA
df$Original_Loan_To_Value[df$Original_Loan_To_Value == 999] <- NA
df$Loan_Purpose[df$Loan_Purpose == 9] <- NA
df$Number_of_Borrowers[df$Number_of_Borrowers == 99] <- NA
df$Property_Valuation_Method[df$Property_Valuation_Method == 9] <- NA

#Create dataframe for descriptive stats - will use later on
desc_df <- subset(df, select = c("Credit_Score","Original_Debt_To_Income","Original_Interest_Rate","Original_Loan_Term"))
desc_df <- na.omit(desc_df)

#create dataframes with independent variable of interest and interest rate for univariate and bivariate analysis
credit_df <- subset(df, select = c("Credit_Score","Original_Interest_Rate"))
credit_df <- na.omit(credit_df)

debt_df <- subset(df, select = c("Original_Debt_To_Income","Original_Interest_Rate"))
debt_df <- na.omit(debt_df)

loan_term_df <- subset(df, select = c("Original_Loan_Term"))
loan_term_df <- na.omit(loan_term_df)


int_df <- subset(df, select = c("Original_Interest_Rate"))
int_df <- na.omit(int_df)

loan_purpose_df <- subset(df, select = c("Loan_Purpose", "Original_Interest_Rate"))
loan_purpose_df <- na.omit(loan_purpose_df)

first_home_df <- subset(df, select = c("First_Time_Homebuyer", "Original_Interest_Rate"))
first_home_df <- na.omit(first_home_df)
#create dataframe with only numeric variables for correlation matrix
### drop loan to value
numeric_df <- subset(df, select = -c(2,3,4,5,8,14,15,16,17,18,19,20,21,24,25,26,27,28,29,30,31))

numeric_df <- na.omit(numeric_df)

colnames(numeric_df) <- c("Credit Score","Insurance %", "# of Units", "Comb. loan To value", "Debt To Income", "UPB","Loan To Value", "Interest Rate", "Loan Term", "# of borrowers")

#corrplot
df.cor = round(cor(numeric_df),3)
df.rcorr = rcorr(as.matrix(numeric_df),3)
ggcorrplot(df.cor) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

#slight pos. correlation between loan term and loan to value - throw out loan to value
df.rcorr[["P"]]
cor(numeric_df$`Loan To Value`, numeric_df$`Loan Term`)

#narrow down dataframe to variables of interest
df <- subset(df, select = -c(2,4,5,6,7,8,9,11,14,15,16,18,19,20,23,24,25,26,27,28,29,30,31))

df <- na.omit(df)

#### Univariate Analysis ###

#Credit_Score
credit_mn <- min(credit_df$Credit_Score, na.rm = T)
credit_mx <- max(credit_df$Credit_Score, na.rm = T)
binw_credit = (credit_mx-credit_mn)/15

#skewed left
options(scipen=10000)
ggplot(credit_df,aes(x=Credit_Score)) + geom_histogram(fill="lightblue", color="black",binwidth=binw_credit)+
  labs(x="Credit Score")+
  theme_bw()

#Original_Debt_To_Income
debt_mn <- min(debt_df$Original_Debt_To_Income, na.rm = T)
debt_mx <- max(debt_df$Original_Debt_To_Income, na.rm = T)
binw_debt = (debt_mx-debt_mn)/15

##also skewed left - interesting because most of the data is around higher debt to income ratios but most of the credit scores in our data are higher
ggplot(debt_df,aes(x=Original_Debt_To_Income))+ geom_histogram(fill="lightblue", color="black",binwidth=binw_debt)+
  labs(x="Debt To Income")+
  theme_bw()

#Interest Rate - seems to follow an approximately normal distribution
int_mn <- min(int_df$Original_Interest_Rate, na.rm = T)
int_mx <- max(int_df$Original_Interest_Rate, na.rm = T)
binw_int = (int_mx-int_mn)/15

ggplot(int_df,aes(x=Original_Interest_Rate))+
  geom_histogram(fill="lightblue", color="black",binwidth=binw_int)+
  labs(x="Interest Rate")+
  theme_bw()

#checking kurtosis of histogram
kurtosis(int_df$Original_Interest_Rate)
skewness(int_df$Original_Interest_Rate)

#Jaque-Bera Normality Test
jarque.test(int_df$Original_Interest_Rate)

#Original Loan Term - seems like almost all of the data is 30 year mortages (360 months)
loan_term_mn <- min(loan_term_df$Original_Loan_Term, na.rm = T)
loan_term_mx <- max(loan_term_df$Original_Loan_Term, na.rm = T)
binw_loan_term = (loan_term_mx-loan_term_mn)/15

ggplot(loan_term_df,aes(x=Original_Loan_Term))+
  geom_histogram(fill="lightblue", color="black",binwidth=binw_loan_term)+
  labs(x="Loan Term")+
  theme_bw()

#First Time Homebuyer
ggplot(first_home_df,aes(x=First_Time_Homebuyer))+
  geom_bar(fill="lightblue", color="black")+
  labs(x="First Time Homebuyer")+
  theme_bw() 
#Property State - notice that US territories have almost no data, mention that were only going to consider the 50 US states in our anlaysis
ggplot(df,aes(x=Property_State))+
  geom_bar(fill="lightblue", color="black")+
  labs(x="Property State")+
  theme(axis.text.y= element_text(size=7, vjust=.5, hjust=1))+ coord_flip()
#Loan purpose
ggplot(loan_purpose_df,aes(x=Loan_Purpose))+
  geom_bar(fill="lightblue", color="black")+
  labs(x="Loan Purpose")+
  theme_bw()

### Descriptive Statistics ###
desc <- describe(desc_df)
desc$vars[desc$vars == 1] <- "Credit Score"
desc$vars[desc$vars == 2] <- "Debt To Income"
desc$vars[desc$vars == 3] <- "Interest Rate"
desc$vars[desc$vars == 4] <- "Loan Term"

#write descriptive statistics to a csv file
write_xlsx(desc_num, path = tempfile(fileext = "Descriptive_Num_stats.xlsx"))

#categorical descriptive stats
homebuyer_freq <- table(df$First_Time_Homebuyer)/length(df$First_Time_Homebuyer)
table(df$Property_State)/length(df$Property_State)
table(df$Loan_Purpose)/length(df$Loan_Purpose)

homebuyer_freq_df<- data.frame(homebuyer_freq)
#write homebuyer relative frequency table to csv
write_xlsx(homebuyer_freq_df, path = tempfile(fileext = "homebuyerfreqtable.xlsx"))

### Bivariate Analysis ###

#sample our data 
df_sample <- sample_n(df, 1000) 
credit_df_sample <- sample_n(credit_df, 1000)

## Heat map   
df_heatmap <- data.frame(df$Property_State,df$Original_Interest_Rate)

df_heatmap <- df_heatmap %>%
  group_by(df.Property_State) %>%
  summarise(mean_Interest_Rate = mean(df.Original_Interest_Rate))

df_heatmap$df.Property_State[df_heatmap$df.Property_State == "DC"] <- NA
df_heatmap$df.Property_State[df_heatmap$df.Property_State == "VI"] <- NA
df_heatmap$df.Property_State[df_heatmap$df.Property_State == "GU"] <- NA
df_heatmap$df.Property_State[df_heatmap$df.Property_State == "PR"] <- NA

df_heatmap <- na.omit(df_heatmap)

names(df_heatmap)[1] <- 'state'

plot_usmap(data = df_heatmap, regions="state", values="mean_Interest_Rate") +
  scale_fill_continuous(name="Avg. Interest Rate",low="white",high="red",
                        label=scales::comma)+
  theme(legend.position = "right")

## Credit Score simple regression 

fit_cr <- lm(Original_Interest_Rate ~ Credit_Score, credit_df)
summary(fit_cr)

ggplot(credit_df_sample, aes(x=Credit_Score, y=Original_Interest_Rate))+geom_point(alpha =.25)+
  xlab("Credit Score")+ylab("Original Interest Rate")+ 
  stat_smooth(method =lm)+theme_minimal()

## Credit rating (category analysis)

dff <- data.frame(credit_df$Credit_Score, credit_df$Original_Interest_Rate)
colnames(dff) <- c("Credit_Score","Original_Interest_Rate")

dff$Credit_Score_cat[dff$Credit_Score >800 & dff$Credit_Score < 850] = "Excellent"
dff$Credit_Score_cat[dff$Credit_Score > 740 & dff$Credit_Score < 800] = "Very Good"
dff$Credit_Score_cat[dff$Credit_Score > 670 & dff$Credit_Score < 740] = "Good"
dff$Credit_Score_cat[dff$Credit_Score > 580 & dff$Credit_Score < 670] = "Fair"
dff$Credit_Score_cat[dff$Credit_Score > 300 & dff$Credit_Score < 560] = "Poor"
describe(dff)

dff <- na.omit(dff)
dff_sample <- sample_n(dff,1000)

dff$Credit_Score_cat <- factor(dff$Credit_Score_cat, levels=c("Poor", "Fair","Good","Very Good","Excellent"))

fit_cat_credit <- lm(dff$Original_Interest_Rate~dff$Credit_Score_cat)
summary(fit_cat_credit)


ggplot(dff, aes(x=Credit_Score_cat, y=Original_Interest_Rate, colour = Credit_Score_cat),alpha=.5)+geom_boxplot()+
  xlab("Credit Score rating")+ylab("Original Interest Rate")+
  theme_minimal()+ scale_colour_discrete(name="Credit Score\nRating")

## First Time Homebuyer simple regression
fit_home <- lm(Original_Interest_Rate ~ First_Time_Homebuyer, df)
summary(fit_home)

ggplot(df_sample, aes(x=First_Time_Homebuyer, y=Original_Interest_Rate, colour = First_Time_Homebuyer))+geom_boxplot()+
  xlab("First Time Homebuyer Flag")+ylab("Original Interest Rate")+ggtitle("First Time Homebuyer Flag Boxplot")+
  scale_colour_discrete(name="First Time\nHomebuyer")+theme_minimal()

#Original Debt to income simple regression 
fit_db <- lm(Original_Interest_Rate ~ Original_Debt_To_Income, df)
summary(fit_db)

ggplot(df_sample, aes(x=Original_Loan_To_Value, y=Original_Interest_Rate))+geom_point(alpha =.25)+
  xlab("Loan to Value")+ylab("Interest Rate")+ggtitle("Scatterplot of Loan to Value on Interest Rate")+ 
  stat_smooth(method =lm)+theme_minimal()

#Loan to Value simple regression
fit_lv <- lm(Original_Interest_Rate ~ Original_Loan_To_Value, df)
summary(fit_lv)

ggplot(df_sample, aes(x=Original_Loan_To_Value, y=Original_Interest_Rate))+geom_point(alpha =.25)+
  xlab("Loan to Value")+ylab("Original Interest Rate")+ggtitle("Scatterplot of Loan to Value on Interest Rate")+ 
  stat_smooth(method =lm)+theme_minimal()

# Loan Purpose
fit_lp <- lm(Original_Interest_Rate ~ Loan_Purpose, df)
summary(fit_lp)

ggplot(df_sample, aes(x=Loan_Purpose, y=Original_Interest_Rate, colour=Loan_Purpose))+geom_boxplot()+
  xlab("Loan Purpose")+ylab("Original Interest Rate")+ggtitle("Loan Purpose Boxplot")+ 
  stat_smooth(method =lm)+theme_minimal()+ scale_colour_discrete(name="Loan\nPurpose")

#Loan Term
fit_lt <- lm(Original_Interest_Rate ~ Original_Loan_Term, df)
summary(fit_lt)

ggplot(df_sample, aes(x=Original_Loan_Term, y=Original_Interest_Rate))+geom_point(alpha =.25)+
  xlab("Loan Term")+ylab("Original Interest Rate")+ggtitle("Scatterplot of Loan Term on Interest Rate")+ 
  stat_smooth(method =lm)+theme_minimal()

#Loan to Value and loan term
fit_db_lt <- lm(Original_Loan_To_Value ~ Original_Loan_Term , df)
summary(fit_db_lt)

ggplot(df_sample, aes(x=Original_Debt_To_Income, y=Original_Loan_Term))+geom_point(alpha =.25)+
  xlab("Debt to Income")+ylab("Original Loan Term")+ggtitle("Scatterplot of Debt to Income on Loan Term")+ 
  stat_smooth(method =lm)+theme_minimal()

#correaltion t-test
cor.test(df$Original_Loan_Term, df$Original_Loan_To_Value)


### Multivariate Analysis ###

#does credit score interact with credit rating (credit_score_cat)
ggplot(dff_sample, aes(x=Credit_Score, y=Original_Interest_Rate, colour=Credit_Score_cat)) +
  geom_point() + xlab("Credit Score")+ylab("Interest Rate")+
  ggtitle("Credit score vs Interest Rate scatter plot by Credit Rating")+
  stat_smooth(method=lm)+scale_colour_discrete(name="Credit Score\nRating")

fit_interaction <- lm(Original_Interest_Rate~Credit_Score_cat*Credit_Score, data=dff)
summary(fit_interaction)

#Multiple linear regression

fit_mult <- lm(Original_Interest_Rate ~ Credit_Score+Original_Loan_To_Value+Original_Loan_Term+Loan_Purpose+First_Time_Homebuyer, data = df)
summary(fit_mult)
#Adding Debt to income to our multiple linear regression only increases Adjusted R-Squared by .0005 and
# we know that debt to income has a statistically significant correlation with Loan term so to avoid collinearity we are going to remove Original Debt To Income from our model.

#cooks distance analysis 
cooks.distance(fit_mult)
p <- length(coef(fit_mult))
n <- length(fit_mult$residuals)
influential_outliers <- which(cooks.distance(fit_mult)>qf(p=.5,df1=p,df2=n-p))
somewhat_influential_outliers <- which(cooks.distance(fit_mult)>qf(p=.2,df1=p,df2=n-p))
#after using cooks distance it seems like we have no influential points, so we should retain all of our data.

#are residuals normally distributed?
histogram(fit_mult$residuals, main = "Multiple Regression Residuals Histogram",
          xlab = "Multiple Regression Residuals")

#Jaque-Bera Normality Test for residuals
jarque.test(fit_mult$residuals)
