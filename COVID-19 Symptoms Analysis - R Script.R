# The aim of this script is to analyze the patterns of COVID-19 symptoms

# The following research questions will guide the analysis:
# 1. What are the predominant symptoms observed in individuals affected by a COVID-19 infection?
# 2. In what ways do the symptoms experienced by individuals with COVID-19 infection vary from those who are unaffected?
# 3. What connections exist between individual characteristics such as sex, and weight, and the manifestation of symptoms among patients diagnosed with COVID-19?
# 4. What is the prevalence rate of persistent symptoms among individuals who have contracted COVID-19?
# 5. What is the pattern of long-COVID symptoms of patients with COVID-19?

# import required packages
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
#library(sjPlot)
library(tidyr)
library(tibble)
library(shadowtext)

# load dataset
covid_data <- read.csv("C:/Users/User/covid-19_wave3_survey_cls (1).csv")

# the dataset contains a very large number of variables 
# to save memory and improve efficiency, only variables needed for the analysis will be selected from the dataset

cols <- c(8,16,35,37:45,67:85,109:126,282,401) #interested variables selected

#reduce the dataset to contain only the variables selected
covid_df <- covid_data[, cols]

# Data preprocessing
# 1. treat all negative values in the dataset as missing values "NA"
covid_df <- covid_df %>% mutate_all(list(~ replace(., . < 0, NA)))

# 2. convert the target variable, "CW3_COVID19" to binary values
# the variable contains values with range 1-4;
# 1 and 2 will be converted to 1, while 3 and 4 will be converted to 0
# 0 will rep those without COVID19, 1 will rep those with the infection

covid_df$CW3_COVID19[covid_df$CW3_COVID19 == 1 | covid_df$CW3_COVID19 == 2] <- 1
covid_df$CW3_COVID19[covid_df$CW3_COVID19 == 3 | covid_df$CW3_COVID19 == 4] <- 0

# convert the CW3_COVID19 variable to factor level variable
covid_df$CW3_COVID19 <- as.factor(covid_df$CW3_COVID19)

# observe the frequency of patients with COVID and those without it
count(covid_df, CW3_COVID19)

# plot a bar chart to show the frequency distribution
COVID_freq <- count(covid_df, CW3_COVID19) #create a dataframe for the freq dist
# plot the bar chart
COVID_freq %>%
  drop_na(CW3_COVID19) %>%
  ggplot(aes(x = CW3_COVID19, y = n, fill = CW3_COVID19)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_x_discrete(labels = c('No', 'Yes')) +
  labs(y = "Frequency", x = "Had COVID-19") +
  scale_fill_manual(name = "Had COVID-19", values = c("blue", "red"))


# Descriptive Statistics
# since most of the variables are categorical variables,
# the descriptive statistics that will be carried out is frequency distribution

# 1. Demographics (Sex distribution)
covid_df$CW3_PSEX <- as.factor(covid_df$CW3_PSEX)
sex_freq <- count(covid_df, CW3_PSEX) #create a dataframe for the sex freq dist
print(sex_freq) # print the frequency dist for sex
# plot the bar chart
sex_freq %>%
  drop_na(CW3_PSEX) %>%
  ggplot(aes(x = CW3_PSEX, y = n, fill = CW3_PSEX)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_x_discrete(labels = c('Male', 'Female')) +
  labs(y = "Frequency", x = "SEX") +
  scale_fill_manual(name = "SEX", values = c("blue", "red"))

# count the number of male and female subjects with and without COVID-19 history
count(had_COVID, CW3_PSEX)
count(no_COVID, CW3_PSEX)

# 2. Demographics (Weight Distribution)
# calculate mean and standard dev of weight to be used for density curve in histogram
mean_weight <- mean(covid_df$CW3_WGHTKG, na.rm = TRUE)
sd_weight <- sd(covid_df$CW3_WGHTKG, na.rm = TRUE)

# Create the histogram plot of the weight distribution with density curve
ggplot(covid_df, aes(x = CW3_WGHTKG)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "steelblue", color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean_weight, sd = sd_weight),
                col = "darkred",
                size = 1.2) +
  labs(title = "Histogram of Weight with Normal Density curve",
       x = "Weight", y = "Density") +
  scale_x_continuous(limits = c(20, 160), breaks = seq(20, 160, 20))


# 2. Descriptive statistics of symptoms using freq distribution
# first, we will divide the covid19 dataset into two (those with COVID and those without it)
# this statistics and vizualisation will show the prevalence of covid symptoms in those with the infection

had_COVID <- filter(covid_df, CW3_COVID19==1) #patients that had COVID19
no_COVID <- filter(covid_df, CW3_COVID19==0) #patients that did not have COVID19

# create a data frame of frequency of patients with covid-19 for each symptom
table_list <- list() #Create an empty list to store the tables

# Loop through symptoms columns and create tables
for (i in c(1:8,10:14,16:20,23)) {
  column_name <- paste0("CW3_COVIDSYMPT_", i)
  table_list[[i]] <- table(had_COVID[[column_name]])
}

# Combine the tables of all symptoms into one using cbind
hadCovidSympt_table <- do.call(cbind, table_list)

# Convert the combined table into a data frame 
hadCovidSympt_df <- as.data.frame(hadCovidSympt_table)

# rename the columns of the dataframe into symptoms respectively
colnames(hadCovidSympt_df) <- c("Fever", "Cough_dry", "Cough_Mucus", "SoreThroat",
                           "ChestTightness", "ShortBreath", "RunnyNose", 
                           "Nasal_Congestion", "MuscleBody_ache", "Fatigue", 
                           "Diarrhoea", "Loss_Smell", "Loss_taste", "Vomiting",
                           "SkinRash", "Sneezing", "Headache", "Others",
                           "No_symptoms")

# transpose the data frame
hadCovidSympt_df <- as.data.frame(t(hadCovidSympt_df))
# rename the columns to contain symptom name and the yes and no labels 
hadCovidSympt_df <- tibble::rownames_to_column(hadCovidSympt_df, "Symptoms")
colnames(hadCovidSympt_df) <- c("Symptoms", "Yes", "No")

# calculate the prevalence of each symptom in patient with history of COVID-19
hadCovidSympt_df$prop.x <- hadCovidSympt_df$Yes / 
  (hadCovidSympt_df$Yes + hadCovidSympt_df$No)

# plot a horizontal bar chart to show the prevalence of each symptom in patients that had COVID19
hadCovidSympt_df <- hadCovidSympt_df %>%
  arrange(prop.x) %>%
  mutate(Symptoms = reorder(Symptoms, prop.x)) # arrange the symptoms in ascending order of syptoms frequency
# the basic horizontal bar chart
plt <- ggplot(hadCovidSympt_df) +
  geom_col(aes(x=prop.x, y=Symptoms), fill = "blue", width = 0.9)
plt

# Customize the layout of the bar chart
plt <- plt + 
  scale_x_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "bottom"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "grey"),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 8)
  )
plt
# Add annotations
plt <- plt +
  labs(
    title = "COVID-19 Symptoms",
    subtitle = "Prevalence of various symptoms developed by COVID-19 Patients"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 8
    )
  )
plt

# In what ways do the symptoms experienced by individuals with COVID-19 infection vary from those who are unaffected?
# To answer the question, we will compute the proportion of patients with each symptom for both groups 
# i.e. those that had COVID19 and those that did not contract it

# create a data frame of frequency of patients without covid-19 for each symptoms
# this is similar to the dataframe created for those with COVID-19

table_list <- list() #Create an empty list to store the tables

# Loop through symptoms columns and create tables
for (i in c(1:8,10:14,16:20,23)) {
  column_name <- paste0("CW3_COVIDSYMPT_", i)
  table_list[[i]] <- table(no_COVID[[column_name]])
}

# Combine the tables of all symptoms into one using cbind
noCovidSympt_table <- do.call(cbind, table_list)

# Convert the combined table into a data frame 
noCovidSympt_df <- as.data.frame(noCovidSympt_table)

# rename the columns of the dataframe into symptoms respectively
colnames(noCovidSympt_df) <- c("Fever", "Cough_dry", "Cough_Mucus", "SoreThroat",
                                "ChestTightness", "ShortBreath", "RunnyNose", 
                                "Nasal_Congestion", "MuscleBody_ache", "Fatigue", 
                                "Diarrhoea", "Loss_Smell", "Loss_taste", "Vomiting",
                                "SkinRash", "Sneezing", "Headache", "Others",
                                "No_symptoms")

# transpose the data frame
noCovidSympt_df <- as.data.frame(t(noCovidSympt_df))
# rename the columns to contain symptom name and the yes and no labels 
noCovidSympt_df <- tibble::rownames_to_column(noCovidSympt_df, "Symptoms")
colnames(noCovidSympt_df) <- c("Symptoms", "Yes", "No")

# Calculate prevalence rate of the symptoms for patients with no history of COVID-19
noCovidSympt_df$prop.y <- noCovidSympt_df$Yes / 
  (noCovidSympt_df$Yes + noCovidSympt_df$No)

# create a new dataframe containing only the symptoms and proportion in both categories
prop.y <- noCovidSympt_df$prop.y
symp_prop <- cbind(hadCovidSympt_df[, c('Symptoms', 'prop.x')], prop.y)
# generate new column for % fold change in symptoms between patients that had covid-19 and those that did not have it
symp_prop$fold_change <- (symp_prop$prop.x - symp_prop$prop.y)*100

# plot a bar graph to show the fold change in the symptoms experienced by the two groups
symp_prop <- symp_prop %>%
  arrange(fold_change) %>%
  mutate(Symptoms = reorder(Symptoms, fold_change)) #arrange the symptoms in order of fold change

# plot the horizontal bar graph for symptoms and their fold change btw the two groups
# the basic horizontal bar chart
plt <- ggplot(symp_prop) +
  geom_col(aes(x=fold_change, y=Symptoms), fill = "steelblue", width = 0.9)
plt

# Customize the layout of the bar chart
plt <- plt + 
  scale_x_continuous(
    limits = c(0, 20),
    breaks = seq(0, 20, by = 5), 
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "bottom"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 8)
  )
plt
# add labels and texts
plt <- plt + 
  geom_shadowtext(
    data = subset(symp_prop, fold_change < 4),
    aes(x=fold_change, y = Symptoms, label = Symptoms),
    hjust = 0,
    nudge_x = 0.3,
    colour = "black",
    bg.colour = "white",
    bg.r = 0.2,
    family = "Econ Sans Cnd",
    size = 3
  ) + 
  geom_text(
    data = subset(symp_prop, fold_change > 4),
    aes(0, y = Symptoms, label = Symptoms),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    face = "bold",
    size = 3
  )

plt
# Add annotations
plt <- plt +
  labs(
    title = "COVID-19 Symptoms Prevalence",
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 8
    )
  )
plt

# determine the most significant symptoms for predicting COVID-19
# this will be determined using logistic regression
# first we will create a subset data that contain symptoms variables and covid-19 variable for all patients

log_data <- covid_df[, c(2, 13:30)]

# create the logistic regression model
model <- glm(CW3_COVID19 ~., family = binomial, data = log_data)

# obtain the summary of the logistic regression
summary(model)

# determine the accuracy of the model by training and testing the model
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(log_data), replace=TRUE, prob=c(0.7,0.3))
train <- log_data[sample, ]
test <- log_data[!sample, ]  

#fit logistic regression model
train_model <- glm(CW3_COVID19~., family = binomial, data = train)

# use the trained model to obtain predictions probablity on test data
test$pred_prob <- predict(train_model,test,type="response")

# transform those probabilities into successes and failures (1’s and 0’s),
# and we save them under the variable “model_pred”.
test <- test  %>% mutate(model_pred = 1*(pred_prob > .53) + 0)

# compare the newly created columns “model_pred” and “CW3_COVID19” 
# this is done to calculate the accuracy of our model.
test <- test %>% mutate(accurate = 1*(model_pred == CW3_COVID19))
sum(test$accurate, na.rm = TRUE)/nrow(test)

# model accuracy = 0.8363957

#obtain the p-values of each symptom
p_values <- coef(summary(model))[,4]

# create a dataframe for the p values of the symptoms
p_df <- as.data.frame(p_values)

# change rownames to the names of symptoms
rownames(p_df) <- c("Intercept", "Fever", "Cough_dry", "Cough_Mucus", "SoreThroat",
                       "ChestTightness", "ShortBreath", "RunnyNose", 
                       "Nasal_Congestion", "Sneezing", "MuscleBody_ache", "Fatigue", 
                       "Diarrhoea", "Vomiting", "Loss_Smell", "Loss_taste",
                       "SkinRash", "Headache", "Others")

# convert the rownames(i.e. the symptoms) to the first column
p_df <- tibble::rownames_to_column(p_df, "Symptoms")

# delete the first row of the dataframe... it contains intercept of the model
p_df <- p_df[-1,]

# identify the significant symptoms. These are symptoms with p-value less than 0.05
sig_symptoms <- filter(p_df, p_values < 0.05)
# arrange the symptoms from the most significant to the least significant
sig_symptoms <- arrange(sig_symptoms, desc(p_values))

# plot a bar chart of the significant symptoms
# Reorder the "Symptoms" variable based on descending "p_values" so as to plot bar chart of the symptoms in order of significance
sig_symptoms$Symptoms <- reorder(sig_symptoms$Symptoms, -sig_symptoms$p_values)

# Create the plot with the reordered data
plt <- ggplot(sig_symptoms) +
  geom_col(aes(x = log(p_values), y = Symptoms), fill = "blue", width = 0.9) +
  scale_y_discrete(position = "right") +
  labs(y = NULL)
plt
# Add annotations
plt <- plt +
  labs(
    title = "Significant Symptoms",
    subtitle = "COVID-19 symptoms that are significant predictors of the infection"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 10
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 8
    )
  )
plt

# does demographic factor such as sex and weight affect the pattern of symptoms?
# first, we will visualize how the pattern of symptoms may be different between males and females
# for this, we will use the dataset of patients that had covid-19 only
# divide the dataset into two for male and female

male_df <- filter(had_COVID, CW3_PSEX==1)
female_df <- filter(had_COVID, CW3_PSEX==2)

# create a data frame of frequency of male patients with covid-19 for each symptoms

table_list <- list() #Create an empty list to store the tables

# Loop through symptoms columns and create tables
for (i in c(1:8,10:14,16:20,23)) {
  column_name <- paste0("CW3_COVIDSYMPT_", i)
  table_list[[i]] <- table(male_df[[column_name]])
}

# Combine the tables of all symptoms into one using cbind
maleSympt_table <- do.call(cbind, table_list)

# Convert the combined table into a data frame 
maleSympt_df <- as.data.frame(maleSympt_table)

# rename the columns of the dataframe into symptoms respectively
colnames(maleSympt_df) <- c("Fever", "Cough_dry", "Cough_Mucus", "SoreThroat",
                               "ChestTightness", "ShortBreath", "RunnyNose", 
                               "Nasal_Congestion", "MuscleBody_ache", "Fatigue", 
                               "Diarrhoea", "Loss_Smell", "Loss_taste", "Vomiting",
                               "SkinRash", "Sneezing", "Headache", "Others",
                               "No_symptoms")

# transpose the data frame
maleSympt_df <- as.data.frame(t(maleSympt_df))
# rename the columns to contain symptom name and the yes and no labels 
maleSympt_df <- tibble::rownames_to_column(maleSympt_df, "Symptoms")
colnames(maleSympt_df) <- c("Symptoms", "Yes", "No")

# create the same dataframe for female patients to show the frequency of each symptom

table_list <- list() #Create an empty list to store the tables

# Loop through symptoms columns and create tables
for (i in c(1:8,10:14,16:20,23)) {
  column_name <- paste0("CW3_COVIDSYMPT_", i)
  table_list[[i]] <- table(female_df[[column_name]])
}

# Combine the tables of all symptoms into one using cbind
femaleSympt_table <- do.call(cbind, table_list)

# Convert the combined table into a data frame 
femaleSympt_df <- as.data.frame(femaleSympt_table)

# rename the columns of the dataframe into symptoms respectively
colnames(femaleSympt_df) <- c("Fever", "Cough_dry", "Cough_Mucus", "SoreThroat",
                            "ChestTightness", "ShortBreath", "RunnyNose", 
                            "Nasal_Congestion", "MuscleBody_ache", "Fatigue", 
                            "Diarrhoea", "Loss_Smell", "Loss_taste", "Vomiting",
                            "SkinRash", "Sneezing", "Headache", "Others",
                            "No_symptoms")

# transpose the data frame
femaleSympt_df <- as.data.frame(t(femaleSympt_df))
# rename the columns to contain symptom name and the yes and no labels 
femaleSympt_df <- tibble::rownames_to_column(femaleSympt_df, "Symptoms")
colnames(femaleSympt_df) <- c("Symptoms", "Yes", "No")

# Calculate prevalence rate of the symptoms for both male and female patients that had covid-19
maleSympt_df$prop.x <- maleSympt_df$Yes / 
  (maleSympt_df$Yes + maleSympt_df$No)

femaleSympt_df$prop.y <- femaleSympt_df$Yes / 
  (femaleSympt_df$Yes + femaleSympt_df$No)

# create a new data frame containing only the symptoms and proportion in both male and female patients
prop.y <- femaleSympt_df$prop.y
sex_sp <- cbind(maleSympt_df[, c('Symptoms', 'prop.x')], prop.y)
# generate new column for % fold change in symptoms between male and female patients that had covid-19
sex_sp$fold_change <- (sex_sp$prop.x - sex_sp$prop.y)*100

# plot a bar graph to show the fold change in the symptoms experienced by the two groups
sex_sp <- sex_sp %>%
  arrange(fold_change) %>%
  mutate(Symptoms = reorder(Symptoms, fold_change)) #arrange the symptoms in order of fold change

# plot the horizontal bar graph for symptoms and their fold change btw the two groups
# the basic horizontal bar chart
plt <- ggplot(sex_sp) +
  geom_col(aes(x=fold_change, y=Symptoms), fill = "red", width = 0.9) +
  labs(y = NULL)
plt
# Add annotations
plt <- plt +
  labs(
    title = "COVID-19 Symptoms Prevalence in Male and Female"
    ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 8
    )
  )
plt

# determine if there is significant association between sex and COVID-19 
# null hypothesis: there is no significant association between sex and COVID-19
# for this, we will carry out chi-square statistic to test the hypothesis, since the two variables are categorical

# convert sex variable to factor level
covid_df$CW3_PSEX <- as.factor(covid_df$CW3_PSEX)

# carry out the chi-square test
print(chisq.test(covid_df$CW3_PSEX, covid_df$CW3_COVID19))

# the output gives a x-square=6.58 and p-value=0.01, therefore there is significant association between sex and covid-19

# is there a significant difference in the average weight of patients that had covid-19 as compared to those without it
# this hypothesis will be tested using a t-test statistic

# obtain the weight vector of patients that had covid19 and the other group separately
hadCovid_wght <- had_COVID$CW3_WGHTKG
noCovid_wght <- no_COVID$CW3_WGHTKG

# perform Two Sample t-test statistic
t.test(hadCovid_wght, noCovid_wght)

# the p-value obtained is 0.4355 which is greater than 0.05 significant level, therefore the null hypothesis is retained 
# i.e. there is no significant difference in the weight of patients with COVID-19 and those without it


# Profiling of new conditions linked to coronavirus
# first, we check the frequency of patients that may have new condition
# convert the variable to categorical variable
had_COVID$CW3_COVNEWILL <- as.factor(had_COVID$CW3_COVNEWILL)

# count the frequency of the patients with and without new condition
new_cond <- count(had_COVID, CW3_COVNEWILL)

# plot bar chart of the distribution
new_cond %>% 
  drop_na(CW3_COVNEWILL) %>%
  ggplot(aes(x = CW3_COVNEWILL, y = n, fill = CW3_COVNEWILL)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  scale_x_discrete(labels = c('Yes', 'No')) +
  labs(y = "Frequency", x = "Has new condition") +
  scale_fill_manual(name = "New condition", values = c("blue", "red"))


# let's do the profiling of the post-COVID condition
# Loop through post-COVID condition columns and create tables

table_list <- list() #Create an empty list to store the tables
for (i in c(1:8)) {
  column_name <- paste0("CW3_COVNEWILT_", i)
  table_list[[i]] <- table(had_COVID[[column_name]])
}

# Combine the tables of all symptoms into one using cbind
newCond_table <- do.call(cbind, table_list)

# Convert the combined table into a data frame 
newCond_df <- as.data.frame(newCond_table)

# rename the column names into the name of the post-covid condition
colnames(newCond_df) <- c("Post-Viral Fatigue", "Blood clot", "Heart Condition",
                          "Lung Condition", "Brain/Mind Condition", "Nervous condition",
                          "Thyroid disease", "Others")

# transpose the data frame
newCond_df <- as.data.frame(t(newCond_df))
# rename the columns to contain the condition, and the yes and no labels 
newCond_df <- tibble::rownames_to_column(newCond_df, "Symptoms")
colnames(newCond_df) <- c("Symptoms", "Yes", "No")

# Calculate prevalence rate of the post-covid conditions in patients that had covid-19
newCond_df$prop <- newCond_df$Yes / (newCond_df$Yes + newCond_df$No)

# arrange the conditions in descending order of occurrence
newCond_df <- newCond_df %>%
  arrange(prop) %>%
  mutate(Symptoms = reorder(Symptoms, prop)) 
# the basic horizontal bar chart
plt <- ggplot(newCond_df) +
  geom_col(aes(x=prop, y=Symptoms), fill = "blue", width = 0.9) +
  labs(x="Prevalence rate", y=NULL) 
plt

# Long-Standing Illness
# first, we check the frequency of patients that may be suffering from long-standing illness
# convert the variable to categorical variable
covid_df$CW3_LLI_17 <- as.factor(covid_df$CW3_LLI_17)

# count the frequency of the patients with and without long-standing illness
longIll <- count(covid_df, CW3_LLI_17)
print(longIll)

# plot clustered column chart of the distribution of long-illness between patients with history of COVID and other group
covid_df %>%
  drop_na(CW3_LLI_17) %>%
  ggplot(aes(x=CW3_LLI_17))+
  geom_bar(fill="blue", width=0.5)+
  scale_x_discrete(labels=c('No', 'Yes'))+
  labs(y="Frequency", x="Has Long-standing Illness")

# using chi-square statistic, determine if there is significant association between long-standing illness and COVID-19
chisq.test(covid_df$CW3_COVID19, covid_df$CW3_LLI_17)
# X-squared = 3.1693, p-value = 0.075


# Multiple Correspondence Analysis (MCA) using FactoMineR package
# We will use MCA to cluster patients based on symptoms presentation
# load dataset... the dataset will be for only patients that had covid-19
data <- filter(covid_df, CW3_COVID19==1)
data2 <- data[, c(1, 13:30)]

# convert all the variables to factor level
col_names <- names(data2) # get the column names
data2[,col_names] <- lapply(data2[,col_names] , factor) # convert to factor level


# rename the column names to the corresponding symptoms to be shown on clusters
# define old and new names
old_names <- c("CW3_PSEX", "CW3_COVIDSYMPT_1", "CW3_COVIDSYMPT_2", "CW3_COVIDSYMPT_3",
               "CW3_COVIDSYMPT_4", "CW3_COVIDSYMPT_5", "CW3_COVIDSYMPT_6", 
               "CW3_COVIDSYMPT_7", "CW3_COVIDSYMPT_8", "CW3_COVIDSYMPT_18", 
               "CW3_COVIDSYMPT_10", "CW3_COVIDSYMPT_11", "CW3_COVIDSYMPT_12",
               "CW3_COVIDSYMPT_16","CW3_COVIDSYMPT_13","CW3_COVIDSYMPT_14",
               "CW3_COVIDSYMPT_17", "CW3_COVIDSYMPT_19", "CW3_COVIDSYMPT_20")
new_names <- c("Sex", "Fever", "Cough_dry", "Cough_Mucus", "SoreThroat",
               "ChestTightness", "ShortBreath", "RunnyNose", 
               "Nasal_Congestion", "Sneezing", "MuscleBody_ache", "Fatigue", 
               "Diarrhoea", "Vomiting", "Loss_Smell", "Loss_taste",
               "SkinRash", "Headache", "Others")

# rename columns using setnames from data.table 
library(data.table)
setnames(data2, old = old_names, new = new_names)

# loading FactoMineR
library(FactoMineR)

# before performing MCA, we will handle the missing data using missMDA library
library(missMDA)

# we will impute the missing values using imputeMCA function with 5 as no of components
imputed_data <- imputeMCA(data2, ncp = 5)

# perform MCA on the symptoms data
res.mca <- MCA(data2, quali.sup = 1, tab.disj = imputed_data$tab.disj)

# Extract results for variables and individuals
library(factoextra)
# variables
get_mca_var(res.mca) # obtain information about variable
head(res.mca$var$contrib) # to show result of the most contributing symptoms in each dimension

# individuals
get_mca_ind(res.mca)
head(res.mca$ind$coord) # to show the coordinate of the individuals

# Visualize variable categories contributions on axes 1; select top 15
fviz_contrib(res.mca, choice ="var", axes = 1, top = 15)
# Visualize variable categories contributions on axes 3; select top 15
fviz_contrib(res.mca, choice ="var", axes = 3, top = 15)

#Visualize individual contributions on axes 2; select the top 20
fviz_contrib(res.mca, choice ="ind", axes = 2, top = 20)

#Graph of variable categories: top 15 variables
fviz_mca_var(res.mca, axes = c(3,4), repel = TRUE, geom=c("text"), select.var = list(contrib = 15))

# Graph of individual categories;
fviz_mca_ind(res.mca, axes = c(3,4), repel = TRUE, geom=c("point"), select.ind = list(contrib=100))

# perform hierarchical clustering on the MCA output 
# this will divide the patients into clusters based on patterns of symptoms expressed
res.hcpc <- HCPC(res.mca)

#plot the tree and the map for the clusters
plot(res.hcpc, axes = c(1,2), choice = "map", ind.names = FALSE, 
     draw.tree = FALSE)
plot(res.hcpc, choice = "tree")

# characterization of the clusters
catdes(res.hcpc$data.clust, ncol(res.hcpc$data.clust))
# summary of the cluster
summary(res.hcpc$data.clust)
# description of the symptoms variables per cluster
res.hcpc$desc.var$category
# visualize the Clusters of the patients based on pattern of COVID-19 symptoms
fviz_cluster(res.hcpc, geom = "point", main = "Factor map")
# obtain the test of association of the variables of the cluster
(res.hcpc$desc.var$test.chi2)
# obtain the number of male and female patients in each gender
res.hcpc$data.clust %>% group_by(clust,Sex) %>% 
  summarise(total_count=n(),.groups = 'drop')

# MCA to cluster individuals with post-covid conditions
# the dataset to be used in this case contained the post-covid conditions to be clustered with sex as quali.suppl variable

data3 <- data[, c(1, 5:12)]

# convert all the columns to categorical variable i.e. factor level
col_names <- names(data3) # get the column names
data3[,col_names] <- lapply(data3[,col_names] , factor) # convert to factor level

# rename the column names to the corresponding post-covid condition to be shown on clusters
# define old and new names
old_names <- c("CW3_PSEX", "CW3_COVNEWILT_1", "CW3_COVNEWILT_2", "CW3_COVNEWILT_3", 
               "CW3_COVNEWILT_4", "CW3_COVNEWILT_5", "CW3_COVNEWILT_6", 
               "CW3_COVNEWILT_7", "CW3_COVNEWILT_8")
new_names <- c("Sex", "Post-Viral Fatigue", "Blood clot", "Heart Condition",
               "Lung Condition", "Brain/Mind Condition", "Nervous condition",
               "Thyroid disease", "Others")
# rename columns using setnames from data.table 
setnames(data3, old = old_names, new = new_names)

# perform MCA - first impute missing data
imputed_data <- imputeMCA(data3, ncp=5)

# obtain the MCA of the imputed data
res.mca2 <- MCA(data3,  quali.sup = 1, tab.disj = imputed_data$tab.disj)

# Visualize variable categories contributions on axes 1
fviz_contrib(res.mca2, choice ="var", axes = 1)
# Visualize variable categories contributions on axes 4
fviz_contrib(res.mca2, choice ="var", axes = 4)

# Graph of variable categories
fviz_mca_var(res.mca2, axes = c(3,4), repel = TRUE, geom=c("text"), 
             select.var = list(contrib = 12), title = NULL)
?fviz_mca_var
# Graph of individual categories;
fviz_mca_ind(res.mca2, axes = c(3,4), repel = TRUE, geom=c("point"))

# Biplot of individuals and variables
fviz_mca_biplot(res.mca2, repel = TRUE, geom.ind = c("point"), 
                geom.var = c("text"), label="var", axes = c(3,4))

#Graph of individuals
# Color by groups
# Add concentration ellipses
# Use repel = TRUE to avoid overplotting
grp <- as.factor(data3[, "Sex"])
fviz_mca_ind(res.mca2,  habillage = grp, axes = c(3,4), addEllipses = TRUE, 
             repel = TRUE, select.ind = list(contrib=50), title = "MCA- indivduals based on sex")

# perform hierarchical cluster on the post-covid condition
res.hcpc2 <- HCPC(res.mca2)

# check the summary of the clustering result
summary(res.hcpc2)

# plot the factor map of the clusters
plot(res.hcpc2, choice = "map", draw.tree = FALSE,
     title = "Clusters of Individuals with Post-Covid conditions")

# characterization of the clusters
catdes(res.hcpc$data.clust, ncol(res.hcpc$data.clust))

# summary of the cluster
summary(res.hcpc2$data.clust)
# description of the symptoms variables per cluster
res.hcpc2$desc.var$category

fviz_cluster(res.hcpc2, axes = c(3,4), geom = "point", main = "Factor map")
head(res.hcpc2$desc.var$test.chi2)
res.hcpc2$data.clust %>% group_by(clust,Sex) %>% 
  summarise(total_count=n(),.groups = 'drop')
