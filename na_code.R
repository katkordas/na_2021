######### Network analysis on trust in COVID-vaccine ##########
# This code analyzes the effect of personality, demographics and belief in science on the trust in the COVID-19 vaccine.
# 
# Code is written by: 
#   Margot Steijger     11680814
#   Kat Kolodziejczyk   13277456
#   Anne Marijn Bruijn  11637234
#   Ilke de Lange 	12118311
# 
# Input:  an Excel file with the raw data from Qualtrics with identifiable information removed
#         All answers are numerical or can be converted by the functions described below
# Output: a network analysis on this data set

#### Information on dataset ####
# Q16 = age
# Q12 = gender: 1 = male, 2 = female, 3 = non-binary, 4 = prefer not to say
# Q18 = education
# Q19 = which field
# Q11 = live in NL?
# Q13 = country of origin
# Q38 = country outside of europe
# Q37_1 = hometown: 0 = rural, 10 = urban
# Q37_4 = current residence: 0 = rural, 10 = urban
# Q21 = politics: 1 = extreme left, 7 = extreme right
# Q24 = religion
# Q39 = risk
# Q40 = had covid
# Q36 = vaccinated
# Q32 = personality: 1 = strongly disagree, 5 = strongly agree, some items need to be reversed
# Q33 = choose 3 -> "3" was second option, so only keep 2
# Q1 = belief in science 1 = strongly disagree, 6 = strongly agree, no modification needed, just sum all 7 values
# Q35 = belief in vaccine 1 = strongly disagree, 6 = strongly agree, some items need to be reversed


#### Run packages and functions ####

# Run each time when opening R
library("bootnet")
library("qgraph")
library("car")
library("NetworkComparisonTest")
library("networktools")
library("psychonetrics")
library('dplyr')
library("psych")
library("readxl")

# Define functions to convert questionnaire answers to numbers
Convert_to_5num <- function(Input){
  # Input is an array with the strings: "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree
  # Output is an array with numbers 1 until 5 corresponding with these answers
  Input[Input == "Strongly agree"] <- 5
  Input[Input == "Somewhat agree"] <- 4
  Input[Input == "Neither agree nor disagree"] <- 3
  Input[Input == "Somewhat disagree"] <- 2
  Input[Input == "Strongly disagree"] <- 1
  return (as.numeric(Input))
}

Convert_to_REV_5num <- function(Input){
  # Input is an array with the strings: "Strongly agree", "Somewhat agree", "Neither agree nor disagree", "Somewhat disagree", "Strongly disagree
  # Output is an array with numbers 1 until 5 corresponding with these answers
  Input[Input == "Strongly agree"] <- 1
  Input[Input == "Somewhat agree"] <- 2
  Input[Input == "Neither agree nor disagree"] <- 3
  Input[Input == "Somewhat disagree"] <- 4
  Input[Input == "Strongly disagree"] <- 5
  return (as.numeric(Input))
}

Convert_to_6num <- function(Input){
  # Input is an array with the strings: "Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Disagree", "Strongly disagree
  # Output is an array with numbers 1 until 6 corresponding with these answers
  Input[Input == "Strongly agree"] <- 6
  Input[Input == "Agree"] <- 5
  Input[Input == "Somewhat agree"] <- 4
  Input[Input == "Somewhat disagree"] <- 3
  Input[Input == "Disagree"] <- 2
  Input[Input == "Strongly disagree"] <- 1
  return (as.numeric(Input))
}

Convert_to_REV_6num <- function(Input){
  # Input is an array with the strings: "Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Disagree", "Strongly disagree
  # Output is an array with numbers 1 until 6 corresponding with these answers
  Input[Input == "Strongly agree"] <- 1
  Input[Input == "Agree"] <- 2
  Input[Input == "Somewhat agree"] <- 3
  Input[Input == "Somewhat disagree"] <- 4
  Input[Input == "Disagree"] <- 5
  Input[Input == "Strongly disagree"] <- 6
  return (as.numeric(Input))
}

descriptive <- function(array_input){
  # Input is an array of which descriptive statistics need to be performed.
  # Array should only contain numerical values!
  # Output is an array including the mean, standard deviation, median and interquartile range
  Output = c(mean(array_input, na.rm=T), sd(array_input, na.rm=T), median(array_input, na.rm=T), IQR(array_input, na.rm=T))
  return (Output)
}

##### Read and manipulate dataframe ####
# Load dataset
raw_data <- read_excel(file.choose())

# Select all useable participants & questions
raw_data_clean<-subset(raw_data, raw_data$Q33=="3" & raw_data$Finished=="True" & raw_data$`Informed consent`=="Yes, I hereby agree to participate in the study") # Based on our inclusion criteria
n = nrow(raw_data_clean)
COVID_data <- raw_data_clean[,-c(1:2)]  # Remove the first 2 columns since this data is irrelevant for the network
COVID_data <- as.data.frame(COVID_data)
COVID_data$Q16 <- as.numeric(COVID_data$Q16)
COVID_data$Q37_1 <- as.numeric(COVID_data$Q37_1)
COVID_data$Q37_4 <- as.numeric(COVID_data$Q37_4)


# Indexes of questionnaires
# Vectors indicate column indexes that correspond to question with normal or reverse scoring and whether the question has 5 or 6 answer options 
normalidx5 = c(17,21,23,24,25)
reverseidx5 = c(16,18,19,20,22)
normalidx6 = c(27,28,29,30,31,32,33,37,38,41,42,43,44,48,49)
reverseidx6 = c(34,35,36,39,40,45,46,47)

# Go over each column and ...
for (i in 1:ncol(COVID_data)){
  # If index is in the vector representing normal/reversed scoring and 5/6 answer options, run matching function
  if (sum(normalidx5 == i) != 0) {
    COVID_data[,i] = Convert_to_5num(COVID_data[,i])
  }
  else if (sum(reverseidx5 == i) != 0) {
    COVID_data[,i] = Convert_to_REV_5num(COVID_data[,i])
  } 
  else if (sum(normalidx6 == i) != 0) {
    COVID_data[,i] = Convert_to_6num(COVID_data[,i])
  } 
  else if (sum(reverseidx6 == i) != 0) {
    COVID_data[,i] = Convert_to_REV_6num(COVID_data[,i])
  }
}

#### Calculate scores ####

# Calculate total scores per personality concept
COVID_data$Agreeableness = COVID_data$Q32_2 + COVID_data$Q32_7
COVID_data$Openness = COVID_data$Q32_5 + COVID_data$Q32_10
COVID_data$Extraversion = COVID_data$Q32_1 + COVID_data$Q32_6
COVID_data$Conscientiousness = COVID_data$Q32_3 + COVID_data$Q32_8
COVID_data$Neuroticism = COVID_data$Q32_4 + COVID_data$Q32_9

# Calculate total scores per trust/belief questionnaire
# Create variable for belief in science 
COVID_data$Belief_in_science = COVID_data$Q1_1 + COVID_data$Q1_2 + COVID_data$Q1_3 + COVID_data$Q1_4 + COVID_data$Q1_5 + COVID_data$Q1_6 + COVID_data$Q1_7

# Create variable for trust in COVID vaccine and vaccine in general
COVID_data$Trust_covvaccine = COVID_data$Q35_1 + COVID_data$Q35_2 + COVID_data$Q35_3 + COVID_data$Q35_4 + COVID_data$Q35_5 + COVID_data$Q35_7 + COVID_data$Q35_8 + COVID_data$Q35_10 + COVID_data$Q35_11 + COVID_data$Q35_12 + COVID_data$Q35_13 + COVID_data$Q35_14 + COVID_data$Q35_15

#### Estimate network ####

# Select variables for network
subset_data <- select(COVID_data,Q16,Q37_1, Q37_4, Agreeableness, Neuroticism, Openness, Belief_in_science, Trust_covvaccine)

# Estimate and plot Network
COVID_network <- estimateNetwork(subset_data,default = "EBICglasso", corMethod = "spearman")
qgraph(COVID_network$graph, details = T, layout = "spring", theme = "colorblind", color = c("#D3B199", "#D86128", "#495B64", "#ECC769"), labels = T,
       nodeNames = c("Age", "Hometown", "Current Residence", "Agreeableness", "Neuroticism", "Openness", "Belief in Science","COVID-19 vaccine trust"), details = T,
       groups = c("Demographics", "Living", "Living", "Personality", "Personality", "Personality", "Science: Belief/trust", "Science: Belief/trust"),  legend = T,
       shape = "circle", label.cex = 1.5)

# Save as pdf
pdf("COVID_network.pdf", width = 14, height = 10)
qgraph(COVID_network$graph, details = T, layout = "spring", theme = "colorblind", color = c("#D3B199", "#D86128", "#495B64", "#ECC769"), labels = T,
       nodeNames = c("Age", "Hometown", "Current Residence", "Agreeableness", "Neuroticism", "Openness", "Belief in Science","COVID-19 vaccine trust"), details = T,
       groups = c("Demographics", "Living", "Living", "Personality", "Personality", "Personality", "Science: Belief/trust", "Science: Belief/trust"),  legend = T,
       shape = "circle", label.cex = 1.5)
dev.off()

# Save as transparent png (without legend)
plot(COVID_network, details = T, layout = "spring", theme = "colorblind", color = c("#D3B199", "#D86128", "#D86128", "#495B64", "#495B64", "#495B64", "#ECC769", "#ECC769"), labels = T, details = T,
     shape = "circle", label.cex = 1.5, bg = 'transparent') 
dev.copy(png, "Network_COVID_transparent.png", width = 700, height =500, bg = 'transparent')
dev.off()

#### Bootstrap edge weights ####

# Estimate accuracy of the EBICglasso network
boot_nonparametric <- bootnet(COVID_network, nBoots = 1000, nCores = 8)

# Plot bootstrapped CI
plot(boot_nonparametric, order = "sample", labels = T)

#### Descriptive statistics ####

# Descriptive statistics on continuous data
Age = descriptive(COVID_data$Q16)
Hometown = descriptive((COVID_data$Q37_1))  
Current_Residence = descriptive((COVID_data$Q37_4))
Agreeableness = descriptive(COVID_data$Agreeableness)
Neuroticism = descriptive(COVID_data$Neuroticism)
Openess_score = descriptive(COVID_data$Openness)
Belief.in.science = descriptive(COVID_data$Belief_in_science)
Trust_covid_vaccine = descriptive(COVID_data$Trust_covvaccine)

# Descriptive statistics on categorical data
Gender = table(COVID_data$Q12)
Education = table(COVID_data$Q18)
Work_field = table(COVID_data$Q19)
NL_habitat = table(COVID_data$Q11)
Country_origin = table(COVID_data$Q13)
Politics = table(COVID_data$Q21)
Religion = table(COVID_data$Q24)
Vaccine = table(COVID_data$Q36)
