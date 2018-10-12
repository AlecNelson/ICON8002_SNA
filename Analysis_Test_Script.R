# Data Processing for ICON 8002
# This script serves the purpose of generating data with various input
# formats for use in developing SNA tool
# Edited on 10/10/18 - AHN

### NOTE: Run all code at once and then view output tables. Edge attribute sheets are dependent upon random name generation in Vertex Sheet.

############################
#basedirectory <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA"
basedirectory <- "/Users/BryanBozeman/Documents/GitHub/ICON8002_SNA"

#inputdata_path <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA/Data"
input_datapath <- "/Users/BryanBozeman/Documents/GitHub/ICON8002_SNA/Data"

vertex_datapath <- "vertex_test_df.csv"
edge_indiv_datapath <- "edge_indiv_test_df.csv"
edge_org_datapath <- "edge_org_test_df.csv"

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr","gtools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages
lapply(list.of.packages, require, character.only = TRUE)
########################################################
########################################################

## input data structure
## 3 data sheets (csv files), 1 for vertices, 1 for connections/edges between individuals,
## 1 for connections/edges between individuals and groups/organizations
## attributes can be numeric and characters, continuous and categorical, or open response

# The attribute data for this script is in a comma-separated-value
# (CSV) file. read.csv() loads a CSV file into a data frame
# object. In this case, we do have a header row, so we set
# header=T, which tells R that the first row of data contains
# column names.

vertex_test <- read.csv(vertex_datapath, header=T, row.names = 1)
edge_indiv_test <- read.csv(edge_indiv_datapath, header=T, row.names = 1)
edge_org_test <- read.csv(edge_org_datapath, header=T, row.names = 1)

str(vertex_test)
summary(vertex_test)
head(vertex_test)
colnames(vertex_test)

str(edge_indiv_test)
summary(edge_indiv_test)
head(edge_indiv_test)
colnames(edge_indiv_test)

str(edge_org_test)
summary(edge_org_test)
head(edge_org_test)
colnames(edge_org_test)

###########################################
################################
###############
### Generating demonstration data
###############
################################
###########################################

## setting parameters
# generated data sample size
n <- 100

# This code uses the "sample" function throughout, so various hypothesized scenarios can
# easily be constructed by altering the probabilities ("prob = ...") within each line of code

################################
###########
##### Sheet 1: Vertex attributes
###########
################################

# VERTEX ATTRIBUTE GENERATOR
# Name generator
ego.df <- randomNames(n, which.names="both",ethnicity = c(3:5),
                      name.order="last.first",name.sep=", ")
ego.df

# Profession generator based on professions and proportions from Tookes IRB (industry role)
industry.role.options <-c ("Commercial fisherman (current)", "Commercial fisherman (former)", "Commercial crabbers or dealer (current)", "Commercial crabbers or dealer (former)",
                           "Dock and fish house (current)", "Dock and fish house (former)", "Shellfish gatherer (current)", "Shellfish gatherer (former)")
profession.df <- sample(industry.role.options, n, replace=TRUE, prob = c(0.265, 0.265,.10, 0.10, 0.075, 0.075, 0.06,.06))

# Prospective survey questions and answers

# 1. In your opinion, what, if any, issues are currently impacting the fishing/shrimping industry on the
# Georgia coast? 
# Answer choices: Economic, environmental/ecological, social, political, other (Explain)
# Output: check all that apply, so binary response for each issue (1 = yes, 0 = no)

q1a.issues.economic.vq <- sample(c(0, 1), n, replace = T)
q1b.issues.environmental.vq <- sample(c(0, 1), n, replace = T)
q1c.issues.social.vq <- sample(c(0, 1), n, replace = T)
q1d.issues.political.vq <- sample(c(0, 1), n, replace = T)
q1e.issues.other.vq <- sample(c(0, 1), n, replace = T)
q1f.issues.other.txt.vq <- ifelse(q1e.issues.other.vq == 1, "Explanation", "N/A")


# 2. How satisfied are you with the current economic state (e.g., available market, pricing, transportation)
# of the shrimping/fishing industry on the Georgia coast?
# Answer choices: Very satisfied, satisfied, no opinion, unsatisfied, very unsatisfied
# Output: likert scale qualitative

satisfaction.options <- c("Very satisfied", "Satisfied", "No opinion", "Unsatisfied", "Very unsatisfied")
q2.satisfaction.opinion.vq <- sample(satisfaction.options, n, replace = T)

# 3. The outlook of the fishing/shrimping industry is positive
# Answer choices: Strongly agree, agree, neutral, disagree, strongly disagree
# Output: likert scale qualitative

agree.options <- c("Strongly agree", "Agree", "Neutral", "Disagree", "Strongly disagree")
q3.future.outlook.vq <- sample(agree.options, n, replace = T)

# 4. Your opinions or concerns as a member of the coastal community are considered in the decision-making process
#  Answer choices: Strongly agree, agree, neutral, disagree, strongly disagree
# Output: likert scale qualitative

q4.opinions.considered.vq <- sample(agree.options, n, replace = T)

# 5. Have you ever attended a meeting hosted by the Georgia Department of Natural Resources?
# Answer choices: Yes, No
# Output: qualitative binary

binary.qual.options <- c("Yes", "No")
q5.DNR.mtg.vq <- sample(binary.qual.options, n, replace = T)

# 6a. Which dock do you use?
# Answer choices: none
# Output: free response

# creating hypothetical dock list for data generation purposes
dock.options <- c("Brunswick Dock", "Savannah Dock", "Piedmont Dock", "South GA Dock", "Derrien Dock", "St. Simons Dock", "Jekyll Dock")
q6a.primary.dock.vq <- sample(dock.options, n, replace = T)

# 6b. If you use multiple docks, list them here
# Answer choices: none
# Output: free response

q6b.additional.dock.vq <- sample(c(dock.options, "N/A"), n, replace = T, p = c(.028571429, .028571429, .028571429, .028571429, .028571429, .028571429, .028571429, 0.8))

# 7. Where is your primary dock located?
# Answer choices: none
# Output: free response

#creating hypothetical dock list for data generation purposes
dock.location.options <- c("Brunswick", "Savannah", "Piedmont", "Derrien", "St. Simons", "Little St. Simons", "Jeckyll Island")
q7.dock.location.vq <- sample(dock.location.options, n, replace = T)

# 8. Primary dock location influences where you are able to fish or shrimp
# Answer choices: Strongly agree, agree, unsure, disagree, strongly disagree
# Output: likert scale qualitative

q8.dock.influence.fish.vq <- sample(agree.options, n, replace = T)

# 9a. Black gill disease in coastal shrimp has influenced where and how you shrimp
# Answer choices: Strongly agree, agree, neutral, disagree, strongly disagree
# Output: likert scale qualitative

q9a.black.gill.influence.vq <- sample(agree.options, n, replace = T)

# 9b. If so, how?
# Output: free response

q9b.black.gill.influence.expl.vq <- ifelse(q9a.black.gill.influence.vq == "Strongly agree" | q9a.black.gill.influence.vq == "Agree", "Explanation", "N/A")

# 10a. Have you observed black gill disease in your shrimp catch? 
# Answer choices: Yes, No
# Output: qualitative binary

q10a.black.gill.observed.vq <- sample(binary.qual.options, n, replace = T)

# 10b. If yes, in which of the following coastal shrimping areas have you caught shrimp with black gill disease?
# Answer choices: North of Savannah, Between Sapelo Island and Savannah, Between St. Simons Island and Sapelo Island, Between St. Simons Island and Jekyll Island,
# South of Jekyll Island
# Output: qualitative categorical

black.gill.location.options <- c("North of Savannah", "Between Sapelo Island and Savannah", "Between St. Simons Island and Sapelo Island", "Between St. Simons Island and Jekyll Island",
                                 "South of Jekyll Island")
q10b.black.gill.location.vq <- ifelse(q10a.black.gill.observed.vq == "Yes", sample(black.gill.location.options, n, replace = T), "N/A")

# 10c. If yes, approximately when did you first observe black gill in your catch (how many years ago)?
# Answer choices: none
# Output: free response, but likely continuous integer

q10c.black.gill.time.vq <- ifelse(q10a.black.gill.observed.vq == "Yes", sample(c(1:15), n, replace = T), "N/A")

# 11. Describe the current economic condition (ex. available market, pricing, transportation) of the 
# shrimping/fishing industry on the Georgia coast.
# Output: qualitative categorical likert
# Answer choices: Very good, good, fair, poor, very poor

quality.choices <- c("Very good", "Good", "Fair", "Poor", "Very poor")
q11.econ.condition.vq <- sample(quality.choices, n, replace = T)

# 12. Describe the current political climate (ex. agency involvement/governmental resources) of the shrimping/fishing
# industry on the Georgia Coast.
# Output: qualitative categorical likert
# Answer choices: Very good, good, fair, poor, very poor

q12.political.climate.vq <- sample(quality.choices, n, replace = T)

# 13. How pressing are the current environmental/ecological issues (e.g., black gill disease, changing weather/climatic conditions)
# of the shrimping/fishing industry on the Georgia coast? 
# Output: qualitative categorical likert
# Answer choices: Very pressing, pressing, moderately pressing, slightly pressing, not pressing

pressing.choices <- c("Very pressing", "Pressing", "Moderately pressing", "Slightly pressing", "Not pressing")
q13.env.eco.issues.vq <- sample(pressing.choices, n, replace = T)

########## End Vertex sheet questions. Now time to put it all together into one data frame

# tag for vertex sheet questions = ".vq"
vq<-ls(pattern = ".vq")

vq<-mixedsort(vq)

vertex.test.df <-as.data.frame(cbind(ego.df, profession.df,as.data.frame(mget(vq))))

names(vertex.test.df)[1]="ego"

write.csv(vertex.test.df,"vertex_test_df.csv")

# note that data frame elements are titled "q1..." to designate the question they pertain to.  

#################################################################

################################
###########
##### Sheet 2: Edge attributes (individual to individual)
###########
################################

# EDGE ATTRIBUTE GENERATOR

# Vertex ego list
ego.df <- as.vector(vertex.test.df$ego)

# Setting connections
max_connections = 7
alter.test.df<-data.frame()

for(i in 1:length(ego.df)){
  ego.df.rm <- ego.df[!ego.df == ego.df[i]]
  alter.i <- sample(ego.df.rm,sample(1:max_connections,1),replace = FALSE)
  alter.df.i <- cbind(rep(ego.df[i],length(alter.i)),alter.i)
  alter.test.df <- rbind(alter.test.df,alter.df.i)
}

str(alter.test.df)

names(alter.test.df)<-c("ego","alter")

# Sample size for generated data
n.edge.indiv <- length(alter.test.df$alter)

# Prospective survey questions and answers

# 1. List individuals with whom you have a working relationship
# Output: qualitative categorical free response
# Above code accounts for this question

# 2. How many years have you known this individual?
# Output: continuous integer

q2.years.known.eiq <- sample(c(1:30), n.edge.indiv, replace = T)

# 3. How many years have you worked with this individual? 
# Output: continuous integer
# Value cannot exceed years known

q3.years.worked.with.eiq <- round(runif(n.edge.indiv, min = 0, max = q2.years.known.eiq))

# 4a&b. Please describe your working relationship to this individual
# Output: qualitative categorical, include room for "Explanation" if Other

wk.relationship.options <- c("Employer", "Employee", "Colleague", "Other (please specify)")
q4a.wk.relationship.eiq <- sample(wk.relationship.options, n.edge.indiv, replace = T)
q4b.wk.relationship.other.eiq <- ifelse(q4a.wk.relationship.eiq == "Other (please specify)", "Explanation", "N/A")

# 5a&b. What is this individual's role in the Georgia coastal shrimp industry?
# Output: qualitative categorical, include room for "Explanation" if Other

ind.role.options <- c("Shrimper (captain)", "Shrimper (deck hand)", "Fisherman (professional or recreationa)", "Dock owner", "Dock employee", 
                      "Georgia Department of Natural Resources", "Georgia Sea Grant Marine Extention", "Shrimp buyer", "Retired", "Other") 
q5a.ind.role.indiv.eiq <- sample(ind.role.options, n.edge.indiv, replace = T)
q5b.ind.role.indiv.other.eiq <- ifelse(q5a.ind.role.indiv.eiq == "Other", "Explanation", "N/A")

# 6a. Are you related to this individual?
# Output: binary qualitative

q6a.related.indiv.eiq <- sample(binary.qual.options, n.edge.indiv, replace = T)

# 6b&c. If yes, how so?
# Output: free response

related.options <- c("Parent", "Child", "Spouse or partner", "Cousin", "Other (please specify)")
q6b.related.descriptor.eiq <- ifelse(q6a.related.indiv.eiq == "Yes", sample(related.options, n.edge.indiv, replace = T), "N/A")
q6c.related.descriptor.other.eiq <- ifelse(q6b.related.descriptor.eiq == "Other (please specify)", "Explain", "N/A")

# 7. Approximately how frequently do you interact with this individual in a professional, work-related context?
# Output: categorical qualitative

interaction.freq.options <- c("Daily", "A few times per week", "Weekly", "Bi-weekly (every other week)", "A few times per month", "Monthly",
                              "Less than once per month")
q7.work.interaction.freq.eiq <- sample(interaction.freq.options, n.edge.indiv, replace = T)

# 8. Please describe the nature of your work-related interactions with this individual
# Output: free response
# Example answers: "We shrimp together", "I help her unload shrimp at the dock", "I do maintenance on his boat", 
# "I hired him to help me shrimp last season"

# 9. Approximately how often do you interact with this individual outside of work (in your personal time)?
# Output: categorical qualitative

q9.personal.interaction.freq.eiq <- sample(interaction.freq.options, n.edge.indiv, replace = T)

# 10. If applicable, please describe the nature of your nonwork-related interactions with this individual
# Output: free response
# Example answers: "We live together", "I live near her (neighbor)", "We play golf together occasionally", "We go to the same church",
# "N/A"

# 11. Do you discuss work-related hardships or problems (e.g., black gill disease, low shrimp harvests, cheap import competitors,
# high price of fuel and boat maintenance, dock or regulatory complaints) with this individual?
# Output: binary qualitative

q11.hardship.discuss.eiq <- sample(binary.qual.options, n.edge.indiv, replace = T)

# 12. I have a healthy, productive, pleasant working relationship with this individual
# Output: qualitative categorical agree options

q12.positive.wk.relationship.eiq <- sample(agree.options, n.edge.indiv, replace = T)

# 13. Please rate the quality of your working relationship with this individual from 0 (extremely unproductive, unenjoyable)
# to 10 (extremely productive, enjoyable). 
# Output: numerical likert scale from 0 (bad) to 10 (good)

q13.wk.relationship.quality.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# 14. This individual contributes to the well-being and sustainability of the Georgia coastal shrimp industry
# Output: qualitative categorical with possible explanation

q14.sust.industry.contribution.eiq <- sample(agree.options, n.edge.indiv, replace = T)

# 15. Please rank your level of trust of this individual in a professional, work-related context from 0 (I do not trust this person)
# to 10 (I trust this person completely).
# Output: numerical likert scale from 0 (bad) to 10 (good)

q15.trust.level.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# 16. Please rank your willingness to interact with this individual in a professional, work-related context from 0 (I only work with this person because it is absolutely necessary)
# to 10 (I go out of my way to work with this person).
# Output: numerical likert scale from 0 (bad) to 10 (good)

q16.willingness.to.wk.with.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# 17. Rank the professonal associates you listed based on willingness to work for positive change in the Georgia shrimp industry 
# on a scale from 0 (not willing at all) to 10 (very willing). 
# Output: continuous integer scale from 0 (bad) to 10 (good)

q17.willingness.to.lead.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

########## End Edge (individual to individual) sheet questions. Now time to put it all together into one data frame

# tag for edge individual sheet questions = ".eiq"
eiq<-ls(pattern = ".eiq")

eiq<-mixedsort(eiq)

edge.indiv.test.df <-as.data.frame(cbind(alter.test.df,as.data.frame(mget(eiq))))

names(edge.indiv.test.df)[1]="ego"
names(edge.indiv.test.df)[2]="alter"

write.csv(edge.indiv.test.df,"edge_indiv_test_df.csv")

# note that data frame elements are titled "q1..." to designate the question they pertain to and 
# get around the "mget" function alphabetical ordering. Switch to roman numerals at Q10.  

#################################################################

################################
###########
##### Sheet 3: Edge attributes (individual to group/organization)
###########
################################


# EDGE ATTRIBUTE GENERATOR

# Vertex ego list
ego.df <- as.vector(vertex.test.df$ego)

# Alter group/organization list
alter.org.list <- c("Docks and fish houses", "Commercial fishermen groups", "Commercial crabber groups", "Georgia Department of Natural Resources", "Shrimp buyers", "Other")

# Setting connections
max_connections = 5
alter.org.test.df<-data.frame()

for(i in 1:length(ego.df)){
  ego.df.rm <- ego.df[!ego.df == ego.df[i]]
  alter.org.i <- sample(alter.org.list,sample(1:max_connections,1),replace = FALSE)
  alter.org.df.i <- cbind(rep(ego.df[i],length(alter.org.i)),alter.org.i)
  alter.org.test.df <- rbind(alter.org.test.df,alter.org.df.i)
}

str(alter.org.test.df)

names(alter.org.test.df)<-c("ego","organization")

# Sample size for generated data
n.edge.org <- length(alter.org.test.df$organization)

# Prospective survey questions and answers

# 1. Please list/select the groups with whom you have a working relationship or are associated but to which you do 
# not belong as a member.
# Output: categorical qualitative, answer choices need to be fleshed out
# Data associated with this question generated with for loop above.

# 1a. From your choices above, indicate the frequency of your interactions with these groups.
# Output: can be integer or qualitative categorical text choices

q1a.org.interaction.freq.eoq <- sample(interaction.freq.options, n.edge.org, replace = T)

# 2. How many years have you interacted with this group or organization in a professional context?
# Output: continuous integer

q2.years.wk.org.eoq <- sample(c(0:30), n.edge.org, replace = T)

# 3. Please describe the nature of your work-related interactions with this group/organization.
# Output: free response
# Example answers: "they buy my shrimp", "I buy fuel from them", "I do maintenance on their fleet",
# "They regulate when, where, and how I shrimp"

# 4. Do you discuss work-related hardships or problems (e.g., black gill disease, low harvests, 
# cheap import competitors, high price of fuel and boat maintenance, dock or regulatory complaints) with individuals within this group/organization?
# Output: binary qualitative

q4.discuss.hardship.org.eoq <- sample(binary.qual.options, n.edge.org, replace = T)

# 5. Do you have a healthy, productive, pleasant working relationship with this group/organization
# Output: categorical qualitative

q5.wk.relationship.org.eoq <- sample(agree.options, n.edge.org, replace = T)

# 6. Please rate the quality of your working relationship with this group/organization from 0 (extremely unproductive, unenjoyable) to 10 (extremely productive, enjoyable)
# Output: numerical likert scale from 0 (bad) to 10 (good)

q6.quality.wk.relationship.org.eoq <- sample(c(0:10), n.edge.org, replace = T)

# 7. This group/organization contributes to the well-being and sustainability of the Georgia coastal shrimp industry
# Output: qualitative categorical agree options

q7.sustainability.industry.org.eoq <- sample(agree.options, n.edge.org, replace = T)

# 8. Please rank your level of trust of this group/organization in a professional, work-related context from 0 (I do not trust this organization) to 10 (I trust this organization completely).
# Output: numerical likert scale from 0 (bad) to 10 (good)

q8.trust.level.org.eoq <- sample(c(0:10), n.edge.org, replace=T)

# 9. Please rank your willingness to interact with this group/organization in a professional, work-related context from 0 (I only work with this organization because it is absolutely necessary) 
# to 10 (I go out of my way to work with this organization).
# Output: numerical likert scale from 0 (bad) to 10 (good)

q9.willingness.wk.org.eoq <- sample(c(0:10), n.edge.org, replace=T)

# 10. How likely are you to discuss issues related to the economic condition of the Georgia coast with this group?
# Output: qualitative categorical
# Answer choices: Very likely, somewhat likely, not likely

likelihood.options <- c("Very likely", "Somewhat likely", "Not likely")
q10.econ.issues.discuss.eoq <- sample(likelihood.options, n.edge.org, replace = T)

# 11. How likely are you to discuss issues related to the political climate of the Georgia coast with this group?

q11.political.climate.discuss.eoq <- sample(likelihood.options, n.edge.org, replace = T)

# 12. Please select the group/organization to which you belong, if applicable.
# Output: qualitative categorical
# Self-sorting question

# 13. How long have you been associated/affiliated with this organization in a work-related context?
# Output: continous integer or categorical predetermined year window
# Self-sorting question

########## End Edge (individual to organization) sheet questions. Now time to put it all together into one data frame

# tag for edge individual sheet questions = ".eiq"
eoq<-ls(pattern = ".eoq")

eoq<-mixedsort(eoq)

edge.org.test.df <-as.data.frame(cbind(alter.org.test.df,as.data.frame(mget(eoq))))

names(edge.org.test.df)[1]="ego"
names(edge.org.test.df)[2]="organization"

write.csv(edge.org.test.df,"edge_org_test_df.csv")

#### NOTE: need to run all code at once because Sheets 2 and 3 (edge attribute sheets) are dependent on random name generation in vertex sheet

## End run

