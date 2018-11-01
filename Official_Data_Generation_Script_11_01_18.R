# Data Processing for ICON 8002
# This script serves the purpose of generating data with various input
# formats for use in developing SNA tool
# Edited on 11/01/18 by BBB

########## THIS IS THE OFFICIAL DATA GENERATION SCRIPT FOR ICON 8002 SNA FALL 2018 PROJECT

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

########################################################
########################################################

## setting parameters
# generated data sample size
n = 100
# Setting connections
mean_connections = 5

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr","plyr","RColorBrewer","keyplayer","sna","MASS","naturalsort")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages
lapply(list.of.packages, require, character.only = TRUE)

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
### This code uses the questions and logical output from the Survey Team's 
### prospective SNA survey questions, found in "Survey_Questions_V3" doc on 
### ICON 8002 google drive. - BBB, 10/30/18
###############
################################
###########################################

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

# Prospective survey questions and answers as designated in "Survey_Questions_V3" google drive doc. in ICON 8002 Google Drive.

# Question 1: What is your occupation in/relationship to Georgia coastal fisheries?
# Profession generator based on professions and proportions from Tookes IRB (industry role)
industry.role.options <-c ("Commercial fisherman", "Commercial crabbers or dealer",
                           "Dock and fish house", "Shellfish gatherer")
q1.profession.df.vq <- sample(industry.role.options, n, replace=TRUE, prob = c(0.53,.20, 0.15, 0.12))

# Question 2a: Are you currently working in the above industry?
# This is question 2 in "Survey_Questions_V3" google drive doc.
binary.qual.options <- c("Yes", "No")
q2a.currently.working.vq <- sample(binary.qual.options, n, replace = T)

# Question 2b: If yes, how long?
# This is question 2a in "Survey_Questions_V3" google drive doc. 
q2b.how.long.work.vq <- ifelse(q2a.currently.working.vq == "Yes", sample(c(0:40)), "N/A")

# Question 2c: If no, when and why did you leave this industry?
# This is question 2b in "Survey_Questions_V3" google drive doc.
q2c.when.leave.work.vq <- ifelse(q2a.currently.working.vq == "No", "Open Response", "N/A")

# Question 3: How would you rate the current economic conditions (e.g., available market, pricing, transportation)
# of Georgia coastal fisheries? 
# This is question 9 in "Survey_Questions_V3" google drive doc.
good.poor.likert <- c("Very good", "Good", "Fair", "Poor", "Very Poor")
q3.rate.econ.conditions.vq <- sample(good.poor.likert, n, replace = T)

# Question 4: In your opinion, how pressing are current environmental issues (e.g., shrimp black gill disease,
# changing weather/climactic conditions) for Georgia coastal fisheries? This is question 10 in "Survey_Questions_V3" google drive doc.
pressing.likert <- c("Very pressing", "Pressing", "Moderately pressing", "Slightly pressing", "Not pressing")
q4.rate.env.conditions.vq <- sample(pressing.likert, n, replace = T)

# Question 5: How would you rate current communication between government agencies and individual fishers within Georgia coastal fisheries?
# This is question 11 in "Survey_Questions_V3" google drive doc.
q5.rate.comm.agencies.vq <- sample(good.poor.likert, n, replace = T)

# Question 6a: Have you ever attended a meeting hosted by the Georgia Department of Natural Resources?
# This is question 12 in "Survey_Questions_V3" google drive doc. 
q6a.attended.GADNR.mtg.vq <- sample(binary.qual.options, n, replace = T)

# Question 6b: If yes, please briefly explain what the meeting(s) were about.
# This is question 13 in "Survey_Questions_V3" google drive doc.
q6b.GADNR.mtg.topic.vq <- ifelse(q6a.attended.GADNR.mtg.vq == "Yes", "Open Response", "N/A")

# Question 6c: If no, please briefly explain why.
# This is question 14 in "Survey_Questions_V3" google drive doc.
q6c.GADNR.why.not.mtg.vq <- ifelse(q6a.attended.GADNR.mtg.vq == "No", "Open Response", "N/A")

# Question 7a: Please provide the name/location of your primary dock within Georgia coastal fisheries.
# This is question 15 in the "Survey_Questions_V3" google drive doc. 
# This could also be an edge question.
dock.options <- c("Brunswick Dock", "Savannah Dock", "Piedmont Dock", "South GA Dock", "Derrien Dock", "St. Simons Dock", "Jekyll Dock")
q7a.primary.dock.vq <- sample(dock.options, n, replace = T)

# Question 7b: Do you have an active professional relationship with this dock?
# This is question 16 in the "Survey_Questions_V3" google drive doc. 
# This could also be an edge question.
q7b.active.dock.relationship.vq <- sample(binary.qual.options, n, replace = T)

# Question 7c: If yes, how long have you worked with this dock?
# This is question 16a in the "Survey_Questions_V3" google drive doc. 
# This could also be an edge question.
q7c.time.worked.w.dock.vq <- ifelse(q7b.active.dock.relationship.vq == "Yes", sample(0:25), "N/A")

# Question 7d: If no, when did you terminate this relationship?
# This is question 16b in the "Survey_Questions_V3" google drive doc. 
# This could also be an edge question.
q7d.ended.dock.relationship.vq <- ifelse(q7b.active.dock.relationship.vq == "No", sample(0:25), "N/A")

# Question 8a: Are there particular fishing grounds where you have observed significant decline in catch/harvest?
# This is question 20 in "Survey_Questions_V3" google drive doc.
q8a.observed.decline.harvest.vq <- sample(binary.qual.options, n, replace = T)

# Question 8b: If yes, please indicate location on below map.
# This is question 20a in "Survey_Questions_V3" google drive doc.
# Listing hypothetical locations for data generation purposes.
harvest.decline.location.options <- c("North of Savannah", "Between Sapelo Island and Savannah", "Between St. Simons Island and Sapelo Island", "Between St. Simons Island and Jekyll Island",
                                 "South of Jekyll Island")
q8b.harvest.decline.location.vq <- ifelse(q8a.observed.decline.harvest.vq == "Yes", sample(harvest.decline.location.options), "N/A")

# Question 8c: If yes, do you still fish these areas?
# This is question 20b in "Survey_Questions_V3" google drive doc.
q8c.still.fish.decline.locations.vq <- ifelse(q8a.observed.decline.harvest.vq == "Yes", sample(binary.qual.options), "N/A")

# Question 9a: Do you catch/harvest multiple species (i.e., shrimp, fish, crab)?
# This is question 22 in "Survey_Questions_V3" google drive doc.
q9a.harvest.multiple.sp.vq <- sample(binary.qual.options, n, replace = T)

# Question 9b: If yes, do you sell all types of catch/harvest to a single dock?
# This is question 22a in "Survey_Questions_V3" google drive doc.
q9b.sell.to.single.dock.vq <- ifelse(q9a.harvest.multiple.sp.vq == "Yes", sample(binary.qual.options), "N/A")

# Question 9c: If yes to 9b, how much do you generally earn per catch/harvest species?
# This is question 22b in "Survey_Questions_V3" google drive doc.
q9c.earned.per.species.vq <- ifelse(q9b.sell.to.single.dock.vq == "Yes", "Open response", "N/A")

# Question 9d: If no to 9b, please explain why.
# This is question 22c in "Survey_Questions_V3" google drive doc.
q9d.why.sell.multiple.docks.vq <- ifelse(q9b.sell.to.single.dock.vq == "No", "Open response", "N/A")

# Question 10: Are you aware of any dock owners who have difficulty selling products to land-based dealers?
# This is question 27 in "Survey_Questions_V3" google drive doc.
# This question could also be folded in as a subquestion for edge/relationships
q10.docks.difficulty.selling.vq <- sample(binary.qual.options, n, replace = T)

# Question 11a: Do you regularly change which docks you sell your catch/harvest to?
# This is question 28 in "Survey_Questions_V3" google drive doc.
q11a.change.docks.vq <- sample(binary.qual.options, n, replace = T)

# Question 11b: Please explain why
q11b.change.docks.explain.vq <- rep("Open response", n)

# Question 11c: If yes, please indicate how often.
# This is question 28c in "Survey_Questions_V3" google drive doc.
change.docks.freq.options <- c("Very often (change dock 1 or more times/month)", "Often (change dock 1 time/3 months)", "Occasionally (change dock about 1 time/6 months",
                               "Rarely (change dock 1 time per 9 months", "Very rarely (change dock less than 1 time/year")
q11c.change.docks.freq.vq <- sample(change.docks.freq.options, n, replace = T)

# Question 12a: Have you experienced a decline in fishery catch/harvest? 
# This is question 29 in "Survey_Questions_V3" google drive doc.
q12a.decline.in.catch.vq <- sample(binary.qual.options, n, replace =T)

# Question 12b: If yes, please briefly explain why you think this decline has occurred.
# This is question 29a in "Survey_Questions_V3" google drive doc. I reworded it slightly.
q12b.explain.decline.vq <- ifelse(q12a.decline.in.catch.vq == "Yes", "Harvest decline explanation", "N/A")

# Question 12c: If yes, which species are declining the most?
# This is question 29b in "Survey_Questions_V3" google drive doc.
q12c.which.spp.decline.vq <- ifelse(q12a.decline.in.catch.vq == "Yes", "Declining species", "N/A")

# Question 12d: If yes, please indicate the severity of the decline.
# This is question 29c in "Survey_Questions_V3" google drive doc. 
severity.options <- c("Very severe", "Severe", "Neither", "Not severe", "Very not severe")
q12d.severity.decline.vq <- ifelse(q12a.decline.in.catch.vq == "Yes", sample(severity.options), "N/A")

# Question 13a: Did the decline in catch/harvest affect your business?
# This is question 30 in "Survey_Questions_V3" google drive doc.
q13a.decline.affect.business.vq <- sample(binary.qual.options, n, replace = T)

# Question 13b: If yes, please indicate the severity of the effect on your business.
# This is question 30a in "Survey_Questions_V3" google drive doc.
q13b.severity.decline.business.vq <- ifelse(q13a.decline.affect.business.vq == "Yes", sample(severity.options), "N/A")

# Question 14: Which of the following agency meeting(s) do you attend?
# This is question 31 in "Survey_Questions_V3" google drive doc. 
# Figure out how to allow ego to select multiple agencies/organizations. 
agency.options <- c("Georgia Sea Grant", "Georgia Department of Natural Resources", "Georgia Shrimping Association (Wild Georgia Shrimp)", 
                    "Southern Shrimp Alliance", "Other (please insert)")
q14.agency.mtg.attend.vq <- sample(agency.options, n, replace = T)

# Question 15a: Are you aware of docks/fishers who are/were in contact with Georgia Department of Natural Resources,
# Sea Grant, or other governmental agencies? 
# This is question 32 in "Survey_Questions_V3" google drive doc. 
q15.folks.contact.gvmt.vq <- sample(binary.qual.options, n, replace = T)

# Question 15b: If yes, please name the docks/fisher
# This is question 32a in "Survey_Questions_V3" google drive doc.
# This also is a possible edge question
q15b.name.folks.contact.gvmt.vq <- ifelse(q15.folks.contact.gvmt.vq == "Yes", "Name of dock/fisher", "N/A")

# Question 15c: If yes, please also name agency or agencies and briefly explain any reasons for contact.
# This is question 32b in "Survey_Questions_V3" google drive doc.
# This also is a possible edge question
q15c.name.agencies.contacted.vq <- ifelse(q15.folks.contact.gvmt.vq == "Yes", sample(agency.options), "N/A")
q15d.reasons.agency.contact.vq <- rep("Reasons for contacting agency", n)

# Question 16a: If you have attended meeting(s) organized by governmental agencies, has your attendance been beneficial to your fishing practice?
# Please explain your answer below
# This is question 33 in "Survey_Questions_V3" google drive doc.
q16a.attendance.mtg.beneficial.vq <- sample(binary.qual.options, n, replace = T)
q16b.attendance.mtg.expl.vq <- ifelse(q16a.attendance.mtg.beneficial.vq == "Yes", "Benefit from attendance explained", "N/A")

# Question 17: Which of the following agencies is helpful in solving the Georgia coastal fishery problems?
# This is question 34 in "Survey_Questions_V3" google drive doc.
# This also is a possible edge question
# Figure out how to allow ego to select multiple agencies
q17.agencies.helpful.vq <- sample(agency.options, n, replace = T)

# Question 18: Which of the following agencies do you think is not helpful in solving the Georgia coastal fishery problems?
# This is question 35 in "Survey_Questions_V3" google drive doc.
# This also is a possible edge question
# Figure out how to allow ego to select multiple agencies and how to prevent ego from selecting same agency listed in above question.
q18.agencies.not.helpful.vq <- sample(agency.options, n, replace = T)

# Question 19: What kind of problems do you think organizations/agencies can help solve?
# This is question 35a in "Survey_Questions_V3" google drive doc.
q19.probs.agencies.could.solve.vq <- rep("Problems agencies could help solve", n)

# Question 20: Do you think organizations/agencies should do more to help solve the Georgia coastal fishery problems?
# If yes, what?
# This is question 35b in "Survey_Questions_V3" google drive doc.
q20a.should.orgs.do.more.vq <- sample(binary.qual.options, n, replace = T)
q20b.how.should.orgs.help.vq <- ifelse(q20a.should.orgs.do.more.vq == "Yes", "Help suggestions", "N/A")

# Question 21: How likely will the following change to harvest restrictions impact where you shrimp: Open state waters for shrimping prior to May 15
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
# This question is split into many subpieces, but I've separated them into separate questions for data generation purposes
likely.options <- c("Very likely", "Likely", "Neither likely nor unlikely", "Unlikely", "Very unlikely")
q21.reg.change.open.water.May15.vq <- sample(likely.options, n, replace = T)

# Question 22: How likely will the following change to harvest restrictions impact where you shrimp: Close state waters to shrimping past December 31st
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
q22.reg.change.close.water.Dec31.vq <- sample(likely.options, n, replace = T)

# Question 23: How likely will the following change to harvest restrictions impact where you shrimp: Open sounds to shrimping
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
q23.reg.change.open.sounds.vq <- sample(likely.options, n, replace = T)

# Question 24: To what degree has Black Gill Disease in shrimp changed where and how you fish?
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
significantly.options <- c("Very significantly", "Significantly", "Neither significant or insignificant", "Insignificantly", "Very insignificantly")
q24.blackgill.change.fishing.vq <- sample(significantly.options, n, replace = T)

# Question 25a: Is black gill disease in shrimp a concern of others who use the same dock as you?
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
q25a.blackgill.concern.dockmates.vq <- sample(binary.qual.options, n, replace = T)

# Question 25b: If yes, how strong is this concern?
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
# I have slightly reworded this so that it makes more sense. 
strength.options <- c("Very strong", "Somewhat strong", "Not very strong")
q25b.blackgill.concern.dockmates.strength.vq <- ifelse(q25a.blackgill.concern.dockmates.vq == "Yes", sample(strength.options), "N/A")

# Question 26a: Have you observed black gill disease in your shrimp harvests? 
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
q26a.seen.blackgill.vq <- sample(binary.qual.options, n, replace = T)

# Question 26b: If yes, in which of the following coastal areas have you observed black gill?
# This is underneath the "Other" category in the "Survey_Questions_V3" google drive doc.
q26b.where.seen.blackgill.vq <- ifelse(q26a.seen.blackgill.vq == "Yes", sample(harvest.decline.location.options), "N/A")


### Put all questions and responses together into single Vertex information data frame.
# tag for vertex sheet questions = ".vq"
vq<-ls(pattern = ".vq")
# sort questions in order written
vq<-mixedsort(vq)
# combine egos and answers to questions
vertex.test.df <-as.data.frame(cbind(ego.df, as.data.frame(mget(vq))))
# create column 1 name in dataframe as "ego"
names(vertex.test.df)[1]="ego"
# check that generated dataframe looks correct
View(vertex.test.df)

##### Now add alters to vertex data sheet that are mentioned by egos in the edge sheet, but are not themselves interviewed
##### All question responses for these additional out-of-network egos will be "N/A"

# EDGE ATTRIBUTE GENERATOR

# Vertex ego list
ego.df <- as.vector(vertex.test.df$ego)

# Setting connections
alter.test.df<-data.frame()
conn_types<-c("In-Network","Out-of-Network","Shared-Profession")

alter.outnetwork.num <- 100
alter.outnetwork.df <- randomNames(alter.outnetwork.num, which.names="both",ethnicity = c(1:2),name.order="last.first",name.sep=", ")
alter.outnetwork.df <- alter.outnetwork.df[!alter.outnetwork.df %in% ego.df]

#Add probability of ego listing other ego that listed them!!! Reciprocity

for(i in 1:length(ego.df)){
  ego.df.rm <- ego.df[!ego.df == ego.df[i]]
  #alter.outnetwork.df
  professional.names<-ego.df[which(vertex.test.df[,2]==as.character(vertex.test.df[i,2]))]
  professional.names.rm<- professional.names[!professional.names == ego.df[i]]
  #names.sample.list<-c(ego.df.rm,alter.outnetwork.df,professional.names.rm)
  
  sample_connections<-rnegbin(1,mean_connections, theta = 10)
  sample_prob<-sample(conn_types, sample_connections, replace = T, p = c(0.1,0.1,0.8))
  
  sample.In_Network<-length(which(sample_prob == conn_types[1]))
  sample.Out_Network<-length(which(sample_prob == conn_types[2]))
  sample.Profess_Link<-length(which(sample_prob == conn_types[3]))
  
  alter.innetwork.i <- sample(ego.df.rm,sample.In_Network,replace = FALSE)
  alter.outnetwork.i <- sample(alter.outnetwork.df,sample.Out_Network,replace = FALSE)
  alter.profession.i <- sample(professional.names.rm,sample.Profess_Link,replace = FALSE)
  
  alter.i<-c(alter.innetwork.i,alter.outnetwork.i,alter.profession.i)
  alter.type<-c(rep("In-Network",sample.In_Network),rep("Out-Network",sample.Out_Network),rep("Shared-Profession",sample.Profess_Link))
  
  if(length(which(duplicated(alter.i)==TRUE))>0){
    
    Rm.num<-which(duplicated(alter.i)==TRUE)
    alter.i.check<-alter.i[-c(Rm.num)]
    alter.type.check<-alter.type[-c(Rm.num)]
    print(paste0("Removed duplicate elemements # ",Rm.num," from ", ego.df[i]," (ego number ",i," out of ",length(ego.df),")"))
  }else{
    alter.i.check<-alter.i
    alter.type.check<-alter.type
  }
  
  alter.df.i <- cbind(rep(ego.df[i],length(alter.i.check)),alter.i.check,alter.type.check)
  alter.test.df <- rbind(alter.test.df,alter.df.i)
  
}

names(alter.test.df)<-c("ego","alter","alter_type")
View(alter.test.df)

#################################################################
#Only once new alter verticies added to dataframe is vertex.test exported

# tag for vertex sheet questions = ".vq"
vq <- ls(pattern = ".vq")
vq <- naturalsort(vq)
vertex.test.df <- as.data.frame(cbind(ego.df, as.data.frame(mget(vq))))
names(vertex.test.df)[1]="ego"

Out_Network_Names<-unique(alter.test.df[which(alter.test.df$alter_type=="Out-Network"),2])
for(k in 1:length(Out_Network_Names)){
  Name.k<-as.character(Out_Network_Names[k])
  Out_Network<-as.character("Out-Network")
  Out.row.k<-c(Name.k,Out_Network,rep("N/A",(length(colnames(vertex.test.df))-2)))
  Out.row.k<-as.data.frame(t(Out.row.k))
  colnames(Out.row.k)<-colnames(vertex.test.df)
  vertex.test.df<-rbind(vertex.test.df,Out.row.k)
}

#### Check to see that vertex dataframe looks correct with added out-network egos
View(vertex.test.df)

#################################################################
##
# Write out official vertex data frame for use in analysis
##
#################################################################
filename.vertex<-paste0("vertex_test_df_",
                        format(Sys.time(), "%m_%d_%y"),".csv")

write.csv(vertex.test.df,filename.vertex)

#################################################################
#################################################################
######### End run for vertex data sheet generation. 
#################################################################
#################################################################


################################
###########
##### Sheet 2: Edge attributes (individual to individual)
###########
################################

# EDGE ATTRIBUTE GENERATOR

# Sample size for generated data
n.edge.indiv <- length(alter.test.df$alter)

# Prospective survey questions and answers

## Individuals listed by egos are generated by above for loop.

# Question 1: How many years have you known this individual?
q1.years.known.eiq <- sample(c(1:30), n.edge.indiv, replace = T)

# Question 2: How many years have you had a professional relationship with this individual? 
# Value cannot exceed years known
q2.years.worked.with.eiq <- round(runif(n.edge.indiv, min = 0, max = q1.years.known.eiq))

# Question 3: Please describe your professional relationship with this individual
# Removed "Colleague" as an option for work relationship and manually added it where alter.type = "Shared-Profession"
wk.relationship.options <- c("Employer", "Employee", "Other (please specify)")
q3a.wk.relationship.eiq <- ifelse(alter.test.df$alter_type == "Shared-Profession", "Colleague", sample(wk.relationship.options))
q3b.wk.relationship.other.eiq <- ifelse(q3a.wk.relationship.eiq == "Other (please specify)", "Professional relationship explanation", "N/A")

# Question 4a: Approximately how frequently do you interact with this person in a professional context?
# This is question 3c in "Survey_Questions_V3" google drive doc.
# I have slightly altered the answer options
interaction.freq.options <- c("Daily", "A few times per week", "Weekly", "Bi-weekly (every other week)", "A few times per month", "Monthly",
                              "Less than once per month")
q4a.work.interaction.freq.eiq <- sample(interaction.freq.options, n.edge.indiv, replace = T)

# Question 4b: Approximately how often do you interact with this individual outside of work (in your personal time)?
q4b.personal.interaction.freq.eiq <- sample(interaction.freq.options, n.edge.indiv, replace = T)

# Question 5a: On a scale of 0 - 10 (0 = extremely unlikely, 10 = certain), how likely are you to discuss current environmental issues
# with this individual?
# This is question 4 in the "Survey_Questions_V3" google drive doc. 
# I have reframed the question so that we don't have to continually sample from previously identified alters.
q5a.likely.discuss.env.issues.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 5b: Please explain your reasoning
q5b.likely.discuss.env.issues.exp.eiq <- rep("Explain why (not) discuss environmental issues", n.edge.indiv)

# Question 6a: On a scale of 0 - 10 (0 = extremely unlikely, 10 = certain), how likely are you to discuss current economic issues
# with this individual?
# This is question 5 in the "Survey_Questions_V3" google drive doc. 
# I have reframed the question so that we don't have to continually sample from previously identified alters.
q6a.likely.discuss.econ.issues.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 6b: Please explain your reasoning
q6b.likely.discuss.econ.issues.exp.eiq <- rep("Explain why (not) discuss economic issues", n.edge.indiv)

# Question 7a: On a scale of 0 - 10 (0 = extremely unlikely, 10 = certain), how likely are you to discuss issues regarding communication
# between Georgia coastal fishery stakeholders/groups with this individual?
# This is question 6 in the "Survey_Questions_V3" google drive doc. 
# I have reframed the question so that we don't have to continually sample from previously identified alters.
q7a.likely.discuss.comm.issues.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 7b: Please explain your reasoning
q7b.likely.discuss.comm.issues.exp.eiq <- rep("Explain why (not) discuss communication issues", n.edge.indiv)

# Question 8a: On a scale of 0 - 10 (0 = extremely unlikely, 10 = certain), how likely are you to discuss matters related to the availability of 
# governmental resources within Georgia coastal fisheries with this individual?
# This is question 7 in the "Survey_Questions_V3" google drive doc. 
# I have reframed the question so that we don't have to continually sample from previously identified alters.
q8a.likely.discuss.gvmtresources.issues.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 8b: Please explain your reasoning
q8b.likely.discuss.gvmtresources.issues.exp.eiq <- rep("Explain why (not) discuss government resource availability issues", n.edge.indiv)

# Question 9a: On a scale of 0 - 10 (0 = extremely unlikely, 10 = certain), how likely are you to discuss matters related to the involvement of federal agencies (e.g., GA DNR)
# in Georgia coastal fisheries with this individual?
# This is question 8 in the "Survey_Questions_V3" google drive doc. 
# I have reframed the question so that we don't have to continually sample from previously identified alters.
q9a.likely.discuss.fedagency.issues.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 9b: Please explain your reasoning
q9b.likely.discuss.fedagency.issues.exp.eiq <- rep("Explain why (not) discuss federal agency issues", n.edge.indiv)

# Question 10: Do you know if this individual has had difficulty selling their catch/harvest?
# This is question 26 in the "Survey_Questions_V3" google drive doc.
q10.difficulty.selling.catch.eiq <- sample(binary.qual.options, n.edge.indiv, replace = T)

# Question 11: Is this individual in contact with Georgia DNR, Georgia Sea Grant, or other governmental agencies?
# This is question 32 in the "Survey_Questions_V3" google drive doc.
q11.indiv.contact.agency.eiq <- sample(binary.qual.options, n.edge.indiv, replace = T)

# Question 12a: Are you related to this individual?
q12a.related.indiv.eiq <- sample(binary.qual.options, n.edge.indiv, replace = T)

# Question 12b. If yes, how so?
related.options <- c("Parent", "Child", "Spouse or partner", "Cousin", "Other (please specify)")
q12b.related.descriptor.eiq <- ifelse(q12a.related.indiv.eiq == "Yes", sample(related.options, n.edge.indiv, replace = T), "N/A")
q12c.related.descriptor.other.eiq <- ifelse(q12b.related.descriptor.eiq == "Other (please specify)", "Explain", "N/A")

# Question 13. Please indicate the degree to which you agree with this statement: I have a healthy, productive, pleasant working relationship with this individual
agree.options <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")
q13.positive.wk.relationship.eiq <- sample(agree.options, n.edge.indiv, replace = T)

# Question 14: Please rate the quality of your working relationship with this individual from 0 (extremely unproductive, unenjoyable)
q14.wk.relationship.quality.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 15: Please indicate the degree to which you agree with this statement: This individual contributes to the well-being and sustainability of the Georgia coastal shrimp industry
q15.sust.industry.contribution.eiq <- sample(agree.options, n.edge.indiv, replace = T)

# Question 16: Please rank your level of trust of this individual in a professional, work-related context from 0 (I do not trust this person)
# to 10 (I trust this person completely).
q16.trust.level.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

# Question 17: Please rank your willingness to interact with this individual in a professional, work-related context from 0 (I only work with this person because it is absolutely necessary)
# to 10 (I go out of my way to work with this person).
q17.willingness.to.wk.with.eiq <- sample(c(0:10), n.edge.indiv, replace = T)

########## End Edge (individual to individual) sheet questions. Now time to put it all together into one data frame
# Tag for edge individual sheet questions = ".eiq"
eiq <- ls(pattern = ".eiq")
# Sort questions in order written
eiq <- naturalsort(eiq)
# Combine questions and answers with alter.test.df egos and alters
edge.indiv.test.df <-as.data.frame(cbind(alter.test.df,as.data.frame(mget(eiq))))
# Create column names for egos and alters
names(edge.indiv.test.df)[1]="ego"
names(edge.indiv.test.df)[2]="alter"
# Check that generated dataframe looks correct
View(edge.indiv.test.df)


#################################################################
##
# Write out official edge-individual data frame for use in analysis
##
#################################################################
filename.edge.individual<-paste0("edge_individual_test_df_",
                        format(Sys.time(), "%m_%d_%y"),".csv")

write.csv(edge.indiv.test.df,filename.edge.individual)



###### End run here on 11.1.18


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

# 10. Please select the group/organization to which you belong, if applicable.
# Output: qualitative categorical
# Self-sorting question

# 11. How long have you been associated/affiliated with this organization in a work-related context?
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

