#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse", "ggplot2", "readxl", "xtable"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Data Manipulation
#####################

### 1

# load MEP info (EP1 sheet only)
# I'm assuming all datasets have been downloaded into Downloads folder
mep_info <- read_excel("~/Downloads/mep_info_26Jul11.xls", sheet = "EP1")

# load roll-call votes (comma-delimited text)
rcv_ep1 <- read_delim("~/Downloads/rcv_ep1.txt", delim = ",", col_names = TRUE)

### 3

# identify vote columns (starting from V1)
vote_cols <- str_subset(names(rcv_ep1), "^V\\d+$")

# pivot to long format
rcv_long <- rcv_ep1 %>%
  pivot_longer(cols = all_of(vote_cols), 
               names_to = "vote_id", 
               values_to = "decision") %>%
  mutate(vote_num = as.integer(str_remove(vote_id, "V")))

# create summary table of decision counts
xtable(rcv_long %>%
         count(decision))

### 4

# need to rename first column of mep_info for join
names(mep_info)[1] <- "MEPID"
# drop any invalid votes if present
votes_joined <- rcv_long %>%
  left_join(mep_info %>% select(MEPID, `NOM-D1`, `NOM-D2`), by = "MEPID") 

# check for missingness
# convert NOM-1 and NOM-2 to numeric
votes_joined$`NOM-D1` <- as.numeric(votes_joined$`NOM-D1`)
votes_joined$`NOM-D2` <- as.numeric(votes_joined$`NOM-D2`)

# now look at the rows for which we have NAs in "NOM-D1" & "NOM-D2"
missing_check <- votes_joined %>% filter_at(vars(`NOM-D1`, `NOM-D2`), any_vars(is.na(.)))

# look at which MEPs this is for
xtable(unique(missing_check[, c("MEPNAME", "EPG")]) %>%
         count(EPG))

### 5

# create yes or "other" variable first
votes_joined$yes_decision <- ifelse(votes_joined$decision==1, 1, 0)
# then the abstention variable
votes_joined$abstain_decision <- ifelse(votes_joined$decision==3, 1, 0)

# get the mean:
# yes across all roll-calls
# abstention across all roll-calls
# NOM-1 and NOM-2
aggregate(votes_joined[,c("yes_decision", "abstain_decision", "NOM-D1", "NOM-D2")], by=list(votes_joined$EPG), FUN=mean, na.rm=TRUE)

#####################
# Data Visualization
#####################

### 1

# again, there's going to be some non-numeric values that go to NA
mep_info$`NOM-D1` <- as.numeric(mep_info$`NOM-D1`)
mep_info$`NOM-D2` <- as.numeric(mep_info$`NOM-D2`)

pdf("NOM-D1_EPG_density.pdf")
ggplot(mep_info, aes(x=`NOM-D1`, y=`EP Group`, fill=`EP Group`)) +
  geom_density_ridges(alpha = 0.5) +
  labs(x="NOM-D1", y="") +
  scale_fill_brewer(palette="Set1") + theme_minimal()
dev.off()

pdf("NOM_EPG_scatter.pdf")
ggplot(mep_info, aes(x=`NOM-D1`, y=`NOM-D2`, color=`EP Group`)) +
  geom_point(alpha=0.6) +
  labs(x="NOM-D1", y="NOM-D2") + lims(x=c(-1,1), y=c(-1,1)) +
  scale_color_brewer(palette="Set1") + theme_minimal()
dev.off()
       
# get proportion that voted yes for each vote by EP group
prop_votes_EPG <- votes_joined %>%          
  group_by(vote_id, yes_decision, EPG) %>%
  summarise(N=n()) %>%
  group_by(vote_id, EPG) %>%
  mutate(prop_vote=N/sum(N))

# this gives us a dataframe with proporitions of each vote type, yes and no
prop_yes_EPG <- prop_votes_EPG[which(prop_votes_EPG$yes_decision==1),]
# we want to isolate the proportion of "yes" votes
# so what do we do if no one in an EPG voted yes?
# we'll start by creating an empty data frame with an observation for
# all parties for all votes filled with zeroes
no_votes <- expand.grid(vote_id = unique(prop_votes_EPG$vote_id), 
                        EPG = unique(prop_votes_EPG$EPG), 
                        prop_vote_no = 0)
# then we'll merge that with out previous dataframe
prop_yes_EPG <- merge(prop_yes_EPG[,c("vote_id", "EPG", "prop_vote")], no_votes, by=c("vote_id", "EPG"), all.y = T)
# so now we'll have an observation for EPGs that had no one that vote yes (prop_vote==1)
prop_yes_EPG$prop_vote <- ifelse(is.na(prop_yes_EPG$prop_vote), 0, prop_yes_EPG$prop_vote)

pdf("Yes_EPG_boxplot.pdf")
# plot only the yes proportions
ggplot(prop_yes_EPG[,c("vote_id", "EPG", "prop_vote")], 
       aes(x = EPG, y = prop_vote, fill = EPG)) +
  geom_boxplot() +
  labs(y = "Proportion Yes", x="") +
  scale_color_brewer(palette="Set1") + theme_minimal()
dev.off()

# do similar thing as previous problem, but now with NP
# we don't have the same issue where we want to get the proportion for each vote
prop_yes_NP <- votes_joined %>%
  group_by(yes_decision, NP) %>%
  summarise(N=n()) %>%
  group_by(NP) %>%
  mutate(prop_vote=N/sum(N))

# just need to make our NP variable a factor
prop_yes_NP$NP <- as.factor(prop_yes_NP$NP)

pdf("Yes_NP_boxplot.pdf")
# plot only the yes proportions
ggplot(prop_yes_NP[which(prop_yes_NP$yes_decision==1),], 
       aes(x = NP, y = prop_vote, fill = NP)) +
  geom_col() +
  labs(y = "Proportion Yes", x="") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
