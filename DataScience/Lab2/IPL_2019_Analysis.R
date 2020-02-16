library(tabulizer)
library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(tidyr)

matches <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/Lab2/matches.csv", stringsAsFactors = FALSE)

data <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/Lab2/deliveries.csv", stringsAsFactors = FALSE)

matches <- matches[,-18]
data$wickets <- as.numeric(ifelse(data$player_dismissed =="" ,"",1))

#number of matches in the dataset
summarize(matches,no_of_matches = n())
# Output = 60

#which Team won by maximum runs
max_run <- matches[which.max(matches$win_by_runs),]
select(max_run, winner, win_by_runs)
# Sunrisers Hyderabad by 118 runs

#which Team won by maximum runs
max_run <- matches[which.max(matches$win_by_wickets),]
select(max_run, winner, win_by_wickets)
# Sunrisers Hyderabad by 9 wicket


teams <- data %>% select(batting_team)%>%
  distinct()
teams <- rename(teams, team = batting_team) 
teams
s_team <- c("RCB","CSK","SRH","KKR","DC","MI","KXIP","RR")
teams <- cbind(teams, s_team)
player_of_match <- matches%>% select(id,player_of_match,season) %>%
  distinct()
player_of_match <- rename(player_of_match, player=player_of_match)

matches$city <- as.character(matches$city)
matches$city[matches$city==""] <- "Dubai"
venue_city <- matches %>%
  select(city)%>%
  distinct()


########################## Dissmissal type and number of dismissal ###################################
dismissal <- data%>%
  left_join(matches, by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  filter(dismissal_kind!="")%>%
  group_by(season,dismissal_kind,s_team)%>%
  summarize(wickets =n())
ggplot(dismissal,aes(x=dismissal_kind,y=wickets,colour=as.factor(season), fill=as.factor(season)))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  coord_flip()+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="wickets")+
  scale_x_discrete(name="dismissal kind")+
  ggtitle("Breakdown of dismissal type ")


######################## Run scored in 1s to 7s
runs_cat <- data %>%
  left_join(matches,by=c("match_id"="id"))%>%
  left_join(teams,by=c("batting_team"="team"))%>%
  group_by(s_team,batsman_runs)%>%
  summarize(no=n(),runs=sum(total_runs))

runs_cat$batsman_runs <- as.factor(runs_cat$batsman_runs)

ggplot(runs_cat,aes(x=s_team,y=runs,colour=batsman_runs,fill=batsman_runs))+
  geom_bar(position = "stack", show.legend = TRUE, width =.6,stat="identity")+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  scale_y_continuous(name="Runs")+
  scale_x_discrete(name="Teams")+
  ggtitle("Total runs scored in 1s to 7s")



############### toss and match win
toss <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())
toss$type <- "toss"

wins <-matches%>%
  left_join(teams,by=c("winner"="team") )%>%
  select(s_team,winner)%>%
  group_by(s_team)%>%
  summarize(wins=n())
wins$type <- "wins"

toss_w <- rbind(toss,wins)
toss_w <- toss_w %>%
  group_by(s_team, type)%>%
  summarize(wins=sum(wins))
ggplot(toss_w,aes(x=s_team,y=wins,colour=type,fill=type))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss and Match Wins")+
  scale_x_discrete(name="Toss and Match winner")+
  ggtitle("Toss and Match wins by each Team")
