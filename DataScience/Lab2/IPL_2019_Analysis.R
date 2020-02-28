library(tabulizer)
library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(tidyr)

####   READING MATCHES CSV FILE #########
matches <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/Lab2/matches.csv", stringsAsFactors = FALSE)

data <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/Lab2/deliveries.csv", stringsAsFactors = FALSE)
View(data)
matches <- matches[,-18]
data$wickets <- as.numeric(ifelse(data$player_dismissed =="" ,"",1))

########number of matches in the dataset
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

########################### Teams and matches won
matches%>%
  group_by(winner)%>%
  summarize(most_win = n())%>%
  ggplot(aes(x = winner,y = most_win,fill = winner))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous("Matches won")


teams <- data %>% select(batting_team)%>%
  distinct()
teams <- rename(teams, team = batting_team) 
teams
s_team <- c("RCB","CSK","SRH","KKR","DC","MI","KXIP","RR")
s_team
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


################## toss decision of toss winner
wins_1 <- matches%>%
  left_join(teams,by=c("toss_winner"="team") )%>%
  select(s_team,toss_winner,toss_decision)%>%
  group_by(s_team,toss_decision)%>%
  summarize(wins=n())


ggplot(wins_1,aes(x=s_team,y=wins,colour=toss_decision,fill=toss_decision))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="right")+
  scale_y_continuous(name="Toss decision")+
  scale_x_discrete(name="Toss winners and toss decisions")+
  ggtitle("Toss decisions by each Team")

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

######################### city with most number of match
venue_c <- data%>%
  left_join(matches,by=c("match_id"="id"))%>%
  select(match_id,city,total_runs,wickets)%>%
  group_by(city)%>%
  summarize(runs=sum(total_runs),wickets=sum(wickets,na.rm=TRUE))

city_mat <- matches %>%
  group_by(city)%>%
  summarize(matches=n())

venue_c <- venue_c %>%
  left_join(city_mat, by=c("city"="city"))%>%
  mutate(Avg_runs=runs/matches)%>%
  mutate(Avg_wkt =wickets/matches)%>%
  arrange(city)

venue_all <- venue_c%>%
  left_join(venue_city, by=c("city"="city"))%>%
  arrange(Avg_runs)
venue_all$city <- factor(venue_all$city, levels = venue_all$city[order(venue_all$matches)])

ggplot(venue_all,aes(x=city,y=matches,colour=city,fill=city))+
  geom_bar(position = "dodge",stat = "identity")+
  theme(legend.position="none")+
  coord_flip()+
  scale_y_continuous(name="Total no of Matches in each city")+
  scale_x_discrete(name="Cities ")+
  ggtitle("Cities with most no of matches")


############### READIG FILE ###################################

most_runs <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/Lab2/batting_stats.csv")
head(most_runs)

############## Ordering According to Priority ################
a <- most_runs[order(-most_runs$RUNS),]
a
b <- a[order(-most_runs$AVG),]
b
c <- b[order(most_runs$INN),]
c
d <- c[order(-most_runs$SR),]
d
e <- d[order(-most_runs$X4S),]
e

############# Player who topped the list ###################
select(head(e, n=1), PLAYER)

########### Player who scored maximum runs #################
max_run <- e[which.max(e$RUNS),]
select(max_run, PLAYER)

########### Player who've highest Strike Rate ##############
max_sr <- e[which.max(e$SR),]
select(max_sr, PLAYER)

############# Player who hit highest 4rs ###################
max_fours <- e[which.max(e$X4S),]
select(max_fours, PLAYER)

############# Player who've highest Average ###################
max_avg <- e[which.max(e$AVG),]
select(max_avg, PLAYER)

############# Player who hit highest Sixes ###################
max_sixes <- e[which.max(e$X6S),]
select(max_sixes, PLAYER)

############# Player who played minimum match ###############
min_match <- e[which.min(e$MATCHES),]
select(min_match, PLAYER)

############# Top ten player's name in my list ###################
select(head(e, n=10), PLAYER)

########## Top ten player's with their data in my list ###########
select(head(e, n=10), PLAYER,INN, RUNS, AVG, SR ,X4S, X6S )


############### READIG FILE ###################################

bowling_stats <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/Lab2/bowling_stats.csv")
head(bowling_stats)  

############## Ordering According to Priority ################
a <- bowling_stats[order(-bowling_stats$WKTS),]
a
b  <- a[order(bowling_stats$BALLS),]
b
c <- b[order(bowling_stats$MATCHES),]
c
e <- c[order(-bowling_stats$RUNS),]
e

############# Player who topped the list ###################
select(head(e, n=1), PLAYER)

########### Player who taken maximum wicket ################
max_wkt <- e[which.max(e$WKTS),]
select(max_wkt, PLAYER)

########### Player who've thrown maximum balls ##############
max_ball <- e[which.max(e$BALLS),]
select(max_ball, PLAYER)

############# Player who gave minimum runs ##################
min_run <- e[which.min(e$RUNS),]
select(min_run, PLAYER)

############# Player who played minimum match ###############
min_match <- e[which.min(e$MATCHES),]
select(min_match, PLAYER)

############# Top ten player's name in my list ###################
select(head(e, n=10), PLAYER)

########## Top ten player's with their data in my list ###########
select(head(e, n=10), PLAYER, MATCHES, BALLS, RUNS, WKTS )




players_runs = data [, c ("batsman", "batsman_runs")]
players = unique (players_runs$batsman)
players



