#The dataset International football results from 1872 to 2020 contains results of all the international football matchs
#The dataset contains the home team, away team score , tournament , the date of the match;
# city and country it was held and wether it was on neutral grounds
#In this project we perform data analysis on this dataset and try to predict iff the home team wins or away team wins


#1. Extracting data from the dataset 

#read the dataset from csv file and store it in a dataframe
dataset = read.csv('39_InternationalFootballMatchs.csv')

#R includes extensive facilities for accessing documentation using help function
help()

#importing the libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#we can install libraries using the install.package() function
install.packages(
  "tidyr",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)

#Extracting data from the dataset
#The class function returns class of data 
class(dataset) 

#The dim function returns dimensions of the data i.e. no. of rows and cols
dim(dataset) 

#we can also use individual nrow() and ncol() function to get no. of rows and cols
nrow(dataset)
ncol(dataset)

#The name() function is used to return the name of columns
names(dataset)

#We can check the memory size of the data by using object.size
object.size(dataset) 

#displays first few rows of data using head() 
head(dataset) 

#displays last few rows of data using tail()
tail(dataset) 

#summary() gives detailed summary of data - mean, median, Q1, Q2,Q3, etc 
summary(dataset) 

#str() returns dataypes of each column
str(dataset)


#2. cleaning dataset

#we check  if the dataset has any null values
is.null(dataset)
#no null values were present, but if there were null values we would remove them

#we check for duplicate values in the dataset
#the duplicated() function returns a data frame with Boolean values
# we run it throug a for loop to check for 'TRUE' 
#if duplicate values were found, we would remove them using unique() function
check <- duplicated(dataset)
print(check[3])
count = 0
for (i in 1:nrow(dataset))
  if (check[i]== TRUE)
    count = count + 1
print(count)
#the count here is 0 thus we have no duplicate values


#3. EDA on the dataset
#EDA stands for exploratory data analysis

#for data pre processing we make to copies of the dataset
football <- dataset
df <- dataset

#first we use the football data frame 

#we set the date format as Year-month-day for uniformity of date format accross the dataset
football$date <- as.Date(football$date , "%Y-%m-%d")

# replacing typos found in the original dataset 
football$home_team[football$home_team =="Guersney"] <- "Guernsey"
football$away_team[football$away_team =="Guersney"] <- "Guernsey"

# adding new columns in the dataset
#we add a year column for data analysis of matchs aas per year
# we add a winner column which stores 'Draw, home, away'
#if the home team wins it stores 'home', if the away team wins it stores 'away' and 'draw' for draw
#mutate function is used to add columns in the 'football' dataframe
football <- football %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  rowwise() %>%
  mutate(winner = ifelse(home_score > away_score, "home",
                         ifelse(home_score < away_score, "away", "draw")))%>%
  ungroup()
#as.facctor is used to encode the vector as a factor indicationg categorical data
football$winner = as.factor(football$winner)
head(football)

#we display the summay of the new processed dataset
summary(football)

#graphical analysis

#Graph to check home and away goals
#we plot graph considering if the venue of the match was on neutral grounds or non neutral ground
#the non neutral part shows the home team wins vs the away team wins
#the neutral grounds shows home indicating first team listed to the second team 
options(repr.plot.height=4)

football %>%
  group_by(neutral) %>%
  summarize(home = round(mean(home_score),2),
            away = round(mean(away_score),2)) %>%
  gather(home_away_avg_score, avg_score, home:away) %>%
  ggplot(aes(home_away_avg_score, avg_score, fill = home_away_avg_score)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits=c('home','away'))+
  geom_text(aes(label=avg_score), vjust=-0.3, size=3.5) +
  scale_fill_manual(values = c("#F8766D", "#619CFF")) +
  facet_wrap(~neutral, labeller = as_labeller(c("FALSE" = 'non-neutral venue',
                                                "TRUE" = 'neutral venue')))+
  labs(title="Goal average per game", x = "Team", y="Goal average", fill = 'Team')+
  theme(legend.position="none",
        panel.background = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
#we see here that the home team has higher chances of winning on non neutral grounds
#thus yes home advantage does exist

#Graph to most common scores
#the graph shows the most common score with the hom teams to the away teams
my_breaks <- c(1,10,100,1000, 3500)
my_colors <- c("#0000CD", "#67005F","#CC0000", "#FF4E00")

football %>%
  group_by(home_score, away_score) %>%
  summarize(n = n()) %>% 
  ggplot(aes(x = home_score, y = away_score)) +
  geom_tile(aes(fill = n),width=0.8, height=0.8) +
  scale_fill_gradientn(trans = 'log', colors = my_colors,
                       breaks = my_breaks, labels = my_breaks,
                       guide = 'legend',
                       values = scales::rescale(log(c(0.1, 1, 100, 1000, 3500))))+
  scale_x_continuous(breaks = seq(0,31,2)) +
  scale_y_continuous(breaks = seq(0,21,2))+
  labs(title="Results density", fill = "number of matches",
       x = "home goals", y = "away goals") +
  theme(panel.background = element_blank())
#here we can see the most common score is 0-0
#the graph has most points lying in the low scores range from both home and away sides


#Graph for wins in neutral and non neutral venues
#the graph show the number of wins in home,away side and the number of draws
#similar to above this graph also has two parts, neutral grounds and non neutral grounds
options(repr.plot.height=4)

football %>%
  group_by(winner, neutral) %>%
  summarize(number = length(winner)) %>%
  ggplot(aes(winner, number, fill = winner)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits=c('home','away','draw')) +
  geom_text(aes(label=number), vjust=-0.3, size=3.5) +
  facet_wrap(~neutral, labeller = as_labeller(c("FALSE" = 'non-neutral venue',
                                                "TRUE" = 'neutral venue'))) +
  labs(title="Wins on neutral and non-neutral venues",
       x = "winning team", y="number of wins") +
  theme(legend.position="none",
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
#we observe that on non neutral grounds the most wins are achieved by the home teams

#we create another dataframe, here we add more coloumns in the dataset
df<-football
#we create 3 functions to get the game_outcome, wiining team name and loosing team name

#the game_outcome function returns the winner of the game indicated by D,H,A
#D stands for draw, H stads for home team win, A stands for away team win
game_outcome <- function(home_score, away_score) {
  outcome <- "D"
  if (home_score > away_score) {outcome <- "H"}
  if (home_score < away_score) {outcome <- "A"}
  return(outcome)
}
#winning team function returns the name of the winning team
winning_team <- function(home_score, away_score, home_team, away_team) {
  winning_team <- NA
  if (home_score > away_score) {winning_team <- home_team}
  if (home_score < away_score) {winning_team <- away_team}
  return(winning_team)
}
#losing_team returns the name of the loosing team
losing_team <- function(home_score, away_score, home_team, away_team) {
  losing_team <- NA
  if (home_score < away_score) {losing_team <- home_team}
  if (home_score > away_score) {losing_team <- away_team}
  return(losing_team)
}
#we add coloumns outcome and winning team
#the outcome stores categorical data with D,H,A indicating if it was a draw or home win or away win
#the winner column stores the name of the winning team and NA if its a draw
df <- df %>%
  rowwise() %>%
  mutate(outcome = game_outcome(home_score, away_score),
         winning_team = winning_team(home_score, away_score, home_team, away_team),
         losing_team = losing_team(home_score, away_score, home_team, away_team)) %>%
  ungroup()


#handling categorical data
#we need to convert the categorical data to numeric data 
#the outcome column contains 'D,H,A' , we give them corresponting numeric values
#Characters are not supported in machine learning algorithm, and the only way is to convert a string to an integer.
#this is done using factor() function
df$outcome = factor(df$outcome, 
                    levels = c('D','H','A'),
                    labels = c(1,2,3))

#number of games of each compition for year 2016
#we check the number of games held in each compition for the year 2016
df_competitions %>% filter(year == 2016) %>% arrange(desc(nb_games))

#We check the number of games played by each team
#the teams have been categorised and indicateed by corresponding number
all_teams <- data.frame(teams = c(df$home_team, df$away_team), year=as.numeric(c(df$year, df$year)))
all_teams_count <- all_teams %>%
  group_by(teams) %>%
  summarise(number_games = length(teams)) %>%
  arrange(desc(number_games))

head(all_teams_count, 10)

#4. Regression 
#regression is a machine learning algorithm used to predict dependant variables values based on some independant data
#here we use multiple linear regression to predict if the home team will win or away team or a draw
head(df)

#caTools library is used which contains the lm() function
#lm() is the funciton to perform regression in R
#we split the dataset into training and test set with 80% columns in training set
library(caTools)
set.seed(123)
split = sample.split(df$outcome,SplitRatio = 0.8)
training_set = subset(df,split == TRUE)
test_set = subset(df, split == FALSE)

#regressor object is created which stores the regression model
#we have taken home_score, away_score and venue as the X (independant variables)
#the model predicts the chanes for home win. away win or a draw
regressor = lm(formula = outcome ~ home_score + away_score + neutral,
               data = training_set)


#we test the model to predict the outcome for test set and store in y_pred 
y_pred = predict(regressor, newdata = test_set)

#the predicted values are in decimal format so e round them off for analysis
#here 1 indicates a draw, 2 indicates a home win and 3 indicates away win
y_pred = round(y_pred)
#while rounding we get some values as 4 so we set them to 3 aas there is no 4th category
for (i in 1:8308)
  if (y_pred[i]== 4)
    y_pred[i] = 3

print(y_pred)

#to compare the predicted result and the actual result we put y_pred and test_set's outcomee in single dataframe
result <- data.frame(PredictValue_Y = y_pred,
                     ActaulValue_Y = test_set$outcome)
print(result)

#diplay the first 20 values to show the predicted vs actual values
test <- result[1:20,]
test

#5. Data visualization using ggplot2

#here we plot a graph for actaul values vs the predicted values
#geom_point is used to plot the actaul values as points
#geom_line is used to plot a lin for the prediction model
#here the dots indicates the actual values and the line indicates the values predicted by the model
ggplot() + geom_point(aes(x=1:20, y=test$ActaulValue_Y, color='red')) + 
  geom_line(aes( x = 1:20, y=test$PredictValue_Y,
                 color='blue') )

#Visulisation of data

#Graph for games per year 
#this graph shows a rising curve for games played every year
#we can see that there is a big drop during the world war 2 era
tmp <- df %>%
  filter(year < 2018) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  summarise(nb_games = length(date))  %>%
  ungroup()

ggplot(tmp, aes(x=year, y=nb_games, group=1)) +
  geom_line() +
  labs(x="Year", title="Number of international soccer games", y="") +
  scale_x_continuous(breaks=seq(1870, 2020, 10))

#games per year with world cup years marked
#this graph shows the year when world cup was held
#we can see here that there are less games during he world cup years
wc_years <- c(1930, 1934, 1938, seq(1950, 2014, 4))

tmp <- tmp %>%
  mutate(is_wc = year %in% wc_years)

ggplot(tmp, aes(x=year, y=nb_games, group=1)) +
  geom_line() +
  geom_point(data = tmp %>% filter(is_wc), aes(colour=is_wc)) +
  labs(x="Year", title="Number of international soccer games", y="", colour="World cup year") +
  geom_vline(xintercept=c(1914,1918,1939,1945), lwd=0.3, colour="gray80") +
  scale_x_continuous(breaks=seq(1870, 2020, 10))

#Graph for matchs as per compitition 
#we check the number of games held in each compition for the year 2016
df_competitions %>% filter(year == 2016) %>% arrange(desc(nb_games))

#graph indiacates the number of matchs held for each compitition from year 2006 to year 2016
df_competition_filtered <- df_competitions %>% 
  filter(year >= 2006 & year < 2018 & tournament %in% c("Friendly","UEFA Euro qualification","FIFA World Cup", "FIFA World Cup qualification", "African Cup of Nations qualification")) 

ggplot(df_competition_filtered, aes(x=year, y=nb_games, group=tournament, colour=tournament)) +
  geom_point() +
  geom_line() +
  labs(x="Year", y="Nb games", colour="Competition")
