# global.R

# Used in conjunction with the shiny app.
# Loads the Lahman baseball data set and performs basic
# manipulation.

require(Lahman) 
require(plyr)

################## Data set for option 1 in the app ###############################################
# Select certain columns from the data set
dat = Teams[,c('yearID', 'name', 'G', 'SO', 'HR', 'H', 'ERA', 'E', 'AB')]

# Create new columns and remove all NA's
team_data = na.omit(transform(dat, SOG = round(SO/G, 2), HRG = round(HR/G, 2),
                              HG = round(H/G, 2), EG = round(E/G, 2), 
                              ABG = round(AB/G, 2)))

# Data set with average over all teams
league_data = ddply(team_data, .(yearID), summarize, SOG = mean(SOG), HRG = mean(HRG),
                    HG = mean(HG), EG = mean(EG), ERA = mean(ERA), ABG = mean(ABG))

# Visualize individual team data for teams that appear more than THRESHOLD times
nobs<-dim(team_data)[1]
THRESHOLD = 10
team_appearances = count(team_data, .(name))
teams_in_menu = subset(team_appearances, freq > THRESHOLD)$name

############################################################################################

################ Data set for option 2 in the app ##########################################
# Utilize salary data set
avesal <- aggregate(salary ~ yearID + teamID, data=Salaries, FUN=mean)

# Salary data just starts after 1980
teamsSal <- subset(Teams, yearID>=1980)

# Add salary to team data
teamsSal <- merge(teamsSal, 
                  avesal[,c("yearID", "teamID", "salary")], 
                  by=c("yearID", "teamID"), all.x=TRUE)

teamsSal <- na.omit(teamsSal)
teamsSal$lgID<-factor(teamsSal$lgID)
teamsSal$Rank<-factor(teamsSal$Rank)

# Select certain columns
saldat = teamsSal[,c('lgID', 'name', 'salary', 'Rank', 'attendance', 'G', 
                     'SO', 'HR', 'H', 'ERA', 'E', 'W', 'AB')]

# Add new columns to the final data set used for option 2 in the app
sal_data = transform(saldat, SOG = round(SO/G, 2), HRG = round(HR/G, 2),
                              HG = round(H/G, 2), EG = round(E/G, 2), 
                              Wper = round(W/G*100, 2), 
                              AttG = round(attendance/G, 2), ABG = round(AB/G, 2))

