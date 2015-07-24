# server.R

# Visualize various baseball statistics using the Lahman data set


#### Required Packages ###############
require(rCharts)
require(ggplot2)
# For ggplot
theme_set(theme_grey(base_size = 18))
#######################################

# Where the data set is loaded and manipulated
source("global.R")


# Main function
shinyServer(function(input, output) {
  
  
  # Initial choice for layout
  output$choices <- renderUI({
    selectInput(inputId = 'type',
                label = "What would you like to view?",
                choices = list('App Documentation' = 'doc',
                        'Baseball statistics by year' = 'ystats', 
                        'Interactions between statistics' = 'sal'),
                selected = 'doc')})
  
  
  # Documentation text
  #############################################################################
  output$doctext <- renderUI({
    if('doc' == input$type){
      h3(strong('Documentation:'),' This shiny application performs data visualization and some basic
        exploratory data analysis of the Lahman baseball data set in R 
        (http://www.seanlahman.com/baseball-archive/statistics/).  This first option from 
        the dropdown menu on the left displays the 
        documentation for the application. The second option plots a user chosen baseball statistic 
        (e.g., strikeouts per game, homeruns per game, etc.) as a function of 
        the year using rPlot from rcharts. The third option displays scatterplots of two 
        user chosen statistics in order to visually discern if interactions exist. The user
        can also choose to group the data in certain ways as well as add regression lines to the plot.
        This application is based off and inspired by the NYT Interactive Graphic tutorial from the 
        rCharts gallery (http://rcharts.io/nytinteractive/).',
        style="margin:150px 0px 10px 20px;")
    }else {return()}
  })
  
  #######################################################################
  
  
  output$htext1 <- renderUI({
    if('doc' != input$type){
      helpText('The first option displays the documentation. The second option plots a user chosen
               baseball statistic (e.g., strikeouts per 
               game, homeruns per game, etc.) as a function of 
               the year using rPlot from rcharts. The third option displays scatterplots of two 
               statistics that the user chooses in order to visually discern interactions.')
    }
  })
  
    
  # All these UI's depend on the input above
  
  ########################################## Option 1  ############################################
  output$statchoice <- renderUI({
      if('ystats' == input$type){
      selectInput(inputId = 'stat',
                            label = "Choose statistic to display as a function of the year",
                            choices = list('Strikeouts per game' = 'SOG', 'Homeruns per game' = 'HRG',
                                            'Hits per game' = 'HG', 'At bats per game' = 'ABG',
                                            'Errors per game' = 'EG', 'Earned run average' = 'ERA'),
                                             selected = 'SOG')
      } else {return()}
  })
  
  output$htext2 <- renderUI({
    if('ystats' == input$type){
      helpText('League average is plotted in blue, while individual team data are plotted as gray points. 
               Place the cursor over a point to obtain the team name.')
    }else {return()}
  })
    
  output$slider <- renderUI({
      if('ystats' == input$type){
      sliderInput("psample", label = "Percentage of data points to plot",
                                           min = .1, max = 1, value = 1)
      } else{return()}
  })
    
  output$htext3 <- renderUI({
      if('ystats' == input$type){
      helpText('Making this smaller allows for faster re-plotting, but randomly resamples data points for every new plot.')
        }else {return()}
  })
    
  output$tid <- renderUI({
      if('ystats' == input$type){
      selectInput(inputId = 'team',label = "Individual team data (red line)",
                                    choices = teams_in_menu,
                                    selected = 'New York Yankees')
        } else {return()}
  })
    
  tdata <- reactive({
      if('ystats' == input$type){
      data.frame('yearID' = team_data$yearID, 'stat' = team_data[,input$stat],
                               'name' = team_data$name)
      }else {return()}
  })
    
  ldata <- reactive({
      if('ystats' == input$type){
      data.frame('yearID' = league_data$yearID, 'stat' = league_data[,input$stat])
      } else{return()}
  })
  
  itdata <- reactive({
      if('ystats' == input$type){
      data.frame('yearID' = team_data$yearID[team_data$name == input$team], 
                                'stat' = team_data[team_data$name == input$team,input$stat])
      } else{return()}
  })
  
  nsample <- reactive({
      if('ystats' == input$type){
        round(as.numeric(input$psample)*nobs,0)
      } else{return()}
  })

    
  sname <- reactive({
      if('ystats' == input$type){
        if(input$stat == 'SOG'){
          return("Strikeouts per Game")
        } else if(input$stat == 'HRG'){
           return('Homeruns per Game')
        } else if(input$stat == 'HG'){
           return('Hits per Game')
        } else if(input$stat == 'EG'){
           return('Errors per Game')
        } else if(input$stat == 'ABG'){
           return('At Bats per Game')
        } else if(input$stat == 'ERA'){
           return('Earned Run Average')
        }
      } else {return()}
  })
  
  
  
  ################################################################################################
    
  ############################### Option 2 #######################################################
  output$salchoicex <- renderUI({
    if('sal' == input$type){
      selectInput(inputId = 'statx',
                  label = "Choose a statistic to plot on the x-axis",
                  choices = list('Team Salary' = 'salary', 'Winning percentage' = 'Wper',
                                 'Attendance per game' = 'AttG', 'At bats per game' = 'ABG',
                                 'Strikeouts per game' = 'SOG', 'Homeruns per game' = 'HRG',
                                 'Hits per game' = 'HG', 'Errors per game' = 'EG', 
                                 'Earned run average' = 'ERA'),
                  selected = 'salary')
    } else {return()}
  })
  
  output$salchoicey <- renderUI({
    if('sal' == input$type){
      selectInput(inputId = 'staty',
                  label = "Choose a statistic to plot on the y-axis",
                  choices = list('Team Salary' = 'salary', 'Winning percentage' = 'Wper',
                                 'Attendance per game' = 'AttG', 'At bats per game' = 'ABG',
                                 'Strikeouts per game' = 'SOG', 'Homeruns per game' = 'HRG',
                                 'Hits per game' = 'HG', 'Errors per game' = 'EG', 
                                 'Earned run average' = 'ERA'),
                  selected = 'Wper')
    } else {return()}
  })
  
  output$leg <- renderUI({
    if('sal' == input$type){
      selectInput(inputId = 'cleg',
                  label = "Choose how you would like the points to be grouped",
                  choices = list('League ID' = 'lgID', 'Rank at the end of the season' = 'Rank',
                                 'No grouping' = 'none'),
                  selected = 'Rank')
    } else {return()}
  })
  
  output$htext4 <- renderUI({
    if('sal' == input$type){
      helpText('Points can be grouped by the team ranking at the end of the regular season, by the
               team league (American League AL or National League NL), or grouping can be turned off.')
    }else {return()}
  })
  
  output$lines <- renderUI({
    if('sal' == input$type){
      checkboxInput("cbox", label = "Add linear regression lines", value = FALSE)
    } else {return()}
  })
  
  xname <- reactive({
    if('sal' == input$type){
      if(input$statx == 'SOG'){
        return("Strikeouts per Game")
      } else if(input$statx == 'HRG'){
        return('Homeruns per Game')
      } else if(input$statx == 'HG'){
        return('Hits per Game')
      } else if(input$statx == 'EG'){
        return('Errors per Game')
      } else if(input$statx == 'salary'){
        return('Team Salary')
      } else if(input$statx == 'ERA'){
        return('Earned Run Average')
      } else if(input$statx == 'Wper'){
        return('Winning Percentage')
      } else if(input$statx == 'Rank'){
        return('Rank at the end of the Season')
      } else if(input$statx == 'AttG'){
        return('Attenance per Game')
      } else if(input$statx == 'ABG'){
        return('At Bats per Game')
      } 
    } else {return()}
  })
  
  yname <- reactive({
    if('sal' == input$type){
      if(input$staty == 'SOG'){
        return("Strikeouts per Game")
      } else if(input$staty == 'HRG'){
        return('Homeruns per Game')
      } else if(input$staty == 'HG'){
        return('Hits per Game')
      } else if(input$staty == 'EG'){
        return('Errors per Game')
      } else if(input$staty == 'salary'){
        return('Team Salary')
      } else if(input$staty == 'ERA'){
        return('Earned Run Average')
      } else if(input$staty == 'Wper'){
        return('Winning Percentage')
      } else if(input$staty == 'Rank'){
        return('Rank at the end of the Season')
      } else if(input$staty == 'AttG'){
        return('Attenance per Game')
      } else if(input$staty == 'ABG'){
        return('At Bats per Game')
      }
    } else {return()}
  })
  
  
  legname <- reactive({
    if('sal' == input$type){
      if(input$cleg == 'lgID'){
        return("League ID")
      } else if(input$cleg == 'Rank'){
        return('Rank at the end of the Season')
      } else {return()}
    } else {return()}
  })
  
  
  #################################################################################################
  
  # Plotting output
  
  ################################################# Option 1 ######################################
  output$myChart <- renderChart({
    
    if('ystats' == input$type){
      mytooltip = "#! function(item){return 'Team Name: ' + item.name +  ',   ' 
              + 'Value: ' + item.stat + ',    ' + 'Year: ' + item.yearID} !#"
      p1 <- rPlot(stat~yearID, data = tdata(), type = 'point', sample = nsample(), 
                  size = list(const = 3), color = list(const = 'gray'), 
                  tooltip = mytooltip)
      p1$layer(data = ldata(), type = 'line', size = list(const = 3), color = list(const = 'blue'), 
               copy_layer = T, tooltip = NULL)
      p1$layer(data = ldata(), type = 'point', size = list(const = 4), color = list(const = 'blue'), 
               copy_layer = T, tooltip = NULL)
      p1$layer(data = itdata(), type = 'line', color = list(const = 'red'), copy_layer = T)
      p1$set(height = 550)
      p1$guides(x = list(title = "Year", numticks = 10, font = 100))
      p1$guides(y = list(title = ""))
      p1$set(title = sname())
      p1$guides(x = list(min = 1865, max = 2030))
      p1$set(dom = 'myChart')
      return(p1)
    }else {return()}
  })
  #################################################################################################
  
  ####################################### Option 2 ################################################
  output$myplot <- renderPlot({
    
    if('sal' == input$type){
      if(input$cleg != 'none'){
        df <- data.frame('x' = sal_data[,input$statx], 'y' = sal_data[,input$staty], 
                         'f' = sal_data[,input$cleg])
        g <- ggplot(data = df, aes(x=x,y=y,color=f)) + geom_point(size=3)
        g <- g + xlab(xname()) + ylab(yname()) + labs(color=legname())
      } else {
        df <- data.frame('x' = sal_data[,input$statx], 'y' = sal_data[,input$staty])
        g <- ggplot(data = df, aes(x=x,y=y)) + geom_point(size=3)
        g <- g + xlab(xname()) + ylab(yname()) 
      }
      if(input$cbox){
        g <- g + stat_smooth(method = 'lm', se = TRUE, size=2)
      }
      return(g)
    } else {return()}
    
  },height=500, width=850,units='px')
  
})