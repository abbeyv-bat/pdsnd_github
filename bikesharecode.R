
#I did not learn about refactoring in any of the courses included in my
# R focused Nanodegree, but I will add additional comments to the code to 
#demonstrate my new skills in version control! 

getwd()
list.files()

df1 <- read.csv('chicago.csv')
df2 <-read.csv('new-york-city.csv')
df3 <- read.csv('washington.csv')

#Creating a new column titled 'city':

df1$city <- "Chicago"
df2$city <- "New York City"
df3$city <- "Washington DC"
install.packages("dplyr")
library(dplyr)

#Combining the three files into one: 
bikes <- bind_rows(df1,df2,df3)


#QUESTION1: Do males or females take longer trips? 

names(bikes)
table(bikes$Gender)
library(ggplot2)
qplot(x= Gender, y=Trip.Duration/60, data= subset(bikes, omit.na(Gender))
      
      
      #This removed all rows where gender wasn't listed:
      cleanbikes <- bikes[!is.na(bikes$Gender),] 
      
      #I still had blanks showing up, this add on took out anything that had an empty string:
      cleanbikes <- bikes[!is.na(bikes$Gender) & bikes$Gender !="", ] 
      
      #Making my plot:
      
      library(ggplot2)
      
      qplot(x=Gender, y=Trip.Duration/60, data=cleanbikes, 
            main = "Trip Duration by Gender",
            xlab='Gender',
            ylab='Trip Duration in Minutes', 
            geom = 'boxplot') +
        coord_cartesian(ylim=c(0,50))
      
      #Looking at boxplot summary stats: 
      by(cleanbikes$Trip.Duration, cleanbikes$Gender, summary)
      
      #Output: 
      cleanbikes$Gender: Female
      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
      60.0    400.0    651.0    837.9   1047.0 332970.0 
      ----------------------------------------------------------------------------------------- 
        cleanbikes$Gender: Male
      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
      60.0     339.0     545.0     726.3     887.0 2155775.0
      
      #The median trip length is slightly longer for Females than it is for males. 
      
      
      
      
      #QUESTION 2: Are the number of rentals higher for females or males in each city? 
      
      ggplot(cleanbikes, aes(x=Gender, fill=Gender)) +
        geom_bar() +
        facet_wrap (~ city) +
        scale_fill_manual(values =c("Male"="blue", "Female"= "pink"))+
        labs(title= "Number of Rentals by Gender",
             x= "Gender",
             y= "Rental Count") +
        theme_minimal()
      
      #Washington DC did not show up in the graph, so I ran the code "table(df3$Gender)" to determine that no genders had been recorded for that city. 
      #Washington DC was not included in this analysis. 
      
      table(cleanbikes$city, cleanbikes$Gender) 
      #Output: 
      Female   Male
      Chicago        57758 181190
      New York City  66783 204008
      
      #Males have rented more bikes than females in both Chicago and New York City. 
      
      prop.table(table(cleanbikes$Gender, cleanbikes$city), margin=2) *100
      #Output:
      Chicago     New York City
      Female 24.17179      24.66219
      Male   75.82821      75.33781
      
      #Females made up about a quarter of bike rentals in both Chicago and New York City. 
      
      #QUESTION 3: What time of Day is most popular for bike rentals?
      
      install.packages("lubridate")
      library(lubridate)
      
      cleanbikes$Start.Time <- ymd_hms(cleanbikes$Start.Time) #Extracted time components 
      
      cleanbikes$time <- hour(cleanbikes$Start.Time) + minute(cleanbikes$Start.Time)/60 #Convert to decimal hours to see time of day
      
      head(cleanbikes$time)
      
      ggplot(cleanbikes, aes(x=time)) +
        geom_histogram(binwidth =1, fill="red", color='white')+
        labs(title = "Bike Rentals per Time of Day",
             x= "Time (Hour of Day)",
             y= "Number of Rentals")+ 
        theme_minimal()
      
      #Looking for summary of rental count per each hour: 
      count(cleanbikes$count, cleanbikes$hour)
      table(cleanbikes$hour)
      
      #Output:
      0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18    19 
      3444  1875  1103   681   841  4125 14526 31340 46870 29814 20206 22224 25654 25722 26110 29130 40960 57920 45974 30743 
      20    21    22    23 
      20312 14092  9906  6166 
      
      #The highest number of rentals occur at 17:00 and 18:00 (this makes sense as those are normal commuting hours!). 
      
      #Thank you for taking the time do review my project!