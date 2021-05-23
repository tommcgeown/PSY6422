#Locate the data
datafile = "~/Documents/CNHN/PSY6422/project/data/prevalence-of-schizophrenia-by-age.csv"
#Load the Data
mydata=read.csv(datafile)


#renaming columns so they are easier to use and in better format
new_names = c("country","code","year","prevalence_10_14", "prevalence_15_19", "prevalence_20_24",
              "prevalence_25_29", "prevalence_30_34", "prevalence_all_ages", "prevalence_5_14", 
              "prevalence_15_49", "prevalence_50_69", "prevalence_70_up", "prevalence_stand")

#loops through each column and asigns a new name to each
for (i in 1:14){
  colnames(mydata)[i] <- new_names[i]
}

#display raw data with new column names
head(mydata)


#load in the tidyverse library which provides functions to efficiently manage data 
library(tidyverse)

#selecting the uk data
uk<-mydata[c(6049:6076),]

#function to produce individual data frames for each age group
#inputs are the selected column, the column name in characters and the age group in characters
prev_by_age <- function(prev,prev_name,age) {
  #select only the year and prevalence (for a specific age group) columns
  uk<-uk %>% 
    select("year",prev_name)
  #create a new column which states the age group
  uk<-uk %>% 
    mutate(age_group=age)
  #create a new column which calculates the relative change in prevalence. It does this by subtracting the prevalence in 1990 for a specific age group from the prevalence of each subsequent year for that age group and dividing by the prevalence in 1990
  uk<-uk %>%
    mutate(rel_change=(((uk$prev-uk[1,2])/uk[1,2])*100))    
  #the prevalence column is renamed "prevalence" i.e. removing the age group from the column name so each data frame is the same and can be later combined
  colnames(uk)[2]<-"prevalence"                        
  return(uk)                                         
}

#function is used on each age group to produce a separate data frame for each
uk_1<-prev_by_age(prevalence_10_14,"prevalence_10_14","10-14")
uk_2<-prev_by_age(prevalence_15_19,"prevalence_15_19","15-19")
uk_3<-prev_by_age(prevalence_20_24,"prevalence_20_24","20-24")
uk_4<-prev_by_age(prevalence_25_29, "prevalence_25_29","25-29")
uk_5<-prev_by_age(prevalence_30_34, "prevalence_30_34","30-34")
uk_6<-prev_by_age(prevalence_all_ages, "prevalence_all_ages","All ages")
uk_7<-prev_by_age(prevalence_5_14, "prevalence_5_14","5-14")
uk_8<-prev_by_age(prevalence_15_49, "prevalence_15_49","15-49")
uk_9<-prev_by_age(prevalence_50_69, "prevalence_50_69","50-69")
uk_10<-prev_by_age(prevalence_70_up, "prevalence_70_up","70+")
uk_11<-prev_by_age(prevalence_stand, "prevalence_stand","Standardised")

#each dataframe is combined to create one dataframe consisting of all age groups in a format that can be used to make graphs
prev_combined <- rbind(uk_1, uk_2, uk_3, uk_4, uk_5, uk_6, uk_7, uk_8, uk_9, uk_10, uk_11)

#display first few lines of processed data
head(prev_combined)


#load in ggplot2, a package for creating visualisations, and plotly, to make it interactive. 
library(ggplot2)
library(plotly)

#visualising prevalence of schizophrenia by age group
#select 'prev_combined' as the data to plot. Within aesthetics, select which variable will be plotted on the x and y axes. Include 'group=1' simply to override default behaviour of ggplotly. Assign each of the data variables a better formatted name which will feature when hovering over interactive graph.
p <- ggplot(data=prev_combined, mapping = aes(x=year, y=prevalence, group=1, 
                                              text=paste("Age Group: ", age_group,
                                                         "<br>Year: ", year,
                                                         "<br>Prevalence (%): ", 
                                                         round(prevalence, digits = 4)))) + 
  
  #plot data points and lines, and display each in a different colour dependent on the age group. The specfic colours are defined. A visual style for the graph is selected and the font size defined. The x axis labels are adjusted and the y axis limits are defined.
  geom_point(aes(color=age_group)) + geom_line(aes(color=age_group)) + 
  scale_color_manual(values =c(c('#FF6666','#FFB266','#FFFF66','#B2FF66',
                                 '#66FF66','#66FFB2','#66FFFF','#66B2FF','#6666FF','#B266FF','#FF66FF'))) +
  theme_light(base_size=10) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 7)) +
  expand_limits(y=c(0.00, 0.45)) +
  
  #the graph title, x axis label and y axis label are defined. The intervals along each axis are also defined.
  ggtitle("Prevalence of Schizophrenia by Age Group in the UK") +
  scale_x_continuous(name="Year", breaks = seq(1990, 2017,3)) + 
  scale_y_continuous(name = "Prevalence by percentage of age group (%)", 
                     breaks= seq(0.0,    0.5, 0.05)) +
  
  #the legend name and its position are defined
  labs(color="Age Group") +
  theme(legend.position="right")

#the graph is made interactive
ggplotly(p, tooltip='text')



#visualising relative change in prevalence of schizophrenia by age group
#select 'prev_combined' as the data to plot. Within aesthetics, select which variable will be plotted on the x and y axes. Include 'group=1' simply to override default behaviour of ggplotly. Assign each of the data variables a better formatted name which will feature when hovering over interactive graph.
p2 <- ggplot(data=prev_combined, mapping = aes(x=year, y=rel_change, group=1,
                                               text=paste("Age Group: ", age_group,
                                                          "<br>Year: ", year,
                                                          "<br>Relative change (%): ", 
                                                          round(rel_change, digits = 4)))) +
  
  #plot data points and lines, and display each in a different colour dependent on the age group. The specfic colours are defined. A visual style for the graph is selected and the font size defined. The x axis labels are adjusted and the y axis limits are defined.
  geom_point(aes(color=age_group)) + geom_line(aes(color=age_group)) + 
  scale_color_manual(values = c(c('#FF6666','#FFB266','#FFFF66','#B2FF66',
                                  '#66FF66','#66FFB2','#66FFFF','#66B2FF','#6666FF','#B266FF','#FF66FF'))) +
  theme_light(base_size=10) +
  theme(axis.text.x = element_text(angle=45, hjust = 1, size = 7)) +
  expand_limits(y=c(-6, 15)) +
  
  #the graph title, x axis label and y axis label are defined. The intervals along each axis are also defined.
  ggtitle("Relative Change in Prevalence of Schizophrenia by Age Group in the UK") +
  scale_x_continuous(name="Year", breaks = seq(1990, 2017,3)) + 
  scale_y_continuous(name = "Prevalence as relative change in percentage of age group (%)", 
                     breaks= seq(-6, 16, 2)) +
  
  #the legend name and its position are defined
  labs(color="Age Group") +
  theme(legend.position="right")

#the graph is made interactive
ggplotly(p2, tooltip='text')