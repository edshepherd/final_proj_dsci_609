setwd("/Users/burrisfaculty/Desktop/DSCode/DSCI609/Final Project")
#library(datasets)
library(ggplot2)
library(dplyr)
library(usmap)
library(hrbrthemes)
library(lattice)
library(plotly)
#General Wrangling of Data
raw_dta <- read.csv("BothTests.csv")
raw_dta["Region"]<- state.region[match(raw_dta$State, state.name)]
ap_dta = raw_dta %>%
  select(Year, State, Test, Region, Total, Total.Passed) %>%
  mutate(Year = as.factor(Year),
         Test = as.factor(Test),
         state = as.factor(State),
         Region = as.factor(Region)) %>%
  arrange(Year)
  
ap_dta["Percent Passed"]<- ap_dta$Total.Passed/ap_dta$Total

  

csp_dta<-subset(ap_dta, ap_dta$Test == "CSP")

csa_dta<-subset(ap_dta, ap_dta$Test == "CSA")

#explore CSA Data

test_dta <- ap_dta %>%
  select(Total,Year) %>%
  group_by(Year) %>%
  summarise(success = sum(Total)) %>%
  as.data.frame()
 
csa_total <- csa_dta %>%
  select(Total, Year) %>%
  group_by(Year) %>%
  summarise(Tests = sum(Total)) %>%
  as.data.frame()

csa_plot <- csa_total %>%
  ggplot( aes(x=Year, y=Tests)) + ggtitle("Number of AP CSA Tests") +  geom_col()  + theme_classic()




csa_passed <- csa_dta %>%
  select(Total.Passed, Year) %>%
  group_by(Year) %>%
  summarise(Passed = sum(Total.Passed)) %>%
  as.data.frame()
csa_plot2 <- csa_passed %>%
  ggplot( aes(x=Year, y=Passed)) +
  geom_col() + ggtitle("Number of AP CSA Passed") + theme_classic()

csa_percent <- csa_dta %>%
  select(Total.Passed, Total, Year) %>%
  group_by(Year) %>%
  summarise(Percent_Passed = sum(Total.Passed)/sum(Total)) %>%
  as.data.frame()
csa_plot3 <- csa_percent %>%
  ggplot(aes (x = Year, y = Percent_Passed)) +
  geom_col() + ggtitle("Percent AP CSA Passing") + theme_classic()

csa_plot
csa_plot2
csa_plot3
csa_by_state <- csa_dta %>%
  select(State, Total.Passed, Total, Year) %>%
  group_by(State) %>%
  summarise(Passed = sum(Total.Passed), Test = sum(Total)) %>%
   as.data.frame()
csa_plot4 <- csa_by_state %>%
  ggplot(aes(x = Test, y = Passed)) + 
  geom_point() + ggtitle("Number Passed vs Number of Tests by State")

csa_plot4
#Explore CSP Data

csp_total <- csp_dta %>%
  select(Total, Year) %>%
  group_by(Year) %>%
  summarise(Tests = sum(Total)) %>%
  as.data.frame()

csp_plot <- csp_total %>%
  ggplot( aes(x=Year, y=Tests)) +
  geom_col() + ggtitle("Number of AP CSP Tests") + theme_classic()



csp_passed <- csp_dta %>%
  select(Total.Passed, Year) %>%
  group_by(Year) %>%
  summarise(Passed = sum(Total.Passed)) %>%
  as.data.frame()
csp_plot2 <- csp_passed %>%
  ggplot( aes(x=Year, y=Passed)) +
  geom_col() + ggtitle("Number of AP CSP Passed") + theme_classic()

csp_percent <- csp_dta %>%
  select(Total.Passed, Total, Year) %>%
  group_by(Year) %>%
  summarise(Percent_Passed = sum(Total.Passed)/sum(Total)) %>%
  as.data.frame()
csp_plot3 <- csp_percent %>%
  ggplot(aes (x = Year, y = Percent_Passed)) +
  geom_col() + ggtitle("Percent AP CSP Passing") + theme_classic()

csp_plot
csp_plot2
csp_plot3

csp_by_state <- csp_dta %>%
  select(State, Total.Passed, Total, Year) %>%
  group_by(State) %>%
  summarise(Passed = sum(Total.Passed), Test = sum(Total)) %>%
  as.data.frame()
csp_plot4 <- csp_by_state %>%
  ggplot(aes(x = Test, y = Passed)) + 
  geom_point() + ggtitle("Number Passed vs Number of Tests by State-- CSP")

csp_plot4
#Spatial Data 
#While there are many questions about rural vs urban and red states vs blue states and education, from the data 
# I have there is not much I can do.
#I am going to revisit this with regional data and pass rates for women, Black, and Hispanic students.
plot_usmap(data = csa_dta, values = 'Total', color = 'red')+
  scale_fill_continuous(name = "Total Passed", label = scales::comma) +
  theme(legend.position = 'right')
csa_dta_2019 <- subset(csa_dta, Year == "2019")
plot_usmap(data = csa_dta_2019, values = 'Percent Passed', color = 'red')+
  scale_fill_continuous(name = "AP CSA Percent Passed 2019", label = scales::comma) +
  theme(legend.position = 'right')

plot_usmap(data = csp_dta, values = 'Total', color = 'red')+
  scale_fill_continuous(name = "Total Passed", label = scales::comma) +
  theme(legend.position = 'right')
csp_dta_2019 <- subset(csp_dta, Year == "2019")
plot_usmap(data = csp_dta_2019, values = 'Percent Passed', color = 'red')+
  scale_fill_continuous(name = "AP CSP Percent Passed 2019", label = scales::comma) +
  theme(legend.position = 'right')


#Look at ANOVA test
#Comparing Percent Passed
csa.two.way <- aov(`Percent Passed` ~ Year + Region, data = csa_dta)
summary(csa.two.way)
TukeyHSD(csa.two.way)

csp.two.way <- aov(`Percent Passed` ~ Year + Region, data = csp_dta)
summary(csp.two.way)
TukeyHSD(csp.two.way)

#Comparing Total Participation
csa.one.way <- aov(Total ~ Year, data = csa_dta)
summary(csa.one.way)
TukeyHSD(csa.one.way)

csp.one.way <- aov(Total ~ Year, data = csp_dta)
summary(csp.one.way)
TukeyHSD(csp.one.way)

#Comparing Total Passed
csa1.one.way<- aov(Total.Passed ~ Year, data = csa_dta)
summary(csa1.one.way)
TukeyHSD(csa1.one.way)

csp1.one.way <- aov(Total.Passed ~ Year, data = csp_dta)
summary(csp1.one.way)
TukeyHSD(csp1.one.way)

#Comparing Percent Passed 2019
csp2019.one.way<- aov(`Percent Passed` ~ Region, data = csp_dta_2019)
summary(csp2019.one.way)
TukeyHSD(csp2019.one.way)


csa2019.one.way<- aov(`Percent Passed` ~ Region, data = csa_dta_2019)
summary(csa2019.one.way)
TukeyHSD(csa2019.one.way)
# ****************** Looking at Partipation Data Based on Sex ***********************

by_sex_dta = raw_dta %>%
  select(Year, State, Test, Region, Total, Total.Passed,Fem.Total, Fem.Passed, X..Fem.Passed, X..Fem, Male.Total,
         Male.Passed, X..Male, X..Male.Passed) %>%
  mutate(Year = as.factor(Year),
         Test = as.factor(Test),
         state = as.factor(State),
         Region = as.factor(Region)) %>%
  arrange(Year)

csa_by_gender <- subset(by_sex_dta, by_sex_dta$Test == "CSA")


csa_by_gender_total <- csa_by_gender %>%
  select(Fem.Total, Male.Total, Year) %>%
  group_by(Year) %>%
  summarise(F_Tests = sum(Fem.Total), M_Tests = sum(Male.Total)) %>%
  as.data.frame()

fig <-plot_ly(csa_by_gender_total, x = ~Year, y = ~F_Tests, type = 'bar', name =" Female Tests")
fig <- fig %>% add_trace(y = ~ M_Tests, name = "Male Tests")
fig <- fig %>% layout (yaxis = list(title = "Tests"), title = "Number of CSA Tests by Sex",barmode = "group")
fig

csp_by_gender <- subset(by_sex_dta, by_sex_dta$Test == "CSP")
csp_by_gender_total <- csp_by_gender %>%
  select(Fem.Total, Male.Total, Year) %>%
  group_by(Year) %>%
  summarise(F_Tests = sum(Fem.Total), M_Tests = sum(Male.Total)) %>%
  as.data.frame()
fig1 <-plot_ly(csp_by_gender_total, x = ~Year, y = ~F_Tests, type = 'bar', name =" Female Tests")
fig1 <- fig1 %>% add_trace(y = ~ M_Tests, name = "Male Tests")
fig1 <- fig1 %>% layout (yaxis = list(title = "Tests"), title = "Number of CSP Tests by Sex",barmode = "group")
fig1
# ********* Looking at Participation Based on Race ****************
by_race_dta = raw_dta %>%
  select(Year, State, Test, Region, Total, Total.Passed,Num.Black, Black.Passed, X..Black.Passed, Num.Hispanic, Hisp.Passed,
          X..Hisp, Num.White, White.Passed, X..White.Passed) %>%
  mutate(Year = as.factor(Year),
         Test = as.factor(Test),
         state = as.factor(State),
         Region = as.factor(Region)) %>%
  arrange(Year)

csa_by_race <- subset(by_race_dta, by_race_dta$Test == "CSA")

csa_by_race_total <- csa_by_race %>%
  select(Num.Black, Num.Hispanic,Num.White, Year) %>%
  group_by(Year) %>%
  summarise(B_Tests = sum(Num.Black), H_Tests = sum(Num.Hispanic), W_Tests = sum(Num.White)) %>%
  as.data.frame()

fig2 <-plot_ly(csa_by_race_total, x = ~Year, y = ~H_Tests, type = 'bar', name ="Hispanic Tests")
fig2 <- fig2 %>% add_trace(y = ~ B_Tests, name = "Black Tests")
fig2 <- fig2 %>% add_trace(y = ~ W_Tests, name = "White Tests")
fig2 <- fig2 %>% layout (yaxis = list(title = "Tests"), title = "Number of CSA Tests by Race",barmode = "group")
fig2

csp_by_race <- subset(by_race_dta, by_race_dta$Test == "CSP")

csp_by_race_total <- csp_by_race %>%
  select(Num.Black, Num.Hispanic,Num.White, Year) %>%
  group_by(Year) %>%
  summarise(B_Tests = sum(Num.Black), H_Tests = sum(Num.Hispanic), W_Tests = sum(Num.White)) %>%
  as.data.frame()

fig3 <-plot_ly(csp_by_race_total, x = ~Year, y = ~H_Tests, type = 'bar', name ="Hispanic Tests")
fig3 <- fig3 %>% add_trace(y = ~ B_Tests, name = "Black Tests")
fig3 <- fig3 %>% add_trace(y = ~ W_Tests, name = "White Tests")
fig3 <- fig3 %>% layout (yaxis = list(title = "Tests"), title = "Number of CSP Tests by Race",barmode = "group")
fig3

# US Maps of Pass Rates of Underrepresented Populations

csp_by_gender_2019 <- subset(csp_by_gender, csp_by_gender$Year == "2019")
plot_usmap(data = csp_by_gender_2019, values = 'X..Fem.Passed', color = 'red')+
  scale_fill_continuous(name = "Female Percent Passed 2019", label = scales::comma) +
  theme(legend.position = 'right')

plot_usmap(data = csp_by_gender_2019, values = 'X..Male.Passed', color = 'red')+
  scale_fill_continuous(name = "Male Percent Passed 2019", label = scales::comma) +
  theme(legend.position = 'right')
csp_by_race_2019 <- subset(csp_by_race, csp_by_race$Year == "2019")
plot_usmap(data = csp_by_race_2019, values = 'X..White.Passed', color = 'red')+
  scale_fill_continuous(name = "% Passed (White)", label = scales::comma) +
  theme(legend.position = 'right')


plot_usmap(data = csp_by_race_2019, values = 'X..Black.Passed', color = 'red')+
  scale_fill_continuous(name = "% Passed (Black Students 2019)", label = scales::comma) +
  theme(legend.position = 'right')
csp_by_race_2019$Hisp.Per.Passed <- (csp_by_race_2019$Hisp.Passed)/(csp_by_race_2019$Num.Hispanic)*100
plot_usmap(data = csp_by_race_2019, values = 'Hisp.Per.Passed', color = 'red')+
  scale_fill_continuous(name = "% Passed (Hispanic Students 2019)", label = scales::comma) +
  theme(legend.position = 'right')
# ************* ONE Way ANOVA based on region
cspf.one.way <- aov(X..Fem.Passed ~ Region, data = csp_by_gender_2019)
summary(cspf.one.way)
TukeyHSD(cspf.one.way)

cspfnum.one.way <- aov(Fem.Total ~ Region, data = csp_by_gender_2019)
summary(cspfnum.one.way)
TukeyHSD(cspfnum.one.way)

cspm.one.way <- aov(X..Male.Passed ~ Region, data = csp_by_gender_2019)
summary(cspm.one.way)
TukeyHSD(cspm.one.way)

# ******** Create Grouped Barplots for Pass Rates

csa_by_gender_pass <- csa_by_gender %>%
  select(Fem.Total, Male.Total, Year, Fem.Passed, Male.Passed) %>%
  group_by(Year) %>%
  summarise(F_Tests = sum(Fem.Total), M_Tests = sum(Male.Total), F_Passed = sum(Fem.Passed), M_Passed = sum(Male.Passed)) %>%
  as.data.frame()

csa_by_gender_pass$Fem.Percent = csa_by_gender_pass$F_Passed/csa_by_gender_pass$F_Tests

csa_by_gender_pass$Male.Percent = csa_by_gender_pass$M_Passed/csa_by_gender_pass$M_Tests

fig5 <-plot_ly(csa_by_gender_pass, x = ~Year, y = ~Fem.Percent, type = 'bar', name =" Female Pass Rate")
fig5 <- fig5 %>% add_trace(y = ~ Male.Percent, name = "Male Pass Rate")
fig5 <- fig5 %>% layout (yaxis = list(title = "Pass Rate"), title = "CSA Pass Rate by Sex",barmode = "group")
fig5

csp_by_gender_pass <- csp_by_gender %>%
  select(Fem.Total, Male.Total, Year, Fem.Passed, Male.Passed) %>%
  group_by(Year) %>%
  summarise(F_Tests = sum(Fem.Total), M_Tests = sum(Male.Total), F_Passed = sum(Fem.Passed), M_Passed = sum(Male.Passed)) %>%
  as.data.frame()
csp_by_gender_pass$Fem.Percent = csp_by_gender_pass$F_Passed/csp_by_gender_pass$F_Tests

csp_by_gender_pass$Male.Percent = csp_by_gender_pass$M_Passed/csp_by_gender_pass$M_Tests

fig6 <-plot_ly(csp_by_gender_pass, x = ~Year, y = ~Fem.Percent, type = 'bar', name =" Female Pass Rate")
fig6 <- fig6 %>% add_trace(y = ~ Male.Percent, name = "Male Pass Rate")
fig6 <- fig6 %>% layout (yaxis = list(title = "Pass Rate"), title = "CSP Pass Rate by Sex",barmode = "group")
fig6


csp_by_race_pass <- csp_by_race %>%
  select(Num.Black, Num.Hispanic,Num.White, Year, Black.Passed, Hisp.Passed, White.Passed) %>%
  group_by(Year) %>%
  summarise(B_Tests = sum(Num.Black), H_Tests = sum(Num.Hispanic), W_Tests = sum(Num.White), W_Pass = sum(White.Passed), B_Pass = sum(Black.Passed), H_Pass = sum(Hisp.Passed)) %>%
  as.data.frame()

csp_by_race_pass$B.Percent = csp_by_race_pass$B_Pass/csp_by_race_pass$B_Tests
csp_by_race_pass$H.Percent = csp_by_race_pass$H_Pass/csp_by_race_pass$H_Tests
csp_by_race_pass$W.Percent = csp_by_race_pass$W_Pass/csp_by_race_pass$W_Tests
fig7 <-plot_ly(csp_by_race_pass, x = ~Year, y = ~H.Percent, type = 'bar', name ="Hispanic Pass Rate")
fig7 <- fig7 %>% add_trace(y = ~ B.Percent, name = "Black Pass Rate")
fig7 <- fig7 %>% add_trace(y = ~ W.Percent, name = "White Pass Rate")
fig7 <- fig7 %>% layout (yaxis = list(title = "Pass Rate"), title = "CSP Pass Rates by Race",barmode = "group")
fig7

csa_by_race_pass <- csa_by_race %>%
  select(Num.Black, Num.Hispanic,Num.White, Year, Black.Passed, Hisp.Passed, White.Passed) %>%
  group_by(Year) %>%
  summarise(B_Tests = sum(Num.Black), H_Tests = sum(Num.Hispanic), W_Tests = sum(Num.White), W_Pass = sum(White.Passed), B_Pass = sum(Black.Passed), H_Pass = sum(Hisp.Passed)) %>%
  as.data.frame()
csa_by_race_pass$B.Percent = csa_by_race_pass$B_Pass/csa_by_race_pass$B_Tests
csa_by_race_pass$H.Percent = csa_by_race_pass$H_Pass/csa_by_race_pass$H_Tests
csa_by_race_pass$W.Percent = csa_by_race_pass$W_Pass/csa_by_race_pass$W_Tests
fig8 <-plot_ly(csa_by_race_pass, x = ~Year, y = ~H.Percent, type = 'bar', name ="Hispanic Pass Rate")
fig8 <- fig8 %>% add_trace(y = ~ B.Percent, name = "Black Pass Rate")
fig8 <- fig8 %>% add_trace(y = ~ W.Percent, name = "White Pass Rate")
fig8 <- fig8 %>% layout (yaxis = list(title = "Pass Rate"), title = "CSP Pass Rates by Race",barmode = "group")
fig8

