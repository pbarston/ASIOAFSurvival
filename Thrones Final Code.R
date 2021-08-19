#load and install
library(survival) #for computing survival analyses
library(survminer) #summarizing and visualizing the results of survival analysis
library(tidyverse) 
library(ggplot2)

#load the data
thrones=read.csv("./Data/character-deaths.csv")
battles = read.csv("./Data/battles.csv") #for later
predictions = read.csv("./Data/character-predictions.csv") #for later

#check out initial structure 
str(thrones) #do not make anything a factor!

#remove the "House" leading string from allegiances
#before:
table(thrones$Allegiances)
#after:
thrones$Allegiances=str_remove(thrones$Allegiances,"House ")
table(thrones$Allegiances)

#create the "intro.chapter" column - to fill in later
colnames(thrones)[which(names(thrones) == "Book.Intro.Chapter")] <- "Intro.Chapter"
#one last check
str(thrones)

#manual, character-based cleaning
#all done via ASOIAF wiki - others could be out there!
#change those who need to be dead in book 2
thrones$Book.of.Death[thrones$Name=="Cressen"]=2
thrones$Book.of.Death[thrones$Name=="Shyra Errol"]=2
#manually update the intro NAs
thrones$Intro.Chapter[thrones$Name=="Aemon Costayne"]=33
thrones$Intro.Chapter[thrones$Name=="Aemon Estermont"]=71
thrones$Intro.Chapter[thrones$Name=="Alyn Estermont"]=71
thrones$Intro.Chapter[thrones$Name=="Bearded Ben"]=47
thrones$Intro.Chapter[thrones$Name=="Big Walder Frey"]=60
thrones$Intro.Chapter[thrones$Name=="Cuger"]=42
thrones$Intro.Chapter[thrones$Name=="Garse Goodbrook"]=71
thrones$Intro.Chapter[thrones$Name=="Groleo"]=9
thrones$Intro.Chapter[thrones$Name=="Joss Stilwood"]=31
thrones$Intro.Chapter[thrones$Name=="Muttering Bill"]=83
thrones$Intro.Chapter[thrones$Name=="Orphan Oss"]=34
thrones$Intro.Chapter[thrones$Name=="Tytos Frey"]=53
#update the book death NAs
thrones$Book.of.Death[thrones$Name=="Lennocks"]=3
thrones$Book.of.Death[thrones$Name=="Pate (Old)"]=3
#update the chapter death NAs
thrones$Death.Chapter[thrones$Name=="Garse Goodbrook"]=82
thrones$Death.Chapter[thrones$Name=="Lysa Tully"]=81
thrones$Death.Chapter[thrones$Name=="Merrett Frey"]=82
thrones$Death.Chapter[thrones$Name=="Petyr Frey"]=82
thrones$Death.Chapter[thrones$Name=="Tytos Frey"]=82
thrones$Death.Chapter[thrones$Name=="Alester Florent"]=47
thrones$Death.Chapter[thrones$Name=="Aenys Frey"]=74
thrones$Death.Chapter[thrones$Name=="Kevan Lannister"]=73
thrones$Death.Chapter[thrones$Name=="Pycelle"]=73
thrones$Death.Chapter[thrones$Name=="Shyra Errol"]=71

#need to impute how long a character is around for
#first, create the df with all the chapter counts
chapters=data.frame("Book Name" = c("GoT","CoK","SoS","FfC","DwD"),
                    "Chapter Count" = c(74,71,83,47,74))

#create the "book of intro" column
thrones = thrones %>%
  mutate(Book.of.Intro=ifelse(GoT>0,1,
                              ifelse(CoK>0,2,
                                     ifelse(SoS>0,3,
                                            ifelse(FfC>0,4,5
                                            )))))
#change Harmune book of intro
#harmune needs to go here - book of intro column doesn't exist in beginning
thrones$Book.of.Intro[thrones$Name=="Harmune"]=3
thrones$SoS[thrones$Name=="Harmune"]=1


#now, create our target "length of time in the series" column
#first, find the sum of chapters
sum(chapters$Chapter.Count)
#nothing can exceed that 349
#create the max possible value
max.possible = sum(chapters$Chapter.Count)
#add 1 to each column to account for prologue = 0 in the dataset but not the wiki
thrones = thrones %>%
  mutate(Intro.Chapter=Intro.Chapter+1,
         Death.Chapter=Death.Chapter+1)
#create the chapter column amounts
count.GoT=chapters$Chapter.Count[chapters$Book.Name=="GoT"]
count.CoK=chapters$Chapter.Count[chapters$Book.Name=="CoK"]
count.SoS=chapters$Chapter.Count[chapters$Book.Name=="SoS"]
count.FfC=chapters$Chapter.Count[chapters$Book.Name=="FfC"]
count.DwD=chapters$Chapter.Count[chapters$Book.Name=="DwD"]

#then populate the new column
thrones = thrones %>%
  mutate(sequential.intro=ifelse(GoT>0,Intro.Chapter, #can also just make "GoT" into "book of intro"
                                 ifelse(CoK>0,count.GoT+Intro.Chapter,
                                        ifelse(SoS>0,count.GoT+count.CoK+Intro.Chapter,
                                               ifelse(FfC>0,count.GoT+count.CoK+count.SoS+Intro.Chapter,count.GoT+count.CoK+count.SoS+count.FfC+Intro.Chapter
                                               )))))

#now create sequential death (not the same equation same as the other one)
thrones = thrones %>%
  mutate(sequential.death=ifelse(Book.of.Death==1,Death.Chapter, #can also just make this "book of intro"
                                 ifelse(Book.of.Death==2,count.GoT+Death.Chapter,
                                        ifelse(Book.of.Death==3,count.GoT+count.CoK+Death.Chapter,
                                               ifelse(Book.of.Death==4,count.GoT+count.CoK+count.SoS+Death.Chapter,count.GoT+count.CoK+count.SoS+count.FfC+Death.Chapter
                                               )))))


#now create the length of time - the target column
thrones = thrones %>%
  mutate(chapter.span=ifelse(!is.na(Death.Chapter),
                             sequential.death - sequential.intro,
                             max.possible - sequential.intro))
#now create the status column
thrones = thrones %>%
  mutate(is.dead=ifelse(!is.na(Death.Chapter),
                        1,0)) #do not make this a factor!
#final checks: 
str(thrones)
table(thrones$is.dead)



#create and plot a baseline survival model
#length of time between introduction and either death or end of book 5
#time-to-event for a group of individuals
thrones.km1 <- survfit(Surv(chapter.span,is.dead)~1, data=thrones) #create a survival object via Surv()
summary(thrones.km1)
plot(thrones.km1, xlab = "Time", ylab = "Survival Probability")

# Compare survival rate by gender (relationship of variables to event)
table(thrones$Gender)
#survival rates
km2 <- survfit(Surv(chapter.span,is.dead)~Gender, data=thrones)
summary(km2)
#observations are divided into groups above
#to plot:
plot(km2, xlab = "Time", ylab = "Survival Probability")
#now, make it a fit that is measured by gender
km2.plot=ggsurvplot(
  fit=survfit(Surv(chapter.span,is.dead)~Gender, data=thrones),
  xlab='Time',
  ylab='Survival Probability'
)
km2.plot
#gender affects survival likelihood - being a male leads to lower survival rate

# Compare survival rate by nobility (1 is noble)
table(thrones$Nobility)
km3 <- survfit(Surv(chapter.span,is.dead)~Nobility, data=thrones)
summary(km3)
plot(km3, xlab = "Time", ylab = "Survival Probability")
km3.plot=ggsurvplot(
  fit=survfit(Surv(chapter.span,is.dead)~Nobility, data=thrones),
  xlab='Time',
  ylab='Survival Probability'
)
km3.plot
#nobility affects survival likelihood - being noble leads to higher survival rates

#compare survival rate by Stark and Lannister
table((thrones$Allegiances)[thrones$Allegiances=="Stark"|thrones$Allegiances=="Lannister"])
km4 <- survfit(Surv(chapter.span,is.dead)~Allegiances, data=thrones %>% filter(Allegiances=="Stark"|Allegiances=="Lannister"))
summary(km4)
plot(km4, xlab = "Time", ylab = "Survival Probability")
km4.plot=ggsurvplot(
  fit=survfit(Surv(chapter.span,is.dead)~Allegiances, data=thrones %>% filter(Allegiances=="Stark"|Allegiances=="Lannister")),
  xlab='Time',
  ylab='Survival Probability'
)
km4.plot
#allegiance affects survival likelihood - being Lannister leads to higher survival rates

#now, graph multiple plots simultaneously
#experimenting with theme adjusting as well as arrange_ggsurvplots
#for arrange_ggsurvplots, we need to fit the curves - done above
#make the list and components
arranged.plots=list()
arranged.plots[[1]]=km2.plot
arranged.plots[[2]]=km3.plot #might have to adjust themes

# Arrange multiple ggsurvplots and print the output
arranged1=arrange_ggsurvplots(arranged.plots, print = FALSE,
                              title = "Survival Rates by Gender and Nobility",
                              ncol = 2, nrow = 1)

#to check, save the output (printing is problematic for quartz for whatever reason)
# Arrange and save into pdf file
ggsave("./Images/arranged1.jpg", arranged1)


#this works, how about if you want to make some edits to the ggplots you made above
#use the fits themselves, not the plots
arranged.plots2=list()
arranged.plots2[[1]]=ggsurvplot(km2,ggtheme = theme_bw(),xlab="Chapter Span")
arranged.plots2[[2]]=ggsurvplot(km3,ggtheme = theme_bw(),xlab="Chapter Span")

arranged2=arrange_ggsurvplots(arranged.plots2, print = FALSE,
                              title = "Survival Rates by Gender and Nobility",
                              ncol = 2, nrow = 1)
ggsave("./Images/arranged2.jpg", arranged2)

#add a caption, change xlabel font size, etc
arranged.plots3=list()
arranged.plots3[[1]]=ggsurvplot(km2,
                                xlab="Chapter Span")+
  theme_survminer(font.x = c(10, "bold.italic", "red"))
arranged.plots3[[2]]=ggsurvplot(km3,
                                xlab="Chapter Span",
                                caption="h/t Survminer")+
  theme_survminer(font.caption = c(8,"purple"),
                  font.x = c(10, "bold.italic", "red"))

arranged3=arrange_ggsurvplots(arranged.plots3, print = FALSE,
                              title = "Survival Rates by Gender and Nobility",
                              ncol = 1, nrow = 2)
ggsave("./Images/arrange3.jpg", arranged3)

#make a dataframe out of the summary - not necessary but interesting:
km3.updated.summary=surv_summary(km3)
head(km3.updated.summary)


#updating plots:
#let's do nobility - as you can see ,more balanced
table(thrones$Nobility)
#part 1: Change color, add a risk.table color by strata, xlimit
#keep the x label and color
km3.updated.plot=
  ggsurvplot(km3, #survfit object with calculated stats
             xlab="Chapter Span",
             risk.table = TRUE, # Add risk table
             risk.table.col = "strata", # Change risk table color by groups
             linetype = "strata", # Change line type by groups
             ggtheme = theme_bw(), # Change ggplot2 theme
             palette = c("#E7B800", "#2E9FDF"),
             xlim=c(0,300))+
  theme_survminer(font.x = c(10, "bold.italic", "red"))

km3.updated.plot
#part 2: further customized with break.time, 
#risk.table.y.text = T to show labels
#legend.labs to change legend labels
#palette can be changed to anything
km3.updated.plot2=ggsurvplot(km3,
                             break.time.by=50,
                             xlab="Chapter Span",
                             risk.table.col="strata",
                             risk.table = TRUE,  # absolute number and percentage at risk.
                             risk.table.y.text = T,# show bars instead of names in text annotations
                             # in legend of risk table.
                             legend.labs = 
                               c("Non-Noble","Noble"),    # change legend labels.
                             palette = c("#E7B800", "#2E9FDF"),
                             xlim=c(0,300))+
  theme_survminer(font.x = c(10, "bold.italic", "red"))

km3.updated.plot2
#part 3: changing around  function
# cumulative events (the inverse of survival prob)
km3.updated.plot3=
  ggsurvplot(km3,
             risk.table.col = "strata", # Change risk table color by groups
             palette = c("#E7B800", "#2E9FDF"),
             fun = "event",
             break.time.by=50,
             xlab="Chapter Span",
             risk.table = TRUE,  # absolute number and percentage at risk.
             risk.table.y.text = T,# show bars instead of names in text annotations
             # in legend of risk table.
             legend.labs = 
               c("Non-Noble","Noble"),    # change legend labels.
             xlim=c(0,300))+
  theme_survminer(font.x = c(10, "bold.italic", "red"))

km3.updated.plot3
#cumhaz: H(t)=−log(survivalfunction)=−log(S(t)). 
#The cumulative hazard (H(t)) can be interpreted as the cumulative force of mortality.
#In other words, it corresponds to the number of events that would be expected for each individual by time t if the event were a repeatable process.
km3.updated.plot4=
  ggsurvplot(km3,
             risk.table.col = "strata", # Change risk table color by groups
             palette = c("#E7B800", "#2E9FDF"),
             fun = "cumhaz",
             break.time.by=50,
             xlab="Chapter Span",
             risk.table = TRUE,  # absolute number and percentage at risk.
             risk.table.y.text = T,# show bars instead of names in text annotations
             # in legend of risk table.
             legend.labs = 
               c("Non-Noble","Noble"),    # change legend labels.
             xlim=c(0,300))+
  theme_survminer(font.x = c(10, "bold.italic", "red"))

km3.updated.plot4
#not a great example dataset but still insightful

#log rank: 
#Essentially, the log rank test compares the observed number of events in each group 
#to what would be expected if the null hypothesis were true (i.e., if the survival curves were identical). 

#The function survdiff() [in survival package] can be used to compute log-rank test comparing two or more survival curves.
#survdiff() can be used as follow:
km3.surv.diff <- survdiff(Surv(chapter.span,is.dead)~Nobility, data=thrones)
km3.surv.diff
#The log rank test for difference in survival gives a p-value of 
#p = 2e-05, indicating that the nobility groups differ significantly in survival.

#complex curves:
#In this section, we’ll compute survival curves using the combination of multiple factors. Next, we’ll facet the output of ggsurvplot() by a combination of factors
require("survival")
attach(thrones)
complex.fit <- survfit( Surv(chapter.span,is.dead) ~ Gender+Nobility,
                        data = thrones )


#Visualize the output using survminer 
#The plot below shows survival curves by the nobility variable faceted according to the gender.
# Plot survival curves by nobility and facet by gender
complex.fit.plot <- ggsurvplot(complex.fit,
                               break.time.by=50,
                               xlab="Chapter Span",
                               xlim=c(0,300))+
  theme_survminer(font.x = c(10, "bold.italic", "red"))

complex.fit.plot$plot+
  facet_wrap(~Gender, ncol=1)+
  theme(legend.position = "right")

#Cox PH model - testing signficance of multivariate models
#Consequently, the Cox model is a proportional-hazards model: 
#the hazard of the event in any group is a constant multiple of the hazard in any other. (x times more likely, etc)

# Using Proportional Hazards to test whether an attribute is significant in survival
#first = is nobility statistically significant?
#similar to log test
coxph <- coxph(Surv(chapter.span,is.dead)~Nobility, method="breslow", data=thrones) #remember that surv creates the survival object
summary(coxph)
#n = starting amount
#number of events in this case is death (all the 1s)
#DV in cox regression is hazard aka risk of something happening
#measuring EVENTS, not survival
#can see this p value is the same as the logrank test  - excellent

#coefficient is negative
#because a dummy variable, nobility = 1 is DECREASED risk of event occurring aka dying
#hazard ratio is e^-.4935
#aka 61% as likely (exp coef)  if nobility is 1 (reduces the hazard ratio by a factor of .61, or 39%)
#or if the nobility = 0 is 1.64 times more likely

#let's try it with allegiances
coxph <- coxph(Surv(chapter.span,is.dead)~Allegiances, method="breslow", data=thrones %>%
                 filter(Allegiances=="Lannister"|Allegiances=="Stark"))
summary(coxph)
km4.plot #to compare, makes sense

#creating the other variables:
#is a house often in battle? 
#let's call it "is.aggressive"
#table with count of # of times a house was "attacker_1"
#then filter on arbitrary 6 or more times for aggressive
battles.modified = battles %>%
  count(attacker_1,name = "instances") %>%
  filter(instances>=6)

#create the variable
thrones = thrones %>%
  mutate(is.aggressive=Allegiances %in% battles.modified$attacker_1)


#does a character have a title, any title whatsoever?
#clean the column:
predictions$title[predictions$title==""]="NA"
predictions$title[predictions$title=="[1]"]="NA"
predictions.modified = predictions %>%
  filter(title!="NA")
#create the variable
thrones = thrones %>%
  mutate(has.title = Name %in% predictions.modified$name)

#now, for multivariate model
#test nobility, gender, is.aggressive, has.title, book of intro

# To test all independent variables
attach(thrones)
#first- grab all the variables:
X <- cbind(Nobility,Gender,is.aggressive,has.title,Book.of.Intro)
coxph2 <- coxph(Surv(chapter.span,is.dead)~X, method="breslow", data=thrones)
summary(coxph2)

#running model, you get different results than before

#significant: nobility (slightly), gender, has a title
#not significant: is from an aggressive house, what book they were introduced in
#you can see how nobility's significance really declines here

#multiplicative - holding all covariates CONSTANT and you get this likelihood
#i.e. can't yet suss out interaction 



