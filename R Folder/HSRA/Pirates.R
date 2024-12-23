# Pirate Worksheet

# Working directory
setwd("~/R Folder/HSRA")

# Packages Libraries and Databases
library(yarrr)
library(dplyr)
library(ggplot2)

# Pirates Database
names(pirates)
head(pirates)
str(pirates)
View(pirates)

#Descriptive Statistics
average_age = pirates %>%
  summarize(mean_age = mean(age))

max_height = pirates %>%
  summarize(maxheight = max(height))
max_height

men_women_count = pirates %>%
  group_by(sex) %>%
  summarize(count = n())
men_women_count

men_women_average_age = pirates %>%
  group_by(sex) %>%
  summarize(mean_age = mean(age))
men_women_average_age

summary(pirates)

#Data Plotting: Scatter plots
viz <- ggplot(data=pirates, aes(x=height, y=weight)) +
  geom_point(alpha=0.5) +
  labs(title="Pirates weight with respect to height", 
       x="height (cm)", y="weight (kg)") +
  geom_smooth(method="lm", se=FALSE)
viz

age_parrots <- ggplot(data=pirates, 
                      aes(x=age, y=parrots)) +
  geom_point(pch=16, 
             col=gray(level=0.5, alpha = 0.6)) +
  theme_classic() +
  labs(title="Pirate Age VS Number of Parrots")
age_parrots

#Data Plotting: Bar Graphs
myplot <- ggplot(data=pirates,
                 aes(sex)) +
  geom_bar(fill="#bf5700") +
  labs(title="Number of Pirates by Sex")
myplot

myplot <- ggplot(data=pirates,
                 aes(sex)) +
  geom_bar(aes(fill=headband), position="fill")
myplot

beard_summary <- pirates %>%
  group_by(sex) %>%
  summarize(
    mean_beard_length = mean(beard.length),
    sd_beard_length = sd(beard.length)
  )
beard_summary

beard_plot = ggplot(data=beard_summary,
                    aes(x=sex, y=mean_beard_length)) +
  geom_col() +
  geom_errorbar(aes(ymin=mean_beard_length - sd_beard_length, 
                ymax=mean_beard_length + sd_beard_length))
beard_plot

#Data Plotting: Box Plots
box_plot = ggplot(data=pirates, 
                  aes(x=sex, y=age, fill=headband)) +
  geom_boxplot() +
  facet_wrap(~headband)
box_plot

#Data Plotting: Violin plots
pirateplot(formula=age~sword.type, data=pirates, 
           main="Pirateplot of ages by favorite sword")

pirateplot(formula=height~sex, 
           data=pirates, theme=3, pal="pony",
           main="height range by sex")

piratepal(palette="all",
          plot.result=TRUE)

#Hypothesis Testing: Histograms
avg_age_pirates_headband = pirates %>%
  group_by(headband) %>%
  summarize(mean_age = mean(age))
avg_age_pirates_headband

no_headband = pirates %>%
  filter(headband=="no")
head(no_headband, n=10)

no_headband_shorter = pirates %>%
  filter(headband=="no") %>%
  select(headband, age)

yes_headband_shorter = pirates %>%
  filter(headband=="yes") %>%
  select(headband, age)

yes_histogram = ggplot(data=yes_headband_shorter, 
                       aes(x=age)) +
  geom_histogram(color="black", fill="#bf5700",
                 binwidth=2) + 
  labs(title="Number of headband-wearers by age") +
  theme_classic()
yes_histogram

no_histogram = ggplot(data=no_headband_shorter, 
                       aes(x=age)) +
  geom_histogram(color="black", fill="#bf5700",
                 binwidth=2) + 
  labs(title="Number of non-headband-wearers by age") +
  theme_classic()
no_histogram

headband_histograms = ggplot(data=pirates, 
                             aes(x=age)) +
  geom_histogram(color="black", fill="white",
                 binwidth=2) + 
  theme_classic() +
  facet_grid(.~headband) +
  geom_vline(data = avg_age_pirates_headband, 
             aes(xintercept = mean_age), color = "blue", 
             linetype = "dashed")
headband_histograms

box_plot = ggplot(data=pirates, 
                  aes(x=headband, y=age)) +
  geom_boxplot() 
box_plot

#Hypothesis Testing: Statistical tests
age_headband.htest <- t.test(no_headband_shorter$age, 
                           yes_headband_shorter$age, 
                           paired=FALSE, 
                           alternative='two.sided')
age_headband.htest$p.value

age_headband.htest <- t.test(formula = age ~ headband, 
                             data = pirates)
age_headband.htest$p.value

age_headband.htest <- pirates %>%
  t.test(formula = age ~ headband, data=.)
age_headband.htest$p.value

cor.test(formula = ~ height + weight, data = pirates)

box_plot = ggplot(data=pirates, 
                  aes(x=sword.type, y=tattoos)) +
  geom_boxplot() 
box_plot

results.aov <-aov(tattoos ~ sword.type, pirates)
summary(results.aov)

#A little programming
no_headband[1,1] 
no_headband[1:3,4:8] 
no_headband[3:1, c(12,11,16)] 

swordtime = pirates[, "sword.time"]
swordtime

mean_swordtime = mean(swordtime)
mean_swordtime

length_swordtime = length(swordtime)
length_swordtime

sum_swordtime = sum(swordtime)
sum_swordtime

swordresults = c(mean_swordtime, 
                 length_swordtime, 
                 sum_swordtime)
swordresults
sample(swordresults, 1)

#Coinflip
    # Vector for CoinFlip outcomes
coin = c("head", "tail")
print(sample(coin, 1))

#if statement
if((sample(coin, 1)) == "tail") {
  print("TAILS Woohoo!")
} else {
  print("HEADS Woohoo!")
}

#Count
numtails = 0

if((sample(coin, 1)) == "tail") {
  print("TAILS Woohoo!")
  numtails = numtails + 1
} else {
  print("HEADS Woohoo!") 
}
print(numtails)

#Loop 5 times
for(i in 1:5) {
  if((sample(coin, 1)) == "tail") {
    message <- "TAILS Woohoo!"
  } else {
    message <- "HEADS Woohoo!"
  }
  print(message)
  
}

#numtails
  coin = c("head", "tail")
numtails = 0
for(i in 1:5) {
if((sample(coin, 1)) == "tail") {
  numtails = numtails + 1
} else {}


}
numtails

#function
toss = function(n) {
  coin = c("head", "tail")
  numtails = 0
  for(i in 1:n) {
    if((sample(coin, 1)) == "tail") {
      numtails = numtails + 1
    } else {}
    
    
  }
  return(numtails)
}
toss(40)

#5000 experiments
vector <- rep(NA,5000)

for(i in 1:5000) {
  vector[i] = toss(40)
}
View(vector)

coinexpf <- as.data.frame(vector)
View(coinexpf)

ggplot(data=coinexpf, aes(vector)) +
  geom_histogram(color="black", 
                 fill="#bf5700",
                 binwidth=2) +
  labs(title="Coinflip", x="experiment")



    




  

