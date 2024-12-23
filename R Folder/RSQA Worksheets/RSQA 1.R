setwd("~/R Folder/RSQA Worksheets")
library(readr)
library(dplyr)
library(ggplot2)

mydata <- read.csv("CSQA_AllOrganics/Results.csv")

# Count frequency of each parameter
exp_count <- mydata %>%
  group_by(PARM_NM) %>%
  summarise(count = n())
exp_count


# Plot histogram of exp_count
ggplot(data = exp_count, aes(x=count)) +
  geom_histogram(binwidth=50) + 
  theme_classic() + 
  labs(title="Number of parameters",
       x="Parameter Count")

# Record how many different parameters were measured
num_parameters <- nrow(exp_count)
cat("Number of different parameters measured:",
    num_parameters, "\n")

# Identify how many parameters measured once
num_measured_once <- nrow(filter(exp_count, 
                                 count == 1))
cat("Number of parameters measured only once:",
    num_measured_once, "\n")

# Find the parameter measured the most
most_measured_parameter <- exp_count %>%
  filter(count == max(count)) %>%
  pull(PARM_NM)
cat("Parameter measured the most often:",
    most_measured_parameter, "\n")

#rewriting exp_count to only include rows with count>50
exp_count <- exp_count[exp_count$count > 50, ]

#reorders the rows in order of decreasing count
exp_count <- exp_count[order(exp_count$count, 
                             decreasing = T), ]
head(exp_count)

#Making dataframe for Atrazine, bs
Atrazine_data = mydata[mydata$PARM_NM 
                       == "Atrazine, bs", ]

#Making scatterplot for Atrazine measurement
ggplot(data=Atrazine_data, aes(x=PARM_NM, 
                               y=RESULT_VA)) +
  geom_jitter() +
  labs(title="Atrazine, bs", x="Chemical", 
       y="Concentration Detected")

#Making jitter plot of top 5 data
top_5_data <- mydata[mydata$PARM_NM %in% 
                       exp_count$PARM_NM[1:5], ]
ggplot(data=top_5_data, aes(x=PARM_NM, y=RESULT_VA)) +
  geom_jitter(aes(color=PARM_NM)) +
  theme(axis.text.x=element_blank()) +
  facet_grid(PARM_NM ~ .) +
  facet_wrap(vars(PARM_NM), scales="free")










