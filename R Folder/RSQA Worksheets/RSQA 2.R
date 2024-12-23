#Setup
library(readr)
library(dplyr)
library(ggplot2)

mydata <- read.csv("CSQA_AllOrganics/Results.csv")

exp_count <- mydata %>%
  group_by(PARM_NM) %>%
  summarise(count = n())
exp_count

#rewriting exp_count to only include rows with count>50
exp_count <- exp_count[exp_count$count > 50, ]

#reorders the rows in order of decreasing count
exp_count <- exp_count[order(exp_count$count, 
                             decreasing = T), ]

View(mydata)


#Correcting the dataset

#Adding filters to remove NAs
mydatacorr = mydata %>%
  filter(RESULT_VA != "NA") %>%
  filter(REMARK_CD != "NA")

#Set all non-detects to zero
mydatacorr <- mydata %>% 
  filter(RESULT_VA != "NA") %>%
  filter (REMARK_CD != "NA") %>%
  mutate(RESULT_CORR = ifelse(REMARK_CD=="<", 0,
                              ifelse(is.na(REMARK_CD) | 
                                       REMARK_CD == "" | 
                                       REMARK_CD == "E", 
                                     RESULT_VA, 
                                     RESULT_VA)))


#Counting detections

ggplot(data = exp_count, aes(x=count)) +
  geom_histogram(binwidth=50) + 
  theme_classic() + 
  labs(title="Number of parameters",
       x="Parameter Count")

detect_count = mydatacorr %>%
  filter(REMARK_CD!="<") %>%
  group_by(PARM_NM) %>%
  summarise(count = n())
 
# Plot histogram of detect_count
ggplot(data=detect_count, aes(x=count)) +
  geom_histogram(binwidth=50) +
  theme_classic() +
  labs(y="Detection count")

# Record how many different parameters were measured
num_parameters <- nrow(detect_count)
cat("Number of different parameters measured:",
    num_parameters, "\n")

#reorders the rows in order of decreasing count
detect_count <- detect_count[order(detect_count$count, 
                             decreasing = T), ]

#Plotting 5 most detected chemicals
top_5_corrdata <- mydatacorr[mydatacorr$PARM_NM %in% 
                       detect_count$PARM_NM[1:5], ]
ggplot(data=top_5_corrdata, aes(x=PARM_NM, y=RESULT_CORR)) +
  geom_jitter(aes(color=PARM_NM)) +
  theme(axis.text.x=element_blank()) +
  facet_grid(PARM_NM ~ .) +
  facet_wrap(vars(PARM_NM), scales="free")

#Count non-detects
measured_count = mydatacorr %>%
  group_by(PARM_NM) %>%
  summarise(count=n(), 
            nonzerocount=sum(RESULT_CORR!=0.0)) %>%
  mutate(percentdetections=nonzerocount/count*100)
measured_count = 
  measured_count[order(measured_count$percentdetections, 
                       decreasing=T), ]

#Plot histogram of measured_count
ggplot(data=measured_count, aes(x=nonzerocount)) +
  geom_histogram(binwidth=50) +
  theme_classic() +
  labs(y="Nonzero count")

# Record how many different parameters were measured
num_parameters <- nrow(measured_count)
cat("Number of different parameters measured:",
    num_parameters, "\n")




