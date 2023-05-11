# Load libraries that the project will use
library(here)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)


# -------------IMPORT DATA-------------------

# Use library(here) to import the data and assign it to a data frame 
df <- read_csv(here("Data", "raw_data.csv"))

# Check data frame
head(df)



#-------------TIDYING DATA-------------------

# Make new data frame looking only at columns of interest which have been renamed for ease and have had non-numerical information removed from 
#the alzheimers and parkinsons columns, and numerical information discarded from the mental health column. 
df1 <- df %>% 
  rename(ppt_id = "ID", dementia = "DEMENTIA", alzheimers = "ALZHEIMER", mental_health = "MNTLHLTH", parkinsons = "PARKINSON", sleep_length = "ACTIGRAPH_SLEEP") %>%
  select(ppt_id, alzheimers, parkinsons, mental_health, sleep_length) %>%
  mutate(alzheimers = parse_number(str_sub(alzheimers, start = 1))) %>%
  mutate(parkinsons = parse_number(str_sub(parkinsons, start = 1))) %>%
  mutate(mental_health = str_sub(mental_health, start = 5, end = 13))

# Check the first few lines of the data
head(df1)

# Remove NA values from the data. 
which(is.na(df1), arr.ind = TRUE)
df1 <- na.omit(df1)

# Change values in Parkinson's disease column from '1' to '2'.  
df1$parkinsons[df1$parkinsons == '1'] <- 2

# Add the Parkinson's disease column and Alzheimer's disease columns together to make a new disease state column, where 0 is neither, 1 is Alzheimer's, 2 is Parkinson's and 3 is both. 
df1$disease_state <- df1$parkinsons + df1$alzheimers


#-------------CHECKING DATA------------------

# Check to see that disease state value 1 represents participants with Alzheimer's disease.
filter(df1, disease_state == '1')

# Check to see that disease_state value 2 represents participants with Parkinson's disease.
filter(df1, disease_state == '2')

# Check to see that disease_state value 3 represents participants with both diseases. 
filter(df1, disease_state == '3')


# ------- DATA PREPARATION ----------
  
# Make new data frame grouping the mean length of sleep by disease state and mental health.
df2 <- df1 %>%
  group_by(disease_state, mental_health) %>%
  summarise(mean_sleep_length = mean(sleep_length))

# Change values in disease_state column from numbers to reflect the disease state in words.
for(i in 1:14) {
  if (df2$disease_state[i] == 0){
    df2$disease_state[i] = 'Neither'} 
  else if (df2$disease_state[i] == 1){
    df2$disease_state[i] = 'Alzheimers'}
  else if (df2$disease_state[i] == 2){
    df2$disease_state[i] = 'Parkinsons'}
  else if (df2$disease_state[i] == 3){
    df2$disease_state[i] = 'Both'}
  }

# Arrange the mental health column so it is ordered from poor to excellent.
df2$mental_health <- factor(df2$mental_health, levels = c('poor', 'fair', 'good', 'very good', 'excellent'))

# Arrange the disease state column so it is ordered Neither, Alzheimers, Parkinsons, Both
df2$disease_state <- factor(df2$disease_state, levels = c('Neither', 'Alzheimers', 'Parkinsons', 'Both'))

# Add rows to the data frame to have data to plot for the 'excellent' and 'very good' conditions for the Alzheimers disease state
df2[nrow(df2) + 1,] <- list('Alzheimers', 'very good', 0)
df2[nrow(df2) + 1,] <- list('Alzheimers', 'poor', 0)

# Add rows to the data frame to have data to plot for the 'poor', 'good', 'very good' and 'excellent' conditions for the combined disease state 
df2[nrow(df2) + 1,] <- list('Both', 'poor', 0)
df2[nrow(df2) + 1,] <- list('Both', 'good', 0)
df2[nrow(df2) + 1,] <- list('Both', 'very good', 0)
df2[nrow(df2) + 1,] <- list('Both', 'excellent', 0)

# Save the data frame as a file 
write.csv(df2, here("Data/Tidy_Data.csv"))


#------------MAKING THE PLOT--------------------

# Make a grouped bar graph that is grouped by mental health, with disease state along the x axis, mean sleep length along the y axis
p <- ggplot(df2, aes(x = disease_state, y = mean_sleep_length, fill = mental_health)) 
p + geom_col(color = 'black', position = position_dodge2(width=.9, preserve="single")) +
  labs(x = substitute(paste(bold("Disease state"))), y = substitute(paste(bold("Average Length of Sleep (mins)"))), title = 
         "Average length of sleep in Alzheimer's and Parkinson's disease grouped by mental health") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(title = substitute(paste(bold('Mental health'))))) +
  scale_fill_brewer(breaks=c('poor', 'fair', 'good', 'very good', 'excellent'), palette = "Pastel1")
 
# Save the figure  
ggsave(filename = 'figures/fig_220225339.png')
  