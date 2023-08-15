#loading the necessary libraries
library(lubridate)
library(reshape2)
library(tidyverse)

#generating random dates in the year 2017
date <- as.data.frame(sample(seq(as.Date('2017/01/01'), as.Date('2017/12/31'), by="day"), 500, replace = T))

#generating random revenue values
revenue <- as.data.frame(runif(500, min=0, max=1000))
?runif
#generating random user_ID's
users <- as.data.frame(sample(1:300, 500, replace=TRUE))

data <- cbind(date,revenue,users)
colnames(data) <- c("date","revenue","users")
head(data)

data$date <- as.character(data$date)
data$month <- month(data$date)
data$year <- year(data$date)


length(unique(data$users))
data2
#changing the date format and grouping by date
data2 <-data %>%
  mutate(date = as.character(date) %>% as.Date(format = "%Y-%m-%d"))
data3<- format(data2, format="%Y-%m")
str(data3)

cohort <- data3 %>% #store data in table called cohort
  group_by(users) %>% #group all the users/clients together
  mutate(first = min(date)) %>% #for every user/client take the first period
  group_by(first, date) %>% #group by this first period + other periods
  summarise(users = n()) %>% #for each combinations, count the number of users
  spread(date, users) #make columns with the period names

head(cohort)
View(cohort)

#align the table created a bove to the left like a normal cohort table starting from column 2
shiftrow <- function(v){
  #put a vactor in, strip off leading NA values, and place that amount at the end
  first_na_index <- min(which(!is.na(v)))
  
  #return that bit to the end, and pad with NA's
  c(v[first_na_index:length(v)], rep(NA, first_na_index-1))
}

#create a new dataframe, with shifted rows(ad keep the first row)
shifted <- data.frame(
  cohort = cohort$first,
  t(apply(select(as.data.frame(cohort), 2:ncol(cohort)), #from the 2nd column to the end
          1, #for every row
          shiftrow
  ))
)

View(shifted)
#make the column names readable
#first should be "cohort" and the rest dates

colnames(shifted) <-c("cohort", sub("","month.", str_pad(1:(ncol(shifted)-1),2, pad = "0")))


#percentages
#we want every year and month to be expressed as a percentange
#create new table for this . We divide all year,month columns by the first year,month of that row
shifted_pct <- data.frame(
  cohort = shifted$cohort, #first row
  shifted[,2:nrow(shifted)] / shifted[["month.01"]] #rest:divide by year&month1
)
View(shifted_pct)
#####################################################################################################################
#                                             PLOTS
######################################################################################################################

#ggplot loves long data.Let's melt it. One for the absolute values, one for the pcts
plotdata_abs <- gather(shifted,     "cohort_age", "people"  ,2:ncol(shifted    ))
plotdata_pct <- gather(shifted_pct, "cohort_age", "percent" ,2:ncol(shifted_pct))

?gather()
?pivot_longer

# now add some data.. we need pretty labels..
# first bit is the length of the width of the wide column (minus 1, that's the cohort name)
# that contains the absolute numbers
# last bit is the rest, those are percentages.
labelnames <- c( plotdata_abs$people[1:(ncol(shifted)-1)],
                 plotdata_pct$percent[(ncol(shifted)):(nrow(plotdata_pct))])

# we need pretty labels.
pretty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") # for NA values, skip the label
}

# create the plot data
plotdata <- data.frame(
  cohort     = plotdata_pct$cohort,
  cohort_age = plotdata_pct$cohort_age,
  percentage = plotdata_pct$percent,
  label      = pretty_print(labelnames)
)

#plot (with reordered y axis, oldesr group on top)
p <- ggplot(plotdata, aes(x = cohort_age, y = reorder(cohort, desc(cohort)))) +
  geom_raster(aes(fill = percentage)) +
  #scale_fill_gradient(low = "white", high = "red") + coord_fixed() +
  scale_fill_continuous(guide = FALSE) + coord_equal(ratio = 1) + # no legend
  geom_text(aes(label = label), size = 4, color = "white") +
  xlab("cohort age") + ylab("cohort") + 
  ggtitle(paste("Retention table (cohort) for E-Commerse Business"))
plot(p)

As you can see from the graph above, the company needs to really work on retaining their customers

######################################################################################################################
#                                                  PLOT2
######################################################################################################################
a <- shifted[,c(2:13)]
b <- shifted[,2]
retention <- apply(a, 2, function(a) a/b )
retention <- data.frame(cohort=(shifted$cohort), retention)
retention <- retention[,-2]

cohort_plot <- melt(retention, id.vars = "cohort")
colnames(cohort_plot) <- c("cohort", "month", "retention")
cohort_plot <- filter(cohort_plot, retention != 0)
c <- ggplot(cohort_plot, aes(x=month, y=retention, group=cohort, colour=cohort))
c <-c + geom_line(size=2, alpha=0.5) +
  geom_point(size=3, alpha=1) +
  geom_smooth(aes(group=1), method = "loess", size=3, colour="turquoise", se=FALSE) +
  labs(title="Cohorts Retention ratio")

c
install.packages('digest', repos='http://cran.us.r-project.org')
c + scale_colour_brewer(palette="Set3") + theme(panel.background = element_blank())
