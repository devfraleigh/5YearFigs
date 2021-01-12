#5 year averages of C & N
#using Beluga_Bulk_R & Beluga.Metadata_R
library(ggplot2)
library(tidyr)
library(dplyr) #her royal highness
library(cowplot)

#**************STEP 1: ORGANIZATION & CLEANING**********************
#join both dfs using Individual column as identifier
beluga <- left_join(Beluga_Bulk_R, Beluga.Metadata_R...Sheet1, by = "Individual")
#select columns I want
beluga <- select(beluga, Individual, d13Ccor, d15N, Year, Age, AgeClass, AgeDeath, GLG1Ccor, GLG1N, Ocean, SeaGulf)

#group by year to calculate mean d13C per year
b<- beluga %>% 
  group_by(Year) %>% 
  summarize(AvgC = mean(d13Ccor), n = n()) #mean C plus sample size per year, "n" header

#****************STEP 2: PLOTTING TIME*****************************
#*******this plot below is PER YEAR, not every 5 years
#link b data (so 13C in this case by year) to main df to color code by ID, Sea, etc
beluga <- left_join(beluga, b, by = "Year")

#CAN'T DO INDIVIDUAL, because AvgC is well AVERAGED by year
bplot <- ggplot(beluga, aes(Year, AvgC)) +
  geom_point(fill = "gray",
             color = "black",
             size = 2,
             shape = 21) +
  ylab("d13C (corrected)") +
  theme_minimal() 
bplot

#********************STEP 3: 5 YEAR INCREMENTS*********************
#take year data and round down to the nearest 5

# function to round all years to floor of 5 year period (1967 => 1965, 1969 => 1965, etc)
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

#mutate to add this to a new column called year5
bround <- beluga %>% 
  mutate(year5 = round_any(Year, 5, floor))
beluga <- bround #just renaming this new one

#make timeline figure 5year
#this is rounding each year of samples to 5 year floor, similar to the rest
#this allows me to stack everything cleanly later on when combining plots
timeline <- ggplot(beluga, aes(year5, as.factor(Individual), color = SeaGulf)) +
  geom_line() +
  ylab("Individual") +
  theme_minimal() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
timeline  

#now plot w/ 5 year increments
#first need to group by year5 now instead of year
b5<- beluga %>% 
  group_by(year5) %>% 
  summarize(AvgC = mean(d13Ccor), n5 = n())

#remove single NA from d13C column (only one column, but screws up 5year figure)
b5 <- b5 %>% 
  drop_na(AvgC)

#plot carbon averages over 5 year periods
C5 <- ggplot(b5, aes(year5, AvgC)) +
  geom_point(fill = "gray",
             color = "black",
             size = 2,
             shape = 21) +
  ylab("d13C (cor)") +
  xlab("") +
  theme_minimal() 
C5

#**********************STEP 4: NITROGEN******************************
#ok so same as before but with nitrogen now

nit <- beluga %>% 
  group_by(year5) %>% 
  summarize(AvgN = mean(d15N), nN5 = n())


nit <- left_join(beluga, nit, by = "year5")

#plot nitrogen over 5 year periods
N5 <- ggplot(nit, aes(year5, AvgN)) +
  geom_point(fill = "gray",
             color = "black",
             size = 2,
             shape = 21) +
  ylab("d15N") +
  xlab("Year") +
  theme_minimal() 
N5

#cowplot here, puts all together, 1 column, aligning vertically by similar X axes
plot_grid(timeline, C5, N5, ncol = 1, align = "v")


#PACIFIC DECADAL OSCILLATION data from NOAA
# link: https://www.ncdc.noaa.gov/teleconnections/pdo/
library(tidyr) #for extract
#years are as YYYYMM, so Jan 1963 = 196301. Separate year from month with EXTRACT
pdo <- extract(PDOdata, Date, into = c("Year", "Month"), "(.{4})(.{2})")

PDOdata <- PDOdata %>% 
  filter(Date < 190000) #all Pacific data are prior to 1900, so remove anything after

#plot PDO 
pdoplot <- ggplot(PDOdata, aes(as.factor(Date), PDO, group = 1)) + #group=1 links points
  geom_line() +
  scale_x_discrete(breaks=c("186001","187001","188001","189001"), #select where ticks are
                   labels=c("1860", "1870", "1880", "1890")) + #name those ticks 
  xlab("Year") +
  theme_minimal()
pdoplot