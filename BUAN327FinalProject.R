library(dplyr)
nursing = read.csv("2021_CostReport.csv")
provinfo = read.csv("ProviderInfo_2021.csv")

colnames(nursing)

#calculations 
averagerevenue<- nursing %>% summarise(mean(Gross.Revenue, na.rm = TRUE))

avgnetincome<- nursing %>%
  group_by(Facility.Name) %>%
  summarize(AvgNetIncome = mean(Net.Income, na.rm = TRUE))

profitmargin<- nursing %>%
  group_by(County) %>%
  summarize(profit_margin = sum(Net.Income) / sum(Gross.Revenue) * 100)

top10counties<- profitmargin %>%
  arrange(desc(profit_margin)) %>%
  head(10)

lowestbeds <- nursing %>%
  group_by(County) %>%
  summarize(total_beds = sum(Number.of.Beds)) %>%
  arrange(total_beds) %>%
  head(5)

#create two maps
#samplemap ~ Total Number of Beds by County
library(tidyverse)
library(sf)
library(maps)

# Load county map data
counties <- map_data("county")

# Prepare the data: clean and aggregate
bedsdata <- nursing %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Total_Beds = sum(Number.of.Beds, na.rm = TRUE))

# Prepare the map data: merge and clean
counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(bedsdata, by = c("subregion" = "County"))

# Plot the map
ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Total_Beds)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Total Beds") +
  labs(title = "Total Number of Beds by County") +
  theme_minimal()   


#firstmap
netincome <- nursing %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Net_Income = sum(Net.Income, na.rm = TRUE))

counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(netincome, by = c("subregion" = "County"))

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Net_Income)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Net Income") +
  labs(title = "Net Income per County") +
  theme_minimal()

#second map 
discharges <- nursing %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Total_Discharges = sum(Total.Discharges.Total, na.rm = TRUE))

counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(discharges, by = c("subregion" = "County"))

ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Total_Discharges)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Total Discharges") +
  labs(title = "Total Discharges by County") +
  theme_minimal()


#length of stay vs Overall Rating 
colnames(provinfo)[colnames(provinfo) == "Provider.Name"] <- "Facility.Name"

stayxrating<- merge(provinfo, nursing, by.x = "Facility.Name", by.y = "Facility.Name")


stayxrating2 <- stayxrating %>%
  filter(!is.na(Overall.Rating) & !is.na(SNF.Average.Length.of.Stay.Total))


ggplot(data = stayxrating2, aes(x = Overall.Rating, y = SNF.Average.Length.of.Stay.Total, fill = Overall.Rating)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Length of Stay and Overall Ratings", x = "Overall Rating", y = "Average Length of Stay") +
  theme_minimal()


#piechart

total_nursing_homes <- nrow(provinfo)

ownershipcount <- provinfo %>%
  count(Ownership.Type)

library(RColorBrewer)
set3_colors <- brewer.pal(12, "Set3")
dark2_colors <- brewer.pal(8, "Dark2")

custompalette <- c(set3_colors, dark2_colors)

ggplot(ownershipcount, aes(x = "", y = n, fill = Ownership.Type)) +
  geom_bar(stat = "identity", color = "white", size = 0.5) +  # Add white borders
  coord_polar("y", start = 0) +  # Convert to polar coordinates
  labs(title = "Distribution of Nursing Homes by Ownership Type") +
  theme_void() +  # Remove background and gridlines
  theme(legend.position = "right") +  # Position legend
  theme(plot.title = element_text(hjust = 0.5)) +  # Center title
  scale_fill_manual(values = custompalette)


#ANOVA
states <- c("CA", "TX", "NY")
statesdata <- stayxrating %>% filter(Provider.State %in% states)

ares<- anova(lm(Gross.Revenue ~ Provider.State, statesdata))
ares
summary(ares)

#The results of ANOVA indicate a significant overall effect of the State on Gross
#Revenue. As indicated by the substantial sum of squares between groups, 1.8057e+16, associated 
#with the provider state factor, there are significant differences in Gross Revenue 
#among the three states of California, Texas, and New York. The variation in Gross Revenue
#that cannot be explained by the differences in states is represented by the sum of squares
# within groups. The mean squares for the state variable , 9.0286e+15, is much larger than 
#the mean squares for residuals,8.4030e+16, indicating that the variation between the three
#chosen states explains a good proportion of the total variation in Gross Revenue.
#According to our hypothesis test, the pvalue is extremely tiny, < 2.2e-16, providing very 
#strong evidence against the null hypothesis (Ha: There are significant differences in Gross
#Revenue among California, Texas, and New York.)

#Regression Analysis
rmodel <-lm (Net.Income ~ Number.of.Beds, stayxrating)
summary(rmodel)
#p-value: 0.0002663

rmodel <-lm (Net.Income ~ Rural.versus.Urban, stayxrating)
summary(rmodel)
#p-value: 0.0002663

rmodel <-lm (Net.Income ~ Total.Costs, stayxrating)
summary(rmodel)
#p-value: 0.0002663

rmodel <-lm (Net.Income ~ Total.liabilities, stayxrating)
summary(rmodel)
#p-value: 0.0002663

rmodel <-lm (Net.Income ~ Less.Contractual.Allowance.and.discounts.on.patients..accounts, stayxrating)
summary(rmodel)
#p-value: 0.0002663

rmodel <-lm (Net.Income ~ Less.Total.Operating.Expense, stayxrating)
summary(rmodel)
#p-value: 0.7132

rmodel <-lm (Net.Income ~ Total.Bed.Days.Available, stayxrating)
summary(rmodel)
#p-value: 0.7132


