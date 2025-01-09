# hello world
# Female Happiness Index Codes
#I'm sure there is a way to make a for loop to number but it didn't like a vector with the diff titles to sort in an automatic loop
#FinalMerge <- finalmerged_7%>% filter(!is.na(c(X.60, Value.x, ...6, Value.y, Value.x.x, Value.y.y, Value)))
CompleteFinalMerge_1 <- filter(finalmerged_7, Disaggregation == "female")
CompleteFinalMerge_2 <- filter(CompleteFinalMerge_1, Disaggregation.y.y == "total")
CompleteFinalMerge_3 <- filter(CompleteFinalMerge_2, Disaggregation.x.x == "female")
CompleteFinalMerge_4 <- filter(CompleteFinalMerge_3, Disaggregation.y == "female")
CompleteFinalMerge_5 <- filter(CompleteFinalMerge_4, ...5 == "Sexual, Reproductive, Maternal,
Newborn, Child and Adolescent Health Policy Survey, 2018-2019")
CompleteFinalMerge_6 <- filter(CompleteFinalMerge_5, Disaggregation.x == "female, Modeled")
# 3 NA in Parliament are West Bank, Gibon and Afghanistan. Others are missing data but they were eliminated on other rounds of cleaning.
CompleteFinalMerge_7 <- CompleteFinalMerge_6%>% filter(!is.na(X.60))
#rename() is how to fix the headers variable names
CompleteFinalMergeRenamed <- rename(CompleteFinalMerge_7, "Country Name 1" = "Data.Source")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Country Code 1" =
                                      "World.Development.Indicators")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Proportion of Females in
Parliament" = "X.60")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Maternal Mortality" =
                                      "Value.x")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Free Family Planning
Available" = "...6")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Life Expectancy" =
                                      "Value.y")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Pay Ratio" = "Value.y.y")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Expected Years in School" =
                                      "Value")
CompleteFinalMergeRenamed <- rename(CompleteFinalMergeRenamed, "Expected Years in School" =
                                      "Value")
AllDataFinal <- CompleteFinalMergeRenamed[ , c(1, 2, 5, 10, 12, 16, 21, 27, 33, 39)]
AllDataFinal <- rename(AllDataFinal, "World Region" = "Export date")
# move World Region from Column 5 to Column 3?
# Use function select from tidyverse to select specific column names, can use to rearrange columns
all_data_final <- AllDataFinal_1_ %>%
  select(c(`Country Name 1`, `Country Code 1`, `World Region`, `Proportion of Females in Parliament`,
           `Maternal Mortality`, `Free Family Planning Available`, `Life Expectancy`,
           `Length of Paid Leave`, `Pay Ratio`, `Expected Years in School`))
#I spot checked a few countries from original spreadsheets and CompleteFinalMerge_7
# Aruba, Belarus, Montenegro, Oman, Vietnam, all 7 indicators match
#This gives a vector of all countries and regions I deleted :)
TotalCountryNames <- finalmerged_7$Data.Source
KeptNames <- CompleteFinalMergeRenamedAndReduced$`Country Name 1`
CountriesDeleted <- unlist(setdiff(TotalCountryNames, KeptNames))###-----------------------------------------------------------------------------------------------###
#For the report, how does the US compare on these metrics
Averages <- c(mean(AllDataFinal$`Proportion of Females in Parliament`), mean(AllDataFinal$`Life
                                                                             Expectancy`), mean(AllDataFinal$`Length of Paid Leave`), mean(AllDataFinal$`Pay Ratio`),
              mean(AllDataFinal$`Expected Years in School`))
Averages
US_Data <- c(US$`Proportion of Females in Parliament`, US$`Life Expectancy`, US$`Length of Paid
             Leave`, US$`Pay Ratio`, US$`Expected Years in School`)
US_Data
Averages_and_US_Data <- rbind(Averages, US_Data)
rename(Averages_and_US_Data, "Proportion of Females in Parliament" = "V1")
###-----------------------------------------------------------------------------------------------###
#data exploration
# column for maternal mortality should be low. Needs to have numbers reversed to give positive
correlation
Pairs <- CompleteFinalMerge_7%>% ggpairs(c("X.60", "Value", "Value.x", "Value.y", "Value.x.x",
                                           "Value.y.y"))
ggplotly(Pairs)
###-----------------------------------------------------------------------------------------------###
# Read in final dataset; note need library readxl
AllDataFinal_1_ <- read_excel("Final Data/AllDataFinal.xlsx")
# Recoding some of the variables by using function recode, others just being renamed for ease of use (I
#learned underscore doesn't need "" so it is faster to type)
final_ds_st_measures <- all_data_final %>%
mutate(country_name = `Country Name 1`,
country_code = `Country Code 1`,
world_region = `World Region`,
prop_fe_parl = (`Proportion of Females in Parliament`), # Percentage
life_exp = (`Life Expectancy`), # Years for females
length_pd_leave = (`Length of Paid Leave`), # Number of days of paid leave for the mother
pay_ratio = (`Pay Ratio`), # Percentage
exp_years_school = (`Expected Years in School`) # Number of years in school
) %>%
select(c(country_name, country_code, world_region, prop_fe_parl, life_exp,
length_pd_leave, pay_ratio, exp_years_school))
final_ds_st_measures %>% skim_without_charts()
###-----------------------------------------------------------------------------------------------#### CREATE GRAPHICS AND MAPS FOR EVERY MEASURE
#examples on google
boxplot(CompleteFinalMergeRenamedAndReduced$`Proportion of Females in Parliment`, ylab =
"Percentage", main = "Proportion of Females in Parliament")
boxplot(CompleteFinalMergeRenamedAndReduced$`Maternal Mortality`, ylab = "Deaths per 100,000
live births (modeled estimates)", main = "Maternal Mortality Ratio, by country")
boxplot(CompleteFinalMergeRenamedAndReduced$`Life Expectancy`, ylab = "Years", main = "Female
Life Expectancy, by country")
boxplot(CompleteFinalMergeRenamedAndReduced$`Length of Paid Leave`, ylab = "Days of paid
leave", main = "Paid Maternity Leave, by country")
boxplot(CompleteFinalMergeRenamedAndReduced$`Pay Ratio`, ylab = "Percentage of Male Pay
Females Receive", main = "Female to Male Pay Ratio, by country")
boxplot(CompleteFinalMergeRenamedAndReduced$`Expected Years in School`, ylab = "Years in
School", main = "Expected Years in School for Females, by country")
boxplot(US$`Proportion of Females in Parliament`, col = 1)
# Proportion of females in parliament
map.world <- map_data(map = "world")
ggplot() +
geom_map(
data = map.world, map = map.world,
aes(x = long, y = lat, group = group, map_id = region),
fill = "gray73", colour = "white", size = 0.4
) +
geom_map(
data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
aes(fill = prop_fe_parl, map_id = country_name), # Filling in colors by prop of fe in parliament
colour = "white", size = 0.4
) +
geom_map(
data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
aes(fill = prop_fe_parl, map_id = country_code), 
# # The US didn't show up in the previous command
# even though it is in our data set, prob because of some small difference in the full name of country, so we
# used country code to ensure it is included
colour = "white", size = 0.4
) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 110)) +
  scale_fill_gradient(
    low = "red1", high = "blue1",
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    labels = c("0", "10", "20", "30", "40",
               "50", "60", "70", "80", "90", "100")
  ) +
  labs(
    fill = "Percentage",
    x = NULL, y = NULL,
    title = "Proportion of females in parliament 2018"
  ) +theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
# Life expectancy of females 2018
ggplot() +
  geom_map(
    data = map.world, map = map.world,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "gray73", colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
    aes(fill = life_exp, map_id = country_name), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
    aes(fill = life_exp, map_id = country_code), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 110)) +
  scale_fill_gradient(
    low = "red1", high = "blue1",
    breaks = c(55, 60, 65, 70, 75, 80, 85, 90),
    labels = c("55", "60", "65", "70", "75", "80", "85", "90")
  ) +
  labs(
    fill = "Number of years",
    x = NULL, y = NULL,
    title = "Life expectancy of females 2018"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
# Length of paid family leave 2018
ggplot() +
  geom_map(
    data = map.world, map = map.world,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "gray73", colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-setaes(fill = length_pd_leave, map_id = country_name), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
    aes(fill = length_pd_leave, map_id = country_code), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 110)) +
  scale_fill_gradient(
    low = "yellow", high = "blue1",
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400),
    labels = c("0", "50", "100", "150", "200", "250", "300", "350", "400")
  ) +
  labs(
    fill = "Number of days",
    x = NULL, y = NULL,
    title = "Length of paid family leave 2018"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
# Pay ratio
ggplot() +
  geom_map(
    data = map.world, map = map.world,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "gray73", colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
    aes(fill = pay_ratio, map_id = country_name), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
    aes(fill = pay_ratio, map_id = country_code), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 110)) +
  scale_fill_gradient(
    low = "red1", high = "blue1",
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    labels = c("0", "10", "20", "30", "40",
               "50", "60", "70", "80", "90", "100")
  ) +
  labs(fill = "Percentage",
       x = NULL, y = NULL,
       title = "Female to male pay ratio 2018"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
# Expected years in school
ggplot() +
  geom_map(
    data = map.world, map = map.world,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "gray73", colour = "white", size = 0.4  )  +
  geom_map(
    data = final_ds_st_measures, map = map.world, # Data argument takes in our final data-set
    aes(fill = exp_years_school, map_id = country_code), # Filling in colors by prop of fe in parliament
    colour = "white", size = 0.4
  ) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 110)) +
  scale_fill_gradient(
    low = "red1", high = "blue1",
    breaks = c(2, 4, 6, 8, 10, 12, 14),
    labels = c("2", "4", "6", "8", "10", "12", "14")
  ) +
  labs(
    fill = "Number of years",
    x = NULL, y = NULL,
    title = "Expected years in school 2018"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
###-----------------------------------------------------------------------------------------------###
#Correlation of measures and Principal Compenents Analysis coding, from my mentor Rrita Zejnullahi
measures <- final_ds_st_measures %>%
  select(c(prop_fe_parl, life_exp, exp_years_school, pay_ratio, length_pd_leave))
measures
corr_meas <- cor(measures)
# Get lower triangle of correlation matrix
get_lower_tri <- function(corr_meas) {corr_meas[lower.tri(corr_meas)] <- NA
return(corr_meas)
}
lower_tri <- get_lower_tri(corr_meas)
melted_corr_meas <- melt(lower_tri, na.rm = TRUE)
melted_corr_meas
labels <- c(
  1.000, 0.305, 1.000, 0.197, 0.830, 1.000,
  0.342, 0.240, 0.248, 1.000, 0.048, 0.142,
  0.111, 0.106, 1.000
)
# Plot 1
ggplot(data = melted_corr_meas, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradient2(
    low = "red1", high = "darkslategray3", mid = "white",
    midpoint = 0, limit = c(-1, 1), seq(-1, 1, 0.25),
    name = "Pearson\nCorrelation"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = labels),
            color = "black", size = 4
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Measures of Happiness for Females"
  ) +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.text = element_text(size = 13)
  ) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 15))
# Analysis
female_happiness.pca <- prcomp(measures, scale=T) # Scale variables so they have variance 1
female_happiness.pca # See results of the pr components analysis (i.e. the matrix of principal components)
summary(female_happiness.pca) # This gives you the variances of each PC and the proportion of variance explained in the composite index
# Vector of pc1 = weighted combination of the six indices
pc1_loadings <- c(-0.3672479, -0.6015427, -0.5805238, -0.3680228, -0.1755754)mat <- data.matrix(measures) # Put measure data into a data matrix
pc1 <- mat %*% pc1_loadings
comp_index <- (pc1/(max(pc1)))*100# Create the index
summary(comp_index) # Get summary statistics
comp_index <- data.frame(comp_index)
# Scree plot
plot(female_happiness.pca, type='l', main = "Scree Plot")
###-----------------------------------------------------------------------------------------------###
# World heat map for the composite index
comp_index_wm <- cbind(all_data_final, comp_index)
ggplot() +
  geom_map(
    data = map.world, map = map.world,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "gray73", colour = "white", size = 0.4
  ) +
  geom_map(
    data = comp_index_wm, map = map.world,
    aes(fill = comp_index, map_id = `Country Name 1`),
    colour = "white", size = 0.4
  ) +
  geom_map(
    data = comp_index_wm, map = map.world,
    aes(fill = comp_index, map_id = `Country Code 1`),
    colour = "white", size = 0.4
  ) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 90)) +
  scale_fill_gradient(
    low = "red1", high = "blue1",
    labels = c(
      "100", "150", "200", "350", "400"
    )
  ) +
  labs(
    fill = "Score",
    x = NULL, y = NULL,
    title = "Female Happines"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
###-----------------------------------------------------------------------------------------------###
#compare World Happiness Report to Our Female Happiness ReportCompare_Old_New <- full_join(comp_index_wm, World_Happiness_Report, by = c("Country Name 1" = "Country"))
Subset_Compare_Old_New <- Compare_Old_New%>%
  select(c("Country Name 1", "Country Code 1", "World Region", "comp_index", "Happiness.score"))
subset_2 <- filter(Subset_Compare_Old_New, !is.na(Subset_Compare_Old_New$comp_index))
subset_3 <- filter(subset_2, !is.na(subset_2$Happiness.score))
sum(is.na(subset_3))
# zero NA left, yeah!
rank_FHI <- rank(-subset_3$comp_index)
rank_WHI <- rank(-subset_3$Happiness.score)
rank_subset <- cbind(subset_3, rank_FHI, rank_WHI)
change_rank <- c(rank_subset$rank_WHI - rank_subset$rank_FHI)
final_change_rank <- cbind(rank_subset, change_rank)
# tried and failed with mutate(rank_subset, change_rank = rank_subset$rank_WHI - rank_subset$rank_FHI)
max(final_change_rank$change_rank)
# +86 Albania
min(final_change_rank$change_rank)
#-100 UAE
final_change_rank <- rename(final_change_rank, "Country" = "Country Name 1")
final_change_rank <- rename(final_change_rank, "Country_code" = "Country Code 1")
#make final graphic for change of rank
ggplot() +
  geom_map(
    data = map.world, map = map.world,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "gray73", colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_change_rank, map = map.world, # Data argument takes in our final data-set
    aes(fill = change_rank, map_id = Country), # Filling in colors by change rank
    colour = "white", size = 0.4
  ) +
  geom_map(
    data = final_change_rank, map = map.world, # Data argument takes in our final data-set
    aes(fill = change_rank, map_id = Country_code), # Filling in colors by change rank
    colour = "white", size = 0.4
  ) +
  coord_map("rectangular", lat0 = 0, xlim = c(-180, 180), ylim = c(-60, 110)) +
  scale_fill_gradient(
    low = "red", high = "green",
    breaks = c(-100, -50, 0, 50, 100),labels = c("-100", "-50", "0", "50", "100")
  ) +
  labs(
    fill = "Change in Rank, World Happiness to Female Happiness",
    x = NULL, y = NULL,
    title = "Change in Rank"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 25, hjust = 0.05)
  ) +
  guides(fill = guide_colorbar(barwidth = 25, barheight = 1, title.position = "top"))
#plot WHI with FHI, in theory if they are linearly correlated, #1 should be 1, and #150 should be #150, a
# straight line on x=y, but these are not at all, it looks random
plot(final_change_rank$rank_WHI, final_change_rank$rank_FHI, main = "Change in Rank", xlab =
       "World Happiness Report Rank", ylab = "Female Happiness Report")
#correlation based on visual should be close to 0
cor(final_change_rank$rank_WHI, final_change_rank$rank_FHI)
# 0.4434216
###-----------------------------------------------------------------------------------------------###
#Save files for export to report and email
write_xlsx(CompleteFinalMerge_7, "C:\\Users\\PC8\\Desktop\\Biostats App\\Directed Reading Program
DRP\\Final Data\\CompleteFinalMerge_7.xlsx")
write_xlsx(CompleteFinalMergeRenamedAndReduced, "C:\\Users\\PC8\\Desktop\\Biostats
App\\Directed Reading Program DRP\\Final Data\\CompleteFinalMergeRenamedAndReduced.xlsx")
write_xlsx(AllDataFinal, "C:\\Users\\PC8\\Desktop\\Biostats App\\Directed Reading Program DRP\\Final
Data\\AllDataFinal.xlsx")
write_xlsx(CountriesDeleted, "C:\\Users\\PC8\\Desktop\\Biostats App\\Directed Reading Program
DRP\\Final Data\\CountriesDeleted.xlsx")
write_xlsx(comp_index_wm, "C:\\Users\\PC8\\Desktop\\Biostats App\\Directed Reading Program
DRP\\Final Data\\comp_index_wm.xlsx")
write_xlsx(Averages_and_US_Data, "C:\\Users\\PC8\\Desktop\\Biostats App\\Directed Reading Program
DRP\\Final Data\\Averages_and_US_Data.xlsx")