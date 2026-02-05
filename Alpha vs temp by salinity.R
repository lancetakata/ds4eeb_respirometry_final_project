library(tidyverse)
library(respirometry)
library(stats)


setwd("~/UCSC Grad School/IRA Respirometry/Data ready for analyses")

# Alpha Int ---------------------------------------------------------------

alpha_int <- read.csv("alpha_int.csv")

#add column "Salinity" identifying if a fish was saltwater or freshwater
alpha_int <- alpha_int %>%
  mutate(salinity = case_when(
    sal_ppt == 35 ~ "Saltwater",
    sal_ppt == 0  ~ "Freshwater"))

#keep only the highest alpha value
highest_alpha_int <- alpha_int %>%
  group_by(fish_id) %>%
  slice_max(order_by = alpha_int_mgo2_kg_h_kPa, n = 1) %>%
  ungroup()


#avg the alpha_max value
#create new df with the avg and the highest alpha value

alpha_values_int<- alpha_int %>% 
  group_by(fish_id, salinity) %>% 
  summarise(avg_alpha_int= mean(alpha_int_mgo2_kg_h_kPa)) %>% 
  left_join(highest_alpha_int %>%
            select(fish_id, alpha_int_mgo2_kg_h_kPa), by = "fish_id") %>% 
  rename(highest_alpha_int = alpha_int_mgo2_kg_h_kPa)


#add in SMR values from int trials
#convert O2 to PO2 using conv_o2 function
int_smr <- read.csv("smr_exp.csv")

int <- int_smr %>% 
  left_join(alpha_values_int, by = "fish_id") %>% 
  mutate(o2_kpa = conv_o2(o2 = o2_mgl,
          from = "mg_per_l",
          to = "kPa",
          temp = temp_c,
          sal = sal_ppt,
          atm_pres = pres_hpa),
         Salinity = if_else(sal_ppt == 35, "Saltwater", "Freshwater"))

#calculate alpha value at smr


# Alpha closed ------------------------------------------------------------
# read in closed alpha values

alpha_closed <- read.csv("alpha_close.csv")

#add in salinity
alpha_closed <- alpha_closed %>%
  mutate(salinity = case_when(
    sal_ppt == 35 ~ "Saltwater",
    sal_ppt == 0  ~ "Freshwater"))

#keep highest alpha value
highest_alpha_closed <- alpha_closed %>%
  group_by(fish_id) %>%
  slice_max(order_by = alpha_close_mgo2_kg_h_kPa, n = 1) %>%
  ungroup()

#avg alpha value and make new df with both avg and highest values
alpha_values_closed<- alpha_closed %>% 
  group_by(fish_id, salinity) %>% 
  summarise(avg_alpha_closed= mean(alpha_close_mgo2_kg_h_kPa)) %>% 
  left_join(highest_alpha_closed %>%
              select(fish_id, alpha_close_mgo2_kg_h_kPa), by = "fish_id") %>% 
  rename(highest_alpha_closed = alpha_close_mgo2_kg_h_kPa)


# Alpha Values combined ---------------------------------------------------

#combining alpha values from int and closed portions of exp.
#not all fish will have both values

alpha_values_combined <- alpha_values_int %>% 
  left_join(alpha_values_closed %>% 
            select(fish_id, avg_alpha_closed),
            by = "fish_id")

#calculate difference between alpha_closed and alpha_int
alpha_values_combined <- alpha_values_combined %>% 
  mutate(alpha_diff_avg = avg_alpha_int -avg_alpha_closed)
  
#run paired sample t-test, with avg alpha_int and avg_alpha_closed
ttest <- alpha_values_combined %>% 
  with(t.test(avg_alpha_int, avg_alpha_closed, paired = TRUE, na.action = na.omit))
#avg_alpha_int is statistically different than alpha_closed
linear <- alpha_values_combined %>% 
  lm(alpha_diff_avg ~)

# Plots -------------------------------------------------------------------

#plot alpha vs temp by salinity
ggplot(highest_alpha_int, aes(x = temp_c, y = alpha_int_mgo2_kg_h_kPa, color = salinity)) +
  geom_point(alpha = 0.7)+
  labs(
    title = "Alpha vs Temperature by Salinity",
    x = "Temperature (°C)",
    y = "Alpha (mgO2kg-1h-1kPa-1)") +
  scale_color_brewer(palette = "Set2")

#plot smr vs temp by salinity
ggplot(int, aes(x = temp_c, y = smr_mgo2_kg_h, color = Salinity)) +
  geom_point(alpha = 0.7) +
  geom_smooth(alpha = 0.2) +
  labs(
    title = "SMR vs Temperature by Salinity",
    x = "Temperature (°C)",
    y = "SMR (mgO2kg-1h)") +
  scale_color_brewer(palette = "Set2")


#plot alpha vs o2 by salinity
ggplot(highest_alpha_int, aes(x = o2_mgl, y = alpha_int_mgo2_kg_h_kPa, color = o2_mgl)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red")+
  facet_wrap(~salinity) +
  labs(
    title = "Alpha vs DO by Salinity",
    x = "O2 mg/L",
    y = "Alpha (mgO2kg-1h-1kPa-1)") +
  theme_bw()

#summary statistics
#count of each intermittent completed
highest_alpha_int %>% 
  filter(salinity == "Freshwater") %>% 
  count()

highest_alpha_int %>% 
  filter(salinity == "Saltwater") %>% 
  count()

alpha_values_closed %>% 
  filter(salinity == "Freshwater") %>% 
  count()

alpha_values_closed %>% 
  filter(salinity == "Saltwater") %>% 
  count()
