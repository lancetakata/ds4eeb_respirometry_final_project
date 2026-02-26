library(tidyverse)
library(respirometry)
library(stats)
library(car)
library(lubridate)

# Intro -------------------------------------------------------------------

### OBJ 1: Determine which alpha values are most significant between the intermittent and closed experiments
### OBJ 2: Determine the relevant variables driving differences between alpha values of intermittent and closed experiments


# Alpha Int ---------------------------------------------------------------

# This section will calculate the avg of the 3 highest alpha values from the intermittent experiment for each fish
alpha_int <- read.csv("alpha_int.csv")

#add column "Salinity" identifying if a fish was saltwater or freshwater
alpha_int <- alpha_int %>%
  mutate(salinity = case_when(
    sal_ppt == 35 ~ "Saltwater",
    sal_ppt == 0  ~ "Freshwater"))

# add column classifying if fish was ran in fresh or saltwater and what lifestage it is
# use lubridate on datetime column
alpha_int$datetime <- ymd_hms(alpha_int$datetime)

# use dates of exp to determine if saltwater juvenile"SJ", freshwater juvenile"FJ" or subadult saltwater"SS"
sj_cutoff <- ymd_hms("2025-07-24 00:00:00", tz = "UTC")
fj_cutoff <- ymd_hms("2025-08-03 00:00:00", tz = "UTC")
alpha_int <- alpha_int %>%
  mutate(lifestage = case_when(
    datetime < sj_cutoff ~ "SJ",
    datetime < fj_cutoff ~ "FJ",  # anything >= SJ cutoff but < FJ cutoff
    TRUE ~ "SS"                   # everything else
  ))


#keep only the highest alpha value
#this is to test if there is any difference between the single highest and the avg of the highest 3 values
highest_alpha_int <- alpha_int %>%
  group_by(fish_id) %>%
  slice_max(order_by = alpha_int_mgo2_kg_h_kPa, n = 1) %>%
  ungroup()


#avg the alpha_max value
#create new df with the avg and the highest alpha value

alpha_values_int<- alpha_int %>% 
  group_by(fish_id, salinity) %>% 
  summarise(avg_alpha_int= mean(alpha_int_mgo2_kg_h_kPa),
            avg_temp_int = mean(temp_c)) %>% 
  left_join(highest_alpha_int %>%
              select(fish_id, alpha_int_mgo2_kg_h_kPa,lifestage), by = "fish_id") %>% 
  rename(highest_alpha_int = alpha_int_mgo2_kg_h_kPa) %>% 
  ungroup()


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

#calculate alpha value at smr, in progress


# Alpha closed ------------------------------------------------------------

# read in closed alpha values
# This section will calculate the avg of the 3 highest alpha values from the closed experiment for each fish
# There not all fish have a closed experiment because some died during the intermittent experiment
alpha_closed <- read.csv("alpha_close.csv")

#add in salinity
alpha_closed <- alpha_closed %>%
  mutate(salinity = case_when(
    sal_ppt == 35 ~ "Saltwater",
    sal_ppt == 0  ~ "Freshwater"))

#keep highest alpha value
#this is to test if there is any difference between the single highest and the avg of the highest 3 values
highest_alpha_closed <- alpha_closed %>%
  group_by(fish_id) %>%
  slice_max(order_by = alpha_close_mgo2_kg_h_kPa, n = 1) %>%
  ungroup()

#avg alpha value and make new df with both avg and highest values
alpha_values_closed<- alpha_closed %>% 
  group_by(fish_id, salinity) %>% 
  summarise(avg_alpha_closed= mean(alpha_close_mgo2_kg_h_kPa),
            avg_temp_closed = mean(temp_c)) %>% 
  left_join(highest_alpha_closed %>%
              select(fish_id, alpha_close_mgo2_kg_h_kPa), by = "fish_id") %>% 
  rename(highest_alpha_closed = alpha_close_mgo2_kg_h_kPa) %>% 
  ungroup()


# Alpha Values combined ---------------------------------------------------

#combining alpha values from int and closed portions of exp.
#not all fish will have both values

alpha_values_combined <- alpha_values_int %>% 
  left_join(alpha_values_closed %>% 
              select(fish_id, avg_alpha_closed, avg_temp_closed),
            by = "fish_id")

# calculate difference between alpha_closed and alpha_int
# avg temp values of closed and int values, can do a simple avg because there are equal # of values in closed and int
alpha_values_combined <- alpha_values_combined %>% 
  mutate(alpha_diff_avg = avg_alpha_int -avg_alpha_closed,
         temp_avg = ((avg_temp_closed + avg_temp_int)/2)) %>% 
  select(-c(avg_temp_int, avg_temp_closed))

#Post-mortem analysis ---------------------------------------------------

# looking at other variables and interactions
# lets start by looking at the post mortem data
# condition factor (CF), hepatosomatic index (HSI), % tissue and liver dry weight

post_mort <- read.csv("meta_postmort.csv")

# add relevant columns to the combined df
alpha_values_combined <- alpha_values_combined %>% 
  left_join(post_mort %>% 
              select(fish_id, cf, hsi, l_perc_dw, t_perc_dw, posttrial_wet_weight_g),
            by = "fish_id")

#adding date to do some exploratory plotting
alpha_values_combined <- alpha_values_combined %>% 
  left_join(highest_alpha_int %>% 
              select(fish_id, datetime),
            by = "fish_id")

alpha_values_combined <- alpha_values_combined %>% 
  mutate(date = as.Date(datetime)) %>% 
  select(-datetime)
  
###### t-test ###### 
  
# run paired sample t-test, with avg alpha_int and avg_alpha_closed
# this will tell us if the two values are different, essentially calculates the difference between them

#ttest <- alpha_values_combined %>% 
#  with(t.test(avg_alpha_int, avg_alpha_closed, paired = TRUE, na.action = na.omit))
#print(ttest)

# avg_alpha_int is statistically different than avg_alpha_closed
# mean difference 4.48
# therefore we should use the alpha values from the intermittent experiment


# Multiple regression model -----------------------------------------------

# These are linear regression models to evaluate which variables are related to a change in alpha 
# first is the effect of salinity
lm1 <- lm(formula = alpha_diff_avg ~ salinity, data = alpha_values_combined)
summary(lm1)

# effect of temp
lm2 <- lm(formula = alpha_diff_avg ~ temp_avg, data = alpha_values_combined)
summary(lm2)

# additive effect of salinity and temp
lm3 <- lm(formula = alpha_diff_avg ~ salinity + temp_avg, data = alpha_values_combined)
summary(lm3)

# interactive effect of salinity and temp
lm4 <- lm(formula = alpha_diff_avg ~ salinity + temp_avg + salinity:temp_avg, data = alpha_values_combined)
summary(lm4)

### To summarize the output we can say:
# In freshwater there is a weak positive effect of temp (0.2409)
# In saltwater there is a stronger positive effect of temp (0.5497)


# are data normally distributed?
shapiro.test(alpha_values_combined$alpha_diff_avg)
qqPlot(alpha_values_combined$alpha_diff_avg)

# function for producing a plot of pairwise correlations among predictors

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y,use = "all.obs",method = "spearman"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt) }


temp <- alpha_values_combined %>%
  filter(!is.na(alpha_diff_avg)) %>%
  ungroup() %>%
  mutate(ww_log = log(posttrial_wet_weight_g)) %>%
  select(ww_log,temp_avg)

# plot correlations
pairs(temp,lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)


model1 <- lm(alpha_diff_avg ~ salinity, alpha_values_combined)
model2 <- lm(alpha_diff_avg ~ salinity + log(posttrial_wet_weight_g) + salinity:log(posttrial_wet_weight_g), alpha_values_combined)
model3 <- lm(alpha_diff_avg ~ salinity + temp_avg + salinity:temp_avg, alpha_values_combined)
model4 <- lm(alpha_diff_avg ~ salinity + log(posttrial_wet_weight_g) + temp_avg +
              salinity:log(posttrial_wet_weight_g) + salinity:temp_avg, alpha_values_combined)

##AIC evaluation####

#df that shows AIC and BIC results
evaluation <- data.frame(AIC(model1, model2, model3,  model4)) %>%
  mutate(BIC = BIC(model1, model2, model3,  model4)$BIC) %>%
  arrange(BIC)


plot(model3)
plot(model4)

Anova(model3)
Anova(model4)

summary(model3)

# creating new data of all combinations of salinity and temp
newdata <- expand.grid(salinity = unique(alpha_values_combined$salinity),
                       temp_avg = seq(min(alpha_values_combined$temp_avg,na.rm=T),
                                      max(alpha_values_combined$temp_avg,na.rm=T),length.out=100))

# how does model3 predict this new data
prdata <- predict(model3,newdata = newdata, se.fit = T)

#creating plot of the new data fit

newdata$fit <- prdata$fit
newdata$lcl <- prdata$fit - prdata$se.fit*2 #lower cl, -fit x2
newdata$ucl <- prdata$fit + prdata$se.fit*2 #upper cl, +fit x2

#plot of this fit with the ucl and lcl
newdata %>%
  ggplot(aes(temp_avg,fit,fill=salinity)) +
  geom_point(data=alpha_values_combined,aes(temp_avg,alpha_diff_avg,col=salinity)) +
  geom_line(aes(col=salinity)) +
  geom_ribbon(aes(ymin = lcl,ymax = ucl),alpha = 0.3)


# Plots -------------------------------------------------------------------
###### Plots of o2 and salinity ######

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

# alpha diff vs temp by salinity
ggplot(alpha_values_combined, aes(x = temp_avg, y = alpha_diff_avg, color = salinity)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = lm)

#plot alpha diff by mass
alpha_values_combined %>% 
  ggplot(aes(x = posttrial_wet_weight_g, y = alpha_diff_avg, color = salinity)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = lm)


###### Post mortem plots ######

#plot alpha diff by hsi
alpha_values_combined %>% 
  filter(fish_id != "FV077") %>% 
  ggplot(aes(x = hsi, y = alpha_diff_avg, color = salinity)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = lm)

#plot alpha diff by cf
alpha_values_combined %>% 
  filter(fish_id != "FV091") %>% 
  ggplot(aes(x = cf, y = alpha_diff_avg, color = salinity)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = lm)

#plot alpha diff by l %dw
ggplot(alpha_values_combined, aes(x = l_perc_dw, y = alpha_diff_avg, color = salinity)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = lm)

#plot alpha diff by tissue %dw
ggplot(alpha_values_combined, aes(x = t_perc_dw, y = alpha_diff_avg, color = salinity)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set2") +
  geom_smooth(method = lm)


###### Summary statistics ######

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
