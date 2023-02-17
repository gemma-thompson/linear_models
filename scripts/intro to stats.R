library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv"))

#DATA ANALYSIS ________________________________________________________ ----

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

#VISULISATION ____________________________________________________________________ ----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+ 
  geom_point()

#COMPARING GROUPS _________________________________________________________________ ----

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

#KABLE FUNCTION ___________________________________________________________________ ----

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# make a new object
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# DIFFERENCES_______________________________________________ ----

# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

# STANDARD ERROR OF THE DIFFERENCE _________________________________________ ----

difference_summary %>% 
  mutate(se= sd/sqrt(n))

# NORMAL DISTRIBUTION ________________________________________________________ ----

#Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# CONFIDENCE INTERVALS ________________________________ ----

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

# NEW PACKAGES ____________________________________________ ----

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

# LINEAR MODEL ANALYSIS FOR COMPARING GROUPS ____________________________ ----

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

# MODEL SUMMARY TIDYVERSE________________________ ----

broom::tidy(lsmodel0)
mean(darwin$height)

# MODEL SUMMARY BASE R ___________________________________ ----

summary(lsmodel0)
mean(darwin$height)

# COMPARE MEANS ___________________________________ ----

lsmodel1 <- lm(height ~ type, data=darwin)

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin)

broom::tidy(lsmodel1)

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))

summary(lsmodel1)

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# CONFIDENCE INTERVALS 

confint(lsmodel1)

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

# MEAN AND STANDARD ERROR _____________________________ ----

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

# EMMEANS ____________________________________________ ----

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

# ASSUMPTION CHECKING BASE R ___________________________________________ ----

performance::check_model(lsmodel1)

performance::check_model(lsmodel1, check=c("normality","qq"))

# ASSUMPTION CHECKING TIDYVERSE __________________________________ ----

plot(lsmodel1)

# NORMAL DISTRIBUTION _____________________________________________ ----

performance::check_model(lsmodel1, check=c("normality","qq"))

plot(lsmodel1, which=c(2,2))

# EQUAL VARIANCE __________________________________________________________ ----

performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))

# OUTLIERS _______________________________________ ----

performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))

# SUMMARY _____________________________________________ ----

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

