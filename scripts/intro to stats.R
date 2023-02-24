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

# STUDENT T TEST BASE R ______________________________________ ----

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

lsmodel1 <- lm(height ~ type, data = darwin)

# SUMMARY OF MODEL BASE R __________________________________________ ----

summary(lsmodel1)

# SUMMARY OF MODEL TIDYVERSE ____________________________________________ ----

broom::tidy(lsmodel1)

# OBSERVED PLANT HEIGHTS __________________________________________ ----

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

# PAIRED T BASE R _____________________________________________________ ----

lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

# PAIRED T TIDYVERSE _____________________________________________ ----

darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()

