# INSTALL LIBRARY AND PACKAGES
library(tidyverse)
library("car")
library(effsize)
library(hrbrthemes)
library(viridis)
install.packages("dplyr")
install.packages("devtools")


# IMPORTING ACTIGRAPH DATA IN .csv FORMAT FOR TWO WALKING TASKS
## Condition 1 (c1) : Normal, self-paced walking
## Condition 2 (c2) : Walking with a cognitive task (Dual-task walking)

c1=read_csv("/Users/C1_GAIT.csv")
c2=read_csv("/Users/C2_GAIT.csv")


# SORT DATA BY AGE
c1 = c1[order(c1$Age,decreasing=FALSE),]
c2 = c2[order(c2$Age,decreasing=FALSE),]


# DATAFRAMES FOR AGE, AGE_GROUP AND A GAIT PARAMETER :
## HARMONIC RATIO (HR) - VERTICAL (V), MEDIOLATERAL (ML), ANTEROPOSTERIOR (AP)
c1_df= data.frame(Age = c1$Age,
                  AgeGroup=as.factor(ifelse(c1$Age < 30, "Young", "Middle age")),
                  HR_V= c1$HarmonicRatio_V,
                  HR_ML=c1$HarmonicRatio_ML,
                  HR_AP= c1$HarmonicRatio_AP)

c2_df= data.frame(Age = c2$Age,
                  AgeGroup=as.factor(ifelse(c2$Age < 30, "Young", "Middle age")),
                  HR_V= c2$HarmonicRatio_V,
                  HR_ML=c2$HarmonicRatio_ML,
                  HR_AP= c2$HarmonicRatio_AP)


# DESCRIPTIVE STATISTICS
c1_df %>% group_by(AgeGroup) %>%  summarise(n = n(), mean = mean(Age), sd = sd(Age))
c2_df %>% group_by(AgeGroup) %>%  summarise(n = n(), mean = mean(Age), sd = sd(Age))


# CHECK DATA NORMALITY
## Q-Q PLOT FOR HARMONIC RATIO
qqPlot(c1_df$HR_V)
qqPlot(c1_df$HR_AP)
qqPlot(c1_df$HR_ML)
qqPlot(c2_df$HR_V)
qqPlot(c2_df$HR_AP)
qqPlot(c2_df$HR_ML)

## HISTOGRAM
hist(c1_df$HR_V)
hist(c1_df$HR_AP)
hist(c1_df$HR_ML)
hist(c2_df$HR_V)
hist(c2_df$HR_AP)
hist(c2_df$HR_ML)


# HOMOSCEDASTICITY USING LEVENE’S TEST
c1_HR_V = leveneTest(c1_df$HR_V ~ c1_df$AgeGroup, c1_df)
c1_HR_AP = leveneTest(c1_df$HR_AP ~ c1_df$AgeGroup, c1_df)
c1_HR_ML = leveneTest(c1_df$HR_ML ~ c1_df$AgeGroup, c1_df)
c1_HR_V 
c1_HR_AP
c1_HR_ML

c2_HR_V = leveneTest(c2_df$HR_V ~ c2_df$AgeGroup, c2_df)
c2_HR_AP = leveneTest(c2_df$HR_AP ~ c2_df$AgeGroup, c2_df)
c2_HR_ML = leveneTest(c2_df$HR_ML ~ c2_df$AgeGroup, c2_df)
c2_HR_V 
c2_HR_AP
c2_HR_ML


# TWO SAMPLE T-TEST
t.test(c1_df$HR_V~c1_df$AgeGroup, var.equal = TRUE)
t.test(c2_df$HR_V~c2_df$AgeGroup, var.equal = TRUE)
t.test(c1_df$HR_AP~c1_df$AgeGroup, var.equal = TRUE)
t.test(c2_df$HR_AP~c2_df$AgeGroup, var.equal = TRUE)
t.test(c1_df$HR_ML~c1_df$AgeGroup, var.equal = TRUE)
t.test(c2_df$HR_ML~c2_df$AgeGroup, var.equal = TRUE)


# EFFECT SIZE USING COHEN'S D
cohen.d(formula= c1_df$HR_V~c1_df$AgeGroup, data=c1_df, paired = FALSE)
cohen.d(formula= c1_df$HR_AP~c1_df$AgeGroup, data=c1_df, paired = FALSE)
cohen.d(formula= c1_df$HR_ML~c1_df$AgeGroup, data=c1_df, paired = FALSE)

cohen.d(formula= c2_df$HR_V~c2_df$AgeGroup, data=c2_df, paired = FALSE)
cohen.d(formula= c2_df$HR_AP~c2_df$AgeGroup, data=c2_df, paired = FALSE)
cohen.d(formula= c2_df$HR_ML~c2_df$AgeGroup, data=c2_df, paired = FALSE)


# DATA VISUALISATION
## BOX PLOTS WITH JITTER POINTS
c1_df %>%
  ggplot( aes(x=AgeGroup, y=HR_V, fill=AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_ipsum() + 
  theme(legend.position="centre", plot.title = element_text(size=11)) +
  ggtitle("Single task walking") +
  xlab("")

c2_df %>%
  ggplot( aes(x=AgeGroup, y=HR_V, fill=AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_ipsum() + 
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("Dual task walking") +
  xlab("")

c1_df %>%
  ggplot( aes(x=AgeGroup, y=HR_AP, fill=AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_ipsum() + 
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("Single task walking") +
  xlab("")

c2_df %>%
  ggplot( aes(x=AgeGroup, y=HR_AP, fill=AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_ipsum() + 
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("Dual task walking") +
  xlab("")

c1_df %>%
  ggplot( aes(x=AgeGroup, y=HR_ML, fill=AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_ipsum() + 
  theme(legend.position="centre", plot.title = element_text(size=11)) +
  ggtitle("Single task walking") +
  xlab("")

c2_df %>%
  ggplot( aes(x=AgeGroup, y=HR_ML, fill=AgeGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_ipsum() + 
  theme(legend.position="none", plot.title = element_text(size=11)) +
  ggtitle("Dual task walking") +
  xlab("")

