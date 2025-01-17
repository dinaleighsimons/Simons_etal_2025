#Simons, Stubbs, Bradbruy, Evans 2024 main script

#Set environment ---------------------------------------------------------------
rm(list = ls())

##Load packages ----
packages <- c("dplyr",
              "ggfortify",
              "lme4",
              "tidyverse",
              "devtools",
              "janitor",
              "stringr",
              "survey",
              "FactoMineR",
              "corrplot",
              "lubridate",
              "car",
              "ggstatsplot",
              "unheadr",
              "tidyxl",
              "Hmisc",
              "factoextra",
              "ggpointdensity",
              "cowplot",
              "MASS",
              "lmtest",
              "rcompanion",
              "pwr",
              "pwr2",
              "powerMediation")

invisible(lapply(packages, library, character.only = TRUE))

## Read and tidy data ----
projectdata <- read.csv('Data/Project_data.csv')
glimpse(projectdata)
str(projectdata)

## Remove 'Other' gender categories
# Delete rows 102, 209, and 603 from the dataset
projectdata <- projectdata[-c(102, 209, 603),]
colSums(is.na(projectdata))

# Power analyses ---------------------------------------------------------------

#getting effect sizes - Give the conventional effect size (small, medium, large) for the tests available in this package
cohen.ES(test = "f2", size = "small") #small = 0.02, medium = 0.15, large= 0.35

#(https://data-se.netlify.app/2018/07/24/power-calculation-for-the-general-linear-model/)

pwr.f2.test(u = 10,
            v = NULL,
            f2 = 0.02,
            sig.level = 0.05, 
            power = 0.8)

810.2499 + 10

#u = degrees of freedom for numerator
#v = degrees of freedom for denominator
#f2 =  	effect size (R2/(1-R2) where R2 is proportion of variance accounted)
#sig.level = Significance level (Type I error probability)
#power = Power of test (1 minus Type II error probability)

#v = 810
#n = v + p = 810 + 10
#n = 820 overall, 205 per treatment (80% power, 10 variables, sig level = 0.05)

# PCAs -------------------------------------------------------------------------

#subset finance variables
finance <- projectdata %>% subset(select = c(Financial_compost, Financial_allocation))

#scale
finance_normalized <- scale(finance)
head(finance_normalized)

#complete PCA - they don't collapse very well. Probs best to just keep separate.
finance.pca <- princomp(finance_normalized)
summary(finance.pca) #results
finance.pca$loadings[, 1:2] #loadings
finance.pca$sdev

#visualise PCA
fviz_eig(finance.pca, addlabels = TRUE)
fviz_pca_var(finance.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

# add PCA finance into dataset ---
projectdata$financePCA <- finance.pca$scores[, "Comp.1"]

# Check sample sizes -------------------------
sum(projectdata$Message_framing == "BIO")
sum(projectdata$Message_framing == "ES")
sum(projectdata$Relational_values == "Present")
sum(projectdata$Relational_values == "Absent")

# Check data structures --------------------------------------------------------

## Responses ---- 
### Behaviour scores ----
ggplot(projectdata, aes(x = (Behaviour_sum)^2))+
  geom_histogram(bins = 20)+
  theme_bw() #slight left skew, sq improves

plotNormalHistogram(projectdata$Behaviour_sum^2)

### Financial ratings ----
ggplot(projectdata, aes(x = log(financePCA + 2))) +
  geom_histogram(bins = 10) +
  theme_bw() #extreme right skew, log improves

plotNormalHistogram(projectdata$financePCA)

### Change in attitude scores ----

### Degraded peatland change score
ggplot(projectdata, aes(x = Degraded_peatland_score))+
  geom_histogram(bins = 10)+
  theme_bw() #approaching normal

plotNormalHistogram(projectdata$Degraded_peatland_score)

### Regenerate peatland change score
ggplot(projectdata, aes(x = Regenerate_peatland_score))+
  geom_histogram(bins = 20)+
  theme_bw() #approaching normal

plotNormalHistogram(projectdata$Regenerate_peatland_score)

### Final attitudes ---

### Degraded peatland final score
ggplot(projectdata, aes(x = Degraded_peatlands_after^3))+
  geom_histogram(bins = 10)+
  theme_bw() #left skewed

plotNormalHistogram(projectdata$Degraded_peatlands_after^3)

### Regenerate peatland final score
ggplot(projectdata, aes(x = Regenerate_peatlands_after))+
  geom_histogram(bins = 10)+
  theme_bw() #left skewed

### Advert sufficiency ----
ggplot(projectdata, aes(x = (Advert_sufficiency)^2))+
  geom_histogram(bins = 7)+
  theme_bw() #left

## Manipulation check variables ----

### ES_Manipulation ----

ggplot(projectdata, aes(x = (ES_Manipulation)^3))+
  geom_histogram(bins = 10)+
  theme_bw() #left-skew, cube kinda improves

plotNormalHistogram(projectdata$ES_Manipulation)
plotNormalHistogram(projectdata$ES_Manipulation^3)
plotNormalHistogram(sqrt(projectdata$ES_Manipulation))

### BIO_Manipulation ----

ggplot(projectdata, aes(x = (BIO_Manipulation)^3))+
  geom_histogram(bins = 10)+
  theme_bw() #left-skew

plotNormalHistogram(projectdata$BIO_Manipulation)
plotNormalHistogram(projectdata$BIO_Manipulation^3)
plotNormalHistogram(sqrt(projectdata$BIO_Manipulation))

T_tuk = transformTukey(projectdata$BIO_Manipulation,
                 plotit=FALSE)

plotNormalHistogram(T_tuk)

### Relational_Manipulation ----

ggplot(projectdata, aes(x = Relational_Manipulation))+
  geom_histogram(bins = 10)+
  theme_bw() #left-skew

plotNormalHistogram(projectdata$Relational_Manipulation)
plotNormalHistogram(sqrt(projectdata$Relational_Manipulation))

## Predictors -----

### Nature connection (NR6 scores) ----
ggplot(projectdata, aes(x = NR6_score))+
  geom_histogram(binwidth = 0.5)+
  theme_bw() #normally distributed

### Greenspace visitation ----
ggplot(projectdata, aes(x = Greenspace_visitation))+
  geom_histogram(bins = 10) +
  theme_bw() #not sure whether we can treat as continuous?

plotNormalHistogram(projectdata$Greenspace_visitation)

### Upland visitation ----
ggplot(projectdata, aes(x = Upland_visitation))+
  geom_histogram(bins = 10)+
  theme_bw() #highly left skew - very few people visited 

plotNormalHistogram(log(projectdata$Upland_visitation + 1))

### Peatland knowledge ----
ggplot(projectdata, aes(x = Peatland_knowledge))+
  geom_histogram(bins = 5)+
  theme_bw() #approaching normal

### Age ----
ggplot(projectdata, aes(x = Age_midpoint))+
  geom_histogram(bins = 7)+
  theme_bw() #normally distributed

### IMD_Decile ----
ggplot(projectdata, aes(x = IMD_decile))+
  geom_histogram(bins = 5)+
  theme_bw() #left-skewed

##Correlation matrix plot----

#remove all non-numeric variables
cor_data<- subset(projectdata, select = -c(Respondent, Prolific_ID, Message_framing, Relational_values, Gender, Ethnicity))
cor_data<- mutate_all(cor_data, function(x) as.numeric(as.character(x)))
NAs <- cor_data[is.na(cor_data$MD_index),]
cor_data<- subset(cor_data,  IMD_decile != is.na(IMD_decile))

#compute a correlation matrix
res<- cor(cor_data, method = "pearson", use = "complete.obs")
res

#get p values
res2<- rcorr(as.matrix(cor_data), type = "pearson")
res2

res2$r
res2$P

# Preprocess row and column names: Replace underscores with spaces and capitalize words
rownames(res) <- gsub("_", " ", rownames(res))
colnames(res) <- gsub("_", " ", colnames(res))
rownames(res) <- tools::toTitleCase(rownames(res))
colnames(res) <- tools::toTitleCase(colnames(res))


#plot
png("Figures/correlation_plot.png", width = 1500, height = 1100)

cor_plot<- corrplot(res, method = 'color',
                    order = 'alphabet', 
                    tl.col = "black", 
                    addCoef.col = 'black',
                    tl.srt = 45,
                    type = "upper")

dev.off()

#no factors over 0.7 apart from behavior individual 

# Means, SEs and T-tests --------------------------------------------------------
## BD man ----
min(projectdata$BIO_Manipulation)
max(projectdata$BIO_Manipulation)

mean(projectdata$BIO_Manipulation) #8.083333
sd(projectdata$BIO_Manipulation)/sqrt(nrow(projectdata)) #0.07729655

t.test(projectdata$BIO_Manipulation, mu = 5)

## ES man ----
min(projectdata$ES_Manipulation)
max(projectdata$ES_Manipulation)

mean(projectdata$ES_Manipulation) #7.36715
sd(projectdata$ES_Manipulation)/sqrt(nrow(projectdata)) #0.08670969

t.test(projectdata$ES_Manipulation, mu = 5)

## relation man ----
min(projectdata$Relational_Manipulation)
max(projectdata$Relational_Manipulation)

mean(projectdata$Relational_Manipulation) #7.324879
sd(projectdata$Relational_Manipulation)/sqrt(nrow(projectdata)) #0.07456897

t.test(projectdata$Relational_Manipulation, mu = 5)

## Peat know change ----
min(projectdata$Peatland_knowledge)
max(projectdata$Peatland_knowledge)

mean(projectdata$Peatland_knowledge) #3.493961
sd(projectdata$Peatland_knowledge)/sqrt(nrow(projectdata)) #0.05909887

t.test(projectdata$Peatland_knowledge, mu = 0)

## Regen understanding ----
min(projectdata$Regenerate_peatland_score)
max(projectdata$Regenerate_peatland_score)

mean(projectdata$Regenerate_peatland_score) #1.307971
sd(projectdata$Regenerate_peatland_score)/sqrt(nrow(projectdata)) #0.07165069

###Wilcoxon test to measure difference from zero----
wiltest1<- wilcox.test(projectdata$Peatland_knowledge, mu = 0)
wiltest1

Zstat1<-qnorm(wiltest1$p.value/2)
Zstat1

## Advert suff ----
mean(projectdata$Advert_sufficiency) #7.2343
sd(projectdata$Advert_sufficiency)/sqrt(nrow(projectdata)) #0.07445085

t.test(projectdata$Advert_sufficiency, mu = 5)

## Behaviour----
min(projectdata$Behaviour_sum)
max(projectdata$Behaviour_sum)

mean(projectdata$Behaviour_sum) #19.26932
sd(projectdata$Behaviour_sum)/sqrt(nrow(projectdata)) #0.2389872

t.test(projectdata$Behaviour_sum, mu = 15)

## Finance ----
mean(projectdata$Financial_allocation) #11.90942
sd(projectdata$Financial_allocation)/sqrt(nrow(projectdata)) #0.4307852

mean(projectdata$Financial_compost) #4.125604
sd(projectdata$Financial_compost)/sqrt(nrow(projectdata)) #0.2427859


###Wilcoxon test to measure difference from zero----
wiltest1<- wilcox.test(projectdata$Financial_allocation, mu = 0)
wiltest1

wiltest2<- wilcox.test(projectdata$Financial_compost, mu = 0)
wiltest2

Zstat1<-qnorm(wiltest1$p.value/2)
Zstat1

Zstat2<-qnorm(wiltest2$p.value/2)
Zstat2

# Models -----------------------------------------------------------------------

## Manipulation checks ----

### Biodiversity manipulation check----

projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(BIO_Manipulation), 
            sd = sd(BIO_Manipulation),
            se = plotrix::std.error(BIO_Manipulation),
            samp_size = n())

BIO_mc_mod <- lm(BIO_Manipulation^2 ~ 
                   Message_framing + 
                   Relational_values + 
                   NR6_score + 
                   Greenspace_visitation + 
                   Upland_visitation + 
                   Peatland_knowledge + 
                   Age_midpoint + 
                   Gender + 
                   Ethnicity +
                   IMD_decile, 
                 data = projectdata)

# Check model diagnostics
autoplot(BIO_mc_mod)

#Tests
dwtest(BIO_mc_mod) #Independence (no autocor), pass
bptest(BIO_mc_mod) #Homoscedasticity, fail
shapiro.test(residuals(BIO_mc_mod)) #normality, fail (Q-Q ok)
vif(BIO_mc_mod) #multicol, no values above 10, pass

# Model coefficients
summary(BIO_mc_mod)

# Create histogram of the residuals
ggplot(projectdata, aes(x = BIO_mc_mod$residuals))+
  geom_histogram(binwidth = 20)+
  labs(title = 'Histogram of Residuals (Biodiversity manipulation)', x = 'Residuals', y = 'Frequency')+
  theme_bw() #looks normal

#Anovas
output <- Anova(BIO_mc_mod, test = "F")
output

#FDR adjustment
BIO_mc_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
BIO_mc_mod_adjust

### Ecosystem service manipulation check ----

projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(ES_Manipulation), 
            sd = sd(ES_Manipulation),
            se = plotrix::std.error(ES_Manipulation),
            samp_size = n())

projectdata %>% 
  group_by(Message_framing, Relational_values) %>% 
  summarise(mean = mean(ES_Manipulation), 
            sd = sd(ES_Manipulation),
            se = plotrix::std.error(ES_Manipulation),
            samp_size = n())

ES_mc_mod_sq <- lm(ES_Manipulation^2 ~ 
                  Message_framing + 
                  Relational_values + 
                  NR6_score + 
                  Greenspace_visitation + 
                  Upland_visitation + 
                  Peatland_knowledge + 
                  Age_midpoint + 
                  Gender + 
                  Ethnicity +
                  IMD_decile, 
                data = projectdata)

# Check model diagnostics
autoplot(ES_mc_mod_sq)

#Tests
dwtest(ES_mc_mod_sq) #Independence (no autocor), pass
bptest(ES_mc_mod_sq) #Homoscedasticity, pass
shapiro.test(residuals(ES_mc_mod_sq)) #normality, fail (Q-Q ok)
vif(ES_mc_mod_sq) #multicol, no values above 10, pass

# Model coefficients
summary(ES_mc_mod_sq)

#Anovas
output <- Anova(ES_mc_mod_sq, test = "F")
output

#FDR adjustment
ES_mc_mod_sq_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
ES_mc_mod_sq_adjust

### Relational values manipulation check ----

projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(Relational_Manipulation), 
            sd = sd(Relational_Manipulation),
            se = plotrix::std.error(Relational_Manipulation),
            samp_size = n())

# Construct full model with all predictors.
RV_mc_mod <- lm(Relational_Manipulation^2 ~ Message_framing + 
                  Relational_values + 
                  NR6_score + 
                  Greenspace_visitation + 
                  Upland_visitation + 
                  Peatland_knowledge + 
                  Age_midpoint + 
                  Gender + 
                  Ethnicity +
                  IMD_decile, data = projectdata)

# Check model diagnostics
autoplot(RV_mc_mod)

#Tests
dwtest(RV_mc_mod) #Independence (no autocor), pass
bptest(RV_mc_mod) #Homoscedasticity, pass
shapiro.test(residuals(RV_mc_mod)) #normality, fail (Q-Q ok)
vif(RV_mc_mod) #multicol, no values above 10, pass

# Model coefficients
summary(RV_mc_mod)

# Create histogram of the residuals
ggplot(projectdata, aes(x = RV_mc_mod$residuals))+
  geom_histogram(bins = 20)+
  labs(title = 'Histogram of Residuals (Relational values manipulation)', x = 'Residuals', y = 'Frequency')+
  theme_bw() #roughly normal

#Anovas
output <- Anova(RV_mc_mod, test = "F")
output

#FDR adjustment
RV_mc_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
RV_mc_mod_adjust

## Advert sufficiency ----
Advert_sufficiency_mod <- lm(Advert_sufficiency ~ 
                               Message_framing +
                               Relational_values + 
                               NR6_score + 
                               Greenspace_visitation + 
                               Upland_visitation + 
                               Peatland_knowledge + 
                               Age_midpoint + 
                               Gender + 
                               Ethnicity +
                               IMD_decile,
                             data = projectdata)

# Check model diagnostics:
autoplot(Advert_sufficiency_mod)

#Tests
dwtest(Advert_sufficiency_mod) #Independence (no autocor), pass
bptest(Advert_sufficiency_mod) #Homoscedasticity, pass
shapiro.test(residuals(Advert_sufficiency_mod)) #normality, fail
vif(Advert_sufficiency_mod) #multicol, no values above 10

# Plot a histogram of the residuals from the model:
ggplot(projectdata, aes(x = Advert_sufficiency_mod$residuals))+
  geom_histogram()+
  labs(title = 'Histogram of Residuals (Advert sufficiency: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() # close to normal

# Model coefficients:
summary(Advert_sufficiency_mod)

#Anovas
output <- Anova(Advert_sufficiency_mod, test = "F")
output

#FDR adjustment
Advert_sufficiency_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Advert_sufficiency_mod_adjust

## Peat awareness (change) ----
projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(Degraded_peatland_score), 
            sd = sd(Degraded_peatland_score),
            samp_size = n())

Degraded_peatland_score_mod <- lm(Degraded_peatland_score ~ 
                                    Message_framing + 
                                    Relational_values + 
                                    NR6_score + 
                                    Greenspace_visitation + 
                                    Upland_visitation + 
                                    Peatland_knowledge + 
                                    Age_midpoint + 
                                    Gender + 
                                    Ethnicity +
                                    IMD_decile, 
                                  data = projectdata)

# Check model diagnostics:
autoplot(Degraded_peatland_score_mod)

#Tests
dwtest(Degraded_peatland_score_mod) #Independence (no autocor), pass
bptest(Degraded_peatland_score_mod) #Homoscedasticity, fail
shapiro.test(residuals(Degraded_peatland_score_mod)) #normality, fail (Q-Q ok)
vif(Degraded_peatland_score_mod) #multicol, no values above 10, pass

# Plot a histogram of the residuals from the model:
ggplot(projectdata, aes(x = Degraded_peatland_score_mod$residuals))+
  geom_histogram()+
  labs(title = 'Histogram of Residuals (Degraded peatland score: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw()# Residuals are roughly normally distributed.

summary(Degraded_peatland_score_mod)

#Anovas
output <- Anova(Degraded_peatland_score_mod, test = "F")
output

#FDR adjustment
Degraded_peatland_score_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Degraded_peatland_score_mod_adjust

## Peat awareness (after) ----
Degraded_peatland_afterscore_sq_mod <- lm(Degraded_peatlands_after^2 ~ 
                                              Message_framing + 
                                              Relational_values + 
                                              NR6_score + 
                                              Greenspace_visitation + 
                                              Upland_visitation + 
                                              Peatland_knowledge + 
                                              Age_midpoint + 
                                              Gender + 
                                              Ethnicity +
                                              IMD_decile, 
                                            data = projectdata)

autoplot(Degraded_peatland_afterscore_sq_mod)

#Tests
dwtest(Degraded_peatland_afterscore_sq_mod) #Independence (no autocor), pass
bptest(Degraded_peatland_afterscore_sq_mod) #Homoscedasticity, pass
shapiro.test(residuals(Degraded_peatland_afterscore_sq_mod)) #normality, fail
vif(Degraded_peatland_afterscore_sq_mod) #multicol, no values above 10, pass

ggplot(projectdata, aes(x = Degraded_peatland_afterscore_sq_mod$residuals))+
  geom_histogram()+
  labs(title = 'Histogram of square root transformed Residuals (Degraded peatland after score)', x = 'Residuals', y = 'Frequency')+
  theme_bw() #better now, approaching normal

summary(Degraded_peatland_afterscore_sq_mod)

#Anovas
output <- Anova(Degraded_peatland_afterscore_sq_mod, test = "F")
output

#FDR adjustment
Degraded_peatland_afterscore_sq_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Degraded_peatland_afterscore_sq_mod_adjust

## Regeneration understanding (change)----

#GLM
#reflect
projectdata$Regenerate_peatland_score_reflect = projectdata$Regenerate_peatland_score - min(projectdata$Regenerate_peatland_score) + 1

plotNormalHistogram(projectdata$Regenerate_peatland_score)
plotNormalHistogram(projectdata$Regenerate_peatland_score_reflect)

Regenerate_peatland_score_mod_glm <- glm(
  Regenerate_peatland_score_reflect ~ 
    Message_framing + 
    Relational_values + 
    NR6_score + 
    Greenspace_visitation + 
    Upland_visitation + 
    Peatland_knowledge + 
    Age_midpoint + 
    Gender + 
    Ethnicity  +
    IMD_decile,
  data = projectdata, 
  family = Gamma(link = "log")
)

autoplot(Regenerate_peatland_score_mod_glm)

ggplot(projectdata, aes(x = Regenerate_peatland_score_mod_glm$residuals))+
  geom_histogram(bins = 20)+
  labs(title = 'Histogram of Residuals (Regenerate peatland score: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() # Residuals are roughly normally distributed.

# Model coefficients
summary(Regenerate_peatland_score_mod_glm)

#Anovas
output <- Anova(Regenerate_peatland_score_mod_glm, test = "F")
output

with(summary(Regenerate_peatland_score_mod_glm), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(Regenerate_peatland_score_reflect ~ 1, data = projectdata, family = Gamma(link = "log"))
anova(Regenerate_peatland_score_mod_glm, mod_0, test="F") #F stats and p-value

confint(Regenerate_peatland_score_mod_glm)

adjustedregen <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 11)
adjustedregen

## Regen understanding (after)----

Regenerate_peatland_afterscore_mod <- lm((Regenerate_peatlands_after + 11)^3 ~ 
                                           Message_framing + 
                                           Relational_values +
                                           NR6_score + 
                                           Greenspace_visitation + 
                                           Upland_visitation + 
                                           Peatland_knowledge + 
                                           Age_midpoint + 
                                           Gender + 
                                           Ethnicity +
                                           IMD_decile, 
                                         data = projectdata)

autoplot(Regenerate_peatland_afterscore_mod)

#Tests
dwtest(Regenerate_peatland_afterscore_mod) #Independence (no autocor), pass
bptest(Regenerate_peatland_afterscore_mod) #Homoscedasticity, fail
shapiro.test(residuals(Regenerate_peatland_afterscore_mod)) #normality, fail
vif(Regenerate_peatland_afterscore_mod) #multicol, no values above 10

ggplot(projectdata, aes(x = Regenerate_peatland_afterscore_mod$residuals))+
  geom_histogram()+
  labs(title = 'Histogram of Residuals (After regenerate peatland score: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() # Residuals are left

# Model coefficients:
summary(Regenerate_peatland_afterscore_mod)

#Anovas
output <- Anova(Regenerate_peatland_afterscore_mod, test = "F")
output

#FDR adjustment
Regenerate_peatland_afterscore_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Regenerate_peatland_afterscore_mod_adjust

## Behavioural support ----
Behaviour_mod <- lm(Behaviour_sum ~ 
                      Message_framing + 
                      Relational_values + 
                      NR6_score + 
                      Greenspace_visitation + 
                      Upland_visitation + 
                      Peatland_knowledge + 
                      Age_midpoint + 
                      Gender + 
                      Ethnicity +
                      IMD_decile, data = projectdata)

autoplot(Behaviour_mod)

#Tests
dwtest(Behaviour_mod) #Independence (no autocor), pass
bptest(Behaviour_mod) #Homoscedasticity, pass
shapiro.test(residuals(Behaviour_mod)) #normality, fail
vif(Behaviour_mod) #multicol, no values above 10

# Plot a histogram of the residuals from the model:

ggplot(projectdata, aes(x = Behaviour_mod$residuals))+
  geom_histogram()+
  labs(title = 'Histogram of Residuals (Behaviour support: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() # Residuals are roughly normally distributed

# Model coefficients:
summary(Behaviour_mod)

#Anovas
output <- Anova(Behaviour_mod, test = "F")
output

#FDR adjustment
Behaviour_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Behaviour_mod_adjust

### checking for individual behaviour ----

Behaviour_mod_1 <- lm(Behaviour_1 ~ 
                      Message_framing + 
                      Relational_values + 
                      NR6_score + 
                      Greenspace_visitation + 
                      Upland_visitation + 
                      Peatland_knowledge + 
                      Age_midpoint + 
                      Gender + 
                      Ethnicity +
                      IMD_decile, data = projectdata)

anova(Behaviour_mod_1)

Behaviour_mod_2 <- lm(Behaviour_2 ~ 
                        Message_framing + 
                        Relational_values + 
                        NR6_score + 
                        Greenspace_visitation + 
                        Upland_visitation + 
                        Peatland_knowledge + 
                        Age_midpoint + 
                        Gender + 
                        Ethnicity +
                        IMD_decile, data = projectdata)

anova(Behaviour_mod_2)

Behaviour_mod_3 <- lm(Behaviour_3 ~ 
                        Message_framing + 
                        Relational_values + 
                        NR6_score + 
                        Greenspace_visitation + 
                        Upland_visitation + 
                        Peatland_knowledge + 
                        Age_midpoint + 
                        Gender + 
                        Ethnicity +
                        IMD_decile, data = projectdata)

anova(Behaviour_mod_3)

## Financial support ----

### PCA ----

#reflect to remove negs
projectdata$financePCA_reflect = projectdata$financePCA - min(projectdata$financePCA)

#model

Finance_PCA <- glm(
  financePCA_reflect ~
    Message_framing +
    Relational_values +
    NR6_score +
    Greenspace_visitation +
    Upland_visitation +
    Peatland_knowledge +
    Age_midpoint +
    Gender +
    Ethnicity +
    IMD_decile,
  data = projectdata,
  family = quasipoisson
)

autoplot(Finance_PCA)

#Tests
dwtest(Finance_PCA) #Independence (no autocor), pass
bptest(Finance_PCA) #Homoscedasticity, fail
shapiro.test(residuals(Finance_PCA)) #normality, fail
vif(Finance_PCA) #multicol, no values above 10

#Check Residuals
ggplot(projectdata, aes(x = Finance_PCA$residuals))+
  geom_histogram(bins = 10)+
  labs(title = 'Histogram of square root transformed Residuals (Financial allocation: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() #slight right skew still but better toward ND

summary(Finance_PCA)

#Anovas
output <- Anova(Finance_PCA, test = "F")
output

with(summary(Finance_PCA), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financePCA_reflect ~ 1, data = projectdata, family = quasipoisson)
anova(Finance_PCA, mod_0, test="F") #F stats and p-value

confint(Regenerate_peatland_score_mod_glm)

#FDR adjustment
Finance_pca_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Finance_pca_mod_adjust

### Allocation ----
Finance_allocation_sqrt_mod <- lm(sqrt(Financial_allocation) ~ 
                                                Message_framing + 
                                                Relational_values + 
                                                NR6_score + 
                                                Greenspace_visitation + 
                                                Upland_visitation + 
                                                Peatland_knowledge + 
                                                Age_midpoint + 
                                                Gender + 
                                                Ethnicity +
                                                IMD_decile, 
                                              data = projectdata)

autoplot(Finance_allocation_sqrt_mod)

#Tests
dwtest(Finance_allocation_sqrt_mod) #Independence (no autocor), pass
bptest(Finance_allocation_sqrt_mod) #Homoscedasticity, fail
shapiro.test(residuals(Finance_allocation_sqrt_mod)) #normality, fail
vif(Finance_allocation_sqrt_mod) #multicol, no values above 10

#Check Residuals
ggplot(projectdata, aes(x = Finance_allocation_sqrt_mod$residuals))+
  geom_histogram(bins = 10)+
  labs(title = 'Histogram of square root transformed Residuals (Financial allocation: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() #slight right skew still but better toward ND

summary(Finance_allocation_sqrt_mod)

#Anovas
output <- Anova(Finance_allocation_sqrt_mod, test = "F")
output

#FDR adjustment
Finance_allocation_sqrt_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Finance_allocation_sqrt_mod_adjust

### Compost ----
Finance_compost_sqrt_mod <- lm(log(Financial_compost + 1) ~ 
                                 Message_framing + 
                                 Relational_values + 
                                 NR6_score + 
                                 Greenspace_visitation + 
                                 Upland_visitation + 
                                 Peatland_knowledge + 
                                 Age_midpoint + 
                                 Gender + 
                                 Ethnicity +
                                 IMD_decile, 
                               data = projectdata)

autoplot(Finance_compost_sqrt_mod)

#Tests
dwtest(Finance_allocation_sqrt_mod) #Independence (no autocor), pass
bptest(Finance_allocation_sqrt_mod) #Homoscedasticity, fail
shapiro.test(residuals(Finance_allocation_sqrt_mod)) #normality, fail
vif(Finance_allocation_sqrt_mod) #multicol, no values above 10

ggplot(projectdata, aes(x = Finance_compost_sqrt_mod$residuals))+
  geom_histogram(bins = 15)+
  labs(title = 'Histogram of square root transformed Residuals (Financial compost: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() #slight right skew still but better toward ND

summary(Finance_compost_sqrt_mod)

#Anovas
output <- Anova(Finance_compost_sqrt_mod, test = "F")
output

#FDR adjustment
Finance_compost_sqrt_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Finance_compost_sqrt_mod_adjust

# PLOTS ----

# Box plots ----------------------------------------------------------------

mean_checks <- projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(BIO_Manipulation),
            se = sd(BIO_Manipulation) / sqrt(n()))

mean_checks

## Manipulation checks ----
BIO <- ggplot(projectdata, aes(x=Message_framing, y=BIO_Manipulation, fill=Message_framing)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Message framing", y = "Biodiversity manipulation check") +
  scale_fill_manual(values = c("BIO" = "#7BB2D9", "ES" = "#BFD2BF"),
                    labels = c("BIO" = "Biodiversity", "ES" = "Ecosystem services")) +
  scale_x_discrete(labels = c("BIO" = "Biodiversity", "ES" = "Ecosystem services"))  +
  theme(legend.position = "none") +
  facet_wrap(~Relational_values,
             labeller = labeller(Relational_values = c("Absent" = "Relational values absent", 
                                                       "Present" = "Relational values present"))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),                          # Increase y-axis text size
        axis.title.x = element_text(size = 16),                         # Increase x-axis title size
        axis.title.y = element_text(size = 16),                         # Increase y-axis title size
        strip.text = element_text(size = 14))
#axis.title.y = element_blank(),
#guides(color=guide_legend("Message framing"), fill = "none")

BIO

mean_checks <- projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(ES_Manipulation),
            se = sd(ES_Manipulation) / sqrt(n()))

mean_checks

mean_checks_re <- projectdata %>% 
  group_by(Message_framing, Relational_values) %>% 
  summarise(mean = mean(ES_Manipulation),
            se = sd(ES_Manipulation) / sqrt(n()))

mean_checks_re

ES <- ggplot(projectdata, aes(x=Message_framing, y=ES_Manipulation, fill=Message_framing)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Message framing", y = "Ecosystem service manipulation check") +
  scale_fill_manual(values = c("BIO" = "#7BB2D9", "ES" = "#BFD2BF"))+
  scale_x_discrete(labels = c("BIO" = "Biodiversity", "ES" = "Ecosystem services"))  +
  theme(legend.position = "none")+
  facet_wrap(~Relational_values,
             labeller = labeller(Relational_values = c("Absent" = "Relational values absent", 
                                                       "Present" = "Relational values present"))) +
theme(legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y = element_text(size = 16),                          # Increase y-axis text size
      axis.title.x = element_text(size = 16),                         # Increase x-axis title size
      axis.title.y = element_text(size = 16),                         # Increase y-axis title size
      strip.text = element_text(size = 14))
#axis.title.y = element_blank(),
#guides(color=guide_legend("Message framing"), fill = "none")

ES

mean_checks <- projectdata %>% 
  group_by(Message_framing) %>% 
  summarise(mean = mean(Relational_Manipulation),
            se = sd(Relational_Manipulation) / sqrt(n()))

mean_checks

relation <- ggplot(projectdata, aes(x=Relational_values, y=Relational_Manipulation, fill=Relational_values)) +
  geom_boxplot() +
  theme_classic() +
  labs(x = "Relational framing", y = "Relational values manipulation check") + 
    scale_fill_manual(values = c("Absent" = "grey50", "Present" = "#FFC847"))+
  #scale_x_discrete(labels = c("Absent" = "Relational values absent", "Present" = "Relational values present"))  +
  theme(legend.position = "none")+
  facet_wrap(~Message_framing,
             labeller = labeller(Message_framing = c("BIO" = "Biodiversity", 
                                                       "ES" = "Ecosystem services"))) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),                          # Increase y-axis text size
        axis.title.x = element_text(size = 16),                         # Increase x-axis title size
        axis.title.y = element_text(size = 16),                         # Increase y-axis title size
        strip.text = element_text(size = 14))
#axis.title.y = element_blank(),
#guides(color=guide_legend("Message framing"), fill = "none")

relation 

man_figures <- plot_grid(BIO, ES, relation, ncol = 2, labels = c('a', 'b', 'c'), align = "v",
                         label_size = 20)
man_figures

ggsave(file = "Figures/man_figure.png", man_figures, units = "mm", height = 430, width = 350)

# Scatter plots ----
## Peatland degradation awareness ----

### Nature connection ----
nature_awareness <- ggplot(projectdata, aes(x=NR6_score, y= Degraded_peatland_score))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Nature connection", y = "Change in peatland degradation awareness") +
  theme(
        legend.position = "none")
  #guides(color=guide_legend("Message framing"), fill = "none")

nature_awareness

### Peatland knowledge ----
knowledge_awareness <- ggplot(projectdata, aes(x=Peatland_knowledge, y= Degraded_peatland_score))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Peatland knowledge", y = "Degradation awareness")#+
  #theme(text = element_text(size = 15),
      #axis.title.y = element_blank(),
     #legend.position = "none")+
  #guides(color=guide_legend("Message framing"), fill = "none")

knowledge_awareness

## Peatland regeneration understanding  ----

### Nature connection ---
nature_understanding <- ggplot(projectdata, aes(x=NR6_score, y= Regenerate_peatland_score_reflect))+
  geom_point(alpha=0.2)+
  #geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Nature connection", y = "Change in understanding need for regeneration")#+
#theme(text = element_text(size = 15),
#axis.title.y = element_blank(),
#legend.position = "none")+
#guides(color=guide_legend("Message framing"), fill = "none")

nature_understanding

### Greenspace visitation ----
greenspace_understanding <- ggplot(projectdata, aes(x=Greenspace_visitation, y= Regenerate_peatlands_after))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Greenspace visitation", y = "Regeneration understanding")#+
#theme(text = element_text(size = 15),
#axis.title.y = element_blank(),
#legend.position = "none")+
#guides(color=guide_legend("Message framing"), fill = "none")

greenspace_understanding

## Behavioural support (summed score) ----

### Nature connection ----
nature_behav <- ggplot(projectdata, aes(x = NR6_score, y = Behaviour_sum))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  ylab('Behavioural support')+
  xlab('Nature connection')+
  theme_classic()

nature_behav 

### Greenspace visitation----
green_behav<- ggplot(projectdata, aes(x = Greenspace_visitation, y = Behaviour_sum))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  ylab('Behavioural support')+
  xlab('Greenspace visitation')+
  theme_bw()

green_behav

## Financial support (amount of £100 allocated to the campaign) ----

# Response variable is square root transformed (y-axis) as sqrt transformations 
# of the response were used in the final model.

### Nature connection----
nature_fin_pca <- ggplot(projectdata, aes(x = NR6_score, y = financePCA))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = glm, colour = '#048A81', se = TRUE)+
  ylab('Financial support')+
  xlab('Nature connection')+
  theme_classic()

nature_fin_pca

### Greenspace visitation----
green_fin_allo <- ggplot(projectdata, aes(x = Greenspace_visitation, y = financePCA_reflect))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  scale_y_sqrt()+
  ylab('Financial allocation')+
  xlab('Greenspace visitation')+
  theme_bw()

green_fin_allo

## Financial support (additional money for peat-free compost) ----

# Response variable is square root transformed (y-axis) as sqrt transformations 
# of the response were used in the final model.

### Nature connection ----
nature_fin_don <- ggplot(projectdata, aes(x = NR6_score, y = Financial_compost))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  scale_y_sqrt()+ # transform response scale to square root
  ylab('Financial compost')+
  xlab('Nature connection')+
  theme_bw()

nature_fin_don

## Advert sufficiency ----

### Nature connection ----
nature_suff<- ggplot(projectdata, aes(x = NR6_score, y = Advert_sufficiency))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  ylab('Advertisement sufficiency')+
  xlab('Nature connection')+ 
  theme_classic() +
  theme(
        legend.position = "none") 

nature_suff

### Greenspace visitation ----
green_suff <- ggplot(projectdata, aes(x = Greenspace_visitation, y = Advert_sufficiency))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  ylab('Advert sufficiency')+
  xlab('Greenspace visitation')+
  theme_bw()

green_suff

### Peatland knowledge ----
knowledge_suff <- ggplot(projectdata, aes(x = Peatland_knowledge, y = Advert_sufficiency))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  ylab('Advert sufficiency')+
  xlab('Peatland knowledge')+
  theme_bw()

knowledge_suff

###plot all scatter ----

all_nature_scatter <- plot_grid(nature_suff, nature_awareness,nature_behav, nature_fin_pca, nature_understanding, labels = c("a", "b", "c", "d", "e"), ncol = 2, align = "hv")

all_nature_scatter 

ggsave(file = "Figures/nature_scatter.png", all_nature_scatter, units = "mm", height = 310, width = 230)

# Bar plots --------------------------------------------------------------------

## Message framing----

### Degraded peatlands ----
degraded_peatlands_after_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(Degraded_peatlands_after),
            sd = sd(Degraded_peatlands_after),
            se = sd(Degraded_peatlands_after) / sqrt(sum(Degraded_peatlands_after)))

degraded_peatlands_after_summary

degraded_peatlands_mf <- ggplot(degraded_peatlands_after_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Degradation awareness') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  labs(colour = "Message framing")+
  theme(legend.position = "none")

degraded_peatlands_mf

### Regenerate peatlands ----

regenerate_peatlands_after_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(Regenerate_peatlands_after),
            sd = sd(Regenerate_peatlands_after),
            se = sd(Regenerate_peatlands_after) / sqrt(sum(Regenerate_peatlands_after)))

regenerate_peatlands_after_summary

regenerate_peatlands_mf <- ggplot(regenerate_peatlands_after_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Regeneration understanding') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  labs(colour = "Message framing")+
  theme(legend.position = "none")

regenerate_peatlands_mf

### Behavioural support ----

behaviour_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(Behaviour_sum),
            sd = sd(Behaviour_sum),
            se = sd(Behaviour_sum) / sqrt(sum(Behaviour_sum)))

behaviour_summary

behav_mf <- ggplot(behaviour_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Behavioural support') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  theme(legend.position = "none")

behav_mf

### Financial allocation ----

financial_allocation_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(Financial_allocation),
            sd = sd(Financial_allocation),
            se = sd(Financial_allocation) / sqrt(sum(Financial_allocation)))

financial_allocation_summary

finance_allo_mf <- ggplot(financial_allocation_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Financial allocation') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  theme(legend.position = "none")

finance_allo_mf

### Financial compost ----

financial_compost_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(Financial_compost),
            sd = sd(Financial_compost),
            se = sd(Financial_compost) / sqrt(sum(Financial_compost)))

financial_compost_summary

finance_compost_mf <- ggplot(financial_compost_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Financial compost') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  theme(legend.position = "none")

finance_compost_mf

### finance PCR ----

financial_PCA_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(financePCA_reflect),
            sd = sd(financePCA_reflect),
            se = sd(financePCA_reflect) / sqrt(sum(financePCA_reflect)))

financial_PCA_summary

finance_pca_mf<- ggplot(financial_PCA_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Financial support (reflected)') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  theme(legend.position = "none")

finance_pca_mf

### Advert sufficiency ----

advert_sufficiency_summary <- projectdata %>%
  group_by(Message_framing) %>%
  summarise(mean = mean(Advert_sufficiency),
            sd = sd(Advert_sufficiency),
            se = sd(Advert_sufficiency) / sqrt(sum(Advert_sufficiency)))

advert_sufficiency_summary

suff_mf<- ggplot(advert_sufficiency_summary, aes(x = Message_framing, y = mean, colour = Message_framing))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Advert sufficiency') + xlab('Message framing')+
  scale_colour_manual(values = c(BIO = '#B4E7CE', ES = '#59A96A'))+
  theme_bw()+
  theme(legend.position = "none")

suff_mf

### Plot all together ----

all_mf <- plot_grid(suff_mf, degraded_peatlands_mf, regenerate_peatlands_mf, behav_mf, finance_pca_mf, labels = c("a", "b", "c", "d", "e"), ncol = 2)

all_mf

ggsave(file = "Figures/all_mf.png", all_mf, units = "mm", height = 350, width = 250)

## Relational values ----

### Degraded peatlands ----

degraded_peatlands_after_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(Degraded_peatlands_after),
            sd = sd(Degraded_peatlands_after),
            se = sd(Degraded_peatlands_after) / sqrt(sum(Degraded_peatlands_after)))

degraded_peatlands_after_summary_RV

degraded_peatlands_relation <- ggplot(degraded_peatlands_after_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Degradation awareness') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

degraded_peatlands_relation

### Regenerate peatlands ----

regenerate_peatlands_after_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(Regenerate_peatlands_after),
            sd = sd(Regenerate_peatlands_after),
            se = sd(Regenerate_peatlands_after) / sqrt(sum(Regenerate_peatlands_after)))

regenerate_peatlands_after_summary_RV

regenerate_peatlands_relation <- ggplot(regenerate_peatlands_after_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Regeneration understanding') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

regenerate_peatlands_relation

### Behavioural support ----

behaviour_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(Behaviour_sum),
            sd = sd(Behaviour_sum),
            se = sd(Behaviour_sum) / sqrt(sum(Behaviour_sum)))

behaviour_summary_RV

behav_relation<- ggplot(behaviour_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Behavioural support') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

behav_relation

### Financial allocation ----

financial_allocation_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(Financial_allocation),
            sd = sd(Financial_allocation),
            se = sd(Financial_allocation) / sqrt(sum(Financial_allocation)))

financial_allocation_summary_RV

finance_allo_relation <- ggplot(financial_allocation_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Financial allocation') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

finance_allo_relation

### Financial compost ----

financial_compost_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(Financial_compost),
            sd = sd(Financial_compost),
            se = sd(Financial_compost) / sqrt(sum(Financial_compost)))

financial_compost_summary_RV

finance_compost_relation <- ggplot(financial_compost_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Financial compost') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

finance_compost_relation

### Financial pca ----

financial_pca_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(financePCA_reflect),
            sd = sd(financePCA_reflect),
            se = sd(financePCA_reflect) / sqrt(sum(financePCA_reflect)))

financial_pca_summary_RV

finance_pca_relation <- ggplot(financial_pca_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Financial support (reflected)') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

finance_pca_relation

### Advert sufficiency ----

advert_sufficiency_summary_RV <- projectdata %>%
  group_by(Relational_values) %>%
  summarise(mean = mean(Advert_sufficiency),
            sd = sd(Advert_sufficiency),
            se = sd(Advert_sufficiency) / sqrt(sum(Advert_sufficiency)))

advert_sufficiency_summary_RV

suff_relation<- ggplot(advert_sufficiency_summary_RV, aes(x = Relational_values, y = mean, colour = Relational_values))+
  geom_point(cex = 6)+
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se), width = 0.1, linewidth = 2)+
  ylab('Advert sufficiency') + xlab('Relational values')+
  scale_colour_manual(values = c(Absent = '#D9CAB3', Present = '#BC8034'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

suff_relation

### Plot all together ----

all_relation <- plot_grid(suff_relation, degraded_peatlands_relation, regenerate_peatlands_relation, behav_relation, finance_pca_relation, labels = c("a", "b", "c", "d", "e"), ncol = 2)

all_relation

ggsave(file = "Figures/all_relation.png", all_relation, units = "mm", height = 350, width = 250)

## Model summary plots----
#Variables for plotting
all.models <- list()
all.models[[1]] <- Degraded_peatland_score_mod
all.models[[2]] <- Regenerate_peatland_score_mod_glm
all.models[[3]] <- Advert_sufficiency_mod
all.models[[4]] <- Behaviour_mod
all.models[[5]] <- Finance_PCA

treatment_terms <- c("message_framinges-global",
                     "message_framinges-local",
                     "nudgepresent")

treatment_terms_label <- c("Nudge",
                           "Local-ES",
                           "Global-ES")

socio_terms<- c("age",
                "finance_security",
                "gender(2) Other",
                "gender(3) Male",
                "ethnicity(2) Other",
                "education_rank",
                "MD_index")

socio_terms_labels <- c("MD Index",
                        "Education",
                        "Ethnicity (Other)",
                        "Gender (Male)",
                        "Age",
                        "Financial security")

psyco_terms <- c("connectedness",
                 "experience",
                 "ego",
                 "climate_scores",
                 "meanflood",
                 "social_norm",
                 "log(1 + social_norm_donation)",
                 "efficacy")

psyco_terms_labels <- c("Percieved social norm (donation)",
                        "Percieved social norm",
                        "Flood experience",
                        "Climate change sceptisism",
                        "Egoism",
                        "Global South awareness",
                        "Nature connection",
                        "Self-efficacy")

model_names <- c("Advert sufficency", "Sympathetic attitudes", "Behavioural support", "Finance support")

###Treatments variables----
model_sum_treatments <- sjPlot::plot_models(all.models,
                                    vline.color = "black",
                                    show.values = TRUE,
                                    #rm.terms = c(socio_terms, psyco_terms),
                                    spacing = 0.75,
                                    show.p = T,
                                    colors = NULL,
                                    #m.labels = model_names,
                                    legend.title = "Outcome variables",
                                    #axis.labels = treatment_terms_label,
                                    value.size = 3,
                                    dot.size = 3,
                                    p.shape = T,
                                    legend.pval.title = "Significance level") +
  theme_classic()

model_sum_treatments<- model_sum_treatments + theme(axis.text.y = element_text(angle = 40, vjust = 0.5, hjust=1))
model_sum_treatments

ggsave(path = "Figures", filename = "model_sum_treatments_plot.png", model_sum_treatments, height =13, width =8)

#Demographics-------------------------------------------------------------------

##Total----
nrow(projectdata) #828

#age
(nrow(subset(projectdata, Age_midpoint=="21"))/nrow(projectdata))*100 #8.454106
(nrow(subset(projectdata, Age_midpoint=="29.5"))/nrow(projectdata))*100 #20.77295
(nrow(subset(projectdata, Age_midpoint=="39.5"))/nrow(projectdata))*100 #19.44444
(nrow(subset(projectdata, Age_midpoint=="49.5"))/nrow(projectdata))*100 #16.90821
(nrow(subset(projectdata, Age_midpoint=="59.5"))/nrow(projectdata))*100 #21.73913
(nrow(subset(projectdata, Age_midpoint=="69.5"))/nrow(projectdata))*100 #10.50725
(nrow(subset(projectdata, Age_midpoint=="79.5"))/nrow(projectdata))*100 #1.932367
(nrow(subset(projectdata, Age_midpoint=="89.5"))/nrow(projectdata))*100 #0.2415459

min(projectdata$NR6_score)
max(projectdata$NR6_score)
mean(projectdata$NR6_score) #3.371578
plotrix::std.error(projectdata$NR6_score) #0.02545617

min(projectdata$Greenspace_visitation)
max(projectdata$Greenspace_visitation)
mean(projectdata$Greenspace_visitation) #12.97524
plotrix::std.error(projectdata$Greenspace_visitation) #0.3778233

min(projectdata$Upland_visitation)
max(projectdata$Upland_visitation)
mean(projectdata$Upland_visitation) #1.569444
plotrix::std.error(projectdata$Upland_visitation) #0.1349734

min(projectdata$Peatland_knowledge)
max(projectdata$Peatland_knowledge)
mean(projectdata$Peatland_knowledge) #1.569444
plotrix::std.error(projectdata$Peatland_knowledge) #0.1349734

#gender
(nrow(subset(projectdata, Gender=="Female"))/nrow(projectdata))*100 #53.38164
(nrow(subset(projectdata, Gender=="Male"))/nrow(projectdata))*100 #46.61836

min(projectdata$Peatland_knowledge)
max(projectdata$Peatland_knowledge)
mean(projectdata$Peatland_knowledge) #1.569444
plotrix::std.error(projectdata$Peatland_knowledge) #0.1349734

#ethnicity
(nrow(subset(projectdata, Ethnicity=="White or Caucasian"))/nrow(projectdata))*100 #87.80
(nrow(subset(projectdata, Ethnicity=="Other"))/nrow(projectdata))*100 # 12.19807

min(projectdata$IMD_decile)
max(projectdata$IMD_decile)
mean(projectdata$IMD_decile) #1.569444
plotrix::std.error(projectdata$IMD_decile) #0.1349734

min(projectdata$Age_midpoint)
max(projectdata$Age_midpoint)
mean(projectdata$Age_midpoint) #1.569444
plotrix::std.error(projectdata$Age_midpoint) #0.1349734

#BD framing ----

BD_only <- projectdata %>% subset(Message_framing == "BIO")

#age
(nrow(subset(BD_only, Age_midpoint=="21"))/nrow(BD_only))*100 #6.490385
(nrow(subset(BD_only, Age_midpoint=="29.5"))/nrow(BD_only))*100 #22.83654
(nrow(subset(BD_only, Age_midpoint=="39.5"))/nrow(BD_only))*100 #18.99038
(nrow(subset(BD_only, Age_midpoint=="49.5"))/nrow(BD_only))*100 #15.14423
(nrow(subset(BD_only, Age_midpoint=="59.5"))/nrow(BD_only))*100 #22.11538
(nrow(subset(BD_only, Age_midpoint=="69.5"))/nrow(BD_only))*100 #11.53846
(nrow(subset(BD_only, Age_midpoint=="79.5"))/nrow(BD_only))*100 #2.644231
(nrow(subset(BD_only, Age_midpoint=="89.5"))/nrow(BD_only))*100 #0.2403846

mean(BD_only$NR6_score) 
plotrix::std.error(BD_only$NR6_score)

mean(BD_only$Greenspace_visitation) 
plotrix::std.error(BD_only$Greenspace_visitation)

mean(BD_only$Upland_visitation)
plotrix::std.error(BD_only$Upland_visitation) 

mean(BD_only$Peatland_knowledge)
plotrix::std.error(BD_only$Peatland_knowledge) 

mean(BD_only$IMD_decile) 
plotrix::std.error(BD_only$IMD_decile)

mean(BD_only$Age_midpoint)
plotrix::std.error(BD_only$Age_midpoint) 

mean(BD_only$Relational_Manipulation)
plotrix::std.error(BD_only$Relational_Manipulation) 

#gender
(nrow(subset(BD_only, Gender=="Female"))/nrow(BD_only))*100 #52.40385
(nrow(subset(BD_only, Gender=="Male"))/nrow(BD_only))*100 #47.59615
(nrow(subset(BD_only, Gender=="Other"))/nrow(BD_only))*100

#ethnicity
(nrow(subset(BD_only, Ethnicity=="White or Caucasian"))/nrow(BD_only))*100 #87.98077
(nrow(subset(BD_only, Ethnicity=="Other"))/nrow(BD_only))*100 # 12.01923

#ES framing ----

ES_only <- projectdata %>% subset(Message_framing == "ES")

#age
(nrow(subset(ES_only, Age_midpoint=="21"))/nrow(ES_only))*100 #10.43689
(nrow(subset(ES_only, Age_midpoint=="29.5"))/nrow(ES_only))*100 #18.68932
(nrow(subset(ES_only, Age_midpoint=="39.5"))/nrow(ES_only))*100 #19.90291
(nrow(subset(ES_only, Age_midpoint=="49.5"))/nrow(ES_only))*100 #18.68932
(nrow(subset(ES_only, Age_midpoint=="59.5"))/nrow(ES_only))*100 #21.35922
(nrow(subset(ES_only, Age_midpoint=="69.5"))/nrow(ES_only))*100 # 9.466019
(nrow(subset(ES_only, Age_midpoint=="79.5"))/nrow(ES_only))*100 #1.213592
(nrow(subset(ES_only, Age_midpoint=="89.5"))/nrow(ES_only))*100 #0.2427184

mean(ES_only$NR6_score) 
plotrix::std.error(ES_only$NR6_score)

mean(ES_only$Greenspace_visitation) 
plotrix::std.error(ES_only$Greenspace_visitation)

mean(ES_only$Upland_visitation)
plotrix::std.error(ES_only$Upland_visitation) 

mean(ES_only$Peatland_knowledge)
plotrix::std.error(ES_only$Peatland_knowledge) 

mean(ES_only$IMD_decile) 
plotrix::std.error(ES_only$IMD_decile)

mean(ES_only$Age_midpoint)
plotrix::std.error(ES_only$Age_midpoint) 

mean(ES_only$Relational_Manipulation)
plotrix::std.error(ES_only$Relational_Manipulation) 

#gender
(nrow(subset(ES_only, Gender=="Female"))/nrow(ES_only))*100 #54.36893
(nrow(subset(ES_only, Gender=="Male"))/nrow(ES_only))*100 #45.63107

#ethnicity
(nrow(subset(ES_only, Ethnicity=="White or Caucasian"))/nrow(ES_only))*100 #87.62136
(nrow(subset(ES_only, Ethnicity=="Other"))/nrow(ES_only))*100 #  12.37864

#relational framing ----

relation_only <- projectdata %>% subset(Relational_values == "Present")

#age
(nrow(subset(relation_only, Age_midpoint=="21"))/nrow(relation_only))*100 #8.173077
(nrow(subset(relation_only, Age_midpoint=="29.5"))/nrow(relation_only))*100 #20.43269
(nrow(subset(relation_only, Age_midpoint=="39.5"))/nrow(relation_only))*100 #20.19231
(nrow(subset(relation_only, Age_midpoint=="49.5"))/nrow(relation_only))*100 #15.625
(nrow(subset(relation_only, Age_midpoint=="59.5"))/nrow(relation_only))*100 #24.51923
(nrow(subset(relation_only, Age_midpoint=="69.5"))/nrow(relation_only))*100 #9.375
(nrow(subset(relation_only, Age_midpoint=="79.5"))/nrow(relation_only))*100 #1.682692
(nrow(subset(relation_only, Age_midpoint=="89.5"))/nrow(relation_only))*100 #0

mean(relation_only$NR6_score) 
plotrix::std.error(relation_only$NR6_score)

mean(relation_only$Greenspace_visitation) 
plotrix::std.error(relation_only$Greenspace_visitation)

mean(relation_only$Upland_visitation)
plotrix::std.error(relation_only$Upland_visitation) 

mean(relation_only$Peatland_knowledge)
plotrix::std.error(relation_only$Peatland_knowledge) 

mean(relation_only$IMD_decile) 
plotrix::std.error(relation_only$IMD_decile)

mean(relation_only$Age_midpoint)
plotrix::std.error(relation_only$Age_midpoint) 

mean(relation_only$Relational_Manipulation)
plotrix::std.error(relation_only$Relational_Manipulation) 

#gender
(nrow(subset(relation_only, Gender=="Female"))/nrow(relation_only))*100 #51.44
(nrow(subset(relation_only, Gender=="Male"))/nrow(relation_only))*100 #48.55769

#ethnicity
(nrow(subset(relation_only, Ethnicity=="White or Caucasian"))/nrow(relation_only))*100 #87.01923
(nrow(subset(relation_only, Ethnicity=="Other"))/nrow(relation_only))*100 #  12.98077

# non-relational framing ----

non_relation_only <- projectdata %>% subset(Relational_values == "Absent")

#age
(nrow(subset(non_relation_only, Age_midpoint=="21"))/nrow(non_relation_only))*100 #8.737864
(nrow(subset(non_relation_only, Age_midpoint=="29.5"))/nrow(non_relation_only))*100 #21.1165
(nrow(subset(non_relation_only, Age_midpoint=="39.5"))/nrow(non_relation_only))*100 #18.68932
(nrow(subset(non_relation_only, Age_midpoint=="49.5"))/nrow(non_relation_only))*100 #18.20388
(nrow(subset(non_relation_only, Age_midpoint=="59.5"))/nrow(non_relation_only))*100 #18.93204
(nrow(subset(non_relation_only, Age_midpoint=="69.5"))/nrow(non_relation_only))*100 #11.65049
(nrow(subset(non_relation_only, Age_midpoint=="79.5"))/nrow(non_relation_only))*100 #2.184466
(nrow(subset(non_relation_only, Age_midpoint=="89.5"))/nrow(non_relation_only))*100 #0.4854369

mean(non_relation_only$NR6_score) 
plotrix::std.error(non_relation_only$NR6_score)

mean(non_relation_only$Greenspace_visitation) 
plotrix::std.error(non_relation_only$Greenspace_visitation)

mean(non_relation_only$Upland_visitation)
plotrix::std.error(non_relation_only$Upland_visitation) 

mean(non_relation_only$Peatland_knowledge)
plotrix::std.error(non_relation_only$Peatland_knowledge) 

mean(non_relation_only$IMD_decile) 
plotrix::std.error(non_relation_only$IMD_decile)

mean(non_relation_only$Age_midpoint)
plotrix::std.error(non_relation_only$Age_midpoint) 

#gender
(nrow(subset(non_relation_only, Gender=="Female"))/nrow(non_relation_only))*100 #55.33981
(nrow(subset(non_relation_only, Gender=="Male"))/nrow(non_relation_only))*100 #44.66019

#ethnicity
(nrow(subset(non_relation_only, Ethnicity=="White or Caucasian"))/nrow(non_relation_only))*100 #88.59223
(nrow(subset(non_relation_only, Ethnicity=="Other"))/nrow(non_relation_only))*100 #11.40777

#overall experience and connection by age
projectdata %>% 
  group_by(Age_midpoint) %>% 
  summarise(mean = mean(Greenspace_visitation), 
            sd = sd(Greenspace_visitation),
            se = plotrix::std.error(Greenspace_visitation),
            samp_size = n())

ggplot(projectdata, aes(x = Age_midpoint, y = Greenspace_visitation, group = Age_midpoint, colour = Age_midpoint)) +
  geom_boxplot()

projectdata %>% 
  group_by(Age_midpoint) %>% 
  summarise(mean = mean(Upland_visitation), 
            sd = sd(Upland_visitation),
            se = plotrix::std.error(Upland_visitation),
            samp_size = n())

ggplot(projectdata, aes(x = Age_midpoint, y = Upland_visitation, group = Age_midpoint, colour = Age_midpoint)) +
  geom_boxplot()


