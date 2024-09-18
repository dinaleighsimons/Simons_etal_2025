#Simons, Stubbs, Bradbruy, Evans 2024 main script

#Set environment ---------------------------------------------------------------
rm(list = ls())

##Set wd-----
setwd("C:/Users/dlsimons/OneDrive - The University of Liverpool/Documents/Manuscripts/2. James Conservation Marketing/Simons_Stubbs_Analyses/Simons_Stubbs_Analyses")

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
              "lmtest")

invisible(lapply(packages, library, character.only = TRUE))

## Read and tidy data ----
projectdata <- read.csv('Data/Project_data.csv')
glimpse(projectdata)
str(projectdata)

## Remove 'Other' gender categories
# Delete rows 102, 209, and 603 from the dataset
projectdata <- projectdata[-c(102, 209, 603),]
colSums(is.na(projectdata))

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

### Financial ratings ----
ggplot(projectdata, aes(x = log(Financial_donation))) +
  geom_histogram(bins = 10) +
  theme_bw() #extreme right skew, log improves

ggplot(projectdata, aes(x = log(Financial_allocation)))+
  geom_histogram(bins = 10)+
  theme_bw() #right skewed, log improves

ggplot(projectdata, aes(x = log(Financial_compost)))+
  geom_histogram(bins = 10)+
  theme_bw() #right-skew, log improves but still slight skewed

### Change in attitude scores ----

### Degraded peatland change score
ggplot(projectdata, aes(x = Degraded_peatland_score))+
  geom_histogram(bins = 10)+
  theme_bw() #approaching normal

### Regenerate peatland change score
ggplot(projectdata, aes(x = Regenerate_peatland_score + 10))+
  geom_histogram(bins = 20)+
  theme_bw() #approaching normal

### Final attitudes ---

### Degraded peatland final score
ggplot(projectdata, aes(x = Degraded_peatlands_after))+
  geom_histogram(bins = 10)+
  theme_bw() #left skewed

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
  geom_histogram(bins = 7)+
  theme_bw() #left-skew, cube kinda improves

### BIO_Manipulation ----

ggplot(projectdata, aes(x = BIO_Manipulation))+
  geom_histogram(bins = 10)+
  theme_bw() #left-skew

### Relational_Manipulation ----

ggplot(projectdata, aes(x = Relational_Manipulation))+
  geom_histogram(bins = 15)+
  theme_bw() #left-skew

## Predictors -----

### Nature connection (NR6 scores) ----
ggplot(projectdata, aes(x = NR6_score))+
  geom_histogram(binwidth = 0.5)+
  theme_bw() #normally distributed

### Greenspace visitation ----
ggplot(projectdata, aes(x = Greenspace_visitation))+
  geom_histogram(bins = 10) +
  theme_bw() #not sure whether we can treat as continuous?

### Upland visitation ----
ggplot(projectdata, aes(x = Upland_visitation))+
  geom_histogram(bins = 10)+
  theme_bw() #highly left skewn

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

#plot
png("Figures/correlation_plot.png", width = 1100, height = 1100)

cor_plot<- corrplot(res, method = 'color',
                    order = 'alphabet', 
                    tl.col = "black", 
                    addCoef.col = 'black',
                    tl.srt = 45)

dev.off()

#no factors over 0.7 apart from behavior individual 

# Models -----------------------------------------------------------------------

## Manipulation checks ----

### Biodiversity manipulation check----

BIO_mc_mod <- lm(BIO_Manipulation^2 ~ 
                   Message_framing + 
                   Relational_values + 
                   NR6_score + 
                   Greenspace_visitation + 
                   Upland_visitation + 
                   Peatland_knowledge + 
                   Age_midpoint + 
                   Gender + 
                   Ethnicity, 
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

ES_mc_mod_sq <- lm(ES_Manipulation^2 ~ 
                  Message_framing + 
                  Relational_values + 
                  NR6_score + 
                  Greenspace_visitation + 
                  Upland_visitation + 
                  Peatland_knowledge + 
                  Age_midpoint + 
                  Gender + 
                  Ethnicity, 
                data = projectdata)

# Check model diagnostics
autoplot(ES_mc_mod_sq)

#Tests
dwtest(ES_mc_mod_sq) #Independence (no autocor), pass
bptest(ES_mc_mod_sq) #Homoscedasticity, fail
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

# Construct full model with all predictors.
RV_mc_mod <- lm(Relational_Manipulation^2 ~ Message_framing + 
                  Relational_values + 
                  NR6_score + 
                  Greenspace_visitation + 
                  Upland_visitation + 
                  Peatland_knowledge + 
                  Age_midpoint + 
                  Gender + 
                  Ethnicity, data = projectdata)

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

## Peat awareness (change) ----
Degraded_peatland_score_mod <- lm(Degraded_peatland_score ~ 
                                    Message_framing + 
                                    Relational_values + 
                                    NR6_score + 
                                    Greenspace_visitation + 
                                    Upland_visitation + 
                                    Peatland_knowledge + 
                                    Age_midpoint + 
                                    Gender + 
                                    Ethnicity, 
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
                                              Ethnicity, 
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

Regenerate_peatland_score_mod <- lm(Regenerate_peatland_score + 11 ~ 
                                      Message_framing + 
                                      Relational_values + 
                                      NR6_score + 
                                      Greenspace_visitation + 
                                      Upland_visitation + 
                                      Peatland_knowledge + 
                                      Age_midpoint + 
                                      Gender + 
                                      Ethnicity, 
                                    data = projectdata)

autoplot(Regenerate_peatland_score_mod)

#GLM
Regenerate_peatland_score_mod_glm <- glm((Regenerate_peatland_score + 11)~ 
                                      Message_framing + 
                                      Relational_values + 
                                      NR6_score + 
                                      Greenspace_visitation + 
                                      Upland_visitation + 
                                      Peatland_knowledge + 
                                      Age_midpoint + 
                                      Gender + 
                                      Ethnicity, 
                                    data = projectdata, family = gaussian(link = "inverse"))

autoplot(Regenerate_peatland_score_mod_glm)

#Tests
dwtest(Regenerate_peatland_score_mod) #Independence (no autocor), pass
bptest(Regenerate_peatland_score_mod) #Homoscedasticity, fail
shapiro.test(residuals(Regenerate_peatland_score_mod)) #normality, fail
vif(Regenerate_peatland_score_mod) #multicol, no values above 10

ggplot(projectdata, aes(x = Regenerate_peatland_score_mod$residuals))+
  geom_histogram(bins = 20)+
  labs(title = 'Histogram of Residuals (Regenerate peatland score: Full model)', x = 'Residuals', y = 'Frequency')+
  theme_bw() # Residuals are roughly normally distributed.

# Model coefficients
summary(Regenerate_peatland_score_mod)

#Anovas
output <- Anova(Regenerate_peatland_score_mod, test = "F")
output

#FDR adjustment
Regenerate_peatland_score_mod_adjust <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
Regenerate_peatland_score_mod_adjust

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
                                           Ethnicity, 
                                         data = projectdata)

autoplot(Regenerate_peatland_afterscore_mod)

#Tests
dwtest(Regenerate_peatland_afterscore_mod) #Independence (no autocor), pass
bptest(Regenerate_peatland_afterscore_mod) #Homoscedasticity, fail
shapiro.test(residuals(Regenerate_peatland_score_mod)) #normality, fail
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
                                Ethnicity,
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
                      Ethnicity, data = projectdata)

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

## Financial support (sqrt transformed)----

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
                                                Ethnicity, 
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
                                 Ethnicity, 
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

# Scatter plots ----------------------------------------------------------------

## Peatland degradation awareness (post-treatment score) ----

### Nature connection ----
nature_awareness <- ggplot(projectdata, aes(x=NR6_score, y= sqrt(Degraded_peatlands_after)))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Nature connection", y = "Degradation awareness")#+
  #theme(text = element_text(size = 15),
        #axis.title.y = element_blank(),
        #legend.position = "none")+
  #guides(color=guide_legend("Message framing"), fill = "none")

nature_awareness

### Peatland knowledge ----
knowledge_awareness <- ggplot(projectdata, aes(x=Peatland_knowledge, y= Degraded_peatlands_after))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Peatland knowledge", y = "Degradation awareness")#+
  #theme(text = element_text(size = 15),
      #axis.title.y = element_blank(),
     #legend.position = "none")+
  #guides(color=guide_legend("Message framing"), fill = "none")

knowledge_awareness

## Peatland regeneration understanding (post-treatment score) ----

### Nature connection ---
nature_understanding <- ggplot(projectdata, aes(x=NR6_score, y= Regenerate_peatlands_after))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "#048A81")+
  theme_classic()+
  labs(x = "Nature connection", y = "Regeneration understanding")#+
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
  theme_bw()

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
nature_fin_allo <- ggplot(projectdata, aes(x = NR6_score, y = Financial_allocation))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = lm, colour = '#048A81', se = TRUE)+
  scale_y_sqrt()+ # transform response scale to square root
  ylab('Financial allocation')+
  xlab('Nature connection')+
  theme_bw()

nature_fin_allo 

### Greenspace visitation----
green_fin_allo <- ggplot(projectdata, aes(x = Greenspace_visitation, y = Financial_allocation))+
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
  ylab('Advert sufficiency')+
  xlab('Nature connection')+
  theme_bw()

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

all_nature_scatter <- plot_grid(nature_awareness, nature_understanding, nature_suff, nature_behav, nature_fin_allo, nature_fin_don, labels = c("a", "b", "c", "d", "e", "f"), ncol = 3)

all_nature_scatter 

ggsave(file = "Figures/nature_scatter.png", all_nature_scatter, units = "mm", height = 200, width = 300)

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
  scale_colour_manual(values = c(BIO = '#048A81', ES = '#CDA2AB'))+
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
  scale_colour_manual(values = c(BIO = '#048A81', ES = '#CDA2AB'))+
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
  scale_colour_manual(values = c(BIO = '#048A81', ES = '#CDA2AB'))+
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
  scale_colour_manual(values = c(BIO = '#048A81', ES = '#CDA2AB'))+
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
  scale_colour_manual(values = c(BIO = '#048A81', ES = '#CDA2AB'))+
  theme_bw()+
  theme(legend.position = "none")

finance_compost_mf

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
  scale_colour_manual(values = c(BIO = '#048A81', ES = '#CDA2AB'))+
  theme_bw()+
  theme(legend.position = "none")

suff_mf

### Plot all together ----

all_mf <- plot_grid(degraded_peatlands_mf, regenerate_peatlands_mf, suff_mf, behav_mf, finance_allo_mf, finance_compost_mf, labels = c("a", "b", "c", "d", "e", "f"), ncol = 3)

all_mf

ggsave(file = "Figures/all_mf.png", all_mf, units = "mm", height = 150, width = 250)

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
  scale_colour_manual(values = c(Absent = 'grey60', Present = '#8A89C0'))+
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
  scale_colour_manual(values = c(Absent = 'grey60', Present = '#8A89C0'))+
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
  scale_colour_manual(values = c(Absent = 'grey60', Present = '#8A89C0'))+
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
  scale_colour_manual(values = c(Absent = 'grey60', Present = '#8A89C0'))+
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
  scale_colour_manual(values = c(Absent = 'grey60', Present = '#8A89C0'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

finance_compost_relation

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
  scale_colour_manual(values = c(Absent = 'grey60', Present = '#8A89C0'))+
  theme_bw()+
  labs(colour = "Relational value presence")+
  theme(legend.position = "none")

suff_relation

### Plot all together ----

all_relation <- plot_grid(degraded_peatlands_relation, regenerate_peatlands_relation, suff_relation, behav_relation, finance_allo_relation, finance_compost_relation, labels = c("a", "b", "c", "d", "e", "f"), ncol = 3)

all_relation

ggsave(file = "Figures/all_relation.png", all_relation, units = "mm", height = 150, width = 250)

## Model summary plots----
#Variables for plotting
all.models <- list()
all.models[[1]] <- Degraded_peatland_score_mod
all.models[[2]] <- Regenerate_peatland_score_mod
all.models[[3]] <- Advert_sufficiency_mod
all.models[[4]] <- Behaviour_mod
all.models[[5]] <- Finance_allocation_sqrt_mod

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
