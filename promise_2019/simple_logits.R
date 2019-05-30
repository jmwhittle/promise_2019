library(tidyverse)
library(caret)

mlm_data <- read_csv("mlm_data.csv") %>% dplyr::select(-TWNEED_OFFER, -TWNEED_PAID, -PACIFIC_ISLANDER_IND, -AMERICAN_INDIAN_IND)

## Creating dummy variables
# Fixing factor variable issues
# HIGH_SCHOOL_GPA to factor
mlm_data$HIGH_SCHOOL_GPA <- cut(mlm_data$HIGH_SCHOOL_GPA, c(0, 1, 2, 2.5, 3, 3.5, 4, 5))
mlm_data$HIGH_SCHOOL_GPA[is.na(mlm_data$HIGH_SCHOOL_GPA) == T] <- "(4,5]"

# PRIOR_UG_CREDITS NAs
mlm_data$PRIOR_UG_CREDITS[is.na(mlm_data$PRIOR_UG_CREDITS) == T] <- 0

# PRIOR_UG_GPA to factor
mlm_data$PRIOR_UG_GPA <- cut(mlm_data$PRIOR_UG_GPA,  c(0, 1, 2, 2.5, 3, 3.5, 4, 5))
mlm_data$PRIOR_UG_GPA[is.na(mlm_data$PRIOR_UG_GPA) == T] <- "(4,5]"

# NAs to 0
mlm_data[is.na(mlm_data) == T] <- 0

# COUNTY OF ORIGIN
mlm_data$slco <- ifelse(mlm_data$COUNTY_OF_ORIGIN == "Salt Lake", 1, 0)
mlm_data$uco <- ifelse(mlm_data$COUNTY_OF_ORIGIN == "Utah", 1, 0)
mlm_data$north <- ifelse(mlm_data$COUNTY_OF_ORIGIN == "Davis",  1, 
                         ifelse(mlm_data$COUNTY_OF_ORIGIN == "Weber", 1, 0))

# single indicator
mlm_data$married <- ifelse(mlm_data$MARITAL_STATUS == "Married", 1, 0)

# passed gpa
mlm_data$passed <- ifelse(mlm_data$TERM_UG_GPA >= 2, 1, 0)
mlm_data$passed[is.na(mlm_data$passed == T)] <- 0

# dropping county_of_origin
mlm_data <- mlm_data %>% dplyr::select(-COUNTY_OF_ORIGIN, -MARITAL_STATUS)

# create dummies
dms_mlm <- dummyVars("~ 
                     PROMISE + 
                     F_F +
                     passed + 
                     TERM_UG_CREDITS +
                     AGE_ON_FIRST_DAY +
                     GENDER + 
                     FIRST_GENERATION_IND + 
                     married +  
                     COLLEGE_READY_MATH + 
                     COLLEGE_READY_ENGLISH + 
                     PRIOR_UG_CREDITS +
                     FIRST_TERM_NON_CONCURRENT_IND + 
                     EVER_CONCURRENT_IND +
                     slco + 
                     north +
                     CREDITS_ATTEMTPED", data = mlm_data, fullRank = T)

data <- data.frame(predict(dms_mlm, newdata = mlm_data), mlm_data$ETHNICITY, mlm_data$TERM_CODE, mlm_data$HIGH_SCHOOL_GPA, mlm_data$PRIOR_UG_GPA)
colnames(data)[colnames(data) == "mlm_data.ETHNICITY"] <- "ETHNICITY"
colnames(data)[colnames(data) == "mlm_data.TERM_CODE"] <- "TERM_CODE"
colnames(data)[colnames(data) == "mlm_data.HIGH_SCHOOL_GPA"] <- "HIGH_SCHOOL_GPA"
colnames(data)[colnames(data) == "mlm_data.PRIOR_UG_GPA"] <- "PRIOR_UG_GPA"

mlm_data <- data

### Near Zero Variances
# looking for near zero variance predictors
# https://tgmstat.wordpress.com/2014/03/06/near-zero-variance-predictors/
nzv_list <- nearZeroVar(mlm_data[,-1], saveMetrics = T)
nzv_list
nzv <- nearZeroVar(mlm_data[,-1], saveMetrics = F)

data <- mlm_data[,-1]

data <- data[,-nzv]
#names(data_train)

data$PROMISE <- mlm_data$PROMISE

names(data)
mlm_data <- data

# Centering, Scaling, imputing
#center/scale continuous variables
#cent_scale <- preProcess(within(data_train, 
#                               rm(PIDM, DIM_STUDENT_TERM_KEY, PROMISE, TERM_CODE)),
#                        method = c("center", "scale"))
#data_train <- predict(cent_scale, data_train)

### Correlated predictors
cor_test <- cor(within(mlm_data, rm(TERM_CODE, ETHNICITY, HIGH_SCHOOL_GPA, PRIOR_UG_GPA)))
# no issues with correlation

### Linear Dependencies
# lin_test <- findLinearCombos(mlm_data)
# lin_test


### plotting all variables
all_var_plot <- within(mlm_data, rm(DIM_STUDENT_KEY, DIM_STUDENT_TERM_KEY, TERM_CODE)) %>% 
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") + 
  geom_histogram()


# F-F retention
library(lme4)

# limit analysis to 
mlm_data_ff <- mlm_data %>% filter(TERM_CODE == 201540 |
                                     TERM_CODE == 201640 |
                                     TERM_CODE == 201740 )


mlm_ff <- glmer(F_F ~ PROMISE +
                  scale(CREDITS_ATTEMTPED) + 
                  scale(AGE_ON_FIRST_DAY) + 
                  GENDERMale +
                  married +
                  COLLEGE_READY_ENGLISHY +
                  COLLEGE_READY_MATHY + 
                  FIRST_GENERATION_INDY +
                  FIRST_TERM_NON_CONCURRENT_INDY + 
                  EVER_CONCURRENT_INDY +
                  scale(PRIOR_UG_CREDITS) + 
                  slco + 
                  HIGH_SCHOOL_GPA + 
                  PRIOR_UG_GPA +
                  (1|ETHNICITY) + 
                  (1|TERM_CODE), 
                data = mlm_data_ff,
                family = binomial)


rr1 <- ranef(mlm_ff, condVar = T)
rr2 <- data.frame(eth = rownames(rr1[[1]]),
                  int = unname(rr1[[1]]),
                  se = sqrt(c(attr(rr1[[1]], "postVar"))))

rr4 <- transform(rr2, eth = reorder(eth, int))

mlm_ff_plot_eth <- rr4 %>%
  ggplot(aes(eth, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  labs(x = "Fall-Fall retention") +
  coord_flip()


rr1 <- ranef(mlm_ff, condVar = T)
rr2 <- data.frame(term = rownames(rr1[[2]]),
                  int = unname(rr1[[2]]),
                  se = sqrt(c(attr(rr1[[2]], "postVar"))))

rr4 <- transform(rr2, term = reorder(term, int))

mlm_ff_plot_term <- rr4 %>%
  ggplot(aes(term, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  labs(x = "Fall-Fall retention") +
  coord_flip()


# Term PASSING GPA

mlm_data_pass <- mlm_data

mlm_pass <- glmer(passed ~ PROMISE +
                    scale(CREDITS_ATTEMTPED) + 
                    scale(AGE_ON_FIRST_DAY) + 
                    GENDERMale +
                    married +
                    COLLEGE_READY_ENGLISHY +
                    COLLEGE_READY_MATHY + 
                    FIRST_GENERATION_INDY +
                    FIRST_TERM_NON_CONCURRENT_INDY + 
                    EVER_CONCURRENT_INDY +
                    scale(PRIOR_UG_CREDITS) + 
                    slco + 
                    HIGH_SCHOOL_GPA + 
                    PRIOR_UG_GPA +
                    (1|ETHNICITY) + 
                    (1|TERM_CODE), 
                  data = mlm_data_pass,
                  family = binomial)


rr1 <- ranef(mlm_pass, condVar = T)
rr2 <- data.frame(eth = rownames(rr1[[1]]),
                  int = unname(rr1[[1]]),
                  se = sqrt(c(attr(rr1[[1]], "postVar"))))

rr4 <- transform(rr2, eth = reorder(eth, int))

mlm_pass_plot_eth <- rr4 %>%
  ggplot(aes(eth, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  labs(x = "Ethnicity", title="Passing GPA likelihood by group") +
  coord_flip()


rr1 <- ranef(mlm_pass, condVar = T)
rr2 <- data.frame(term = rownames(rr1[[2]]),
                  int = unname(rr1[[2]]),
                  se = sqrt(c(attr(rr1[[2]], "postVar"))))

mlm_pass_plot_term <- rr2 %>%
  ggplot(aes(term, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  labs(x = "Fall-Fall retention") +
  coord_flip()

# TERM UG CREDITS EARNED

mlm_data_crd <- mlm_data

mlm_crd <- lmer(TERM_UG_CREDITS ~ PROMISE +
                  scale(AGE_ON_FIRST_DAY) + 
                  GENDERMale +
                  married +
                  COLLEGE_READY_ENGLISHY +
                  COLLEGE_READY_MATHY + 
                  FIRST_GENERATION_INDY +
                  FIRST_TERM_NON_CONCURRENT_INDY + 
                  EVER_CONCURRENT_INDY +
                  scale(PRIOR_UG_CREDITS) + 
                  slco + 
                  HIGH_SCHOOL_GPA + 
                  PRIOR_UG_GPA +
                  (1|ETHNICITY) + 
                  (1|TERM_CODE), 
                data = mlm_data_crd)

rr1 <- ranef(mlm_crd, condVar = T)
rr2 <- data.frame(eth = rownames(rr1[[1]]),
                  int = unname(rr1[[1]]),
                  se = sqrt(c(attr(rr1[[1]], "postVar"))))

rr4 <- transform(rr2, eth = reorder(eth, int))
mlm_crd_plot_eth <- rr4 %>%
  ggplot(aes(eth, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  labs(x = "Ethnicity", title="Term Credits Earned") +
  coord_flip()

rr1 <- ranef(mlm_crd, condVar = T)
rr2 <- data.frame(term = rownames(rr1[[2]]),
                  int = unname(rr1[[2]]),
                  se = sqrt(c(attr(rr1[[2]], "postVar"))))


mlm_crd_plot_term <- rr2 %>%
  ggplot(aes(term, int, ymin=int-1.96*se, ymax=int+1.96*se)) + 
  geom_pointrange() + 
  labs(x = "Term", title="Term Credits Earned") +
  coord_flip()
