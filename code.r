# Note: This is an alternative file to code.ipynb containing the exact same code.

# Introduction

# install necessary packages
# if any do not load, please use install.packages("<package name>")
library(tidyverse)
library(gridExtra)
library(leaps)
library(infer)
library(corrplot)
library(car)
library(broom)

# download dataset from url
url <- "https://drive.google.com/uc?export=download&id=1ot8vGL0fcwyhDHVOQwnAgvcAeAUlYwip"
sleep_data <- read_csv(url)
head(sleep_data)

# clean + wrangle the data
sleep <- sleep_data |>
    select("Sleep efficiency", "Age", "Gender", "Caffeine consumption", "Exercise frequency", "Smoking status", "Alcohol consumption") |>
    na.omit()
colnames(sleep) <- c("efficiency", "age", "gender", "caffeine", "exercise", "smoking", "alcohol")
sleep$smoking <- as.factor(sleep$smoking)
sleep$gender <- as.factor(sleep$gender)
head(sleep)

# Exploratory Data Analysis 

# response variable (Y)
options(repr.plot.width = 5, repr.plot.height = 3)
efficiency_plot <- sleep |>
    ggplot(aes(x = efficiency)) +
    geom_histogram(bins = 10, color = "darkgrey", fill = "aliceblue") +
    labs(x = "Sleep Efficiency",
         y = "Count",
         title = "Distribution of Sleep Efficiency") +
    theme_bw() +
    theme(text = element_text(size = 15))
efficiency_plot

sleep |>
    summarize(mean = mean(efficiency),
              sd = sd(efficiency),
              median = median(efficiency),
              IQR = IQR(efficiency))

# histogram for age (X1)
options(repr.plot.width = 5, repr.plot.height = 3)
age_plot <- sleep |>
    ggplot(aes(x = age)) +
    geom_histogram(bins = 10, color = "darkgrey", fill = "antiquewhite1") +
    labs(x = "Age (years)",
         y = "Count",
         title = "Distribution of Age") +
    theme_bw() +
    theme(text = element_text(size = 15))
age_plot

sleep |>
    summarize(mean = mean(age),
              sd = sd(age),
              median = median(age),
              IQR = IQR(age))

# bar chart for gender (X2)
options(repr.plot.width = 5, repr.plot.height = 3)
gender_chart <- sleep |>
    ggplot(aes(x = gender)) +
    geom_bar(width = 0.5, color = "darkgrey", fill = c("moccasin", "mistyrose1")) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = 3) +
    labs(x = "Gender",
         y = "Count",
         title = "Distribution of Gender") +
    theme_bw() +
    theme(text = element_text(size = 15))
gender_chart

sleep |>
    count(gender) |>
    mutate(prop = n / nrow(sleep))

# histogram for caffeine (X3)
options(repr.plot.width = 5, repr.plot.height = 3)
caffeine_plot <- sleep |>
    ggplot(aes(x = caffeine)) +
    geom_histogram(bins = 10, color = "darkgrey", fill = "antiquewhite1") +
    labs(x = "Caffeine Consumption (mg)",
         y = "Count",
         title = "Distribution of Caffeine Consumption") +
    theme_bw() +
    theme(text = element_text(size = 15))
caffeine_plot

sleep |>
    summarize(mean = mean(caffeine),
              sd = sd(caffeine),
              median = median(caffeine),
              IQR = IQR(caffeine))

# histogram for exercise (X4)
options(repr.plot.width = 5, repr.plot.height = 3)
exercise_plot <- sleep |>
    ggplot(aes(x = exercise)) +
    geom_histogram(bins = 10, color = "darkgrey", fill = "antiquewhite1") +
    labs(x = "Exercise Frequency (days per week)",
         y = "Count",
         title = "Distribution of Exercise Frequency") +
    theme_bw() +
    theme(text = element_text(size = 15))
exercise_plot

sleep |>
    summarize(mean = mean(exercise),
              sd = sd(exercise),
              median = median(exercise),
              IQR = IQR(exercise))

# bar chart for smoking (X5)
options(repr.plot.width = 5, repr.plot.height = 3)
smoking_chart <- sleep |>
    ggplot(aes(x = smoking)) +
    geom_bar(width = 0.5, color = "darkgrey", fill = c("moccasin", "mistyrose1")) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = 3) +
    labs(x = "Smoking",
         y = "Count",
         title = "Distribution of Smoking Status") +
    theme_bw() +
    theme(text = element_text(size = 15))
smoking_chart

sleep |>
    count(smoking) |>
    mutate(prop = n / nrow(sleep))

    mutate(prop = n / nrow(sleep))

# scatterplots, coloured by smoking (X5)
options(repr.plot.width = 5, repr.plot.height = 4)

# efficiency vs. age (X1)
y_x1_plot <- sleep |>
    ggplot(aes(x = age, y = efficiency, color = smoking)) +
    geom_point(alpha = 0.5) +
    labs(x = "Age",
         y = "Efficiency",
         color = "Smoking",
         title = "Efficiency vs. Age",
         caption = "Figure 1") +
    theme_bw() +
    theme(text = element_text(size = 15))
y_x1_plot

# efficiency vs. caffeine (X3)
y_x3_plot <- sleep |>
    ggplot(aes(x = caffeine, y = efficiency, color = smoking)) +
    geom_point(alpha = 0.5) +
    labs(x = "Caffeine (mg)",
         y = "Efficiency",
         color = "Smoking",
         title = "Efficiency vs. Caffeine",
         caption = "Figure 2") +
    theme_bw() +
    theme(text = element_text(size = 15))
y_x3_plot

# efficiency vs. exercise (X4)
y_x4_plot <- sleep |>
    ggplot(aes(x = exercise, y = efficiency, color = smoking)) +
    geom_point(alpha = 0.5) +
    labs(x = "Exercise (days per week)",
         y = "Efficiency",
         color = "Smoking",
         title = "Efficiency vs. Exercise",
         caption = "Figure 3") +
    theme_bw() +
    theme(text = element_text(size = 15))
y_x4_plot

# efficiency vs. alcohol (X6)
y_x6_plot <- sleep |>
    ggplot(aes(x = alcohol, y = efficiency, color = smoking)) +
    geom_point(alpha = 0.5) +
    labs(x = "Alcohol (oz)",
         y = "Efficiency",
         color = "Smoking",
         title = "Efficiency vs. Alcohol",
         caption = "Figure 4") +
    theme_bw() +
    theme(text = element_text(size = 15))
y_x6_plot

# Predictor Variable Selection

# correlation matrix for quantitative variables only
options(repr.plot.width = 7, repr.plot.height = 6)
corr_matrix <- round(cor(sleep %>% select(-c(smoking, gender))),2)
colours <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corr_plot <- corrplot(corr_matrix, method = "color", col = colours(200),  
         type = "upper",
         addCoef.col = "black", number.cex = 1.8,
         tl.col = "black", tl.srt = 45, tl.cex = 1.2) 
corr_plot$arg

# linear regression model with 6 predictors
model_full = lm(efficiency ~ ., data = sleep)
summary(model_full)

# Variance Inflation Factor for each predictor variable
vif <- vif(model_full)
tidy(vif)
any(vif > 10)

# Backwards Elimination predictor variable selection
selection <- regsubsets(efficiency ~ ., data = sleep, method = "backward")
selection_summary <- summary(selection)
selection_summary$which

# metrics for evaluating different linear models
selection_metrics <- data.frame(cbind(c(1:6), c(2:7), selection_summary$cp,
                                      selection_summary$rsq, selection_summary$adjr2))
colnames(selection_metrics) <- c("p", "pp1", "cp", "rsq", "adjr2")
selection_metrics

# (adjusted) R^2 plot for selection of p
options(repr.plot.width = 5, repr.plot.height = 4)
rsq_plot <- selection_metrics |>
    ggplot(aes(x = p)) +
    geom_point(aes(y = rsq), color = "salmon", alpha = 0.8, size = 3) +
    geom_point(aes(y = adjr2), color = "cyan", alpha = 0.8, size = 3) +
    labs(x = "p (# of predictors)",
         y = "Proportion of variation \n explained by predictors",
         title = "R^2 (pink) and adjR^2 (blue) vs. p",
         caption = "Figure 6") +
    scale_x_continuous(breaks = c(1:6)) + 
    theme_bw() +
    theme(text = element_text(size = 15))
    
rsq_plot

# Cp plot for selection of p
options(repr.plot.width = 5, repr.plot.height = 4)
cp_plot <- selection_metrics |>
    ggplot(aes(x = p)) +
    geom_point(aes(y = cp), color = "salmon", alpha = 0.8, size = 3) +
    geom_point(aes(y = pp1), color = "cyan", alpha = 0.8, size = 3) +
    labs(x = "p (# of predictors)",
         y = "Mallows' Cp",
         title = "Mallows' Cp (pink) and p + 1 (blue) vs. p",
         caption = "Figure 7") +
    scale_x_continuous(breaks = c(1:6)) + 
    theme_bw() +
    theme(text = element_text(size = 15))
cp_plot

# Model Selection

# 2-fold cross-validation

train <- 1:as.integer(dim(sleep)[1]/2)

# model with p = 4
four_reg1 <- lm(efficiency ~ age + exercise + smoking + alcohol, data = sleep[train,])
four_error1 <- sum((sleep$efficiency[-train] - predict(four_reg1, sleep[-train,]))^2)
four_reg2 <- lm(efficiency ~ age + exercise + smoking + alcohol, data = sleep[-train,])
four_error2 <- sum((sleep$efficiency[train] - predict(four_reg2, sleep[train,]))^2)
four_error <- (four_error1 + four_error2) / dim(sleep)[1]

# model with p = 5
five_reg1 <- lm(efficiency ~ age + exercise + caffeine + smoking + alcohol, data = sleep[train,])
five_error1 <- sum((sleep$efficiency[-train] - predict(five_reg1, sleep[-train,]))^2)
five_reg2 <- lm(efficiency ~ age + exercise + caffeine + smoking + alcohol, data = sleep[-train,])
five_error2 <- sum((sleep$efficiency[train] - predict(five_reg2, sleep[train,]))^2)
five_error <- (five_error1 + five_error2) / dim(sleep)[1]

cross_val <- data.frame(p = c(4, 5), error = c(four_error, five_error))
cross_val

# Regression Analysis

model <- lm(efficiency ~ age + exercise + smoking + alcohol, data = sleep)
summary(model)

# plot: residuals vs. fitted values
options(repr.plot.width = 5, repr.plot.height = 3)
residual_fitted_plot <- model |>
    ggplot(aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
    theme_bw() +
    labs(x = "Fitted Values",
         y = "Residuals")
residual_fitted_plot

# plot: residuals vs. indices
options(repr.plot.width = 5, repr.plot.height = 3)
residual_index_plot <- model |>
    ggplot(aes(x = c(1:length(.resid)), y = .resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
    theme_bw() +
    labs(x = "Index",
         y = "Residuals")
residual_index_plot

# confidence intervals
ci_0 <- confint(model, "(Intercept)")
ci_1 <- confint(model, "age")
ci_2 <- confint(model, "exercise")
ci_3 <- confint(model, "smokingYes")
ci_4 <- confint(model, "alcohol")
ci <- rbind(ci_0, ci_1, ci_2, ci_3, ci_4)
colnames(ci) <- c("ci_lower", "ci_upper")
ci