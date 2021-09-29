# Wrangling
library('tidyverse')

# Plots
library('ggplot2')
library('ggthemr')
library('Cairo')

# Modelling
library('reldist')
library('stats')
library('statmod')
library('xgboost')

# Set up settings
ggthemr('flat dark')
set.seed(150)

# Load data
load(file = 'ausprivauto0405.rda') 
df <- ausprivauto0405
rm(ausprivauto0405)

# Inspect data for errors
inspect <- function(col) {
  # Check type
  data_type <- class(col)
  # Check missing values
  missing <- sum(is.na(col))
  # Check for syntax inconsistencies
  factor_levels <- levels(col)
  return(c(data_type, missing, factor_levels))
}
sapply(df, inspect)

# Cleaning: rearrange factor levels, bin the vehicle value variable
df$DrivAge <- factor(df$DrivAge, 
                     levels = c("youngest people", "young people", 
                                "working people", "older work. people",
                                "old people", "oldest people"))
df$VehAge <- factor(df$VehAge, 
                    levels = c("youngest cars", "young cars", 
                               "old cars", "oldest cars"))

cuts <- c(-1)
for (i in seq(0.25, 0.75, 0.25)) {
  cuts <- c(cuts, wtd.quantile(df$VehValue, q = i, weight = df$Exposure))
}
cuts <- c(cuts, Inf)
df$VehValue <- cut(df$VehValue, breaks = cuts, labels = c('low', 'mid', 'high','v_high'))

# Calculate the losses
df <- 
  df %>%
  mutate(Loss = ClaimAmount / Exposure)

# One-way analysis
one_way <- function(var) {
  df %>%  
    group_by({{var}}) %>%  
    summarise(exposure = sum(Exposure), count = n()) %>% 
    mutate(exposure_percent = exposure / sum(exposure)) %>% 
    arrange(exposure_percent)
}
one_way_vb <- one_way(VehBody)
sum(one_way_vb$exposure_percent[1:4])
df <- 
  df %>% 
  filter(VehBody != 'Roadster' & VehBody != 'Bus' & VehBody != 'Convertible' & 
         VehBody != 'Motorized caravan')
df$VehBody <- droplevels(df$VehBody)

# Graph histogram of loss cost
df %>%  filter(ClaimNb == 0) %>%  nrow() / nrow(df)
loss_hist <- 
  ggplot(data = df %>%  filter(Loss < 10000)) + 
  geom_histogram(aes(x = Loss), alpha = 0.7, color = '#2c73a3') +
  ggtitle('Loss Cost') + 
  theme(plot.title = element_text(size = 10))    

loss_hist_nz <- 
  ggplot(data=df %>%  filter(Loss < 10000 & Loss > 0)) + 
  geom_histogram(aes(x = Loss), alpha = 0.7, color = '#2c73a3') +
  ggtitle('Loss Cost (Exclude Zero)') + 
  theme(plot.title = element_text(size = 10))    

combined <- cowplot::plot_grid(loss_hist, loss_hist_nz)
ggsave(combined, filename = str_c('histogram',".png"), dpi = 150, type = 'cairo',
       width = 6, height = 3, units = 'in')

# Calculate Cramer V statistic
png(filename = 'cramerv.png', type = 'cairo')
corrplot::corrplot(DescTools::PairApply(df[,2:6], DescTools::CramerV), 
                   method = 'circle', tl.col = 'black', tl.cex = 0.9, 
                   type = 'lower', tl.srt = 0)
dev.off()


# Split data into training and test sets
train_index <- caret::createDataPartition(df$Loss, p = .8, list = FALSE, times = 1)
train <-  df[train_index,]
test <- df[-train_index,]

# Fit GLM model
tweedie_p <- 1.5
loss_glm <- glm(Loss ~ VehAge + VehBody + VehValue + Gender + DrivAge,
                data = train, weights = Exposure, family = tweedie(tweedie_p, 0))

# Look for possible interactions
interaction_plot <- function(df, x, y) {
  df <- df %>% group_by({{x}}, {{y}}) %>% summarise(mean_loss = mean(Loss)) 
  plt <- ggplot(data = df) + 
    aes(x = {{x}}, y = mean_loss, color = {{y}}) +
    geom_line(aes(group = {{y}})) +
    geom_point() +
    theme(text = element_text(size = 8))   
  return(plt)  
}
inter_plt1 <- interaction_plot(train, Gender, DrivAge) 
inter_plt2 <- interaction_plot(train, Gender, VehBody)
combined <- cowplot::plot_grid(inter_plt1, inter_plt2)
ggsave(combined, filename = str_c('interaction',".png"), dpi = 150, type = 'cairo',
       width = 6, height = 3, units = 'in')

# Encode interactions and apply LASSO
prep_data <- function(df) {
  dummy_df <- select(df, -Exposure, -ClaimOcc, -ClaimAmount, -Loss, -ClaimNb)
  dummy_df <- model.matrix(~ . + Gender * DrivAge + Gender * VehBody, dummy_df)[,-1]
  return(dummy_df)
}

train_p <- prep_data(train)
test_p <- prep_data(test)

library('glmnet')
loss_glm_cv <- cv.glmnet(x = train_p, y = train$Loss, weights = train$Exposure,
                         family = tweedie(tweedie_p, 0))

# Compare lift charts
lift_chart <- function(data, data_df, model, name) {
  if (name == 'Tweedie GLM: Improved') {
    pred <- predict(model, data, type = 'response', weights = data_df$Exposure,
                    s = 'lambda.min')
  }
  else {
    pred <- predict(model, data, type = 'response', weights = data_df$Exposure) 
  }
  pred <- data.frame(pred * data_df$Exposure)
  names(pred) = 'predicted'
  pred$actual <- data_df$ClaimAmount
  
  cuts <- c(-1)
  for (i in seq(0.1, 0.9, 0.1)) {
    cuts <- c(cuts, wtd.quantile(pred$predicted, q = i, weight = data_df$Exposure))
  }
  cuts <- c(cuts, Inf)
  pred$group <- cut(pred$predicted, breaks = cuts, labels = FALSE)
  
  pred <- pivot_longer(pred, c('predicted', 'actual'), 
                       names_to = 'type', values_to = 'value')
  plt <- ggplot(data=pred) + 
    geom_line(aes(x = group, y = value, color = type), stat = 'summary', fun = 'mean') +
    geom_point(aes(x = group, y = value, color = type), stat = 'summary', fun = 'mean') + 
    ggtitle(str_c('Lift Chart: ', name)) + xlab('Group') + ylab('Average Loss') +
    scale_x_continuous(breaks = 1:10) + coord_cartesian(ylim = c(0, 420))
  return(plt)
}
lift_plt1 <- lift_chart(test, test, loss_glm, 'Tweedie GLM') 
lift_plt2 <- lift_chart(test_p, test, loss_glm_cv, 'Tweedie GLM: Improved')
ggsave(lift_plt1, filename = str_c('lift_1',".png"), dpi = 150, type = 'cairo',
       width = 4, height = 3, units = 'in')
ggsave(lift_plt2, filename = str_c('lift_2',".png"), dpi = 150, type = 'cairo',
       width = 4, height = 3, units = 'in')

# Compare boostrapped RMSE loss
boot_rmse <- function(data, model, type = FALSE, n_iter=1000) {
  rmse <- function(data, indices, model) {
    data <- data[indices,]
    if (type == 'improved') {
      pred <- predict(model, data, type = 'response', weights = test$Exposure,
                      s = 'lambda.min') 
      pred <- pred * test$Exposure
    } 
    else {
      pred <- predict(model, data, type = 'response', weights = test$Exposure)
      pred <- pred * test$Exposure
    }
    rmse <- sqrt(mean((pred - test$ClaimAmount)^2))
    print('Working on it...')
    return(rmse)
  }
  boot_rmse <- boot::boot(data, R = n_iter, statistic = rmse, model = model)
  boot_results <- data.frame(boot_rmse$t)
  boot_ci <- boot_results %>%
    summarise(lower = quantile(boot_rmse.t, probs = 0.025),
              upper = quantile(boot_rmse.t, probs = 0.975),
              mean = mean(boot_rmse.t))

  return(list(boot_results, boot_ci)) 
}

boot <- boot_rmse(test, loss_glm)
boot_improved <- boot_rmse(test_p, loss_glm_cv, 'improved')
hist_data <- union(boot_improved[[1]] %>% mutate(model = 'improved'),
                   boot[[1]] %>% mutate(model = 'original'))

plt <- ggplot() +
  geom_density(data = hist_data, aes(boot_rmse.t, fill = model), alpha = 0.6, color = NA) +
  geom_vline(data = boot_improved[[2]], aes(xintercept = mean), linetype = 'dotted', color = 'white') +
  geom_vline(data = boot[[2]], aes(xintercept = mean), linetype = 'dotted', color = 'white') +
  ggtitle('Bootstrapped Distributions of RMSE') +
  theme(text = element_text(size = 8))  
ggsave(plt, filename = str_c('bootstrap',".png"), dpi = 150, type = 'cairo',
       width = 4, height = 3, units = 'in')
