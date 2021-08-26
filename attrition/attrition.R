library('tidyverse')
library('survival')

# Import 
df <- read.csv('attrition.csv')

# Prep data: 1 filter monthly contracts, 2.get churn status as numerical
prep <- function(df) {
  df <- df %>% 
    filter(Contract=='Month-to-month') %>% 
    mutate(Churn = ifelse(Churn=='No', 1, 2))
  return(df)
}
monthly <- prep(df)

# Fit survival function
cox_fit <- coxph(Surv(tenure, Churn) ~ Partner, data=monthly)
by_partner <- with(monthly, data.frame(Partner = c(0, 1)))
stratified_fit <- summary(survfit(cox_fit, newdata=by_partner))

# Find mean charge
mean_charges <- monthly %>% 
  group_by(Partner) %>% 
  summarise(mean_charge=mean(MonthlyCharges))

# Get estimates, including calculated monthly and cumulative revenue
get_estimates <- function(partner) {
  index <- ifelse(partner, 2, 1)
  # Extract estimates 
  estimates <- data.frame(stratified_fit[['time']])
  estimates$prob <- data.frame(stratified_fit[['surv']])[,index]
  names(estimates) = c('time', 'prob')
  # Add calculated fields
  estimates <- estimates %>% 
    mutate(monthly = prob * pull(mean_charges[index, 2]) * 1000) %>% 
    mutate(cum = cumsum(monthly)) %>% 
    mutate(partner = index - 1)
  return(estimates)
}

estimates_p <- get_estimates(partner=TRUE)
estimates_np <- get_estimates(partner=FALSE)
estimates <- union(estimates_p, estimates_np)

# Export
write.csv(estimates, 'estimates.csv')


