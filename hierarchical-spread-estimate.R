one_poll_per_pollster <- brexit_polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup() %>%
  filter(enddate >= "2016-06-01")

one_poll_per_pollster %>% ggplot(aes(sample = spread)) +
  stat_qq() +
  stat_qq_line()

mu <- 0
tau <- 0.035
bias_sd <- 0.025 
z <- qt(0.975, nrow(one_poll_per_pollster-1))

results <- one_poll_per_pollster %>% summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(lower = avg - z*se, upper = avg + z*se)

Y <- results$avg
sigma <- sqrt(results$se^2 + bias_sd^2)

B <- sigma^2/(sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

estimate <- data.frame(avg = posterior_mean, se = posterior_se, lower = posterior_mean - z*posterior_se, upper = posterior_mean + z*posterior_se)
estimate

t <- - estimate$avg/estimate$se

prob <- data.frame(distribution = c("t", "n"), remain = c(1 - pt(t, nrow(one_poll_per_pollster)), 1 - pnorm(0, posterior_mean, posterior_se)), leave = c(pt(t, nrow(one_poll_per_pollster)), pnorm(0, posterior_mean, posterior_se)))
prob

t_actual <- (d - estimate$avg)/estimate$se

prob_actual <- data.frame(distribution = c("t", "n"), probability = c(pt(t_actual, nrow(one_poll_per_pollster)), pnorm(d, posterior_mean, posterior_se)))
prob_actual