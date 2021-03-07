one_poll_per_pollster <- brexit_polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup() %>%
  filter(enddate >= "2016-06-01")

one_poll_per_pollster %>% ggplot(aes(spread)) +
  geom_histogram(color = "black", binwidth = .01)

z <- qt(0.975, nrow(one_poll_per_pollster) - 1)

results <- one_poll_per_pollster %>% summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(lower = qnorm(0.025,avg, se), upper = qnorm(0.975, avg, se))

average <- results$avg
se_hat <- results$se

results <- results %>% rbind(c(average, se_hat, average - z*se_hat, average + z*se_hat)) %>%
  mutate(distribution = c("n", "t")) %>%
  relocate(distribution, .before = avg)

results

t <- - average/se_hat

prob <- data.frame(distribution = c("t", "n"), remain = c(1 - pt(t, nrow(one_poll_per_pollster)), 1 - pnorm(0, average, se_hat)), leave = c(pt(t, nrow(one_poll_per_pollster)), pnorm(0, average, se_hat)))
prob

t_actual <- (d - average)/se_hat

prob_actual <- data.frame(distribution = c("t", "n"), probability = c(pt(t_actual, nrow(one_poll_per_pollster)), pnorm(d, average, se_hat)))
prob_actual