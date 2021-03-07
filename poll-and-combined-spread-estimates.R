one_poll_per_pollster <- brexit_polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup() %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(X_hat = (spread + 1)/2, se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = qnorm(0.025, spread, se_hat), upper = qnorm(0.975, spread, se_hat))

polls <- c(as.character(one_poll_per_pollster$pollster), "Inititial", "Intermediate", "Hierarchical")

initital <- one_poll_per_pollster %>% summarise(avg = sum(spread*samplesize)/sum(samplesize), X = (avg + 1)/2, se = 2*sqrt(X*(1-X)/sum(samplesize)), lower = qnorm(0.025, avg, se), upper = qnorm(0.975, avg, se))

z <- qt(0.975, nrow(one_poll_per_pollster)-1)

results <- one_poll_per_pollster %>% summarise(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(lower = avg - z*se, upper = avg + z*se)

mu <- 0
tau <- 0.035
bias_sd <- 0.025

Y <- results$avg
sigma <- sqrt(results$se^2 + bias_sd^2)

B <- sigma^2/(sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

hierarchical <- data.frame(avg = posterior_mean, se = posterior_se, lower = posterior_mean - z*posterior_se, upper = posterior_mean + z*posterior_se)

ggplot() +
  geom_vline(xintercept = 0, size = 1.2) +
  geom_errorbar(data = one_poll_per_pollster, aes(y = as.numeric(row.names(one_poll_per_pollster)), xmin = lower, xmax = upper), size = 1, color = "#00B0F6") +
  geom_point(data = one_poll_per_pollster, aes(y = as.numeric(row.names(one_poll_per_pollster)), x = spread), size = 3, color = "#00B0F6") +
  geom_errorbar(data = initital, aes(y = 17, xmin = lower, xmax = upper), size = 1, color = "#FFCC33") +
  geom_point(data = initital, aes(y = 17, x = avg), size = 3, color = "#FFCC33") +
  geom_errorbar(data = results, aes(y = 18.5, xmin = lower, xmax = upper), size = 1, color = "#FF33CC") +
  geom_point(data = results, aes(y = 18.5, x = avg), size = 3, color = "#FF33CC") +
  geom_errorbar(data = hierarchical, aes(y = 20, xmin = lower, xmax = upper), size = 1, color = "#33FF33") +
  geom_point(data = hierarchical, aes(y = 20, x = avg), size = 3, color = "#33FF33") +
  geom_vline(xintercept = d, size = 1, color = "#FF3333", linetype = "dashed") +
  geom_label(data = one_poll_per_pollster, aes(y = as.numeric(row.names(one_poll_per_pollster)), x = upper, label = pollster), hjust = 0, nudge_x = 0.01) +
  geom_label(data = initital, aes(y = 17, x = upper), label = "Initial Model", hjust = 0, nudge_x = 0.01) +
  geom_label(data = results, aes(y = 18.5, x = upper), label = "Model With Pollster Variability", hjust = 0, nudge_x = 0.01) +
  geom_label(data = hierarchical, aes(y = 20, x = upper), label = "Hierarchical Model", hjust = 0, nudge_x = 0.01) +
  ggtitle("Spread Estimates") +
  xlab("Spread Estimates") +
  ylab("Polls") +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.05)) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), plot.title = element_text(size = 20, vjust = 1))