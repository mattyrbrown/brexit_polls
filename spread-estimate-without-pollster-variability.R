y <- brexit_polls %>% filter(enddate >= "2016-06-16")

d_hat <- sum(y$spread*y$samplesize)/sum(y$samplesize)

samplesize <- sum(y$samplesize)

X_hat <- (d_hat + 1)/2

se_hat <- 2*sqrt(X_hat*(1-X_hat)/samplesize)

estimate <- data.frame(avg = d_hat, se = se_hat, lower = qnorm(0.025, d_hat, se_hat), upper = qnorm(0.975, d_hat, se_hat))

estimate

y %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

prob <- data.frame(distribution = c("n"), remain = c(1 - pnorm(0, d_hat, se_hat)), leave = c(pnorm(0, d_hat, se_hat)))
prob

prob_actual <- data.frame(distribution = c("n"), probability = c(pnorm(d, d_hat, se_hat)))
prob_actual