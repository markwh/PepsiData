# amhg.R
# 12/2/2016
# Snooping on how well AMHG *could* work with the Pepsi data

head(Pepsi_new)
summary(Pepsi_new)
sum(Pepsi_new$S <= 0)

Pepsi_new %>% 
  group_by(name) %>% 
  summarize(numWna = sum(is.na(W)),
            numSna = sum(is.na(S)))

amhgdat <- Pepsi_new %>% 
  filter(!is.na(dA)) %>% 
  group_by(name, xs) %>% 
  filter(W < quantile(W, 0.9)) %>% 
  mutate(wbar = mean(log(W)),
          wsd = sd(log(W)),
          qbar = mean(log(Q)),
          qsd = sd(log(Q)),
          QWBM = QWBM[1],
          wc = wc[1],
          hsd = sd(H),
          corwq = cor(log(W), log(Q)),
          b = wsd / qsd * corwq,
          a = wbar - qbar * b,
         qhat_ahg = exp((log(W) - a) / b)) %>% 
  filter(!is.na(corwq)) %>% 
  group_by(name) %>% 
  mutate(wc_meanlog = exp(mean(log(W)))) %>% 
  ungroup()
  

amhgdat %>% 
  filter(name %in% sample(unique(amhgdat$name), 12)) %>%
  filter(xs < 20) %>%
  # filter(name == "Ohio") %>%
  ggplot(aes(x = W, y = Q)) + 
  geom_point(aes(color = xs)) + 
  geom_line(aes(y = qhat_ahg, group = xs)) +
  scale_x_log10() + 
  scale_y_log10() +
  facet_wrap(~name, scales = "free", nrow = 3) +
  geom_vline(aes(xintercept = wc_meanlog))


amhg_calib <- amhgdat %>% 
  filter(a > 0) %>% 
  group_by(name) %>% 
  summarize(beta1 = cor(log(a), b) * sd(b) / sd(log(a)),
            beta0 = mean(b) - mean(log(a)) * beta1) %>% 
  mutate(logQc = - 1 / beta1,
         logWc = logQc * beta0)

amhg_val <- amhgdat %>% 
  left_join(amhg_calib, by = "name") %>% 
  mutate(qhat_amhg = exp(1 / b * (log(W) - logWc) + logQc))


amhg_val %>% 
  filter(name == "Po") %>% 
  # group_by(time) %>% 
  # summarize(Q = median(Q), Qhat = median(qhat_amhg)) %>% 
  ggplot(aes(x = time, y = Q)) + 
  geom_point() + 
  geom_line(aes(y = qhat_amhg, color = xs)) + 
  scale_y_log10()
  

# Take a straight-up regression approach

amhg_reg <- amhgdat %>% 
  group_by(name) %>% 
  mutate(lhs = log(Q) - 1/b * log(W)) %>% 
  summarize(logWc = - cor(1/b, lhs) * sd(lhs) / sd(1/b),
            logQc = mean(lhs) + logWc * mean(1/b))

# compare this

plot(amhg_reg$logWc, amhg_calib$logWc)
plot(amhg_reg$logQc, amhg_calib$logQc)


amhg_val2 <- amhgdat %>% 
  left_join(amhg_reg, by = "name") %>% 
  mutate(qhat_amhg = exp(1 / b * (log(W) - logWc) + logQc))


amhg_val2 %>% 
  filter(name == "Po") %>% 
  # group_by(time) %>% 
  # summarize(Q = median(Q), qhat_amhg = median(qhat_amhg)) %>% 
  ggplot(aes(x = time, y = Q)) + 
  geom_point() + 
  geom_line(aes(y = qhat_amhg, color = xs, group = xs)) + 
  scale_y_log10()

amhg_val2 %>% 
  # filter(name == "Po") %>% 
  group_by(name, time) %>%
  summarize(Q = median(Q), qhat_amhg = median(qhat_amhg)) %>%
  # glimpse()
  ggplot(aes(x = time, y = Q)) + 
  geom_point() + 
  geom_line(aes(y = qhat_amhg)) + 
  scale_y_log10() +
  facet_wrap(~name, scales = "free")


# How are b's distributed?

amhgdat %>% 
  group_by(name, xs) %>% 
  summarize(b = b[1]) %>% 
  ggplot(aes(x = name, y = b)) +
  geom_boxplot()

amhgdat %>% 
  group_by(name, xs) %>% 
  summarize(b = b[1]) %>% 
  `[[`("b") %>% 
  hist()

# Can we predict logWc and logQc from stats?
# Possibilities: mean of mean(logW), median(mean(log(W)))

amhgPreds <- amhgdat %>% 
  group_by(name) %>% 
  summarize(wmean = mean(wbar),
            wmedian = median(wbar),
            hsdmean = mean(hsd),
            wsdmean = mean(wsd),
            qwbm = log(QWBM[1]),
            qmean = mean(qbar))

preddat <- amhgPreds %>% 
  left_join(amhg_reg, by = "name") %>% 
  select(-name) 

pairs(preddat)
plot(logWc ~ wmean, preddat)
abline(0, 1)
plot(logQc ~ qwbm, preddat)
abline(0, 1)

amhg_reg %>% 
  arrange(desc(logQc))

plot(logQc ~ qwbm, preddat, ylim = c(0, 15), xlim = c(0, 15))
abline(0, 1)

plot(logQc ~ qmean, preddat, ylim = c(0, 15), xlim = c(0, 15))
abline(0, 1)
