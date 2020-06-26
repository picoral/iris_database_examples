# This demonstration was created by Adriana Picoral
# (adrianaps@email.arizona) on June 26, 2020.
library(tidyverse)
library(boot)
set.seed(42)

# load data
italian_multiword_data <- read_tsv("data/iris_spina_siyanova.txt")

# check data
italian_multiword_data %>%
  group_by(DATA_COLLECTION, CEFR) %>%
  count() %>%
  pivot_wider(names_from = CEFR, values_from = n)

# do some basic summary
italian_multiword_data %>%
  group_by(CEFR) %>%
  summarise(mean_length = mean(LENGTH),
            sd = sd(LENGTH))

# create function to get these means (by CEFR)
get_mean_length_by_cefr <- function(data) {
  mean_df <- data %>%
    group_by(CEFR) %>%
    summarise(mean_length = mean(LENGTH))
  return(mean_df$mean_length)
}

get_mean_length_by_cefr(italian_multiword_data)

# adapt function to get these means (by CEFR) in a boot function
mean_length_by_cefr <- function(data, indices) {
  sampled_data <- data[indices,] # allows boot to select sample
  mean_df <- sampled_data %>%
    group_by(CEFR) %>%
    summarise(mean_length = mean(LENGTH))
  return(mean_df$mean_length)
}

# bootstrapping with 10000 replications
results <- boot(data=italian_multiword_data,
                statistic=mean_length_by_cefr,
                R=10000)

results
results$t0
results$t
plot(results,
     index = 3)

# get means from bootstrapping results
all_means <- data.frame(results$t)
colnames(all_means) <- c("A1", "A2", "B1")

all_means <- all_means %>%
  pivot_longer(cols = c("A1", "A2", "B1"),
               names_to = "CEFR")

all_means %>%
  group_by(CEFR) %>%
  summarise(mean = mean(value))

# get 95% confidence interval
a1_results <- boot.ci(results,
        type = "basic",
        index = 1)

a2_results <- boot.ci(results,
        type = "basic",
        index = 2)

b1_results <- boot.ci(results,
        type = "basic",
        index = 3)

cefr_results <- data.frame(A1 = a1_results$basic[,c(4:5)],
                           A2 = a2_results$basic[,c(4:5)],
                           B1 = b1_results$basic[,c(4:5)])

cefr_results_transpose <- as.data.frame(t(as.matrix(cefr_results)))
colnames(cefr_results_transpose) <- c("lower", "upper")

cefr_results_transpose$mean <- results$t0
cefr_results_transpose$CEFR <- rownames(cefr_results_transpose)

cefr_results_transpose %>%
  ggplot(aes(x = CEFR, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  coord_flip()

# what would it look like with basic summary
italian_multiword_data %>%
  group_by(CEFR) %>%
  summarise(mean_length = mean(LENGTH),
            sd = sd(LENGTH)) %>%
  mutate(lower = mean_length - sd,
         upper = mean_length + sd) %>%
  ggplot(aes(x = CEFR, y = mean_length)) +
  geom_point() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  coord_flip()

## ANOVA
library(broom)
tidy(aov(LENGTH ~ CEFR + Error(ID_STUDENT),
    data = italian_multiword_data))$statistic

## t-test
a1_length_vector <- italian_multiword_data %>%
  filter(CEFR == "A1") %>%
  select(LENGTH)

a2_length_vector <- italian_multiword_data %>%
  filter(CEFR == "A2") %>%
  select(LENGTH)

b1_length_vector <- italian_multiword_data %>%
  filter(CEFR == "B1") %>%
  select(LENGTH)

tidy(t.test(a1_length_vector, a2_length_vector))
