library(ggplot2)
library(rstanarm)
set.seed(555)
simulate_data <- function(n) {
  data.frame(
    Support = sample(c('Yes', 'No'), n, replace = TRUE, prob = c(0.5, 0.5)),
    Age_Group = sample(c('18-24', '25-34', '35-44', '45-54', '55-64', '65+'), n, replace = TRUE),
    Gender = sample(c('Male', 'Female', 'Other'), n, replace = TRUE),
    Income_Group = sample(c('Low', 'Middle', 'High'), n, replace = TRUE),
    Highest_Education = sample(c('High School', 'Bachelor', 'Master', 'PhD'), n, replace = TRUE)
  )
}

dataset <- simulate_data(1000)
graph <- ggplot(dataset, aes(x = Age_Group, fill = Support)) +
  geom_bar(position = 'dodge') +
  labs(title = 'Support for Political Party by Age Group', x = 'Age Group', y = 'Count of Supporters') +
  scale_fill_manual(values = c('Yes' = 'red', 'No' = 'black'))
print(graph)
dataset$Support <- ifelse(dataset$Support == 'Yes', 1, 0)
model <- stan_glm(Support ~ Age_Group + Gender + Income_Group + Highest_Education,
                  data = dataset, family = binomial, chains = 4, iter = 2000, seed = 123)
print(summary(model))

