#EDA Indexes and conversion
df<- read.csv("New_Freely_destinations_fully_cleaned_indexes.csv", header=TRUE)
library(dplyr)
df <- df %>% rename(`Rule.of.law.index` = X)
df <- df %>%
  mutate(
    across(
      c(Human.rights.index, Rule.of.law.index),
      ~ifelse(.x %in% c("#DIV/0!", "#N/A"), NA, .x)
    ),
    across(
      c(Human.rights.index, Rule.of.law.index),
      as.numeric
    )
    
  )
library(ggplot2)
library(tidyr)

df_long <- df %>%
  select(Human.rights.index, Rule.of.law.index) %>%
  pivot_longer(cols = everything(), names_to = "Index", values_to = "Value")

ggplot(df_long, aes(x = Value, fill = Index)) +
  geom_histogram(
    bins = 25,
    alpha = 0.7,
    position = "identity",
    color = "white"
  ) +
  facet_wrap(~Index, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  labs(
    title = "Distribution of Human Rights and Rule of Law Indices",
    x = "Index Value",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20")
  )
df <- df %>%
  mutate(
    convert = ifelse(convert == "YES", 1, 0)
  )

model <- glm(
  convert ~ Human.rights.index + Rule.of.law.index,
  data = df,
  family = binomial
)
summary(model)
ggplot(df, aes(x = Human.rights.index, y = convert)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Logistic Relationship: Human Rights Index vs Conversion")
table(df$convert)
# rule of law is statistically significant and negatively correlated while human rights is significant and positively correlated.
# it suggests for rule of law, travellers to countries with strong legal systems might feel safer and less inclined to buy travel insurance.
# limited variation in data points can  impact model performance especially with human rights index