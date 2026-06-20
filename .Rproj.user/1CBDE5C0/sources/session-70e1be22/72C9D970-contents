library(tidyverse)

library(NHANES)

data("NHANES")

head(NHANES)

dim(NHANES)

names(NHANES)

glimpse(NHANES)

adults <- NHANES %>%
  filter(Age >= 20)

dim(adults)

adults_bp <- adults %>%
  filter(
    !is.na(BPSysAve),
    !is.na(BPDiaAve)
  )

dim(adults_bp)

nrow(NHANES)

n_distinct(NHANES$ID)

NHANES %>%
  count(ID) %>%
  count(n)

adults_bp <- adults_bp %>%
  mutate(
    BMI_Class = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal",
      BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    )
  )

count(adults_bp, BMI_Class)

adults_bp <- adults_bp %>%
  mutate(
    HTN_Stage = case_when(
      BPSysAve < 120 & BPDiaAve < 80 ~ "Normal",
      BPSysAve >= 120 &
        BPSysAve < 130 &
        BPDiaAve < 80 ~ "Elevated",
      (BPSysAve >= 130 & BPSysAve < 140) |
        (BPDiaAve >= 80 & BPDiaAve < 90) ~ "Stage 1",
      BPSysAve >= 140 |
        BPDiaAve >= 90 ~ "Stage 2",
      TRUE ~ NA_character_
    )
  )

count(adults_bp, HTN_Stage)

adults_bp %>%
  group_by(HTN_Stage) %>%
  summarize(
    Mean_Systolic = mean(BPSysAve),
    Mean_Diastolic = mean(BPDiaAve),
    .groups = "drop"
  )

adults_bp <- adults_bp %>%
  filter(!is.na(BMI))

adults_bp_unique <- adults_bp %>%
  distinct(ID, .keep_all = TRUE)

nrow(adults_bp_unique)

n_distinct(adults_bp_unique$ID)

ggplot(
  adults_bp_unique,
  aes(
    x = factor(
      BMI_Class,
      levels = c("Underweight", "Normal", "Overweight", "Obese")
    ),
    fill = HTN_Stage
  )
) +
  geom_bar(position = "fill") +
  labs(
    title = "Hypertension Stage by BMI Category",
    x = "BMI Category",
    y = "Proportion"
  ) +
  theme_minimal()

ggplot(
  adults_bp_unique,
  aes(
    x = Age,
    y = BPSysAve
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Age and Systolic Blood Pressure",
    x = "Age (years)",
    y = "Average Systolic Blood Pressure"
  ) +
  theme_minimal()

ggplot(
  adults_bp_unique,
  aes(
    x = Age,
    y = BPSysAve,
    color = Smoke100n
  )
) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Age and Systolic Blood Pressure by Smoking Status",
    x = "Age (years)",
    y = "Average Systolic Blood Pressure"
  ) +
  theme_minimal()

ggplot(
  adults_bp_unique,
  aes(
    x = PhysActive,
    y = BPSysAve
  )
) +
  geom_boxplot() +
  labs(
    title = "Systolic Blood Pressure by Physical Activity Status",
    x = "Physically Active",
    y = "Average Systolic Blood Pressure"
  ) +
  theme_minimal()

chol_data <- adults_bp_unique %>%
  filter(
    !is.na(TotChol),
    !is.na(Smoke100n)
  )

t.test(
  TotChol ~ Smoke100n,
  data = chol_data
)

bp_model <- lm(
  BPSysAve ~ Age + BMI,
  data = adults_bp_unique
)

summary(bp_model)

bp_model2 <- lm(
  BPSysAve ~ Age + BMI + PhysActive,
  data = adults_bp_unique
)

summary(bp_model2)

summary(bp_model)$r.squared

summary(bp_model2)$r.squared

diabetes_data <- adults_bp_unique %>%
  filter(!is.na(Diabetes))

diabetes_model <- glm(
  Diabetes ~ Age + BMI,
  family = "binomial",
  data = diabetes_data
)

summary(diabetes_model)

count(adults_bp_unique, Diabetes)

count(adults_bp_unique, Smoke100n)

count(adults_bp_unique, PhysActive)

