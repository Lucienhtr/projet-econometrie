
#bibliothèques#
library(AER)
library(stargazer)
library(knitr)
library(readr)
library(dplyr)
library(lmtest)
library(sandwich)
library(broom)
library(ggplot2)
library(modelsummary)
library(car)



#traitement des données#
unemployment_rate <- read_csv("c:\\Users\\hitie\\Downloads\\unemployment_rate.csv")
temporary_employees <- read_csv("c:\\Users\\hitie\\Downloads\\temporary_employees.csv")
severe_material_deprivation <- read_csv("c:\\Users\\hitie\\Downloads\\severe_material_deprivation.csv")
part_time_employment_rate <- read_csv("c:\\Users\\hitie\\Downloads\\part_time_employment_rate.csv")
mean_and_medium_income <- read_csv("c:\\Users\\hitie\\Downloads\\mean_and_medium_income.csv")

target_countries <- c(
  "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR",
  "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL",
  "PT", "RO", "SI", "SK", "FI", "SE"
)

df_dep <- severe_material_deprivation %>%
  filter(geo %in% target_countries,
         TIME_PERIOD >= 2009,
         TIME_PERIOD <= 2020) %>%
  select(geo, year = TIME_PERIOD, dep_rate = OBS_VALUE)

df_inc <- mean_and_medium_income %>%
  filter(geo %in% target_countries,
         TIME_PERIOD >= 2009,
         TIME_PERIOD <= 2020) %>%
  select(geo, year = TIME_PERIOD, revenu_pps = OBS_VALUE)

df_temp <- temporary_employees %>%
  filter(geo %in% target_countries,
         TIME_PERIOD >= 2009,
         TIME_PERIOD <= 2020) %>%
  select(geo, year = TIME_PERIOD, temp_rate = OBS_VALUE)

df_part <- part_time_employment_rate %>%
  filter(geo %in% target_countries,
         TIME_PERIOD >= 2009,
         TIME_PERIOD <= 2020) %>%
  select(geo, year = TIME_PERIOD, part_time_rate = OBS_VALUE)

df_unemp <- unemployment_rate %>%
  filter(geo %in% target_countries,
         TIME_PERIOD >= 2009,
         TIME_PERIOD <= 2020) %>%
  select(geo, year = TIME_PERIOD, unemp = OBS_VALUE)

df <- df_dep %>%
  inner_join(df_inc, by = c("geo", "year")) %>%
  inner_join(df_temp, by = c("geo", "year")) %>%
  inner_join(df_part, by = c("geo", "year")) %>%
  inner_join(df_unemp, by = c("geo", "year"))

head(df)


#régression#
model_ols <- lm(
  dep_rate ~ log(revenu_pps) + temp_rate + part_time_rate + unemp,
  data = df
)

results_table <- tidy(model_ols) %>%
  mutate(
    Term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "log(revenu_pps)" ~ "Log Income (PPS)",
      term == "temp_rate" ~ "Temporary Rate",
      term == "part_time_rate" ~ "Part-Time Rate",
      term == "unemp" ~ "Unemployment",
      TRUE ~ term
    ),
    Coefficient = round(estimate, 6),
    Std.Error = round(std.error, 6),
    t.statistic = round(statistic, 4),
    p.value = round(p.value, 6)
  ) %>%
  select(Term, Coefficient, Std.Error, t.statistic, p.value)
print(results_table, n = Inf)

stargazer(model_ols, type = "text", single.row = TRUE, digits = 4)

fit_stats <- summary(model_ols)


# Breusch-Pagan test
bp_test <- bptest(model_ols)


#test de wald#


wald_test <- linearHypothesis(
  model_ols,
  c("temp_rate = 0",
    "part_time_rate = 0"),
  vcov = vcovHC(model_ols, type = "HC1")
)

#test endo#
df_iv_prep <- df %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(unemp_lag = lag(unemp)) %>%
  ungroup()

df_iv <- na.omit(df_iv_prep)

model_iv <- ivreg(
  dep_rate ~ revenu_pps + temp_rate + part_time_rate + unemp |
             revenu_pps + temp_rate + part_time_rate + unemp_lag,
  data = df_iv
)

iv_sum <- summary(model_iv, diagnostics = TRUE)

weak_stat <- iv_sum$diagnostics["Weak instruments", "statistic"]
weak_pval <- iv_sum$diagnostics["Weak instruments", "p-value"]

wu_stat <- iv_sum$diagnostics["Wu-Hausman", "statistic"]
wu_pval <- iv_sum$diagnostics["Wu-Hausman", "p-value"]

summary(model_iv, diagnostics = TRUE)


#résumé du modèle#
fit_stats <- summary(model_ols)

model_summary <- data.frame(
  Statistic = c(
    "F-statistic",
    "p-value (F-test)",
    "R²",
    "Adjusted R²",
    "Observations",
    "Breusch-Pagan Statistic",
    "Breusch-Pagan p-value",
    "Wald Statistic",
    "Wald p-value",
    "Weak Instrument Statistic",
    "Weak Instrument p-value",
    "Wu-Hausman Statistic",
    "Wu-Hausman p-value"
  ),
  Value = c(
    round(fit_stats$fstatistic[1], 4),
    round(pf(fit_stats$fstatistic[1], fit_stats$fstatistic[2], fit_stats$fstatistic[3], lower.tail = FALSE), 6),
    round(fit_stats$r.squared, 6),
    round(fit_stats$adj.r.squared, 6),
    nobs(model_ols),
    round(bp_test$statistic, 4),
    round(bp_test$p.value, 6),
    round(wald_test$F[2], 4),
    round(wald_test$`Pr(>F)`[2], 6),
    round(weak_stat, 4),
    round(weak_pval, 6),
    round(wu_stat, 4),
    round(wu_pval, 6)
  )
)

model_summary 


#doc html : disclaimer, je n'ai rien fait et je ne comprends pas les lignes suivantes, elles ont été généré par IA parce que je trouvais ça stylé#
html_content <- paste0(
  "<!DOCTYPE html>
<html>
<head>
  <meta charset='UTF-8'>
  <style>
    body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
    .container { max-width: 900px; margin: 0 auto; background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    h1 { color: #333; border-bottom: 3px solid #4472C4; padding-bottom: 10px; }
    h2 { color: #4472C4; margin-top: 30px; }
    table { width: 100%; border-collapse: collapse; margin: 15px 0; }
    th { background-color: #4472C4; color: white; padding: 12px; text-align: left; font-weight: bold; }
    td { padding: 10px 12px; border-bottom: 1px solid #ddd; }
    tr:hover { background-color: #f9f9f9; }
  </style>
</head>
<body>
  <div class='container'>
    <h1>OLS Regression Results</h1>
    <p><strong>Dependent Variable:</strong> Severe Material Deprivation Rate</p>
    
    <h2>Coefficients</h2>
    <table>
      <tr>
        <th>Variable</th>
        <th>Coefficient</th>
        <th>Std. Error</th>
        <th>t-statistic</th>
        <th>p-value</th>
      </tr>",
  paste(apply(results_table, 1, function(row) {
    paste0("<tr><td>", row[1], "</td><td>", row[2], "</td><td>", row[3], "</td><td>", row[4], "</td><td>", row[5], "</td></tr>")
  }), collapse = "\n"),
  "    </table>
    
    <h2>Model Summary</h2>
    <table>
      <tr>
        <th>Statistic</th>
        <th>Value</th>
      </tr>",
  paste(apply(model_summary, 1, function(row) {
    paste0("<tr><td>", row[1], "</td><td>", row[2], "</td></tr>")
  }), collapse = "\n"),
  "    </table>
  </div>
</body>
</html>"
)

writeLines(html_content, "c:/Users/hitie/Downloads/ols_results.html")
cat("HTML report: c:/Users/hitie/Downloads/ols_results.html\n")




# Résidus vs valeurs ajustées
plot(model_ols, which = 1)

# QQ Plot
plot(model_ols, which = 2)

# Income vs Deprivation
ggplot(df, aes(x = log(revenu_pps), y = dep_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Income and Material Deprivation",
    x = "Income (PPS)",
    y = "Severe Material Deprivation"
  )
  
