# ============================================================
# Santiago Lozano Juana Terrios
# ============================================================

# ----- BLOQUE 0. PREPARACIÓN E IMPORTACIÓN -----
req <- c("tidyverse", "readr", "broom", "scales")
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(readr)
library(broom)
library(scales)


df <- read_csv("C:/Users/SANTIAGO/Downloads/StudentsPerformance.csv", show_col_types = FALSE)

edu_order <- c("some high school","high school","some college",
               "associate's degree","bachelor's degree","master's degree")
df <- df %>%
  mutate(`parental level of education` = factor(`parental level of education`,
                                                levels = edu_order, ordered = TRUE))

theme_acc <- theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey20"),
    plot.caption = element_text(color = "grey30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

outdir <- "plots_accesibles"
dir.create(outdir, showWarnings = FALSE)

# ----- BLOQUE 1. ANOVA -----
fit_anova <- aov(`math score` ~ `parental level of education`, data = df)
anova_tab <- broom::tidy(fit_anova)

p_anova <- df %>%
  ggplot(aes(x = `parental level of education`, y = `math score`)) +
  geom_boxplot(width = 0.7, fill = "grey85", color = "black", outlier.alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = 23, fill = "black") +
  labs(
    title = "ANOVA: Matemáticas según educación de los padres",
    subtitle = paste0("p-valor global = ",
                      formatC(anova_tab$p.value[1], format = "e", digits = 2),
                      " (si p < 0.05 → hay diferencias entre grupos)"),
    x = "Nivel educativo de los padres",
    y = "Puntaje de Matemáticas",
    caption = "Punto negro = media del grupo"
  ) + theme_acc + theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(p_anova)  # <-- MUESTRA
ggsave(file.path(outdir, "1_ANOVA_boxplot_math_by_parent_edu.png"),
       p_anova, width = 10, height = 7, dpi = 160)

cat("\n[ANOVA]\n"); print(anova_tab)

# ----- BLOQUE 2. PEARSON -----
pearson_res <- cor.test(df$`math score`, df$`reading score`, method = "pearson")

p_pearson <- df %>%
  ggplot(aes(x = `reading score`, y = `math score`)) +
  geom_point(alpha = 0.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Relación lineal: Lectura y Matemáticas (Pearson)",
    subtitle = paste0("r = ", round(pearson_res$estimate, 3),
                      " | p = ", formatC(pearson_res$p.value, format = "e", digits = 2),
                      " (relación positiva)"),
    x = "Puntaje de Lectura", y = "Puntaje de Matemáticas"
  ) + theme_acc

print(p_pearson)  # <-- MUESTRA
ggsave(file.path(outdir, "2_Pearson_scatter_math_reading.png"),
       p_pearson, width = 9, height = 7, dpi = 160)

cat("\n[PEARSON]\n"); print(pearson_res)

# ----- BLOQUE 3. SPEARMAN -----
spearman_res <- cor.test(df$`math score`, df$`reading score`, method = "spearman")

df_ranks <- df %>%
  mutate(rank_read = rank(`reading score`, ties.method = "average"),
         rank_math = rank(`math score`, ties.method = "average"))

p_spearman <- df_ranks %>%
  ggplot(aes(x = rank_read, y = rank_math)) +
  geom_point(alpha = 0.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Relación monotónica: Rangos (Spearman)",
    subtitle = paste0("ρ = ", round(spearman_res$estimate, 3),
                      " | p = ", formatC(spearman_res$p.value, format = "e", digits = 2)),
    x = "Rango de Lectura", y = "Rango de Matemáticas"
  ) + theme_acc

print(p_spearman)  # <-- MUESTRA
ggsave(file.path(outdir, "3_Spearman_rank_scatter_math_reading.png"),
       p_spearman, width = 9, height = 7, dpi = 160)

cat("\n[SPEARMAN]\n"); print(spearman_res)

# ----- BLOQUE 4. T-TEST PAREADO -----
tt_paired <- t.test(df$`reading score`, df$`writing score`, paired = TRUE)
mean_diff <- mean(df$`reading score` - df$`writing score`, na.rm = TRUE)

p_ttest <- df %>%
  mutate(diff = `reading score` - `writing score`) %>%
  ggplot(aes(x = diff)) +
  geom_histogram(bins = 20, color = "black", fill = "grey80") +
  geom_vline(xintercept = mean_diff, linetype = "dashed", linewidth = 1) +
  annotate("text", x = mean_diff, y = Inf, vjust = 1.5,
           label = paste0("Media de la diferencia = ", round(mean_diff, 2)),
           size = 5) +
  labs(
    title = "Dentro del mismo grupo: Lectura − Escritura (t pareado)",
    subtitle = paste0("t = ", round(tt_paired$statistic, 2),
                      " | p = ", formatC(tt_paired$p.value, format = "e", digits = 2),
                      " (si p < 0.05 → diferencia promedio)"),
    x = "Diferencia (Lectura − Escritura)", y = "Frecuencia",
    caption = "Nota: diferencia ≠ causalidad."
  ) + theme_acc

print(p_ttest)  # <-- MUESTRA
ggsave(file.path(outdir, "4_Paired_ttest_hist_diff_reading_minus_writing.png"),
       p_ttest, width = 9, height = 7, dpi = 160)

cat("\n[T-TEST PAREADO]\n"); print(tt_paired)

# ----- BLOQUE 5. REGRESIÓN SIMPLE -----
fit_simple <- lm(`math score` ~ `reading score`, data = df)
sum_simple <- broom::glance(fit_simple)
coef_simple <- broom::tidy(fit_simple)

beta0 <- coef_simple$estimate[coef_simple$term == "(Intercept)"]
beta1 <- coef_simple$estimate[coef_simple$term == "`reading score`"]
r2_s  <- sum_simple$r.squared

p_reg_simple <- df %>%
  ggplot(aes(x = `reading score`, y = `math score`)) +
  geom_point(alpha = 0.6, shape = 16) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Regresión simple: Matemáticas ~ Lectura",
    subtitle = paste0("Math = ", round(beta0, 2), " + ",
                      round(beta1, 3), " × Reading | R² = ", round(r2_s, 3)),
    x = "Puntaje de Lectura", y = "Puntaje de Matemáticas"
  ) + theme_acc

print(p_reg_simple)  # <-- MUESTRA
ggsave(file.path(outdir, "5_Simple_Regression_math_on_reading.png"),
       p_reg_simple, width = 9, height = 7, dpi = 160)

cat("\n[REGRESIÓN SIMPLE]\n"); print(summary(fit_simple))

# ----- BLOQUE 6. REGRESIÓN MÚLTIPLE -----
fit_multi <- lm(`math score` ~ `reading score` + `writing score`, data = df)
sum_multi <- broom::glance(fit_multi)
r2_m <- sum_multi$r.squared

df_pred <- df %>%
  mutate(pred = predict(fit_multi))

p_reg_multi <- df_pred %>%
  ggplot(aes(x = `math score`, y = pred)) +
  geom_point(alpha = 0.6, shape = 16) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1) +
  labs(
    title = "Regresión múltiple: Real vs. Predicho",
    subtitle = paste0("Math ~ Reading + Writing | R² = ", round(r2_m, 3),
                      " (cercanía a la línea → buen ajuste)"),
    x = "Matemáticas (real)", y = "Matemáticas (predicho)"
  ) + theme_acc

print(p_reg_multi)  # <-- MUESTRA
ggsave(file.path(outdir, "6_Multiple_Regression_actual_vs_predicted.png"),
       p_reg_multi, width = 9, height = 7, dpi = 160)

cat("\n[REGRESIÓN MÚLTIPLE]\n"); print(summary(fit_multi))

# ===== FIN =====
cat("\nImágenes guardadas en la carpeta '", outdir, "'.\n", sep = "")