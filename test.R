# ============================================================
# PADS+ Criminological Research Methods Analysis
# 12-Variable Correlation Heatmap + Predictive Regression
# ============================================================
 
library(ggplot2)
library(reshape2)
library(dplyr)
 
# ---- 1. LOAD DATA ------------------------------------------
df <- read.csv("data.csv")
 
# ---- 2. SELECT THE 12 KEY VARIABLES (from Essay Titles) ----
vars12 <- c(
  "PoorSelfControl",       # 1. Self-control
  "NeighInforSocControl",  # 2. Neighbourhood informal social control
  "PeerCrime",             # 3. Peer crime
  "MoralEmotions",         # 4. Moral emotions (shame and guilt)
  "Deterrence",            # 5. Deterrence (certainty and severity)
  "AlcoholFreq",           # 6. Alcohol consumption
  "ShopliftingFreq",       # 7. Shoplifting
  "CrimDamageFreq",        # 8. Criminal damage (vandalism and arson)
  "SeriousTheftFreq",      # 9. Serious theft (robbery, burglary, car crime)
  "ViolentCrimeFreq",      # 10. Violent crime (robbery and assault)
  "TotalCrimeFreq",        # 11. Total crime
  "ArrestedFreq"           # 12. Being caught or arrested by the police
)
 
labels12 <- c(
  "Self-Control (poor)",
  "Neigh. Informal\nSocial Control",
  "Peer Crime",
  "Moral Emotions",
  "Deterrence",
  "Alcohol Use",
  "Shoplifting",
  "Criminal Damage",
  "Serious Theft",
  "Violent Crime",
  "Total Crime",
  "Arrested"
)
 
data12 <- df[, vars12]
colnames(data12) <- labels12
 
# ---- 3. CORRELATION HEATMAP --------------------------------
cor_mat  <- cor(data12, use = "pairwise.complete.obs", method = "pearson")
cor_melt <- melt(cor_mat)
cor_melt$value_label <- round(cor_melt$value, 2)
 
p_heat <- ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = value_label), size = 2.6, color = "black") +
  scale_fill_gradient2(
    low = "#2166ac", mid = "white", high = "#d6604d",
    midpoint = 0, limits = c(-1, 1),
    name = "Pearson r"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_text(angle = 40, hjust = 1, size = 8.5),
    axis.text.y  = element_text(size = 8.5),
    axis.title   = element_blank(),
    plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
    legend.position = "right"
  ) +
  labs(
    title    = "Correlation Matrix: 12 Key Criminological Variables",
    subtitle = "PADS+ dataset, age-16 cohort (N ≈ 700)"
  ) +
  coord_fixed()
 
ggsave("./heatmap_12vars.png",
       p_heat, width = 10, height = 9, dpi = 180)
cat("✓ Heatmap saved\n")
 
# ---- 4. REGRESSION: Predicting TotalCrimeFreq --------------
# Predictors: the 5 risk/protective factors (essay topics 1-6, excl. outcome vars)
# Outcomes  : crime frequency variables (essays 7-11) + arrested (12)
 
predictors <- c("Self-Control (poor)", "Neigh. Informal\nSocial Control",
                "Peer Crime", "Moral Emotions", "Deterrence", "Alcohol Use")
outcomes   <- c("Shoplifting", "Criminal Damage", "Serious Theft",
                "Violent Crime", "Total Crime", "Arrested")
 
# Fit OLS models for each outcome, extract standardised betas
get_betas <- function(outcome) {
  fmla <- as.formula(paste0("`", outcome, "` ~ `",
                             paste(predictors, collapse = "` + `"), "`"))
  m    <- lm(fmla, data = as.data.frame(scale(data12)))
  coef_df <- as.data.frame(summary(m)$coefficients)[-1, ]
  coef_df$Predictor <- rownames(coef_df)
  coef_df$Outcome   <- outcome
  coef_df$Beta      <- coef_df$Estimate
  coef_df$Sig       <- ifelse(coef_df[,4] < 0.001, "***",
                       ifelse(coef_df[,4] < 0.01,  "**",
                       ifelse(coef_df[,4] < 0.05,  "*", "")))
  coef_df[, c("Predictor", "Outcome", "Beta", "Sig")]
}
 
reg_df <- do.call(rbind, lapply(outcomes, get_betas))
reg_df$Predictor <- gsub("`", "", reg_df$Predictor)
reg_df$label     <- ifelse(reg_df$Sig != "",
                           paste0(round(reg_df$Beta, 2), reg_df$Sig), "")
 
p_reg <- ggplot(reg_df, aes(x = Predictor, y = Beta, fill = Beta)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label,
                vjust = ifelse(Beta >= 0, -0.3, 1.2)),
            size = 2.8) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 3) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, guide = "none") +
  scale_x_discrete(labels = function(x) {
    gsub("\\\\n", "\n", x)   # restore newlines in axis labels
  }) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 40, hjust = 1, size = 7.5),
    strip.text   = element_text(face = "bold", size = 9),
    plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title    = "OLS Regression: Predicting Crime Outcomes from Risk/Protective Factors",
    subtitle = "Standardised β coefficients  |  * p<.05  ** p<.01  *** p<.001",
    x = NULL, y = "Standardised β"
  )
 
ggsave("./regression_betas.png",
       p_reg, width = 13, height = 8, dpi = 180)
cat("✓ Regression plot saved\n")
 
# ---- 5. MODEL FIT SUMMARY TABLE ----------------------------
fit_tbl <- do.call(rbind, lapply(outcomes, function(o) {
  fmla <- as.formula(paste0("`", o, "` ~ `",
                             paste(predictors, collapse = "` + `"), "`"))
  m  <- lm(fmla, data = as.data.frame(scale(data12)))
  sm <- summary(m)
  data.frame(Outcome = o,
             R2      = round(sm$r.squared, 3),
             Adj_R2  = round(sm$adj.r.squared, 3),
             F_stat  = round(sm$fstatistic[1], 2),
             p_value = round(pf(sm$fstatistic[1], sm$fstatistic[2],
                                sm$fstatistic[3], lower.tail = FALSE), 4))
}))
write.csv(fit_tbl, "./model_fit_summary.csv", row.names = FALSE)
cat("✓ Model fit table saved\n")
print(fit_tbl)
