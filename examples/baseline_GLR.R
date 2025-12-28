# =========================================================
# GLR CLÁSSICO — BASELINE ESTATÍSTICO
# =========================================================

library(ggplot2)
library(gridExtra)
library(caret)
library(dplyr)

# =========================================================
# DADOS
# =========================================================

data(oil_3w_Type_1)
df <- oil_3w_Type_1[[1]]

series <- df$p_tpt
labels <- df$event
labels[is.na(labels)] <- 0
n <- length(series)

# =========================================================
# PARÂMETROS DO GLR
# =========================================================

WARMUP        <- 500
GLR_WINDOW    <- 60     # janela usada no teste
GLR_THRESHOLD <- 8      # limiar de decisão (ajustável)
MIN_SEG       <- 15     # tamanho mínimo de cada segmento

# =========================================================
# FUNÇÃO GLR
# =========================================================

glr_statistic <- function(x) {

  N <- length(x)
  best <- 0

  for (k in MIN_SEG:(N - MIN_SEG)) {

    x1 <- x[1:k]
    x2 <- x[(k + 1):N]

    mu1 <- mean(x1)
    mu2 <- mean(x2)
    s2  <- var(x)

    if (s2 > 0) {
      stat <- k * (N - k) / N * (mu1 - mu2)^2 / s2
      best <- max(best, stat)
    }
  }

  best
}

# =========================================================
# MONITORAMENTO
# =========================================================

results_glr   <- rep(NA, n)
results_drift <- rep(0, n)
drift_indices <- c()

for (t in (WARMUP + GLR_WINDOW):n) {

  win <- series[(t - GLR_WINDOW + 1):t]
  glr_val <- glr_statistic(win)

  results_glr[t] <- glr_val

  if (!is.na(glr_val) && glr_val > GLR_THRESHOLD) {
    results_drift[t] <- 1
    drift_indices <- c(drift_indices, t)
  }
}

# =========================================================
# AVALIAÇÃO (MESMA DO MÉTODO HÍBRIDO)
# =========================================================

label_drift <- rep(0, n)
event_idx <- which(labels == 1)

for (i in event_idx) {
  label_drift[max(1, i-25):min(n, i+25)] <- 1
}

valid_idx <- which(!is.na(results_glr))

pred_vec <- factor(results_drift[valid_idx], levels = c(0,1))
ref_vec  <- factor(label_drift[valid_idx], levels = c(0,1))

cm <- confusionMatrix(pred_vec, ref_vec, positive = "1")
print(cm$table)

cat("\nPrecisão:", cm$byClass["Precision"])
cat("\nRecall:", cm$byClass["Sensitivity"])
cat("\nF1:", cm$byClass["F1"])
cat("\nPrimeiro drift detectado em t =", ifelse(length(drift_indices)>0, drift_indices[1], "Nenhum"))

# =========================================================
# PREPARAÇÃO PARA GRÁFICOS
# =========================================================

df_plot <- data.frame(
  Index  = 1:n,
  Series = series,
  GLR    = results_glr,
  Alarm  = results_drift,
  Real   = labels
)

df_alarm <- df_plot[df_plot$Alarm == 1, ]
df_anom  <- df_plot[df_plot$Real == 1, ]

# =========================================================
# GRÁFICO 1 — SÉRIE + ALARMES GLR
# =========================================================

g1 <- ggplot(df_plot, aes(x = Index)) +
  geom_line(aes(y = Series), color = "gray40") +

  geom_point(
    data = df_alarm,
    aes(y = Series, color = "Alarme GLR"),
    shape = 17,
    size = 3
  ) +

  geom_point(
    data = df_anom,
    aes(y = Series, color = "Anomalia Real"),
    shape = 4,
    size = 3
  ) +

  scale_color_manual(
    name = "Legenda",
    values = c(
      "Alarme GLR"   = "darkgreen",
      "Anomalia Real" = "black"
    )
  ) +

  labs(
    title = "Série temporal com alarmes do GLR clássico",
    y = "Pressão",
    x = "Índice"
  ) +

  theme_minimal() +
  theme(legend.position = "bottom")

# =========================================================
# GRÁFICO 2 — ESTATÍSTICA GLR
# =========================================================

g2 <- ggplot(df_plot, aes(x = Index, y = GLR)) +
  geom_line(color = "darkgreen") +

  geom_hline(
    yintercept = GLR_THRESHOLD,
    linetype = "dashed",
    color = "gray40"
  ) +

  geom_point(
    data = df_alarm,
    aes(y = GLR),
    color = "darkgreen",
    shape = 17,
    size = 3
  ) +

  labs(
    title = "Estatística GLR ao longo do tempo",
    y = "GLR",
    x = "Índice"
  ) +

  theme_minimal()

grid.arrange(g1, g2, ncol = 1, heights = c(1.2, 1))
