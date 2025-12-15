library(daltoolbox)
library(daltoolboxdp)
library(united)
library(ggplot2)
library(dplyr)

# Janela deslizante
ts_data <- function(data, sw) {
  n <- length(data)
  mat <- matrix(NA, nrow = n - sw + 1, ncol = sw)
  for (i in 1:nrow(mat)) {
    mat[i, ] <- data[i:(i + sw - 1)]
  }
  as.data.frame(mat)
}

# Normalização Min-Max
ts_norm_gminmax <- function() {
  obj <- list(min = NULL, max = NULL)
  class(obj) <- "ts_norm_gminmax"
  obj
}

fit.ts_norm_gminmax <- function(obj, data) {
  obj$min <- min(as.matrix(data))
  obj$max <- max(as.matrix(data))
  obj
}

transform.ts_norm_gminmax <- function(obj, data) {
  as.data.frame((as.matrix(data) - obj$min) / (obj$max - obj$min))
}

if (!isGeneric("fit")) setGeneric("fit", function(obj, ...) standardGeneric("fit"))
if (!isGeneric("transform")) setGeneric("transform", function(obj, ...) standardGeneric("transform"))

# Cusum básico
cusum_basic <- function(x, Tc, mu, k, silence) {
  S_pos <- 0
  S_neg <- 0
  alarms <- rep(0, length(x))
  counter <- 0

  for (i in seq_along(x)) {
    if (counter > 0) {
      counter <- counter - 1
      S_pos <- 0; S_neg <- 0
      next
    }

    # Drift K
    S_pos <- max(0, S_pos + (x[i] - mu) - k)
    S_neg <- min(0, S_neg + (x[i] - mu) + k)

    if (S_pos > Tc || S_neg < -Tc) {
      alarms[i] <- 1
      counter <- silence
      S_pos <- 0; S_neg <- 0
    }
  }
  alarms
}

# Cross-Tc
cusum_cross_tc <- function(error_series, Tc_low, Tc_high, mu, k, silence) {

  alarms_low  <- cusum_basic(error_series, Tc_low, mu, k, silence)
  alarms_high <- cusum_basic(error_series, Tc_high, mu, k, silence)

  final_alarm <- rep(0, length(error_series))

  for (i in which(alarms_low == 1)) {
    win <- max(1, i - 2):min(length(error_series), i + 2)
    if (any(alarms_high[win] == 1)) {
      final_alarm[i] <- 1
    }
  }

  final_alarm
}

# Dataset
data(oil_3w_Type_1)
df <- oil_3w_Type_1[[1]]
series <- df$p_tpt

# Parâmetros
WINDOW_SIZE <- 10
LATENT_SIZE <- 3
TRAIN_SIZE  <- 300

# Preparação
ts_df <- ts_data(series, WINDOW_SIZE)

norm <- ts_norm_gminmax()
norm <- fit(norm, ts_df)
ts_norm <- transform(norm, ts_df)

# AE vanilla
model_ae <- autoenc_ed(WINDOW_SIZE, LATENT_SIZE)
model_ae <- fit(model_ae, ts_norm[1:TRAIN_SIZE, ])

# Reconstrução
reconstruction <- transform(model_ae, ts_norm)

error_sq <- (as.matrix(ts_norm) - as.matrix(reconstruction))^2
e_t <- apply(error_sq, 1, mean)

# DEFINIÇÃO DE PARÂMETROS PARA VISIBILIDADE
e_train <- e_t[1:TRAIN_SIZE]
mu_ref  <- mean(e_train)
sd_ref  <- sd(e_train)
max_train <- max(e_train)

k_val       <- 1.0 * sd_ref
silence_val <- 250

#Tc_low  <- quantile(e_t, 0.92)
#Tc_high <- quantile(e_t, 0.99)

Tc_low  <- (max_train * 1.1) - mu_ref
Tc_high <- (max_train * 5.0) - mu_ref

alarms_final <- cusum_cross_tc(e_t, Tc_low, Tc_high, mu_ref, k_val, silence_val)
alarms_low   <- cusum_basic(e_t, Tc_low, mu_ref, k_val, silence_val)


# Visualização
df_plot <- data.frame(
  Index  = (WINDOW_SIZE):length(series),
  Serie  = series[(WINDOW_SIZE):length(series)],
  Erro   = e_t,
  Aviso  = alarms_low,
  ErroAlarme = alarms_final
)

ggplot(df_plot, aes(x = Index)) +

  # Série original
  geom_line(aes(y = Serie, color = "Série"), linewidth = 0.6) +

  # Erro escalado
  geom_line(aes(y = Erro * max(Serie) / max(Erro), color = "Erro (escalado)"),
            linewidth = 0.6, alpha=0.5) +

  #Aviso
  geom_point(data = subset(df_plot, Aviso == 1),
             aes(y = Serie, color = "Aviso (Tc_low)"),
             shape = 16, size = 5) +

  #Erro
  geom_point(data = subset(df_plot, ErroAlarme == 1),
             aes(y = Serie, color = "Erro confirmado (Cross-Tc)"),
             shape = 8, size = 2.5, stroke = 1.5) +

  scale_color_manual(
    values = c(
      "Série" = "gray40",
      "Erro (escalado)" = "red",
      "Aviso (Tc_low)" = "orange",
      "Erro confirmado (Cross-Tc)" = "black"
    )
  ) +

  labs(
    title = "Detecção de Anomalias (AE + CUSUM)",
    y = "Série / Erro escalado",
    x = "Tempo",
    color = NULL
  ) +

  theme_minimal() +
  theme(legend.position = "top")
