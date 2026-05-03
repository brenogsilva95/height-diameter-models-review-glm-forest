################################################################
################################# Pacotes ######################
################################################################
library(ggplot2)
library(dplyr)
library(minpack.lm)
library(hnp)
################################################################
##################### Group 1 - Linear models ##################
################################################################
################################################################
############################ [M1] ##############################
################################################################
mod1 <- lm(h ~ dap + I(dap^2), Dados2); summary(mod1)
novos_dados <- data.frame(dap = seq(0, 200, length.out = 100))
valores_previstos_m1 <- predict(mod1)
# ----------------- GLM ------------------ #
mod1_glm <- glm(h ~ dap + I(dap^2), 
                family = gaussian(link = "identity"),
                data = Dados2); summary(mod1_glm) 
valores_previstos_m1_glm <- predict(mod1_glm)
# ---------------- Gráfico --------------- #
m1_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m1_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m1, color = "[M1]", linetype = "[M1]"), 
            linewidth = 1.4) +
  #ggtitle("(A)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M1]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M1]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(0, 100, length.out = 500))

# Predição para o modelo GLM e o modelo M1 no novo intervalo
valores_previstos_m1_glm_ext <- predict(mod1_glm, newdata = novo_dap)
valores_previstos_m1_ext <- predict(mod1, newdata = novo_dap)

# ---------------- Gráfico --------------- #
# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(0, 100, length.out = 500))

# Predição para o modelo GLM e o modelo M1 no novo intervalo
valores_previstos_m1_glm_ext <- predict(mod1_glm, newdata = novo_dap)
valores_previstos_m1_ext <- predict(mod1, newdata = novo_dap)

# ---------------- Gráfico --------------- #
m1_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m1_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m1_ext, color = "[M1]", linetype = "[M1]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M1]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M1]" = "solid")) +  # Pode ajustar o estilo da linha, se preferir
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m1_glm_ext


# ---------------- Argumentos - HNP ggplot2 --------------- #
hnp_cores <- function(X) {
  out <- X[["residuals"]] < X[["lower"]] | X[["residuals"]] > X[["upper"]]
  c("Dentro", "Fora")[out + 1L]
}
hnp_texto <- function(X) {
  out <- X[["residuals"]] < X[["lower"]] | X[["residuals"]] > X[["upper"]]
  n <- sum(out)
  txt1 <- sprintf("Total points: %d", nrow(X))
  txt2 <- sprintf("Points out of envelope: %d (%2.2g%%)", n, 100*n/nrow(X))
  list(Total = txt1, Fora = txt2,2)
}
theme_hnp <- function(){ 
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))
}
# ---------------- hnp - [M1] --------------- #
Grap1=hnp(mod1, how.many.out = T, resid.type = "deviance")
G1 <- with(Grap1, data.frame(x, lower, upper, median, residuals))
G1$cores = hnp_cores(G1)
G1_texto = hnp_texto(G1) 
GM1 <- ggplot(data = G1, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
#  geom_text(x = 0, y = 3, label = G1_texto$Total, hjust = 0, size=5.2) +
  geom_text(x = 0, y = 4, label = G1_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G1_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M1]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M1]: GLM --------------- #
Grap1_M1_GLM=hnp(mod1_glm, how.many.out = T)
G1_glm <- with(Grap1_M1_GLM, data.frame(x, lower, upper, median, residuals))
G1_glm$cores = hnp_cores(G1_glm)
G1_glm_texto = hnp_texto(G1_glm) 
GM1_GLM <- ggplot(data = G1_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G1_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G1_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M2] ##############################
################################################################
mod2 <- lm(h ~ I(1/dap), Dados2); summary(mod2)
valores_previstos_m2 <- predict(mod2)
valor_assintota_m2 <- coef(mod2)[1]
# ----------------- GLM ------------------ #
mod2_glm <- glm(h ~ I(1/dap), 
                family = gaussian(link = "identity"),
                data = Dados2); summary(mod2_glm) 
valores_previstos_m2_glm <- predict(mod2_glm)
valor_assintota_m2_mlg <- coef(mod2_glm)[1]
# ---------------- Gráfico --------------- #
m2_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m2_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m2, color = "[M2]", linetype = "[M2]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m2, 
             linetype = 2, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m2, 
                label = paste0("y = ", round(valor_assintota_m2, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m2_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  #ggtitle("(B)") +
  geom_text(aes(x = -1, y = valor_assintota_m2_mlg, 
                label = paste0("y = ", round(valor_assintota_m2_mlg, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M2]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M2]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo GLM e o modelo M2 no novo intervalo
valores_previstos_m2_glm_ext <- predict(mod2_glm, newdata = novo_dap)
valores_previstos_m2_ext <- predict(mod2, newdata = novo_dap)

# ---------------- Gráfico --------------- #
m2_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m2_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m2_ext, color = "[M2]", linetype = "[M2]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m2, 
             linetype = 2, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m2, 
                label = paste0("y = ", round(valor_assintota_m2, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m2_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m2_mlg, 
                label = paste0("y = ", round(valor_assintota_m2_mlg, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M2]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M2]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m2_glm_ext

# ---------------- hnp - [M2] --------------- #
Grap2=hnp(mod2, how.many.out = T, resid.type = "deviance")
G2 <- with(Grap2, data.frame(x, lower, upper, median, residuals))
G2$cores = hnp_cores(G2)
G2_texto = hnp_texto(G2) 
GM2 <- ggplot(data = G2, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G2_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G2_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M2]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M2]: GLM --------------- #
Grap2_M2_GLM=hnp(mod2_glm, how.many.out = T)
G2_glm <- with(Grap2_M2_GLM, data.frame(x, lower, upper, median, residuals))
G2_glm$cores = hnp_cores(G2_glm)
G2_glm_texto = hnp_texto(G2_glm) 
GM2_GLM <- ggplot(data = G2_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G2_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G2_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M3] ##############################
################################################################
mod3test <- lm(h ~ I(1/dap) + I((1/dap)^2), Dados2); summary(mod3test)
valores_previstos_m3_test <- predict(mod3test)
valor_assintota_m3_test <- coef(mod3test)[1]

mod3 <- lm(h ~ I((1/dap)^2), Dados2); summary(mod3)
valores_previstos_m3 <- predict(mod3)
valor_assintota_m3 <- coef(mod3)[1]
# ----------------- GLM ------------------ #
mod3_glm <- glm(h ~ I((1/dap)^2), 
                family = gaussian(link = "identity"),
                data = Dados2); summary(mod3_glm) 
valores_previstos_m3_glm <- predict(mod3_glm)
valor_assintota_m3_mlg <- coef(mod3_glm)[1]
# ggplot(data = Dados2, aes(x = dap, y = h)) +
#   geom_point(color = "black", shape = 19, size = 2) +
#   coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
#   geom_line(aes(x = dap, y = valores_previstos_m3, color = "[M3 - Transformation]"),
#             linewidth = 0.9) +
#   geom_line(aes(x = dap, y = valores_previstos_m3_glm, color = "[M3 - Identity]"),
#             linewidth = 0.9) +
#   geom_line(aes(x = dap, y =   valores_previstos_m3_test, color = "[M3 - Square]"),
#             linewidth = 0.9) +
#   labs(x = "Diameter (cm)", y = "Height (m)", color = "GLM") +
#   #ggtitle("Group 1 - Linear Models") +
#   scale_color_manual(values = c("Data" = "black", "[M3 - Transformation]" = "red", 
#                                 "[M3 - Identity]" = "blue", "[M3 - Square]" = "green"
#   )) +
#   theme_bw() +
#   theme(legend.title = element_text(size = 18),
#         legend.text = element_text(size = 18),
#         axis.title = element_text(size = 30),
#         axis.text.x = element_text(color = "black", hjust=1),
#         axis.text.y = element_text(color = "black", hjust=1),
#         axis.text = element_text(size = 25),
#         plot.title = element_text(size = 17),
#         strip.text.x = element_text(size = 12))
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +  # Removendo a legenda extra de 'Data'
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(y = valores_previstos_m3_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.4) +
  geom_line(aes(y = valores_previstos_m3, color = "[M3]", linetype = "[M3]"), 
            linewidth = 2) +
  geom_line(aes(y = valores_previstos_m3_test, color = "[M2] + [M3]", linetype = "[M2] + [M3]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m3, 
             linetype = 2, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m3, 
                label = paste0("y = ", round(valor_assintota_m3, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_text(aes(x = -1, y = valor_assintota_m3_test, 
                label = paste0("y = ", round(valor_assintota_m3_test, 2), " [M2] + [M3]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m3_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_hline(yintercept = valor_assintota_m3_test, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m3_mlg, 
                label = paste0("y = ", round(valor_assintota_m3_mlg, 2), " [M3]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)") +
  scale_color_manual(values = c("GLM" = "red", "[M3]" = "black", "[M2] + [M3]" = "blue"),
                     name = "Cases") +
  scale_linetype_manual(values = c("GLM" = "solid", "[M3]" = "dotted", "[M2] + [M3]" = "solid"),
                        name = "Cases") +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(3.1, 100, length.out = 500))

# Predição para os modelos no novo intervalo
valores_previstos_m3_test_ext <- predict(mod3test, newdata = novo_dap)
valores_previstos_m3_ext <- predict(mod3, newdata = novo_dap)
valores_previstos_m3_glm_ext <- predict(mod3_glm, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +  # Removendo a legenda extra de 'Data'
  coord_cartesian(xlim = c(3.1, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m3_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m3_ext, color = "[M3]", linetype = "[M3]"), 
            linewidth = 1) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m3_test_ext, color = "[M2] + [M3]", linetype = "[M2] + [M3]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m3, 
             linetype = 2, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m3, 
                label = paste0("y = ", round(valor_assintota_m3, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_text(aes(x = -1, y = valor_assintota_m3_test, 
                label = paste0("y = ", round(valor_assintota_m3_test, 2), " [M2] + [M3]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m3_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_hline(yintercept = valor_assintota_m3_test, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m3_mlg, 
                label = paste0("y = ", round(valor_assintota_m3_mlg, 2), " [M3]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)") +
  scale_color_manual(values = c("GLM" = "red", "[M3]" = "black", "[M2] + [M3]" = "blue"),
                     name = "Cases") +
  scale_linetype_manual(values = c("GLM" = "solid", "[M3]" = "solid", "[M2] + [M3]" = "solid"),
                        name = "Cases") +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))


# ---------------- hnp - [M3] --------------- #
Grap3=hnp(mod3, how.many.out = T, resid.type = "deviance")
G3 <- with(Grap3, data.frame(x, lower, upper, median, residuals))
G3$cores = hnp_cores(G3)
G3_texto = hnp_texto(G3) 
GM3 <- ggplot(data = G3, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G3_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G3_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M3]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M3]: GLM --------------- #
Grap3_M3_GLM=hnp(mod3_glm, how.many.out = T)
G3_glm <- with(Grap3_M3_GLM, data.frame(x, lower, upper, median, residuals))
G3_glm$cores = hnp_cores(G3_glm)
G3_glm_texto = hnp_texto(G3_glm) 
GM3_GLM <- ggplot(data = G3_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G3_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G3_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M6] ##############################
################################################################
mod6 <- lm(h ~ I(log(dap)), Dados2); summary(mod6)
valores_previstos_m6 <- predict(mod6)
# ----------------- GLM ------------------ #
mod6_glm <- glm(h ~ I(log(dap)), 
                family = gaussian(link = "identity"),
                data = Dados2); summary(mod6_glm) 
valores_previstos_m6_glm <- predict(mod6_glm)
# ---------------- Gráfico --------------- #
m6_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m6_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m6, color = "[M6]", linetype = "[M6]"), 
            linewidth = 1.4) +
  #ggtitle("(D)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M6]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M6]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))
# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo GLM e o modelo M6 no novo intervalo
valores_previstos_m6_glm_ext <- predict(mod6_glm, newdata = novo_dap)
valores_previstos_m6_ext <- predict(mod6, newdata = novo_dap)

# ---------------- Gráfico --------------- #
m6_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m6_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m6_ext, color = "[M6]", linetype = "[M6]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M6]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M6]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m6_glm_ext
# ---------------- hnp - [M6] --------------- #
Grap6=hnp(mod6, how.many.out = T, resid.type = "deviance")
G6 <- with(Grap6, data.frame(x, lower, upper, median, residuals))
G6$cores = hnp_cores(G6)
G6_texto = hnp_texto(G6) 
GM6 <- ggplot(data = G6, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G6_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G6_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M6]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M6]: GLM --------------- #
Grap6_M6_GLM=hnp(mod6_glm, how.many.out = T)
G6_glm <- with(Grap6_M6_GLM, data.frame(x, lower, upper, median, residuals))
G6_glm$cores = hnp_cores(G6_glm)
G6_glm_texto = hnp_texto(G6_glm) 
GM6_GLM <- ggplot(data = G6_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G6_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G6_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M40] #############################
################################################################
mod40 <- lm(sqrt(h) ~ sqrt(dap), Dados2); summary(mod40)
valores_previstos_m40 <- (predict(mod40))^2
# ----------------- GLM ------------------ #
mod40_glm <- glm(h ~ dap + sqrt(dap), 
                family = gaussian(link = "identity"),
                data = Dados2); summary(mod40_glm) 
valores_previstos_m40_glm <- predict(mod40_glm)
# ----------------- GLM ------------------ #
mod40_glm_root <- glm(h ~ sqrt(dap), 
                      family = gaussian(link = "sqrt"),
                      data = Dados2); summary(mod40_glm_root) 
valores_previstos_m40_glm_root <- predict(mod40_glm_root)^2
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m40, color = "M40", linetype = "M40"), 
            linewidth = 1.4) +
  geom_line(aes(y = valores_previstos_m40_glm_root, color = "Identity", 
                linetype = "Identity"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("M40" = "black", "Identity" = "red"),
                     labels = c( "[M40]", expression(eta == sqrt(mu)))) +  
  scale_linetype_manual(values = c("Identity" = "solid", "M40" = "solid"),
                        labels = c("[M40]", expression(eta == sqrt(mu)))) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))
# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M40 e os GLMs no novo intervalo
valores_previstos_m40_ext <- (predict(mod40, newdata = novo_dap))^2
valores_previstos_m40_glm_root_ext <- (predict(mod40_glm_root, newdata = novo_dap))^2

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m40_ext, color = "M40", linetype = "M40"), 
            linewidth = 1.4) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m40_glm_root_ext, color = "Identity", 
                linetype = "Identity"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("M40" = "black", "Identity" = "red"),
                     labels = c( "[M40]", expression(eta == sqrt(mu)))) +  
  scale_linetype_manual(values = c("Identity" = "solid", "M40" = "solid"),
                        labels = c("[M40]", expression(eta == sqrt(mu)))) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))
# ---------------- hnp - [M40] --------------- #
Grap40=hnp(mod40, how.many.out = T, resid.type = "deviance")
G40 <- with(Grap40, data.frame(x, lower, upper, median, residuals))
G40$cores = hnp_cores(G40)
G40_texto = hnp_texto(G40) 
GM40 <- ggplot(data = G40, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.48, label = G40_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.52, label = G40_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.6))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M40]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M40]: GLM --------------- #
Grap40_M40_GLM=hnp(mod40_glm, how.many.out = T)
G40_glm <- with(Grap40_M40_GLM, data.frame(x, lower, upper, median, residuals))
G40_glm$cores = hnp_cores(G40_glm)
G40_glm_texto = hnp_texto(G40_glm) 
GM40_GLM <- ggplot(data = G40_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G40_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G40_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M41] #############################
################################################################
mod41 <- lm(sqrt(h) ~ log(dap), Dados2); summary(mod41)
valores_previstos_m41 <- (predict(mod41))^2
# ----------------- GLM ------------------ #
mod41_glm <- glm(h ~ log(dap) + I((log(dap))^2), 
                 family = gaussian(link = "identity"),
                 data = Dados2); summary(mod41_glm) 
valores_previstos_m41_glm <- predict(mod41_glm)
# ----------------- GLM ------------------ #
mod41_glm_root <- glm(h ~ log(dap), 
                      family = gaussian(link = "sqrt"),
                      data = Dados2); summary(mod41_glm_root) 
valores_previstos_m41_glm_root <- predict(mod41_glm_root)^2
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m41, color = "M41", linetype = "M41"), 
            linewidth = 1.4) +
  geom_line(aes(y = valores_previstos_m41_glm_root, color = "SquareRoot", 
                linetype = "SquareRoot"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("M41" = "black", "SquareRoot" = "red"),
                     labels = c("[M41]", expression(eta == sqrt(mu)))) +  
  scale_linetype_manual(values = c("M41" = "solid", "SquareRoot" = "solid"),
                        labels = c("[M41]", expression(eta == sqrt(mu)))) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M41 e os GLMs no novo intervalo
valores_previstos_m41_ext <- (predict(mod41, newdata = novo_dap))^2
valores_previstos_m41_glm_root_ext <- (predict(mod41_glm_root, newdata = novo_dap))^2

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m41_ext, color = "M41", linetype = "M41"), 
            linewidth = 1.4) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m41_glm_root_ext, color = "SquareRoot", 
                linetype = "SquareRoot"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("M41" = "black", "SquareRoot" = "red"),
                     labels = c("[M41]", expression(eta == sqrt(mu)))) +  
  scale_linetype_manual(values = c("M41" = "solid", "SquareRoot" = "solid"),
                        labels = c("[M41]", expression(eta == sqrt(mu)))) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))
# ---------------- hnp - [M41] --------------- #
Grap41 = hnp(mod41, how.many.out = T, resid.type = "deviance")
G41 <- with(Grap41, data.frame(x, lower, upper, median, residuals))
G41$cores = hnp_cores(G41)
G41_texto = hnp_texto(G41) 
GM41 <- ggplot(data = G41, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.48, label = G41_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.52, label = G41_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.6))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M41]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M41]: GLM --------------- #
Grap41_M41_GLM=hnp(mod41_glm, how.many.out = T)
G41_glm <- with(Grap41_M41_GLM, data.frame(x, lower, upper, median, residuals))
G41_glm$cores = hnp_cores(G41_glm)
G41_glm_texto = hnp_texto(G41_glm) 
GM41_GLM <- ggplot(data = G41_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G41_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G41_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
################## Group 2 - Inverse polynomials ###############
################################################################
################################################################
############################ [M4] ##############################
################################################################
naslund_model <- function(dap, beta_0, beta_1) {
  return(dap^2 / (beta_0 + beta_1 * dap)^2)
}
mod4 <- nls(h-1.30 ~ naslund_model(dap, beta_0, beta_1), 
            start = list(beta_0 = 1.30, beta_1 = 0.40), Dados2); summary(mod4)
valores_previstos_m4 <- predict(mod4) + 1.30
valor_assintota_m4 <- 1/(coef(mod4)[2])^2 + 1.30
# ----------------- GLM ------------------ #
mod4_glm <- glm(h-1.30 ~ I(1/dap) + I(1/dap^2), 
                family = gaussian(link = "inverse"),
                data = Dados2); summary(mod4_glm) 
valores_previstos_m4_glm <- predict(mod4_glm, type = "response") + 1.30
valor_assintota_m4_mlg <- (1/coef(mod4_glm)[1]) + 1.30
# ---------------- Gráfico --------------- #
m4_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m4_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m4, color = "[M4]", linetype = "[M4]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m4, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m4, 
                label = paste0("y = ", round(valor_assintota_m4, 2), " [M4]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m4_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m4_mlg, 
                label = paste0("y = ", round(valor_assintota_m4_mlg, 2), " [GLM]")), 
            vjust = -0.2, hjust = 0, size = 7, color = "black") +
  #ggtitle("(A)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M4]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M4]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Função para o modelo de Näslund
naslund_model <- function(dap, beta_0, beta_1) {
  return(dap^2 / (beta_0 + beta_1 * dap)^2)
}

# Predição para o modelo M4 e o GLM no novo intervalo
valores_previstos_m4_ext <- predict(mod4, newdata = novo_dap) + 1.30
valores_previstos_m4_glm_ext <- predict(mod4_glm, newdata = novo_dap, type = "response") + 1.30

# ---------------- Gráfico --------------- #
m4_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m4_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m4_ext, color = "[M4]", linetype = "[M4]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m4, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m4, 
                label = paste0("y = ", round(valor_assintota_m4, 2), " [M4]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m4_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m4_mlg, 
                label = paste0("y = ", round(valor_assintota_m4_mlg, 2), " [GLM]")), 
            vjust = -0.2, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M4]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M4]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m4_glm_ext

# ---------------- hnp - [M4] --------------- #
Grap4 = hnp(mod4_glm, how.many.out = T, resid.type = "deviance")
G4 <- with(Grap4, data.frame(x, lower, upper, median, residuals))
G4$cores = hnp_cores(G4)
G4_texto = hnp_texto(G4) 
GM4 <- ggplot(data = G4, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G4_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G4_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M4]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M4]: GLM --------------- #
Grap4_M4_GLM=hnp(mod4_glm, how.many.out = T)
G4_glm <- with(Grap4_M4_GLM, data.frame(x, lower, upper, median, residuals))
G4_glm$cores = hnp_cores(G4_glm)
G4_glm_texto = hnp_texto(G4_glm) 
GM4_GLM <- ggplot(data = G4_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G4_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G4_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M8] ##############################
################################################################
Dados2$h_mod8 = sqrt(1/(Dados2$h - 1.30))
mod8 <- lm(h_mod8 ~ I(1/dap), Dados2); summary(mod8)
valores_previstos_m8 <- 1 / ((predict(mod8))^2) + 1.30
valor_assintota_m8 <- 1/(coef(mod8)[1])^2 + 1.30
# ----------------- GLM ------------------ #
mod8_glm <- glm(h - 1.30 ~ I(1/dap) + I(1/dap^2), 
                family = gaussian(link = "inverse"),
                data = Dados2); summary(mod8_glm) 
valores_previstos_m8_glm <- predict(mod8_glm, type = "response") + 1.30
valor_assintota_m8_mlg <- 1/(coef(mod8_glm)[1]) + 1.30
# ---------------- Gráfico --------------- #
m8_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m8_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m8, color = "[M8]", linetype = "[M8]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m8, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m8, 
                label = paste0("y = ", round(valor_assintota_m8, 2), " [M8]")), 
            vjust = -0.1, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m8_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m8_mlg, 
                label = paste0("y = ", round(valor_assintota_m8_mlg, 2), " [GLM]")), 
            vjust = -0.1, hjust = 0, size = 7, color = "black") +
  ##ggtitle("(B)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M8]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M8]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M8 e o GLM no novo intervalo
valores_previstos_m8_ext <- 1 / ((predict(mod8, newdata = novo_dap))^2) + 1.30
valores_previstos_m8_glm_ext <- predict(mod8_glm, newdata = novo_dap, type = "response") + 1.30

# ---------------- Gráfico --------------- #
m8_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m8_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m8_ext, color = "[M8]", linetype = "[M8]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m8, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m8, 
                label = paste0("y = ", round(valor_assintota_m8, 2), " [M8]")), 
            vjust = -0.1, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m8_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m8_mlg, 
                label = paste0("y = ", round(valor_assintota_m8_mlg, 2), " [GLM]")), 
            vjust = -0.1, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M8]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M8]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m8_glm_ext
# ---------------- hnp - [M8] --------------- #
Grap8 = hnp(mod8, how.many.out = T, resid.type = "deviance")
G8 <- with(Grap8, data.frame(x, lower, upper, median, residuals))
G8$cores = hnp_cores(G8)
G8_texto = hnp_texto(G8) 
GM8 <- ggplot(data = G8, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.034, label = G8_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.039, label = G8_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.04))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M8]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M8]: GLM --------------- #
Grap8_M8_GLM=hnp(mod8_glm, how.many.out = T)
G8_glm <- with(Grap8_M8_GLM, data.frame(x, lower, upper, median, residuals))
G8_glm$cores = hnp_cores(G8_glm)
G8_glm_texto = hnp_texto(G8_glm) 
GM8_GLM <- ggplot(data = G8_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G8_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G8_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M9] ##############################
################################################################
Dados2$h_mod9 = (1/(Dados2$h - 1.30))^(1/3)
mod9 <- lm(h_mod9 ~ I(1/dap), Dados2); summary(mod9)
valores_previstos_m9 <- 1 / ((predict(mod9))^3) + 1.30
valor_assintota_m9 <- 1/(coef(mod9)[1])^3 + 1.30
# ----------------- GLM ------------------ #
mod9_glm <- glm(h-1.30 ~ I(1/dap) + I(1/dap^2) + I(1/dap^3), 
                family = gaussian(link = "inverse"),
                data = Dados2); summary(mod9_glm)
valores_previstos_m9_glm <- predict(mod9_glm, type = "response") + 1.30
valor_assintota_m9_mlg <- 1/(coef(mod9_glm)[1]) + 1.30
# ---------------- Gráfico --------------- #
m9_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m9_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m9, color = "[M9]", linetype = "[M9]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m9, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m9, 
                label = paste0("y = ", round(valor_assintota_m9, 2), " [M9]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m9_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m9_mlg, 
                label = paste0("y = ", round(valor_assintota_m9_mlg, 2), " [GLM]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
#  #ggtitle("(C)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M9]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M9]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(3, 100, length.out = 500))

# Predição para o modelo M9 e o GLM no novo intervalo
valores_previstos_m9_ext <- 1 / ((predict(mod9, newdata = novo_dap))^3) + 1.30
valores_previstos_m9_glm_ext <- predict(mod9_glm, newdata = novo_dap, type = "response") + 1.30

# ---------------- Gráfico --------------- #
m9_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(3, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m9_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m9_ext, color = "[M9]", linetype = "[M9]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m9, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m9, 
                label = paste0("y = ", round(valor_assintota_m9, 2), " [M9]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m9_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m9_mlg, 
                label = paste0("y = ", round(valor_assintota_m9_mlg, 2), " [GLM]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M9]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M9]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m9_glm_ext
# ---------------- hnp - [M9] --------------- #
Grap9 = hnp(mod9, how.many.out = T, resid.type = "deviance")
G9 <- with(Grap9, data.frame(x, lower, upper, median, residuals))
G9$cores = hnp_cores(G9)
G9_texto = hnp_texto(G9) 
GM9 <- ggplot(data = G9, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.034, label = G9_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.039, label = G9_texto$Fora, hjust = 0, size=7) +
 coord_cartesian(ylim=c(0,0.04))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M9]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M9]: GLM --------------- #
Grap9_M9_GLM=hnp(mod9_glm, how.many.out = T)
G9_glm <- with(Grap9_M9_GLM, data.frame(x, lower, upper, median, residuals))
G9_glm$cores = hnp_cores(G9_glm)
G9_glm_texto = hnp_texto(G9_glm) 
GM9_GLM <- ggplot(data = G9_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G9_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G9_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M16] #############################
################################################################
prodan_model <- function(dap, beta_0, beta_1, beta_2) {
  return (dap^2 / (beta_0 + beta_1 * dap + beta_2 * dap^2))
}

mod16 <- nls(h ~ prodan_model(dap, beta_0, beta_1, beta_2), 
             start = c(beta_0 = 20, beta_1 = -2.1, beta_2 = 0.1), 
             data = Dados2); summary(mod16)
valores_previstos_m16 <- predict(mod16)
valor_assintota_m16 <- 1/coef(mod16)[3]
# ----------------- GLM ------------------ #
mod16_glm <- glm(h ~ I(1/dap) + I(1/dap^2), 
                 family = gaussian(link = "inverse"),
                 data = Dados2); summary(mod16_glm)
valores_previstos_m16_glm <- 1/predict(mod16_glm)
valor_assintota_m16_mlg <- (1/coef(mod16_glm)[1]) 
# ---------------- Gráfico --------------- #
m16_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m16_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m16, color = "[M16]", linetype = "[M16]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m16, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m16, 
                label = paste0("y = ", round(valor_assintota_m16, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m16_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m16_mlg, 
                label = paste0("y = ", round(valor_assintota_m16_mlg, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(D)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M16]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M16]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Função para o modelo de Prodan
prodan_model <- function(dap, beta_0, beta_1, beta_2) {
  return(dap^2 / (beta_0 + beta_1 * dap + beta_2 * dap^2))
}

# Predição para o modelo M16 e o GLM no novo intervalo
valores_previstos_m16_ext <- predict(mod16, newdata = novo_dap)
valores_previstos_m16_glm_ext <- 1 / predict(mod16_glm, newdata = novo_dap)

# ---------------- Gráfico --------------- #
m16_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m16_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m16_ext, color = "[M16]", linetype = "[M16]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m16, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m16, 
                label = paste0("y = ", round(valor_assintota_m16, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m16_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m16_mlg, 
                label = paste0("y = ", round(valor_assintota_m16_mlg, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M16]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M16]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m16_glm_ext
# ---------------- hnp - [M16] --------------- #
Grap16 = hnp(mod16_glm, how.many.out = T, resid.type = "deviance")
G16 <- with(Grap16, data.frame(x, lower, upper, median, residuals))
G16$cores = hnp_cores(G16)
G16_texto = hnp_texto(G16) 
GM16 <- ggplot(data = G16, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G16_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G16_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M16]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M16]: GLM --------------- #
Grap16_M16_GLM=hnp(mod16_glm, how.many.out = T)
G16_glm <- with(Grap16_M16_GLM, data.frame(x, lower, upper, median, residuals))
G16_glm$cores = hnp_cores(G16_glm)
G16_glm_texto = hnp_texto(G16_glm) 
GM16_GLM <- ggplot(data = G16_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G16_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G16_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M27] #############################
################################################################
Dados2$Resp_M27 <- (Dados2$dap)^2/(Dados2$h)
mod27_lm <- lm(Resp_M27 ~ dap + I(dap^2),
               data = Dados2); summary(mod27_lm)
valores_previstos_m27 <- (Dados2$dap^2)/predict(mod27_lm)
valor_assintota_m27 <- 1/coef(mod27_lm)[3]
# ----------------- GLM ------------------ #
mod27_glm <- glm(h ~ I(1/dap) + I(1/dap^2), 
                 family = gaussian(link = "inverse"),
                 data = Dados2); summary(mod27_glm)
valores_previstos_m27_glm <- 1/predict(mod27_glm)
valor_assintota_m27_mlg <- (1/coef(mod27_glm)[1]) 
# ---------------- Gráfico --------------- #
m27_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m27_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m27, color = "[M27]", linetype = "[M27]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m27, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m27, 
                label = paste0("y = ", round(valor_assintota_m27, 2), " [M27]")), 
            vjust = 1.4, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m27_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m27_mlg, 
                label = paste0("y = ", round(valor_assintota_m27_mlg, 2), " [GLM]")), 
            vjust = -0.8, hjust = 0, size = 7, color = "black") +
  #ggtitle("(E)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M27]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M27]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M27 e o GLM no novo intervalo
valores_previstos_m27_ext <- (novo_dap$dap^2) / predict(mod27_lm, newdata = novo_dap)
valores_previstos_m27_glm_ext <- 1 / predict(mod27_glm, newdata = novo_dap)

# ---------------- Gráfico --------------- #
m27_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m27_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m27_ext, color = "[M27]", linetype = "[M27]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m27, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m27, 
                label = paste0("y = ", round(valor_assintota_m27, 2), " [M27]")), 
            vjust = 1.4, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m27_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m27_mlg, 
                label = paste0("y = ", round(valor_assintota_m27_mlg, 2), " [GLM]")), 
            vjust = -0.8, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M27]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M27]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m27_glm_ext
# ---------------- hnp - [M27] --------------- #
Grap27 = hnp(mod27_lm, how.many.out = T, resid.type = "deviance")
G27 <- with(Grap27, data.frame(x, lower, upper, median, residuals))
G27$cores = hnp_cores(G27)
G27_texto = hnp_texto(G27) 
GM27 <- ggplot(data = G27, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 2.4, label = G27_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 2.67, label = G27_texto$Fora, hjust = 0, size=7) +
 coord_cartesian(ylim=c(0,3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M27]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M27]: GLM --------------- #
Grap27_M27_GLM=hnp(mod27_glm, how.many.out = T)
G27_glm <- with(Grap27_M27_GLM, data.frame(x, lower, upper, median, residuals))
G27_glm$cores = hnp_cores(G27_glm)
G27_glm_texto = hnp_texto(G27_glm) 
GM27_GLM <- ggplot(data = G27_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G27_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G27_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M28] #############################
################################################################
Dados2$Resp_M28 <- (Dados2$dap)/sqrt(Dados2$h)
mod28_lm <- lm(Resp_M28 ~ dap,
               data = Dados2); summary(mod28_lm)
valores_previstos_m28 <- (Dados2$dap^2)/(predict(mod28_lm))^2
valor_assintota_m28 <- 1/(coef(mod28_lm)[2])^2
# ----------------- GLM ------------------ #
mod28_glm <- glm(h ~ I(1/dap) + I(1/dap^2), 
                 family = gaussian(link = "inverse"),
                 data = Dados2); summary(mod28_glm)
valores_previstos_m28_glm <- 1/predict(mod28_glm)
valor_assintota_m28_mlg <- (1/coef(mod28_glm)[1]) 
# ---------------- Gráfico --------------- #
m28_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m28_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m28, color = "[M28]", linetype = "[M28]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m28, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m28, 
                label = paste0("y = ", round(valor_assintota_m28, 2), " [M28")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m28_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m28_mlg, 
                label = paste0("y = ", round(valor_assintota_m28_mlg, 2), " [GLM]")), 
            vjust = -0.1, hjust = 0, size = 7, color = "black") +
  #ggtitle("(F)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M28]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M28]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M28 e o GLM no novo intervalo
valores_previstos_m28_ext <- (novo_dap$dap^2) / (predict(mod28_lm, newdata = novo_dap))^2
valores_previstos_m28_glm_ext <- 1 / predict(mod28_glm, newdata = novo_dap)

# ---------------- Gráfico --------------- #
m28_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m28_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m28_ext, color = "[M28]", linetype = "[M28]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m28, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m28, 
                label = paste0("y = ", round(valor_assintota_m28, 2), " [M28]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m28_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m28_mlg, 
                label = paste0("y = ", round(valor_assintota_m28_mlg, 2), " [GLM]")), 
            vjust = -0.1, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", "[M28]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M28]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m28_glm_ext
x11()
gridExtra::grid.arrange(m4_glm, m8_glm,
                        m9_glm, m16_glm,
                        m27_glm, m28_glm, ncol = 3)
# ---------------- hnp - [M28] --------------- #
Grap28 = hnp(mod28_lm, how.many.out = T, resid.type = "deviance")
G28 <- with(Grap28, data.frame(x, lower, upper, median, residuals))
G28$cores = hnp_cores(G28)
G28_texto = hnp_texto(G28) 
GM28 <- ggplot(data = G28, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.48, label = G28_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.52, label = G28_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.6))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M28]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M28]: GLM --------------- #
Grap28_M28_GLM=hnp(mod28_glm, how.many.out = T)
G28_glm <- with(Grap28_M28_GLM, data.frame(x, lower, upper, median, residuals))
G28_glm$cores = hnp_cores(G28_glm)
G28_glm_texto = hnp_texto(G28_glm) 
GM28_GLM <- ggplot(data = G28_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G28_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G28_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 

x11()
gridExtra::grid.arrange(GM4, GM8,
                        GM9, GM16,
                        GM27, GM28,
                        GM4_GLM, GM8_GLM,
                        GM9_GLM, GM16_GLM,
                        GM27_GLM, GM28_GLM, ncol = 6)
################################################################
########### Group 3 - Nonlinear linearizable models ############
################################################################
################################################################
############################ [M7] ##############################
################################################################
mod7 <- lm(log(h) ~ I(log(dap)), Dados2); summary(mod7)
valores_previstos_m7 <- exp(predict(mod7))
# ----------------- GLM ------------------ #
mod7_glm <- glm(h ~ log(dap), 
                family = gaussian(link = "log"),
                data = Dados2); summary(mod7_glm)
valores_previstos_m7_glm <- exp(predict(mod7_glm))
# ---------------- Gráfico --------------- #
m7_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m7_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m7, color = "[M7]", linetype = "[M7]"), 
            linewidth = 1.4) +
  #ggtitle("(A)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M7]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M7]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))
# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M7 e o GLM no novo intervalo
valores_previstos_m7_ext <- exp(predict(mod7, newdata = novo_dap))
valores_previstos_m7_glm_ext <- exp(predict(mod7_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m7_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m7_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m7_ext, color = "[M7]", linetype = "[M7]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M7]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M7]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m7_glm_ext

# ---------------- hnp - [M7] --------------- #
Grap7 = hnp(mod7, how.many.out = T, resid.type = "deviance")
G7 <- with(Grap7, data.frame(x, lower, upper, median, residuals))
G7$cores = hnp_cores(G7)
G7_texto = hnp_texto(G7) 
GM7 <- ggplot(data = G7, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.25, label = G7_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.27, label = G7_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M7]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M7]: GLM --------------- #
Grap7_M7_GLM=hnp(mod7_glm, how.many.out = T)
G7_glm <- with(Grap7_M7_GLM, data.frame(x, lower, upper, median, residuals))
G7_glm$cores = hnp_cores(G7_glm)
G7_glm_texto = hnp_texto(G7_glm) 
GM7_GLM <- ggplot(data = G7_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G7_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G7_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M17] #############################
################################################################
mod17 <- lm(log(h) ~ I(1/dap), Dados2); summary(mod17)
valores_previstos_m17 <- exp(predict(mod17))
valor_assintota_m17 <- exp(coef(mod17)[1])
# ----------------- GLM ------------------ #
mod17_glm <- glm(h ~ I(1/dap), 
                 family = gaussian(link = "log"),
                 data = Dados2); summary(mod17_glm)
valores_previstos_m17_glm <- exp(predict(mod17_glm))
valor_assintota_m17_mlg <- exp(coef(mod17_glm)[1])
# ---------------- Gráfico --------------- #
m17_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m17_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m17, color = "[M17]", linetype = "[M17]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m17, 
             linetype = 2, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m17, 
                label = paste0("y = ", round(valor_assintota_m17, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m17_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m17_mlg, 
                label = paste0("y = ", round(valor_assintota_m17_mlg, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(B)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M17]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M17]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M17 e o GLM no novo intervalo
valores_previstos_m17_ext <- exp(predict(mod17, newdata = novo_dap))
valores_previstos_m17_glm_ext <- exp(predict(mod17_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m17_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m17_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m17_ext, color = "[M17]", linetype = "[M17]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m17, 
             linetype = 2, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m17, 
                label = paste0("y = ", round(valor_assintota_m17, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m17_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m17_mlg, 
                label = paste0("y = ", round(valor_assintota_m17_mlg, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M17]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M17]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m17_glm_ext

# ---------------- hnp - [M17] --------------- #
Grap17 = hnp(mod17, how.many.out = T, resid.type = "deviance")
G17 <- with(Grap17, data.frame(x, lower, upper, median, residuals))
G17$cores = hnp_cores(G17)
G17_texto = hnp_texto(G17) 
GM17 <- ggplot(data = G17, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.25, label = G17_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.27, label = G17_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M17]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M17]: GLM --------------- #
Grap17_M17_GLM=hnp(mod17_glm, how.many.out = T)
G17_glm <- with(Grap17_M17_GLM, data.frame(x, lower, upper, median, residuals))
G17_glm$cores = hnp_cores(G17_glm)
G17_glm_texto = hnp_texto(G17_glm) 
GM17_GLM <- ggplot(data = G17_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G17_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G17_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M18] #############################
################################################################
mod18 <- lm(log(h) ~ I(log(1/dap)), Dados2); summary(mod18)
valores_previstos_m18 <- exp(predict(mod18))
# ----------------- GLM ------------------ #
mod18_glm <- glm(h ~ log(1/dap), 
                 family = gaussian(link = "log"),
                 data = Dados2); summary(mod18_glm)
valores_previstos_m18_glm <- exp(predict(mod18_glm))
# ---------------- Gráfico --------------- #
m18_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m18_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m18, color = "[M18]", linetype = "[M18]"), 
            linewidth = 1.4) +
  #ggtitle("(C)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M18]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M18]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M18 e o GLM no novo intervalo
valores_previstos_m18_ext <- exp(predict(mod18, newdata = novo_dap))
valores_previstos_m18_glm_ext <- exp(predict(mod18_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m18_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m18_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m18_ext, color = "[M18]", linetype = "[M18]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M18]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M18]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15));m18_glm_ext

# ---------------- hnp - [M18] --------------- #
Grap18 = hnp(mod18, how.many.out = T, resid.type = "deviance")
G18 <- with(Grap18, data.frame(x, lower, upper, median, residuals))
G18$cores = hnp_cores(G18)
G18_texto = hnp_texto(G18) 
GM18 <- ggplot(data = G18, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.25, label = G18_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.27, label = G18_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M18]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M18]: GLM --------------- #
Grap18_M18_GLM=hnp(mod18_glm, how.many.out = T)
G18_glm <- with(Grap18_M18_GLM, data.frame(x, lower, upper, median, residuals))
G18_glm$cores = hnp_cores(G18_glm)
G18_glm_texto = hnp_texto(G18_glm) 
GM18_GLM <- ggplot(data = G18_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G18_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G18_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M22] #############################
################################################################
mod22 <- lm(log(h) ~ log(dap) + I(1/dap), Dados2); summary(mod22)
valores_previstos_m22 <- exp(predict(mod22))
# ----------------- GLM ------------------ #
mod22_glm <- glm(h ~ log(dap) + I(1/dap), 
                 family = gaussian(link = "log"),
                 data = Dados2); summary(mod22_glm)
valores_previstos_m22_glm <- exp(predict(mod22_glm))
valor_assintota_m22_mlg <- exp(coef(mod22_glm)[1])
# ---------------- Gráfico --------------- #
m22_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m22_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m22, color = "[M22]", linetype = "[M22]"), 
            linewidth = 1.4) +
  #ggtitle("(D)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M22]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M22]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M22 e o GLM no novo intervalo
valores_previstos_m22_ext <- exp(predict(mod22, newdata = novo_dap))
valores_previstos_m22_glm_ext <- exp(predict(mod22_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m22_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m22_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m22_ext, color = "[M22]", linetype = "[M22]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M22]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M22]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico
m22_glm_ext

# ---------------- hnp - [M22] --------------- #
Grap22 = hnp(mod22, how.many.out = T, resid.type = "deviance")
G22 <- with(Grap22, data.frame(x, lower, upper, median, residuals))
G22$cores = hnp_cores(G22)
G22_texto = hnp_texto(G22) 
GM22 <- ggplot(data = G22, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.25, label = G22_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.27, label = G22_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M22]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M22]: GLM --------------- #
Grap22_M22_GLM=hnp(mod22_glm, how.many.out = T)
G22_glm <- with(Grap22_M22_GLM, data.frame(x, lower, upper, median, residuals))
G22_glm$cores = hnp_cores(G22_glm)
G22_glm_texto = hnp_texto(G22_glm) 
GM22_GLM <- ggplot(data = G22_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G22_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G22_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M29] #############################
################################################################
mod29 <- lm(log(h) ~ (1/dap) + I((1/dap)^2), Dados2); summary(mod29)
valores_previstos_m29 <- exp(predict(mod29))
valor_assintota_m29 <- exp(coef(mod29)[1])
# ----------------- GLM ------------------ #
mod29_glm <- glm(h ~ (1/dap) + I((1/dap)^2), 
                 family = gaussian(link = "log"),
                 data = Dados2); summary(mod29_glm)
valores_previstos_m29_glm <- exp(predict(mod29_glm))
valor_assintota_m29_mlg <- exp(coef(mod29_glm)[1])
# ---------------- Gráfico --------------- #
m29_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m29_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m29, color = "[M29]", linetype = "[M29]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m29, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m29, 
                label = paste0("y = ", round(valor_assintota_m29, 2), " [M29]")), 
            vjust = 1.2, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m29_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m29_mlg, 
                label = paste0("y = ", round(valor_assintota_m29_mlg, 2), " [GLM]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(E)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M29]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M29]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M29 e o GLM no novo intervalo
valores_previstos_m29_ext <- exp(predict(mod29, newdata = novo_dap))
valores_previstos_m29_glm_ext <- exp(predict(mod29_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m29_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m29_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m29_ext, color = "[M29]", linetype = "[M29]"), 
            linewidth = 1.4) +
  geom_hline(yintercept = valor_assintota_m29, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m29, 
                label = paste0("y = ", round(valor_assintota_m29, 2), " [M29]")), 
            vjust = 1.2, hjust = 0, size = 7, color = "black") +
  geom_hline(yintercept = valor_assintota_m29_mlg, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m29_mlg, 
                label = paste0("y = ", round(valor_assintota_m29_mlg, 2), " [GLM]")), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M29]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M29]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico
m29_glm_ext

# ---------------- hnp - [M29] --------------- #
Grap29 = hnp(mod29, how.many.out = T, resid.type = "deviance")
G29 <- with(Grap29, data.frame(x, lower, upper, median, residuals))
G29$cores = hnp_cores(G29)
G29_texto = hnp_texto(G29) 
GM29 <- ggplot(data = G29, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.25, label = G29_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.27, label = G29_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M29]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M29]: GLM --------------- #
Grap29_M29_GLM=hnp(mod29_glm, how.many.out = T)
G29_glm <- with(Grap29_M29_GLM, data.frame(x, lower, upper, median, residuals))
G29_glm$cores = hnp_cores(G29_glm)
G29_glm_texto = hnp_texto(G29_glm) 
GM29_GLM <- ggplot(data = G29_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G29_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G29_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M30] #############################
################################################################
mod30 <- lm(log(h) ~ dap + I(dap^2), Dados2); summary(mod30)
valores_previstos_m30 <- exp(predict(mod30))
# ----------------- GLM ------------------ #
mod30_glm <- glm(h ~ dap + I(dap^2), 
                 family = gaussian(link = "log"),
                 data = Dados2); summary(mod30_glm)
valores_previstos_m30_glm <- exp(predict(mod30_glm))
# ---------------- Gráfico --------------- #
m30_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m30_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m30, color = "[M30]", linetype = "[M30]"), 
            linewidth = 1.4) +
  #ggtitle("(F)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M30]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M30]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M30 e o GLM no novo intervalo
valores_previstos_m30_ext <- exp(predict(mod30, newdata = novo_dap))
valores_previstos_m30_glm_ext <- exp(predict(mod30_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m30_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m30_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m30_ext, color = "[M30]", linetype = "[M30]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M30]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M30]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico
m30_glm_ext

# ---------------- hnp - [M30] --------------- #
Grap30 = hnp(mod30, how.many.out = T, resid.type = "deviance")
G30 <- with(Grap30, data.frame(x, lower, upper, median, residuals))
G30$cores = hnp_cores(G30)
G30_texto = hnp_texto(G30) 
GM30 <- ggplot(data = G30, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.25, label = G30_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.27, label = G30_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M30]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M30]: GLM --------------- #
Grap30_M30_GLM=hnp(mod30_glm, how.many.out = T)
G30_glm <- with(Grap30_M30_GLM, data.frame(x, lower, upper, median, residuals))
G30_glm$cores = hnp_cores(G30_glm)
G30_glm_texto = hnp_texto(G30_glm) 
GM30_GLM <- ggplot(data = G30_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G30_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G30_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 
################################################################
############################ [M31] #############################
################################################################
mod31 <- lm(log(h) ~ log(I((dap)^2)), Dados2); summary(mod31)
valores_previstos_m31 <- exp(predict(mod31))
# ----------------- GLM ------------------ #
mod31_glm <- glm(h ~ log(I((dap)^2)), 
                 family = gaussian(link = "log"),
                 data = Dados2); summary(mod31_glm)
valores_previstos_m31_glm <- exp(predict(mod31_glm))
# ---------------- Gráfico --------------- #
m31_glm = ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0,20), ylim = c(0,30)) +
  geom_line(aes(y = valores_previstos_m31_glm, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(y = valores_previstos_m31, color = "[M31]", linetype = "[M31]"), 
            linewidth = 1.4) +
  #ggtitle("(G)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("Data" = "black", "GLM" = "red", 
                                "[M31]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M31]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M31 e o GLM no novo intervalo
valores_previstos_m31_ext <- exp(predict(mod31, newdata = novo_dap))
valores_previstos_m31_glm_ext <- exp(predict(mod31_glm, newdata = novo_dap))

# ---------------- Gráfico --------------- #
m31_glm_ext = ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m31_glm_ext, color = "GLM", linetype = "GLM"), 
            linewidth = 1.8) +
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m31_ext, color = "[M31]", linetype = "[M31]"), 
            linewidth = 1.4) +
  labs(x = "Diameter (cm)", y = "Height (m)", color = "Cases", linetype = "Cases") +
  scale_color_manual(values = c("GLM" = "red", "[M31]" = "black")) +
  scale_linetype_manual(values = c("GLM" = "solid", "[M31]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico
m31_glm_ext

x11()
gridExtra::grid.arrange(m7_glm, m17_glm,
                        m18_glm, m22_glm,
                        m29_glm, m31_glm, ncol = 3)
# ---------------- hnp - [M31] --------------- #
Grap31 = hnp(mod31, how.many.out = T, resid.type = "deviance")
G31 <- with(Grap31, data.frame(x, lower, upper, median, residuals))
G31$cores = hnp_cores(G31)
G31_texto = hnp_texto(G31) 
GM31 <- ggplot(data = G31, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 0.18, label = G31_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 0.22, label = G31_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,0.3))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("[M31]") +
  theme_bw() +
  theme_hnp() 
# ---------------- hnp - [M31]: GLM --------------- #
Grap31_M31_GLM=hnp(mod31_glm, how.many.out = T)
G31_glm <- with(Grap31_M31_GLM, data.frame(x, lower, upper, median, residuals))
G31_glm$cores = hnp_cores(G31_glm)
G31_glm_texto = hnp_texto(G31_glm) 
GM31_GLM <- ggplot(data = G31_glm, aes(x)) +
  geom_point(aes(y = residuals, color = cores), show.legend = FALSE) +
  geom_text(x = 0, y = 4, label = G31_glm_texto$Total, hjust = 0, size=7) +
  geom_text(x = 0, y = 4.37, label = G31_glm_texto$Fora, hjust = 0, size=7) +
  coord_cartesian(ylim=c(0,5))+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.5)+
  geom_line(aes(y = median), linetype = 2, col="black") + 
  scale_color_manual(values = c(Dentro = "black", Fora = "red")) +
  ylab("Deviance residuals") +
  xlab("Half-normal scores")  +
  #ggtitle("GLM") +
  theme_bw() +
  theme_hnp() 

x11()
gridExtra::grid.arrange(GM7, GM17,
                        GM18, GM22,
                        GM29, GM30,
                        GM7_GLM, GM17_GLM,
                        GM18_GLM, GM22_GLM,
                        GM29_GLM, GM30_GLM, ncol = 6)
gridExtra::grid.arrange(GM31,GM31_GLM, ncol = 2)
################################################################
################## Group 4 - Nonlinear models ##################
################################################################
################################################################
############################ [M5] ##############################
################################################################
meyer_model <- function(dap, beta_0, beta_1) {
  return (beta_0 * (1 - exp(beta_1 * dap)) + 1.30)
}

mod5 <- nls(h ~ meyer_model(dap, beta_0, beta_1), 
            start = list(beta_0 = 16, beta_1 = -0.2), 
            Dados2); summary(mod5)

valores_previstos_m5 <- predict(mod5)
valor_assintota_m5 <- coef(mod5)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m5, color = "[M5]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m5, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m5, 
                label = paste0("y = ", round(valor_assintota_m5, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(A)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M5]" = "black")) +
  scale_linetype_manual(values = c("[M5]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M5 no novo intervalo usando a função definida
valores_previstos_m5_ext <- predict(mod5, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m5_ext, color = "[M5]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m5, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m5, 
                label = paste0("y = ", round(valor_assintota_m5, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M5]" = "black")) +
  scale_linetype_manual(values = c("[M5]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M10] #############################
################################################################
logistic_model <- function(dap, beta_0, beta_1, beta_2) {
  return (beta_0 / (1 + beta_1 * exp(-beta_2 * dap)))
}
mod10 <- nls(h ~ logistic_model(dap, beta_0, beta_1, beta_2), 
             start = list(beta_0 = 20, beta_1 = 57, 
                          beta_2 = 0.6), 
             Dados2); summary(mod10)
valores_previstos_m10 <- predict(mod10)
valor_assintota_m10 <- coef(mod10)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m10, color = "[M10]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m10, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m10, 
                label = paste0("y = ", round(valor_assintota_m10, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(B)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M10]" = "black")) +
  scale_linetype_manual(values = c("[M10]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M10 no novo intervalo usando a função definida
valores_previstos_m10_ext <- predict(mod10, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m10_ext, color = "[M10]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m10, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m10, 
                label = paste0("y = ", round(valor_assintota_m10, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M10]" = "black")) +
  scale_linetype_manual(values = c("[M10]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M11] #############################
################################################################
gompertz_model <- function(dap, beta_0, beta_1, beta_2) {
  return (beta_0 * exp(-exp(beta_1 - beta_2 * dap)))
}
mod11 <- nls(h ~ gompertz_model(dap, beta_0, beta_1, beta_2), 
             start = list(beta_0 = 19, beta_1 = 4.4, 
                          beta_2 = 0.8), 
             Dados2); summary(mod11)
valores_previstos_m11 <- predict(mod11)
valor_assintota_m11 <- coef(mod11)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m11, color = "[M11]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m11, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m11, 
                label = paste0("y = ", round(valor_assintota_m11, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(C)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M11]" = "black")) +
  scale_linetype_manual(values = c("[M11]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M11 no novo intervalo usando a função definida
valores_previstos_m11_ext <- predict(mod11, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m11_ext, color = "[M11]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m11, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m11, 
                label = paste0("y = ", round(valor_assintota_m11, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M11]" = "black")) +
  scale_linetype_manual(values = c("[M11]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M12] #############################
################################################################
mitscherlich_model <- function(dap, beta_0, beta_1) {
  return (beta_0 * (1 - exp(-beta_1 * dap)) + 1.30)
}
mod12 <- nls(h ~ mitscherlich_model(dap, beta_0, beta_1), 
             start = list(beta_0 = 16, beta_1 = 0.2), 
             Dados2); summary(mod12)
valores_previstos_m12 <- predict(mod12)
valor_assintota_m12 <- coef(mod12)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m12, color = "[M12]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m12, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m12, 
                label = paste0("y = ", round(valor_assintota_m12, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(D)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M12]" = "black")) +
  scale_linetype_manual(values = c("[M12]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M12 no novo intervalo usando a função definida
valores_previstos_m12_ext <- predict(mod12, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m12_ext, color = "[M12]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m12, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m12, 
                label = paste0("y = ", round(valor_assintota_m12, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M12]" = "black")) +
  scale_linetype_manual(values = c("[M12]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M13] #############################
################################################################
von_Bertalanffy_model <- function(dap, beta_0, beta_1, beta_2) {
  return (beta_0 * (1 - beta_1 * exp(-beta_2 * dap)))
}
mod13 <- nls(h ~ von_Bertalanffy_model(dap, beta_0, beta_1, beta_2), 
             start = list(beta_0 = 19, beta_1 = 5.1, beta_2 = 0.4), 
             Dados2); summary(mod13)
valores_previstos_m13 <- predict(mod13)
valor_assintota_m13 <- coef(mod13)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m13, color = "[M13]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m13, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m13, 
                label = paste0("y = ", round(valor_assintota_m13, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(E)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M13]" = "black")) +
  scale_linetype_manual(values = c("[M13]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M13 no novo intervalo usando a função definida
valores_previstos_m13_ext <- predict(mod13, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m13_ext, color = "[M13]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m13, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m13, 
                label = paste0("y = ", round(valor_assintota_m13, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M13]" = "black")) +
  scale_linetype_manual(values = c("[M13]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M14] #############################
################################################################
richards_model <- function(dap, beta_0, beta_1, beta_2, beta_3) {
  return ((beta_0*(1 + exp(beta_1 - beta_2 * dap))^(-(1/beta_3))))
}
mod14 <- nlsLM(h ~ richards_model(dap, beta_0, beta_1, beta_2, beta_3), 
               start = list(beta_0 = 21.6, beta_1 = 1.4, beta_2 = 0.2, beta_3 = 1.4), 
               Dados2); summary(mod14)
valores_previstos_m14 <- predict(mod14)
valor_assintota_m14 <- coef(mod14)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m14, color = "[M14]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m14, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m14, 
                label = paste0("y = ", round(valor_assintota_m14, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(F)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M14]" = "black")) +
  scale_linetype_manual(values = c("[M14]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M14 no novo intervalo usando a função definida
valores_previstos_m14_ext <- predict(mod14, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m14_ext, color = "[M14]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m14, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m14, 
                label = paste0("y = ", round(valor_assintota_m14, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M14]" = "black")) +
  scale_linetype_manual(values = c("[M14]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M15] #############################
################################################################
ChapmanRichards_model <- function(dap, beta_0, beta_1, beta_2) {
  return (beta_0 * (1 - exp(beta_1 * dap))^ (1 / (1 - beta_2)))
}
mod15 <- nls(h ~ ChapmanRichards_model(dap, beta_0, beta_1, beta_2), 
             start = list(beta_0 = 24.5, beta_1 = -0.2, beta_2 = 0.7), 
             Dados2); summary(mod15)
valores_previstos_m15 <- predict(mod15)
valor_assintota_m15 <- coef(mod15)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m15, color = "[M15]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m15, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m15, 
                label = paste0("y = ", round(valor_assintota_m15, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(G)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M15]" = "black")) +
  scale_linetype_manual(values = c("[M15]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M15 no novo intervalo usando a função definida
valores_previstos_m15_ext <- predict(mod15, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m15_ext, color = "[M15]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m15, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m15, 
                label = paste0("y = ", round(valor_assintota_m15, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M15]" = "black")) +
  scale_linetype_manual(values = c("[M15]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M20] #############################
################################################################
weibull_model <- function(dap, beta_0, beta_1, beta_2) {
  return (beta_0 * (1 - exp(-beta_1 * dap ^ beta_2)))
}

mod20 <- nls(h ~ weibull_model(dap, beta_0, beta_1, beta_2), 
             start = list(beta_0 = 18, beta_1 = 0.7, beta_2 = 0.5), 
             Dados2); summary(mod20)
valores_previstos_m20 <- predict(mod20)
valor_assintota_m20 <- coef(mod20)[1]
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m20, color = "[M20]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m20, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m20, 
                label = paste0("y = ", round(valor_assintota_m20, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  #ggtitle("(H)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M20]" = "black")) +
  scale_linetype_manual(values = c("[M20]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M20 no novo intervalo usando a função definida
valores_previstos_m20_ext <- predict(mod20, newdata = novo_dap)

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m20_ext, color = "[M20]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m20, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m20, 
                label = paste0("y = ", round(valor_assintota_m20, 2))), 
            vjust = -0.5, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M20]" = "black")) +
  scale_linetype_manual(values = c("[M20]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico

################################################################
############################ [M21] #############################
################################################################
baileyClutter_model <- function(dap, beta_0, beta_1, beta_2) {
  return (beta_0 - beta_1 * (1 / dap) ^ beta_2)
}
# ---------------- Gráfico --------------- #
mod21 <- nls(log(h) ~ baileyClutter_model(dap, beta_0, beta_1, beta_2), 
             start = list(beta_0 = 20, beta_1 = 19, beta_2 = 0.6), 
             Dados2); summary(mod21)
valores_previstos_m21 <- exp(predict(mod21))
valor_assintota_m21 <- exp(coef(mod21)[1])
# ---------------- Gráfico --------------- #
ggplot(data = Dados2, aes(x = dap, y = h)) +
  geom_point(color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 30)) +
  geom_line(aes(x = dap, y = valores_previstos_m21, color = "[M21]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m21, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m21, 
                label = paste0("y = ", round(valor_assintota_m21, 2))), 
            vjust = 1.3, hjust = 0, size = 7, color = "black") +
  #ggtitle("(I)") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M21]" = "black")) +
  scale_linetype_manual(values = c("[M21]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Gerando valores de dap até 100 para a predição
novo_dap <- data.frame(dap = seq(1, 100, length.out = 500))

# Predição para o modelo M21 no novo intervalo usando a função definida
valores_previstos_m21_ext <- exp(predict(mod21, newdata = novo_dap))

# ---------------- Gráfico --------------- #
ggplot() +
  geom_point(data = Dados2, aes(x = dap, y = h), color = "black", shape = 19, size = 2) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 30)) +  # Estendendo até x = 100
  geom_line(aes(x = novo_dap$dap, y = valores_previstos_m21_ext, color = "[M21]"), 
            linewidth = 1.8) +
  geom_hline(yintercept = valor_assintota_m21, 
             linetype = 1, color = "black", linewidth = 1) +
  geom_text(aes(x = -1, y = valor_assintota_m21, 
                label = paste0("y = ", round(valor_assintota_m21, 2))), 
            vjust = 1.3, hjust = 0, size = 7, color = "black") +
  labs(x = "Diameter (cm)", y = "Height (m)", color = " ") +
  scale_color_manual(values = c("[M21]" = "black")) +
  scale_linetype_manual(values = c("[M21]" = "solid")) +
  theme_bw() +
  theme(legend.title = element_text(size = 21),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 31),
        axis.text.x = element_text(color = "black", hjust=1),
        axis.text.y = element_text(color = "black", hjust=1),
        axis.text = element_text(size = 31),
        plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 15))

# Exibindo o gráfico


x11()
gridExtra::grid.arrange(m5, m10,
                        m11, m15,
                        m20, m21, ncol = 3)
