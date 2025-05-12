library(dplyr)
install.packages("ggplot2")
library(ggplot2)
d_SH <- read.csv("Shanghai.csv", header = TRUE, fileEncoding = "GB2312")
colnames(d_SH)



species_cols <- c("Muskrat.invasive.recognition",
                  "Brown.Rat.invasive.recognition",
                  "Mosquitofish.invasive.recognition",
                  "Rainbow.trout.invasive.recognition",
                  "Smooth.Cordgrass.invasive.recognition",
                  "Common.Water.Hyacinth.invasive.recognition")


for (col in species_cols) {
  binary_col <- paste0(gsub(" ", ".", col), "_binary")  
  d_SH[[binary_col]] <- ifelse(d_SH[[col]] %in% c(
    "I had already identified it as invasive and did not prefer it", 
    "I had already identified it as invasive and still prefer it"), 
    1, 
    ifelse(d_SH[[col]] %in% c(
      "I had not recognized it as invasive but now prefer it less (regardless of initial choice)",
      "I had not recognized it as invasive but now still prefer it"), 0, NA))
}



d_SH$invasive_recognition <- apply(d_SH[, paste0(species_cols, "_binary")], 1, function(x) { 
  if (any(x == 1, na.rm = TRUE)) { 
    return(1) 
  } else if (all(is.na(x))) { 
    return(NA)  
  } else { 
    return(0)  
  }
})

d_SH$Expertise_numeric <- as.numeric(factor(d_SH$`Level.of.Expertise.in.Biological.or.Environmental.Sciences`, 
                                            levels = c("No Higher Education Studies", "Basic understanding", 
                                                       "Moderate expertise", "Advanced expertise"),
                                            labels = c(0, 1, 2, 3)))




d_SH$total_recognized <- rowSums(d_SH[, paste0(species_cols, "_binary")], na.rm = TRUE)




d_SH$Gender <- as.factor(d_SH$Gender) 
d_SH$Education_Level <- as.factor(d_SH$Education.Level) 

table(d_SH$Expertise_numeric)

d_SH$Residence <- as.factor(d_SH$Residence) 

d_SH$Religious.Affiliation <- as.factor(d_SH$Religious.Affiliation)


unique(d_SH$`Annual.Income..SGD.`)
d_SH$Annual_Income_Numeric <- as.numeric(gsub("Less than 20000", "10000", 
                                              gsub("40000 - 59999", "50000", 
                                                   gsub("20000 - 39999", "30000", 
                                                        gsub("60000 - 99999", "80000", d_SH$`Annual.Income..SGD.`)))))
summary(d_SH$Annual_Income_Numeric)


d_SH$Level <- as.factor(d_SH$Level.of.Expertise.in.Biological.or.Environmental.Sciences) 
# 
# glm_model <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6. + Gender +Religious.Affiliation +reside_scaled,
#                   data = d_SH, family = binomial)
# summary(glm_model)


# glm_model <- glm(invasive_recognition ~ Age * Nature.Relatedness.Scale..NRS.6. + Gender, 
#                  family = binomial, data = d_SH)
# summary(glm_model)

# #=======================================================================================
# glm_model <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6. + Gender+Expertise_numeric+ Education_Level, 
#                  family = binomial, data = d_SH)
# summary(glm_model)
# aic_value <- AIC(glm_model)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# # null_model <- glm(invasive_recognition ~ 1, family = binomial, data = d_SH)
# # anova(null_model, glm_model, test = "Chisq")
# 
# 
# 
# glm_model2 <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6., 
#                          family = binomial, data = d_SH)
# summary(glm_model2)
# aic_value <- AIC(glm_model2)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model2)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model2))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# glm_model2.1 <- glm(invasive_recognition ~  Nature.Relatedness.Scale..NRS.6. + Gender, 
#                     family = binomial, data = d_SH)
# summary(glm_model2.1)
# aic_value <- AIC(glm_model2.1)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model2.1)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model2.1))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# 
# 
# glm_model3 <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6., 
#                   family = binomial, data = d_SH)
# summary(glm_model3)
# aic_value <- AIC(glm_model3)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model3)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model3))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# # 
# # glm_model3.1 <- glm(invasive_recognition ~ Age * Nature.Relatedness.Scale..NRS.6., 
# #                   family = binomial, data = d_SH)
# # summary(glm_model3.1)
# 
# glm_model3.1 <- glm(invasive_recognition ~ Nature.Relatedness.Scale..NRS.6., 
#                   family = binomial, data = d_SH)
# summary(glm_model3.1)
# aic_value <- AIC(glm_model3.1)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model3.1)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model3.1))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# 
# #=========================================================================================
# glm_model4 <- glm(invasive_recognition ~Expertise_numeric+Education_Level, 
#                  family = binomial, data = d_SH)
# summary(glm_model4)
# aic_value <- AIC(glm_model4)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model4)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model4))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# 
# glm_model4.1 <- glm(invasive_recognition ~Expertise_numeric, 
#                   family = binomial, data = d_SH)
# summary(glm_model4.1)
# aic_value <- AIC(glm_model4.1)
# cat("AIC:", aic_value, "\n")
# 
# 
# log_likelihood <- logLik(glm_model4.1)
# cat("Log Likelihood:", log_likelihood, "\n")
# 
# 
# num_parameters <- length(coef(glm_model4.1))
# cat("Number of Parameters:", num_parameters, "\n")
# 
# 
# 
# #==========================================================================
#=======================================================================
#===============================================================================
glm_other <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6. + Gender + 
                    Residence + Annual_Income_Numeric, family = binomial, data = d_SH)
summary(glm_other)
aic_value <- AIC(glm_other)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_other)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_other))
cat("Number of Parameters:", num_parameters, "\n")



glm_other.1 <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6., 
                   family = binomial, data = d_SH)
summary(glm_other.1)
aic_value <- AIC(glm_other.1)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_other.1)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_other.1))
cat("Number of Parameters:", num_parameters, "\n")



glm_other.2 <- glm(invasive_recognition ~ Nature.Relatedness.Scale..NRS.6., 
                   family = binomial, data = d_SH)
summary(glm_other.2)
aic_value <- AIC(glm_other.2)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_other.2)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_other.2))
cat("Number of Parameters:", num_parameters, "\n")



glm_other.3 <- glm(invasive_recognition ~ Age, 
                   family = binomial, data = d_SH)
summary(glm_other.3)
aic_value <- AIC(glm_other.3)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_other.3)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_other.3))
cat("Number of Parameters:", num_parameters, "\n")


#======================================================================
glm_edu <- glm(invasive_recognition ~ Expertise_numeric+ Education_Level, 
family = binomial, data = d_SH)

summary(glm_edu)
aic_value <- AIC(glm_edu)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_edu)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_edu))
cat("Number of Parameters:", num_parameters, "\n")






glm_edu.1 <- glm(invasive_recognition ~ Expertise_numeric, 
                 family = binomial, data = d_SH)
summary(glm_edu.1)
aic_value <- AIC(glm_edu.1)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_edu.1)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_edu.1))
cat("Number of Parameters:", num_parameters, "\n")


glm_edu.2 <- glm(invasive_recognition ~ Education_Level, 
                 family = binomial, data = d_SH)
summary(glm_edu.2)
aic_value <- AIC(glm_edu.2)
cat("AIC:", aic_value, "\n")


log_likelihood <- logLik(glm_edu.2)
cat("Log Likelihood:", log_likelihood, "\n")

num_parameters <- length(coef(glm_edu.2))
cat("Number of Parameters:", num_parameters, "\n")



#======================================================================
model_names <- c("glm_other", "glm_other.1", "glm_other.2", "glm_other.3", "glm_edu", "glm_edu.1", "glm_edu.2")
aic_values <- c(50.91419, 88.76157, 106.8594, 129.154, 121.6793, 133.2741, 121.6793)
min_aic <- min(aic_values)

delta_aics <- aic_values - min_aic


wi <- exp(-0.5 * delta_aics) / sum(exp(-0.5 * delta_aics))


for (i in seq_along(delta_aics)) {
  cat(sprintf("Model: %s, Delta AIC: %.4f, Akaike Weight (Wi): %.4f\n", model_names[i], delta_aics[i], wi[i]))
}




#====================================================================
#-------------------------------------------------------------------------

library(ggplot2)
library(dplyr)


d_SH_clean <- na.omit(d_SH[, c("invasive_recognition", "Age", "Nature.Relatedness.Scale..NRS.6.", 
                               "Gender", "Annual_Income_Numeric")])
d_SH_clean$invasive_recognition <- as.factor(d_SH_clean$invasive_recognition)


glm_other <- glm(invasive_recognition ~ Age + Nature.Relatedness.Scale..NRS.6. + Gender + 
                   Annual_Income_Numeric, family = binomial, data = d_SH_clean)


d_SH_clean$predicted_prob <- predict(glm_other, type = "response")
d_SH_clean <- d_SH_clean %>% arrange(predicted_prob)
d_SH_clean$Index <- 1:nrow(d_SH_clean)


ggplot(d_SH_clean, aes(x = Index, y = predicted_prob, color = invasive_recognition)) +
  geom_point(size = 1.5, alpha = 0.5) +
  labs(
    title = "Predicted Probability of Recognizing Invasive Species",
    x = "Index",
    y = "Predicted probability of knowing invasive species",
    color = "Recognition"
  ) +
  scale_color_manual(values = c("cyan", "skyblue3"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1), xlim = c(0, 180))



ggplot(d_SH_clean, aes(x = Nature.Relatedness.Scale..NRS.6., y = predicted_prob, color = invasive_recognition)) +
  geom_point(size = 1.5, alpha = 0.5) +
  labs(
    title = "Relationship between Nature Relatedness Scale and \nPredicted Probability of Recognizing Invasive Species",
    x = "Nature Relatedness Scale (NRS.6.)",
    y = "Predicted probability of knowing invasive species",
    color = "Recognition"
  ) +
  scale_color_manual(values = c("cyan", "skyblue3"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 1))

