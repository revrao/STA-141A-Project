knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(ROCR)
setwd("/Users/revanthrao/Desktop/UC Davis/STA 141A/Data")
session=list()
for(i in 1:18){
  session[[i]]=readRDS(paste('./session',i,'.rds',sep=''))
}
kbl(x = summary(session[[1]]), format = "html", digits = 0, table.attr = "class='table table-hover'",
    caption = "<center><h1>Summary of Session 1</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)
num_sessions=length(session)
session_info = data.frame(
  name = rep('name',num_sessions),
  date =rep('dt',num_sessions),
  num_areas = rep(0,num_sessions),
  num_neurons = rep(0,num_sessions),
  num_trials = rep(0,num_sessions),
  success_rate = rep(0,num_sessions)
)
for(i in 1:num_sessions){
  temp_data = session[[i]];
  
  session_info[i,1]=temp_data$mouse_name;
  session_info[i,2]=temp_data$date_exp;
  session_info[i,3]=length(unique(temp_data$brain_area));
  session_info[i,4]=dim(temp_data$spks[[1]])[1];
  session_info[i,5]=length(temp_data$feedback_type);
  session_info[i,6]=mean((temp_data$feedback_type+1)/2);
}
session_info$Session = c(1:18)
session_info = session_info[,c(7, 1:6)]

colnames(session_info) = c("Session", "Mouse Name", "Experiment Date", "Number of Brain Areas",
                           "Number of Neurons", "Number of Trials", "Success Rate")
kbl(x = session_info, format = "html", digits = 3, table.attr = "class='table table-hover'",
    caption = "<center><h1>Summary of Sessions 1-18</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)
spk_avg_by_area = function(num_trial,session_name){
  spk.trial = session_name$spks[[num_trial]]
  area = session_name$brain_area
  spk.count = apply(spk.trial,1,sum)
  spk.avg.area = tapply(spk.count, area, mean)
  return(spk.avg.area)
}
trial_summaries_by_area = list()
for (i in 1:18) {
  n.trial=length(session[[i]]$feedback_type)
  n.area=length(unique(session[[i]]$brain_area))
  trial.summary = matrix(nrow=n.trial,ncol= n.area+1+2+1)
  for(num_trial in 1:n.trial){
    trial.summary[num_trial,]=c(spk_avg_by_area(num_trial,session_name = session[[i]]),
                                session[[i]]$feedback_type[num_trial],
                                session[[i]]$contrast_left[num_trial],
                                session[[i]]$contrast_right[i],
                                num_trial)
    colnames(trial.summary)=c(names(spk_avg_by_area(num_trial, session_name = session[[i]])), 
                              "feedback", "left contr.","right contr.","id")
    trial.summary = as.data.frame(trial.summary)  
  }
  trial.summary$session_id = rep.int(session_info$Session[i], times = n.trial)
  trial_summaries_by_area[[i]] = trial.summary
}
trial_summaries_by_area_long = list()
for (i in 1:num_sessions) {
  values = trial_summaries_by_area[[i]] %>%
    pivot_longer(cols = -c(id, feedback, `left contr.`, `right contr.`, session_id),
                 names_to = "Area", values_to = "Mean") %>%
    arrange(Area) %>%
    select(session_id, id, Area, Mean, feedback)
  trial_summaries_by_area_long[[i]] = data.frame(values)
}

all_values_by_area = data.frame()
for (i in 1:num_sessions) {
  all_values_by_area = rbind(all_values_by_area, trial_summaries_by_area_long[[i]])
}

all_values_by_area_long = all_values_by_area %>%
  group_by(Area, session_id, feedback) %>%
  summarise(mean_spikes = mean(Mean))
colnames(all_values_by_area_long) = c("area", "session_id", "feedback", "mean_spikes")

kable_all_values_by_area_long = all_values_by_area_long
colnames(kable_all_values_by_area_long) = c("Brain Area", "Session Number", "Feedback Type", "Mean Spikes")

kbl(x = head(kable_all_values_by_area_long, n = 15), format = "html", digits = 3, table.attr = "class='table table-hover'",
    caption = "<center><h1>Summary of Area Means by Feedback and Session</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)
plot_6_data = trial_summaries_by_area_long[[6]]
plot_6_data$feedback = ifelse(plot_6_data$feedback == 1, "Success", "Failure")
ggplot(plot_6_data, aes(x = Area, y = Mean, color = as.factor(feedback))) + 
  geom_boxplot(position = "dodge") + 
  facet_wrap(~as.factor(feedback)) +
  labs(title = "Mean Spikes by Feedback and Area for Session 6",
       x = "Area",
       y = "Mean Spikes",
       color = "Feedback Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 0.5))
plot_7_data = trial_summaries_by_area_long[[7]]
plot_7_data$feedback = ifelse(plot_7_data$feedback == 1, "Success", "Failure")
ggplot(plot_7_data, aes(x = Area, y = Mean, color = as.factor(feedback))) + 
  geom_boxplot(position = "dodge") + 
  facet_wrap(~as.factor(feedback)) +
  labs(title = "Mean Spikes by Feedback and Area for Session 7",
       x = "Area",
       y = "Mean Spikes",
       color = "Feedback Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 0.5))
mean_bar_graphs_by_area = list()
brain_areas = unique(all_values_by_area_long$area)
for (i in brain_areas) {
  data_area_i = all_values_by_area_long %>%
    filter(area == i)
  area_i_plot = ggplot(data = data_area_i,
                       aes(x = factor(session_id), y = mean_spikes, fill = factor(feedback))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Session Number", y = "Mean Number of Spikes",
         title = paste("Mean Spikes for Brain Area", i, sep = " "),
         fill = "Trial Outcome") +
    scale_fill_manual(values = c("red4", "green4"), labels = c("Failure", "Success")) 
  mean_bar_graphs_by_area[[i]] = area_i_plot
} 
mean_bar_graphs_by_area[["CA3"]]
mean_bar_graphs_by_area[["MOs"]]
mean_bar_graphs_by_area[["MRN"]]
differences = all_values_by_area_long %>%
  group_by(area, session_id) %>%
  summarise(mean_spikes_difference = sum(feedback*mean_spikes))

num_success_over_failure = 0
for (i in 1:length(differences$mean_spikes_difference)) {
  if (differences$mean_spikes_difference[i] > 0) {
    num_success_over_failure = num_success_over_failure + 1
  }
}

proportion = num_success_over_failure / length(differences$mean_spikes_difference)

x = unique(session[[1]]$brain_area)
y = unique(session[[2]]$brain_area)
z = unique(session[[3]]$brain_area)
w = c(x, y, z)

t_test = t.test(mean_spikes ~ as.factor(feedback), data = all_values_by_area_long)
kw_test_brain_area = kruskal.test(Mean ~ as.factor(Area), data = all_values_by_area)
trial_data = list()
for (i in 1:num_sessions){
  df_i = data.frame(name = rep(session_info$`Mouse Name`[i], session_info$`Number of Trials`[i]),
                    session_id = rep(i, session_info$`Number of Trials`[i]),
                    trial_id = 1:session_info$`Number of Trials`[i],
                    left_contrast = session[[i]]$contrast_left,
                    right_contrast = session[[i]]$contrast_right,
                    total_spikes = rep(0, session_info$`Number of Trials`[i]),
                    mean_spikes_all_neurons = rep(0, session_info$`Number of Trials`[i]),
                    proportion_active_neurons = rep(0, session_info$`Number of Trials`[i]),
                    mean_spikes_active_neurons = rep(0, session_info$`Number of Trials`[i]),
                    number_brain_areas = rep(session_info$`Number of Brain Areas`[i],
                                             session_info$`Number of Trials`[i]),
                    feedback_type = session[[i]]$feedback_type,
                    feedback_binary = rep(0, session_info$`Number of Trials`[i]))
  for (j in 1:session_info$`Number of Trials`[i]) {
    spks.trial=session[[i]]$spks[[j]]
    total.spikes = apply(spks.trial,1,sum)
    sum.spikes = sum(total.spikes)
    avg.spikes=mean(total.spikes)
    prop_active_neurons = mean(total.spikes > 0)
    active_neurons_mean = mean(total.spikes[total.spikes>0])
    success_binary = ifelse(session[[i]]$feedback_type[[j]] == 1, 1, 0)
    df_i$total_spikes[j] = sum.spikes
    df_i$mean_spikes_all_neurons[j] = avg.spikes
    df_i$proportion_active_neurons[j] = prop_active_neurons
    df_i$mean_spikes_active_neurons[j] = active_neurons_mean
    df_i$feedback_binary[j] = success_binary
  }
  trial_data[[i]] = df_i
}
full_data = data.frame()
for (i in 1:num_sessions) {
  full_data = rbind(full_data, trial_data[[i]])
}

kable_data = full_data
colnames(kable_data) = c("Mouse Name", "Session", "Trial", "Left Contrast", "Right Contrast", "Total Spikes",
                         "Mean Spikes for All Neurons", "Proportion of Active Neurons",
                         "Mean Spikes for Active Neurons", "Number of Brain Areas", "Feedback Type", 
                         "Feedback Type Binary")

kbl(x = head(kable_data, n = 15), format = "html", digits = 3, table.attr = "class='table table-hover'",
    caption = "<center><h1>Summary of Trials for all Sessions</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)
kw_test_mouse = kruskal.test(mean_spikes_all_neurons ~ name, data = full_data)
cori_data = full_data %>%
  filter(name == "Cori") %>%
  select(-c(name, session_id, trial_id, total_spikes, number_brain_areas, feedback_type))

lederberg_data = full_data %>%
  filter(name == "Lederberg") %>%
  select(-c(name, session_id, trial_id, total_spikes, number_brain_areas, feedback_type))
model_data = full_data %>%
  select(-c(name, session_id, trial_id, total_spikes, proportion_active_neurons, 
            mean_spikes_active_neurons, number_brain_areas, feedback_type))

num_observations = length(full_data$name)
set.seed(1)
sample = sample.int(n = num_observations, size = floor(.8 * num_observations), replace = FALSE)
training_data = model_data[sample, ]
test_data = model_data[-sample, ]
model1 = glm(feedback_binary ~ ., data = training_data, family = "binomial")

pred = predict(model1, newdata = test_data %>% select(-feedback_binary), type = 'response')
predictions = ifelse(pred > 0.5, 1, 0)
error = mean(predictions != test_data$feedback_binary)
succ_rate_lm1 = 1-error
total_success_rate = sum(full_data$feedback_binary) / length(full_data$feedback_binary)
library(glmnet)
cv_glm_model_lasso = cv.glmnet(as.matrix(training_data[, -4]), training_data$feedback_binary,
                               alpha = 1, family = "binomial")
cv_prediction_lasso = predict(cv_glm_model_lasso, newx = as.matrix(test_data[, -4]), s = "lambda.min")

predictions_cv_glm_lasso = ifelse(cv_prediction_lasso > 0.5, 1, 0)
error_cv_glm_lasso = mean(predictions_cv_glm_lasso != test_data$feedback_binary)
succ_rate_lasso = 1-error_cv_glm_lasso

cv_glm_model_ridge = cv.glmnet(as.matrix(training_data[, -4]), training_data$feedback_binary,
                               alpha = 0, family = "binomial")
cv_prediction_ridge = predict(cv_glm_model_ridge, newx = as.matrix(test_data[, -4]), s = "lambda.min")

predictions_cv_glm_ridge_0.5 = ifelse(cv_prediction_ridge > 0.5, 1, 0)
error_cv_glm_ridge_0.5 = mean(predictions_cv_glm_ridge_0.5 != test_data$feedback_binary)
succ_rate_ridge_0.5 = 1-error_cv_glm_ridge_0.5

predictions_cv_glm_ridge_0.4 = ifelse(cv_prediction_ridge > 0.4, 1, 0)
error_cv_glm_ridge_0.4 = mean(predictions_cv_glm_ridge_0.4 != test_data$feedback_binary)
succ_rate_ridge_0.4 = 1 - error_cv_glm_ridge_0.4
succ_rate_diff = -(succ_rate_ridge_0.5 - succ_rate_ridge_0.4)
pr = prediction(pred, test_data$feedback_binary)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]

pr1 = prediction(cv_prediction_lasso, test_data$feedback_binary)
prf2 = performance(pr1, measure = "tpr", x.measure = "fpr")
auc2 = performance(pr1, measure = "auc")
auc2 = auc2@y.values[[1]]

pr2 = prediction(cv_prediction_ridge, test_data$feedback_binary)
prf3 = performance(pr2, measure = "tpr", x.measure = "fpr")
auc3 = performance(pr2, measure = "auc")
auc3 = auc3@y.values[[1]]

plot(prf2, col = 'red', main = 'ROC Curves')
plot(prf, add = TRUE, col = 'blue')
plot(prf3, add = TRUE, col = 'green')
abline(a=0, b=1, col='black', lty=3)

legend("bottomright", legend=c("Logistic", "Lasso", "Ridge", "Bias Line"), 
       col=c("blue", "red", 'green','black'), lty=c(1, 1, 1, 3), cex=0.8)
model_data2 = full_data %>%
  select(-c(name, session_id, trial_id, total_spikes, mean_spikes_active_neurons,
            number_brain_areas, feedback_type))
set.seed(1)
training_data2 = model_data2[sample, ]
test_data2 = model_data2[-sample, ]
lm_2 = glm(feedback_binary ~ ., data = training_data2, family = "binomial")
lm_3 = glm(feedback_binary ~ left_contrast + right_contrast + proportion_active_neurons,
           data = training_data2, family = "binomial")


pred_lm2 = predict(lm_2, newdata = test_data2 %>% select(-feedback_binary), type = 'response')
predictions_lm2 = ifelse(pred_lm2 > 0.5, 1, 0)
error_lm2 = mean(predictions_lm2 != test_data2$feedback_binary)
succ_rate_lm_2 = 1 - error_lm2

pred_lm3 = predict(lm_3, newdata = test_data2 %>% select(-feedback_binary), type = 'response')
predictions_lm3 = ifelse(pred_lm3 > 0.6, 1, 0)
error_lm3 = mean(predictions_lm3 != test_data2$feedback_binary)
succ_rate_lm_3 = 1 - error_lm3
pr3 = prediction(pred_lm2, test_data2$feedback_binary)
prf4 = performance(pr3, measure = "tpr", x.measure = "fpr")
auc4 = performance(pr3, measure = "auc")
auc4 = auc4@y.values[[1]]

pr4 = prediction(pred_lm3, test_data2$feedback_binary)
prf5 = performance(pr4, measure = "tpr", x.measure = "fpr")
auc5 = performance(pr4, measure = "auc")
auc5 = auc5@y.values[[1]]

plot(prf4, col = 'red', main = 'ROC Curves')
plot(prf, add = TRUE, col = 'blue')
plot(prf5, add = TRUE, col = 'orange1')
abline(a=0, b=1, col='black', lty=3)

legend("bottomright", legend=c("Benchmark", "Logistic Model 2", "Logistic Model 3", "Bias Line"),
       col=c("blue", "red", 'orange1','black'), lty=c(1, 1, 1, 3), cex=0.8)
model_data3 = full_data %>%
  select(-c(name, session_id, trial_id, total_spikes, number_brain_areas, feedback_type))
set.seed(1)
training_data3 = model_data3[sample, ]
test_data3 = model_data3[-sample, ]

lm_4 = glm(feedback_binary ~ ., data = training_data3, family = "binomial")
lm_5 = glm(feedback_binary ~ left_contrast + right_contrast + proportion_active_neurons +
             mean_spikes_active_neurons, data = training_data3, family = "binomial")
lm_6 = glm(feedback_binary ~ left_contrast + right_contrast + mean_spikes_all_neurons +
             mean_spikes_active_neurons, data = training_data3, family = "binomial")
lm_7 = glm(feedback_binary ~ left_contrast + right_contrast + mean_spikes_active_neurons,
           data = training_data3, family = "binomial")

pred_lm4 = predict(lm_4, newdata = test_data3 %>% select(-feedback_binary), type = 'response')
predictions_lm4 = ifelse(pred_lm4 > 0.5, 1, 0)
error_lm4 = mean(predictions_lm4 != test_data3$feedback_binary)
succ_rate_lm4 = 1 - error_lm4

pred_lm5 = predict(lm_5, newdata = test_data3 %>% select(-feedback_binary), type = 'response')
predictions_lm5 = ifelse(pred_lm5 > 0.5, 1, 0)
error_lm5 = mean(predictions_lm5 != test_data3$feedback_binary)
succ_rate_lm5 = 1 - error_lm5

pred_lm6 = predict(lm_6, newdata = test_data3 %>% select(-feedback_binary), type = 'response')
predictions_lm6 = ifelse(pred_lm6 > 0.5, 1, 0)
error_lm6 = mean(predictions_lm6 != test_data3$feedback_binary)
succ_rate_lm6 = 1 - error_lm6

pred_lm7 = predict(lm_7, newdata = test_data3 %>% select(-feedback_binary), type = 'response')
predictions_lm7 = ifelse(pred_lm7 > 0.5, 1, 0)
error_lm7 = mean(predictions_lm7 != test_data3$feedback_binary)
succ_rate_lm7 = 1 - error_lm7
pr5 = prediction(pred_lm4, test_data3$feedback_binary)
prf6 = performance(pr5, measure = "tpr", x.measure = "fpr")
auc6 = performance(pr5, measure = "auc")
auc6 = auc6@y.values[[1]]

pr6 = prediction(pred_lm5, test_data3$feedback_binary)
prf7 = performance(pr6, measure = "tpr", x.measure = "fpr")
auc7 = performance(pr6, measure = "auc")
auc7 = auc7@y.values[[1]]

pr7 = prediction(pred_lm6, test_data3$feedback_binary)
prf8 = performance(pr7, measure = "tpr", x.measure = "fpr")
auc8 = performance(pr7, measure = "auc")
auc8 = auc8@y.values[[1]]

pr8 = prediction(pred_lm7, test_data3$feedback_binary)
prf9 = performance(pr8, measure = "tpr", x.measure = "fpr")
auc9 = performance(pr8, measure = "auc")
auc9 = auc9@y.values[[1]]

plot(prf6, col = 'red', main = 'ROC Curves')
plot(prf, add = TRUE, col = 'blue')
plot(prf7, add = TRUE, col = 'green')
plot(prf8, add = TRUE, col = 'pink')
plot(prf9, add = TRUE, col = 'darkorchid1')
abline(a=0, b=1, col='black', lty=3)

legend("bottomright", legend=c("Benchmark", "Logistic Model 4", "Logistic Model 5", 
                               "Logistic Model 6", "Logistic Model 7", "Bias Line"), 
       col=c("blue", "red", 'green', 'pink', 'darkorchid1','black'), lty=c(1, 1, 1, 3), cex=0.8)
set.seed(1)
samp_cori = sample.int(n = nrow(cori_data), size = floor(.8 * nrow(cori_data)), replace = FALSE)
train_cori = cori_data[samp_cori, ]
test_cori = cori_data[-samp_cori, ]
model_cori = glm(feedback_binary ~ mean_spikes_active_neurons + mean_spikes_all_neurons + left_contrast + right_contrast,
                 data = train_cori, family = 'binomial')

pred_cori = predict(model_cori, test_cori %>% select(-feedback_binary), type = 'response')
prediction_cori = ifelse(pred_cori > 0.55, 1, 0)
error_cori = mean(prediction_cori != test_cori$feedback_binary)
succ_rate_cori = 1 - error_cori

samp_led = sample.int(n = nrow(lederberg_data), size = floor(.8 * nrow(lederberg_data)), replace = FALSE)
train_led = lederberg_data[samp_led, ]
test_led = lederberg_data[-samp_led, ]

model_led = glm(feedback_binary ~  proportion_active_neurons * mean_spikes_all_neurons + mean_spikes_all_neurons + left_contrast * right_contrast,
                data = train_led, family = 'binomial')
pred_led = predict(model_led, test_led %>% select(-feedback_binary), type = 'response')
prediction_led = ifelse(pred_led > 0.5, 1, 0)
error_led = mean(prediction_led != test_led$feedback_binary)
succ_rate_led = 1 - error_led

pr10 = prediction(pred_cori, test_cori$feedback_binary)
prf10 = performance(pr10, measure = "tpr", x.measure = "fpr")
auc10 = performance(pr10, measure = "auc")
auc10 = auc10@y.values[[1]]

pr11 = prediction(pred_led, test_led$feedback_binary)
prf11 = performance(pr11, measure = "tpr", x.measure = "fpr")
auc11 = performance(pr11, measure = "auc")
auc11 = auc11@y.values[[1]]
plot(prf11, col = 'red', main = 'ROC Curves')
plot(prf, add = TRUE, col = 'blue')
plot(prf10, add = TRUE, col = 'darkorchid1')
abline(a=0, b=1, col='black', lty=3)

legend("bottomright", legend=c("Benchmark", "Lederberg Model", "Cori Model", "Bias Line"), 
       col=c("blue", "red", "darkorchid1", "black"), lty=c(1, 1, 1, 3), cex=0.8)
model_summaries = data.frame(model_type = c("Logistic (Benchmark)", "Logistic (Lasso Penalty)",
                                            "Logistic (Ridge Penalty)", rep("Logistic", 8)),
                             sessions = c(rep("1-18", 9), "1-3", "12-18"),
                             response_var = rep("feedback_binary", 11),
                             covariates = c("mean_spikes_all_neurons, left_contrast, right_contrast",
                                            "mean_spikes_all_neurons, left_contrast, right_contrast",
                                            "mean_spikes_all_neurons, left_contrast, right_contrast",
                                            "mean_spikes_all_neurons, left_contrast, right_contrast, proportion_active_neurons",
                                            "left_contrast, right_contrast, proportion_active_neurons",
                                            "mean_spikes_all_neurons, left_contrast, right_contrast, proportion_active_neurons, mean_spikes_active_neurons",
                                            "left_contrast, right_contrast, proportion_active_neurons, mean_spikes_active_neurons",
                                            "mean_spikes_all_neurons, left_contrast, right_contrast, mean_spikes_active_neurons",
                                            "left_contrast, right_contrast, mean_spikes_active_neurons",
                                            "mean_spikes_active_neurons, mean_spikes_all_neurons, left_contrast, right_contrast",
                                            "proportion_active_neurons * mean_spikes_all_neurons, mean_spikes_all_neurons, left_contrast * right_contrast"),
                             cutoff = c(0.5, 0.5, 0.4, 0.5, 0.6, rep.int(0.5, times = 4), 0.55, 0.5),
                             success_rate = c(succ_rate_lm1, succ_rate_lasso, succ_rate_ridge_0.4, succ_rate_lm_2,
                                              succ_rate_lm_3, succ_rate_lm4, succ_rate_lm5, succ_rate_lm6, succ_rate_lm7,
                                              succ_rate_cori, succ_rate_led))
colnames(model_summaries) = c("Model Type", "Sessions in Data", "Response Variable",
                              "Covariates", "Prediction Cutoff Point", "Prediction Success Rate")

kbl(x = model_summaries, format = "html", digits = 3, table.attr = "class='table table-hover'",
    caption = "<center><h1>Summary of Predictive Models</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)
sess_1 = readRDS('/Users/revanthrao/Desktop/UC Davis/STA 141A/Data/test1.rds')

n_1 = length(sess_1$contrast_right)

df_1 = data.frame(name = rep(sess_1$mouse_name, n_1),
                  session_id = rep(1, n_1),
                  left_contrast = sess_1$contrast_left,
                  right_contrast = sess_1$contrast_right,
                  mean_spikes_all_neurons = rep(0, n_1),
                  proportion_active_neurons = rep(0, n_1),
                  mean_spikes_active_neurons = rep(0, n_1),
                  number_brain_areas = rep(length(unique(sess_1$brain_area)), n_1),
                  feedback_type = sess_1$feedback_type,
                  feedback_binary = rep(0, n_1))
for (i in 1:n_1) {
  success_binary = ifelse(sess_1$feedback_type[[i]] == 1, 1, 0)
  spks.trial = sess_1$spks[[i]]
  total.spikes = apply(spks.trial,1,sum)
  avg.spikes=mean(total.spikes)
  prop_active_neurons = mean(total.spikes > 0)
  active_neurons_mean = mean(total.spikes[total.spikes>0])
  df_1$mean_spikes_all_neurons[i] = avg.spikes
  df_1$proportion_active_neurons[i] = prop_active_neurons
  df_1$mean_spikes_active_neurons[i] = active_neurons_mean
  df_1$feedback_binary[i] = success_binary
}

df_1 = df_1 %>%
  select(-feedback_type)

kable_df_1 = df_1
colnames(kable_df_1) = c("Mouse Name", "Session", "Left Contrast", "Right Contrast",
                         "Mean Spikes for All Neurons", "Proportion of Active Neurons",
                         "Mean Spikes for Active Neurons", "Number of Brain Areas",
                         "Feedback Type Binary")
sess_18 = readRDS('/Users/revanthrao/Desktop/UC Davis/STA 141A/Data/test2.rds')

n_18 = length(sess_18$contrast_right)

df_18 = data.frame(name = rep(sess_18$mouse_name, n_18),
                   session_id = rep(18, n_18),
                   left_contrast = sess_18$contrast_left,
                   right_contrast = sess_18$contrast_right,
                   mean_spikes_all_neurons = rep(0, n_18),
                   proportion_active_neurons = rep(0, n_18),
                   mean_spikes_active_neurons = rep(0, n_18),
                   number_brain_areas = rep(length(unique(sess_18$brain_area)), n_18),
                   feedback_type = sess_18$feedback_type,
                   feedback_binary = rep(0, n_18))
for (i in 1:n_18) {
  success_binary = ifelse(sess_18$feedback_type[[i]] == 1, 1, 0)
  spks.trial = sess_18$spks[[i]]
  total.spikes = apply(spks.trial,1,sum)
  avg.spikes=mean(total.spikes)
  prop_active_neurons = mean(total.spikes > 0)
  active_neurons_mean = mean(total.spikes[total.spikes>0])
  df_18$mean_spikes_all_neurons[i] = avg.spikes
  df_18$proportion_active_neurons[i] = prop_active_neurons
  df_18$mean_spikes_active_neurons[i] = active_neurons_mean
  df_18$feedback_binary[i] = success_binary
}

df_18 = df_18 %>%
  select(-feedback_type)

kable_df_18 = df_18
colnames(kable_df_18) = c("Mouse Name", "Session", "Left Contrast", "Right Contrast",
                          "Mean Spikes for All Neurons", "Proportion of Active Neurons",
                          "Mean Spikes for Active Neurons", "Number of Brain Areas",
                          "Feedback Type Binary")
kbl(x = head(kable_df_1, n = 10), format = "html", digits = 3, table.attr = "class='table table-hover'",
    caption = "<center><h1>Test Data from Session 1</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)

kbl(x = head(kable_df_18, n = 10), format = "html", digits = 3, table.attr = "class='table table-hover'",
    caption = "<center><h1>Test Data from Session 18</h1><center>", align = "c") %>%
  kable_styling(fixed_thead = TRUE)
df_1 = df_1 %>%
  select(-c(name, session_id, number_brain_areas))
df_18 = df_18 %>%
  select(-c(name, session_id, number_brain_areas))
set.seed(1)
pred_test_1 = predict(model_cori, df_1 %>% select(-feedback_binary), type = 'response')
prediction_test_1 = ifelse(pred_test_1 > 0.55, 1, 0)
error_test_1 = mean(prediction_test_1 != df_1$feedback_binary)
succ_rate_test_1 = 1 - error_test_1

pred_test_2 = predict(model_led, df_18 %>% select(-feedback_binary), type = 'response')
prediction_test_2 = ifelse(pred_test_2 > 0.5, 1, 0)
error_test_2 = mean(prediction_test_2 != df_18$feedback_binary)
succ_rate_test_2 = 1 - error_test_2
ggplot(data = trial_summaries_by_area[[1]], aes(x = id, y = ACA)) +
  geom_line() + 
  labs(title = "Mean of Spikes by Brain Area ACA for Session 1",
       x = "Trial Number", y = "Mean") + 
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = trial_summaries_by_area[[1]], aes(x = id, y = CA3)) +
  geom_line() + 
  labs(title = "Mean of Spikes by Brain Area CA3 for Session 1",
       x = "Trial Number", y = "Mean") +
  theme(plot.title = element_text(hjust = 0.5))