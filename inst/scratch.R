
data("diabetic_data")

# map
data(ids_map)
diabetic_data <- data_ids_map(diabetic_data, ids_map)

# missing
# map NA
mark_list <- c("?", "unknown", "invalid", "not available", "not mapped",
              "null", "none")
diabetic_data <- replace_missing_dataset(diabetic_data, mark_list = mark_list)
# missing
res <- missing_explore(diabetic_data,
                      is_drop = TRUE, upper_pro = 0.95, lower_pro = 0.04,
                      is_fill = TRUE, fill_mark_fac = "others",
                      fill_cre_num = "median")
print(res$missing_table)
plot(res$pattern_plot)
diabetic_data <- res$newdata

# outlier
diabetic_data <- outlier_explore(diabetic_data, outlier_prop = .01,
                                 is_drop = FALSE, is_plot = TRUE)
# extreme imbalance
diabetic_data <- extreme_imbalance_col(diabetic_data, extreme_prop = .95,
                                 is_drop = TRUE, is_table = TRUE)
# map with ICD9
data(icd9_map)
diabetic_data <- encode_icd9(x = diabetic_data, data_icd = icd9_map,
                             maplist = c("diag_1", "diag_2", "diag_3"))
# WOE transformation
diabetic_data_woe <- build_woe(diabetic_data, ycol = "readmitted",
                               positive = "YES", is_dropsame = TRUE)
tab <- get_woe_explain(diabetic_data_woe, diabetic_data)
tab

# filter with iv
iv_res <- iv_filter(diabetic_data_woe, iv_limit = .02,
                        ycol = "readmitted", positive = "YES")
diabetic_data_iv <- iv_res$data_iv
iv_info <- iv_res$iv_info


# model
# map the target to 0/1
diabetic_data_woe <- diabetic_data_woe |>
  ycol_mapped01(ycol = "readmitted", positive = "YES")
summary(diabetic_data_woe$readmitted)
# build logistic model
res_logistic <- build_logistic_regression(diabetic_data_woe,
                      ycol = "readmitted", is_kfold = TRUE, cv_num = 5,
                      is_smote = TRUE, seed = 103221, thre = 0.4)
summary(res_logistic$model)
print(res_logistic$eval_tab)
# build rf model
res_rf <- build_random_forest(diabetic_data_woe, ycol = "readmitted",
                                  is_kfold = TRUE, cv_num = 5, is_smote = TRUE,
                                  seed = 103221, thre = 0.2)
print(res_rf$model)
print(res_rf$eval_tab)

# predictor
true_label <- diabetic_data_woe[1:10, "readmitted"]
data <- diabetic_data_woe[1:10,
                    -which(colnames(diabetic_data_woe) %in% c("readmitted"))]
lr_mod <- res_logistic$model
rf_mod <- res_rf$model

pred_lr <- model_predict(data, lr_mod, thre = 0.4, classname = c("YES", "NO"))
pred_rf <- model_predict(data, rf_mod, thre = 0.2, classname = c("YES", "NO"))
data$true_label <- true_label
data$pred_lr <- pred_lr
data$pred_rf <- pred_rf
data[, c("true_label", "pred_lr", "pred_rf")]
