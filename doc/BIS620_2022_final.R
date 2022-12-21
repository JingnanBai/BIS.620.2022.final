## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BIS620.2022.final)

## ----paged.print=TRUE---------------------------------------------------------
data("diabetic_data")
head(diabetic_data, 5)

## -----------------------------------------------------------------------------
data("ids_map")
diabetic_data <- data_ids_map(diabetic_data, ids_map)

## ---- warning = FALSE---------------------------------------------------------
data(icd9_map)
diabetic_data <- encode_icd9(x = diabetic_data,
                             data_icd = icd9_map,
                             maplist = c("diag_1", "diag_2", "diag_3"))

## -----------------------------------------------------------------------------
# map NA
mark_list <- c("?", "unknown", "invalid", "not available", "not mapped",
              "null", "none")
diabetic_data <- replace_missing_dataset(diabetic_data, mark_list = mark_list)

diabetic_ori <- diabetic_data

## -----------------------------------------------------------------------------
# knitr::opts_chunk$set(fig.width=5, fig.height=4) 
res <- missing_explore(diabetic_data)
# show the missing proportion
res$missing_table

## ----fig.height=8, fig.width=8, out.height="60%", out.width="60%"-------------
# show the missing pattern
plot(res$pattern_plot, oma = c(12, 3, 2, 2))

## -----------------------------------------------------------------------------
res <- missing_explore(diabetic_data,
            is_drop = TRUE, upper_pro = 0.94, lower_pro = 0.04,
            is_fill = TRUE, fill_mark_fac = "others", fill_cre_num = "median")
diabetic_data <- res$newdata

## ----fig.height=6, fig.width=6, out.height="50%", out.width="50%"-------------
diabetic_data <- outlier_explore(diabetic_data, outlier_prop = .01,
                                 is_drop = FALSE, is_plot = TRUE)

## ----paged.print=TRUE---------------------------------------------------------
diabetic_data <- extreme_imbalance_col(diabetic_data, extreme_prop = .98,
                                 is_drop = TRUE, is_table = FALSE)
# For concise output, set is_table = FALSE

## -----------------------------------------------------------------------------
# WOE transformation
res_woe <- build_woe(diabetic_data, ycol = "readmitted",
                               positive = "YES", is_dropsame = TRUE)
diabetic_data_woe <- res_woe$newdata

# get the mapping information for explanation later.
tab_explain_woe <- get_woe_explain(diabetic_data_woe, diabetic_data)

## -----------------------------------------------------------------------------
# codes for sample data to show the function which conduct SMOTE, it will be combined with the modelling function later
sample_smote <- upsample_smote(diabetic_data_woe[1:100, ], ycol = "readmitted")

summary(diabetic_data_woe[1:100, "readmitted"])
summary(sample_smote$readmitted)

## ----fig.height=7, fig.width=9, warning=FALSE, out.width="75%", out.height="75%"----
xcol <- c("race", "payer_code", "age", "insulin", "change", "max_glu_serum")
plot_mosaic(diabetic_ori, ycol = "readmitted", xcol = xcol, ncol = 3)

## ----fig.height=8, fig.width=8, out.height="65%", out.width="65%"-------------
xcol_num <- c("time_in_hospital", "num_lab_procedures", "num_medications", 
              "number_inpatient", "number_diagnoses")
plot_comparehist(diabetic_ori, "readmitted", xcol_num, ncol = 2, bins = 10)

## -----------------------------------------------------------------------------
temp_col <- c("age", "number_diagnoses")
tab_explain_woe[tab_explain_woe$variable_name %in% temp_col, ]

## ---- fig.width = 10, fig.height = 8, out.height="70%", out.width="70%"-------
collist <- c("age", "number_diagnoses", "number_inpatient", "number_emergency")
get_woe_plot(res_woe$woe_bins, xcol = collist, ncol = 2)

## -----------------------------------------------------------------------------
# transform the categorical variables to factor
diabetic_data <- format_factor_dataset(diabetic_data)
# map the target to 0/1
diabetic_data_woe <- diabetic_data_woe |>
  ycol_mapped01(ycol = "readmitted", positive = "YES")
summary(diabetic_data_woe$readmitted)

## -----------------------------------------------------------------------------
# build logistic model
res_logistic <- build_logistic_regression(diabetic_data_woe,
                      ycol = "readmitted", is_kfold = TRUE, cv_num = 10,
                      is_smote = TRUE, seed = 103221, auto_thre = TRUE)

## ----fig.height=5, fig.width=5, out.height="50%", out.width="50%"-------------
res_logistic$eval_tab

## -----------------------------------------------------------------------------
plot(res_logistic$roc_res, print.auc=TRUE, auc.polygon=TRUE,
           grid.col=c("green", "red"), max.auc.polygon=TRUE,
           auc.polygon.col="skyblue", print.thres = T)

## -----------------------------------------------------------------------------
res_logistic$best_thre

## ----paged.print=TRUE---------------------------------------------------------
summary(res_logistic$model)

## -----------------------------------------------------------------------------
xcol <- c("diag_1", "insulin", "metformin")
tab_explain_woe[tab_explain_woe$variable_name %in% xcol, ]

## ----rf, warning=FALSE--------------------------------------------------------
# build rf model
res_rf <- build_random_forest(diabetic_data_woe,
                      ycol = "readmitted", is_kfold = TRUE, cv_num = 10,
                      is_smote = TRUE, seed = 103221, 
                      ntree = 100, maxnodes = 50, mtry = 4)

## -----------------------------------------------------------------------------
res_rf$eval_tab

## ----paged.print=TRUE---------------------------------------------------------
impt <- res_rf$model$importance
impt <- impt[order(impt[, "MeanDecreaseGini"], decreasing = TRUE), ] |> 
  data.frame()
colnames(impt) <- c("MeanDecreaseGini")
impt

## ----filter iv----------------------------------------------------------------
# filter with iv
iv_res <- iv_filter(diabetic_data_woe, iv_limit = .01,
                        ycol = "readmitted", positive = 1)
diabetic_data_iv <- iv_res$data_iv
iv_info <- iv_res$iv_info

dim(diabetic_data_iv)

# get IV value for each variables: 
# print(iv_info)

## ----rf for iv----------------------------------------------------------------
# build rf model
res_rf_iv <- build_random_forest(diabetic_data_iv,
                      ycol = "readmitted", is_kfold = TRUE, cv_num = 10,
                      is_smote = TRUE, seed = 103221, 
                      ntree = 100, maxnodes = 50, mtry = 4)

## -----------------------------------------------------------------------------
res_rf_iv$eval_tab

## ----paged.print=TRUE---------------------------------------------------------

#### demo for building predictor for new data ####

# get 10 patients randomly for prediction
set.seed("2022620")
idx <- sample(1:dim(diabetic_data_woe)[1], 10)
true_label <- diabetic_data[idx, "readmitted"]
data <- diabetic_data_woe[idx,
                    -which(colnames(diabetic_data_woe) %in% c("readmitted"))]
lr_mod <- res_logistic$model
rf_mod <- res_rf$model
rf_mod_iv <- res_rf_iv$model

# do predict with functions below in BIS620.2022.final package:
pred_lr <- model_predict(data, res_logistic$model, 
                         thre = res_logistic$best_thre, 
                         classname = c("YES", "NO"))
pred_rf <- model_predict(data, res_rf$model, thre = res_rf$best_thre, 
                         classname = c("YES", "NO"))
pred_rf_iv <- model_predict(data, res_rf_iv$model, thre = res_rf_iv$best_thre, 
                         classname = c("YES", "NO"))

# show the result
predict_sample <- true_label |> data.frame()
predict_sample$pred_lr <- pred_lr
predict_sample$pred_rf <- pred_rf
predict_sample$pred_rf_iv <- pred_rf_iv
colnames(predict_sample) <- c("true label", 
                              "pred_Logistic", 
                              "pred_RF", 
                              "pred_RF+IV")

predict_sample


