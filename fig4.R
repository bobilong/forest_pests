library(data.table)
library(stringr)
library(dplyr)

library(fastshap)
analyze_pest_data <- function(file_path,
                              holdout_block = NULL,   
                              k_total = 6,
                              k_cv = 5,
                              tune_grid = NULL,
                              beta = 1,
                              out_dir = "/forest_pests/data/",
                              seed = 1234
) {
  
  set.seed(seed)
  
  file_name <- basename(file_path)
  type <- str_split_i(file_name, "_",2)
  service <- str_split_i(file_name, "_",3) %>% str_sub(1, -5)
  
  predictData <- fread(file_path) %>% na.omit()
  
  colnames(predictData)[3] <- 'sum'
  predictData$sum <- factor(predictData$sum)
  predictData$LC_Type1 <- factor(predictData$LC_Type1)
  

  max_per_class <- 20000
  predictData <- predictData %>%
    group_by(sum) %>%
    sample_frac(size = min(1, max_per_class / n())) %>%
    ungroup()
  

  full_data_sf <- sf::st_as_sf(predictData, coords = c("x", "y"), crs = "EPSG:4326")
  
  spatial_blocks_full <- cv_spatial(
    full_data_sf,
    column = 'sum',
    k = k_total,
    size = 7053161,
    seed = seed,
    selection = "random",
    iteration = 10
  )
  
  predictData$block_id <- spatial_blocks_full$folds_ids
  

  valid_blocks <- predictData %>%
    group_by(block_id) %>%
    summarise(n_class = n_distinct(sum)) %>%
    filter(n_class == 2) %>%
    pull(block_id)
  
  if (length(valid_blocks) == 0) {
    stop("No valid block contains both classes")
  }
  
  # holdout_block <- sample(valid_blocks, 1)
  
  train_data <- predictData[predictData$block_id != holdout_block, ]
  test_data  <- predictData[predictData$block_id == holdout_block, ]
  

  if (is.null(tune_grid)) {
    excluded_cols <- c("id","x","y","block_id","sum")
    p <- ncol(train_data) - sum(names(train_data) %in% excluded_cols)
    p <- max(1, p)
    mtry_vals <- unique(as.integer(floor(seq(1, p, length.out = 3))))
    
    tune_grid <- expand.grid(
      mtry = mtry_vals,
      min.node.size = c(1, 3, 5),
      splitrule = "gini",
      stringsAsFactors = FALSE
    )
  } else {
    tune_grid <- as.data.frame(tune_grid, stringsAsFactors = FALSE)
  }
  
  cv_results_list <- list()
  

  for (fold in sort(unique(train_data$block_id))) {
    
    inner_train <- train_data[train_data$block_id != fold, ]
    inner_val   <- train_data[train_data$block_id == fold,]
    

    if (sum(inner_train$sum == 1) < 5 || sum(inner_train$sum == 0) < 5) {
      next
    }
    
    inner_val$sum <- factor(inner_val$sum, levels = levels(inner_train$sum))
    
    # --------- class weights ----------
    class_counts <- table(inner_train$sum)
    weights_vector <- as.numeric(max(class_counts) / class_counts)
    names(weights_vector) <- names(class_counts)
    
    for (i in 1:nrow(tune_grid)) {
      params <- tune_grid[i, ]
      
      model <- ranger(
        formula = sum ~ . - block_id - x - y,
        data = inner_train,
        num.trees = 300,
        mtry = as.integer(params$mtry),
        min.node.size = as.integer(params$min.node.size),
        splitrule = as.character(params$splitrule),
        class.weights = weights_vector,
        seed = seed,
        probability = FALSE,
        classification = TRUE
      )
      
      preds <- predict(model, inner_val)$predictions
      preds <- factor(preds, levels = levels(inner_val$sum))
      
      cm <- caret::confusionMatrix(preds, inner_val$sum, positive = "1")
      
      prec <- as.numeric(cm$byClass['Pos Pred Value'])
      rec  <- as.numeric(cm$byClass['Sensitivity'])
      if (is.na(prec)) prec <- 0
      if (is.na(rec)) rec <- 0
      
      if ((prec + rec) == 0) {
        f1 <- 0
        fbeta_val <- 0
      } else {
        f1 <- 2 * prec * rec / (prec + rec)
        fbeta_val <- (1 + beta^2) * (prec * rec) / (beta^2 * prec + rec)
      }
      
      cv_results_list[[length(cv_results_list) + 1]] <- data.frame(
        fold = fold,
        mtry = as.integer(params$mtry),
        min.node.size = as.integer(params$min.node.size),
        splitrule = as.character(params$splitrule),
        recall = rec,
        precision = prec,
        F1 = f1,
        Fbeta = fbeta_val,
        Accuracy = as.numeric(cm$overall['Accuracy']),
        Kappa = as.numeric(cm$overall['Kappa']),
        stringsAsFactors = FALSE
      )
    }
  }
  
  cv_details <- do.call(rbind, cv_results_list)
  
  # dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(cv_details, file.path(out_dir, paste0(type, "_", service, "_cv_details.csv")))
  
  cv_summary <- cv_details %>%
    group_by(mtry, min.node.size, splitrule) %>%
    summarise(
      mean_recall = mean(recall, na.rm = TRUE),
      mean_precision = mean(precision, na.rm = TRUE),
      mean_F1 = mean(F1, na.rm = TRUE),
      mean_Fbeta = mean(Fbeta, na.rm = TRUE),
      mean_Accuracy = mean(Accuracy, na.rm = TRUE),
      mean_Kappa = mean(Kappa, na.rm = TRUE),
      n_folds = n()
    ) %>%
    arrange(desc(mean_Kappa)) %>%
    ungroup()
  
  best_params <- cv_summary %>% slice(1)
  best_mtry <- as.integer(best_params$mtry[1])
  best_min_node <- as.integer(best_params$min.node.size[1])
  best_splitrule <- as.character(best_params$splitrule[1])
  
  # --------- final model ----------
  class_counts_full <- table(train_data$sum)
  weights_full <- as.numeric(max(class_counts_full) / class_counts_full)
  names(weights_full) <- names(class_counts_full)
  
  final_model <- ranger(
    formula = sum ~ . - block_id - x - y,
    data = train_data,
    num.trees = 500,
    mtry = best_mtry,
    min.node.size = best_min_node,
    splitrule = best_splitrule,
    importance = 'permutation',
    class.weights = weights_full,
    seed = seed + 2,
    probability = TRUE
  )
  
  preds_test <- predict(final_model, test_data)$predictions
  preds_test_class <- ifelse(preds_test[, "1"] > 0.5, "1", "0")
  preds_test_class <- factor(preds_test_class, levels = levels(test_data$sum))
  
  cm_test <- caret::confusionMatrix(preds_test_class, test_data$sum, positive = "1")
  
  prec_test <- as.numeric(cm_test$byClass['Pos Pred Value']); if (is.na(prec_test)) prec_test <- 0
  rec_test  <- as.numeric(cm_test$byClass['Sensitivity']);    if (is.na(rec_test)) rec_test <- 0
  
  if ((prec_test + rec_test) == 0) {
    f1_test <- 0
    fbeta_test <- 0
  } else {
    f1_test <- 2 * prec_test * rec_test / (prec_test + rec_test)
    fbeta_test <- (1 + beta^2) * (prec_test * rec_test) / (beta^2 * prec_test + rec_test)
  }
  
  test_metrics <- data.frame(
    Accuracy = as.numeric(cm_test$overall['Accuracy']),
    Kappa = as.numeric(cm_test$overall['Kappa']),
    Precision = prec_test,
    Recall = rec_test,
    F1 = f1_test,
    Fbeta = fbeta_test,
    stringsAsFactors = FALSE
  )
  
  fwrite(test_metrics, file.path(out_dir, paste0(type, "_", service, "_external_test_metrics.csv")))
  

  imp_values <- importance(final_model)
  var_imp <- data.frame(
    variable = names(imp_values),
    importance = as.numeric(imp_values),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      type = type,
      service = service,
      imp_scaled = importance / max(importance)
    )
  
  fwrite(var_imp,paste0(out_dir, type, '_', service, '_importance.csv'))
  
  # --------- SHAP ----------
  train_for_shap <- train_data %>% mutate(row_id = row_number())
  
  sample_indices <- train_for_shap %>%
    group_by(sum) %>%
    sample_n(min(500, n())) %>%
    ungroup() %>%
    pull(row_id)
  
  sample_data <- train_for_shap[sample_indices, ]
  
  pred_wrapper <- function(object, newdata) {
    preds <- predict(object, data = newdata)$predictions
    if (is.matrix(preds)) {
      return(as.numeric(preds[, "1"]))
    } else {
      return(as.numeric(preds))
    }
  }
  
  X_shap <- sample_data %>% select(-sum, -block_id,-row_id)
  
  shap_values <- fastshap::explain(
    final_model,
    X = X_shap,
    pred_wrapper = pred_wrapper,
    nsim = 50
  )
  
  shap_sample <- as.data.frame(shap_values) %>%
    mutate(id = row_number()) %>%
    tidyr::pivot_longer(-id, names_to = "variable", values_to = "shap_value") %>%
    left_join(
      X_shap %>%
        mutate(id = row_number()) %>%
        mutate(across(-id, as.character)) %>%
        tidyr::pivot_longer(-id, names_to = "variable", values_to = "feature_value"),
      by = c("id", "variable")
    )
  
  fwrite(shap_sample, paste0(out_dir, type, '_', service, '_shap.csv'))
}
#import data ----
predictPath <- list.files('forest_pests/data/driverDf/', full.names = TRUE)

# #process data
purrr::map_dfr(predictPath, analyze_pest_data)



######## import data ############
resultPaths_shap <- list.files('./data/','_.*_shap.csv',full.names = T)
results_shap <- lapply(resultPaths_shap, function(path){
  file_name <- basename(path)
  type <- str_split_i(file_name,'_',1)
  service <- str_split_i(file_name,'_',2)
  df <- fread(path)
  df$type <- type
  df$service <- service
  df <- df[!variable %in% c("x", "y")]
  return(df)
}) %>% rbindlist()

results_shap$variable <- results_shap$variable %>%
  str_replace("wc2.1_10m_bio_12", "mean_precipitation") %>%
  str_replace("wc2.1_10m_bio_1", "mean_temperature") %>%  
  str_replace("wc2.1_10m_bio_3", "isothermality") %>%  
  str_replace("^travel_time_to_cities_12", "travel_time") %>%  
  str_replace("^FFI2020", "FFI") %>% 
  str_replace("^ForestAge_TC010", "forest_age") %>% 
  str_replace("^GNI", "GNI")%>% 
  str_replace("^timberImport", "timber_import")%>% 
  str_replace("^LC_Type1", "forest_type")%>% 
  str_replace("^population_density", "population_density") 

# exlude non-forest of forest_type
results_shap <- results_shap[!(variable == "forest_type" & feature_value > 5),]

results_shap$feature_value <- ifelse(results_shap$variable%in%c('GNI','population_density','travel_time','timber_import'),log(results_shap$feature_value),results_shap$feature_value)
results_shap$variable <- ifelse(results_shap$variable=='GNI','log(GNI)',results_shap$variable)
results_shap$variable <- ifelse(results_shap$variable=='population_density','log(population_density)',results_shap$variable)
results_shap$variable <- ifelse(results_shap$variable=='travel_time','log(travel_time)',results_shap$variable)
results_shap$variable <- ifelse(results_shap$variable=='timber_import','log(timber_import)',results_shap$variable)

label_data <- data.frame(
  variable = c('FFI','forest_age','mean_temperature','mean_precipitation','isothermality','log(timber_import)',
               'log(GNI)','log(population_density)','log(travel_time)'),
  label = paste0(letters[1:9],')'),
  x = -Inf,
  y = Inf
)
add_shap_plot <- function(serviceName, label_data = NULL) {
  
  tmpDf <- results_shap[service == serviceName & feature_value != -Inf, ]
  
  contDf <- tmpDf[variable != "forest_type", ]
  catDf  <- tmpDf[variable == "forest_type", ]
  catDf$forest_type_label <- factor(
    catDf$feature_value,
    levels = c(1,2,3,4,5),
    labels = c("ENF","EBF","DNF","DBF","MF")
  )
  
  contDf <- contDf %>%
    group_by(variable) %>%
    mutate(
      bin = cut(feature_value, breaks = 50, include.lowest = TRUE),
      bin_count = n()
    ) %>%
    ungroup()
  
  median_data <- contDf %>%
    group_by(variable, bin, type) %>%
    filter(n() > 20) %>%
    summarise(
      median_shap = median(shap_value),
      median_value = median(feature_value),
      .groups = "drop"
    )
  

  if (!is.null(label_data)) {

    label_cont <- label_data[label_data$variable %in% unique(contDf$variable), ]
    label_cat  <- label_data[label_data$variable == "forest_type", ]
  } else {
    label_cont <- data.frame()
    label_cat  <- data.frame()
  }
  
  p_cont <- ggplot(contDf, aes(feature_value, shap_value)) +
    facet_wrap(~variable, scales = 'free', ncol = 3) +
    
    geom_point(aes(color = type),
               alpha = 0.1,
               show.legend = FALSE,
               size = 0.3) +
    geom_line(data = median_data,
              aes(x = median_value, y = median_shap, color = type),
              size = 1.2) +
    
    scale_color_manual(values = c('#2166AC','#D35400'),
                       labels = c("within", "outside")) +
    
    {if (nrow(label_cont) > 0) 
      geom_text(data = label_cont,
                aes(x = x, y = y, label = label),
                hjust = -0.1, vjust = 1.2,
                size = 4, fontface = "bold",
                inherit.aes = FALSE)
    } +
    
    labs(x = 'Feature value', y = 'SHAP value') +
    theme_bw() +
    theme(legend.title = element_blank())
  

  catDf$new_type <- factor(catDf$type, 
                           levels = c('native','non-native'),
                           labels = c("within", "outside"))
  
  p_cat <- ggplot(catDf,
                  aes(x = forest_type_label, y = shap_value)) +
    geom_boxplot(
      aes(fill = new_type),
      position = position_dodge2(width = 0.7, preserve = "single"),
      alpha = 0.6,
      na.rm = TRUE,
      outlier.shape = NA) +
    geom_jitter(aes(color = new_type),
                width = 0.2,
                show.legend = FALSE,
                size = 0.3) +
    scale_color_manual(values = c('#A9CBE8','#F7C58E')) +
    scale_fill_manual(
      values = c('#2166AC','#D35400'),
      drop = FALSE
    ) +
    
    {if (nrow(label_cat) > 0)
      geom_text(data = label_cat,
                aes(x = -Inf, y = Inf, label = label),
                hjust = -0.1, vjust = 1.2,
                size = 4, fontface = "bold",
                inherit.aes = FALSE)
    } +
    
    labs(x = 'Forest type', y = 'SHAP value') +
    theme_bw() +
    theme(legend.title = element_blank())
  
  p_final <- p_cont / p_cat + 
    plot_layout(heights = c(3, 1))
  
  return(p_final)
}

add_shap_plot('timber',label_data)
add_shap_plot('diversity',label_data)
add_shap_plot('carbon',label_data)
