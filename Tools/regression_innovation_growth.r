regression_innovation_growth<-function(regression_name, data, formulas, titles_and_restrictions, types="all_firms", subsets="all", threshold_young=5, output_dir_og=output_dir, graphs=F, disag_reg_parameters=NULL){
  
  patenting_prod_firmids <- fread(paste0("firm_lists/patenting_firms_in_BR_no_outliers.csv")) %>%
    pull(patenting_prod_firmids)

  model_table<-titles_and_restrictions
  
  for (i in 1:nrow(formulas)) {
    
    formula <- as.formula(formulas$formula[i])
    y <- formulas$y[i]
    x <- formulas$x[i]
    controls <- formulas$control[i]
    fix_eff <- formulas$fixed_effect[i]
    description_temp <- formulas$description[i]

      output_dir <- paste0(output_dir_og, regression_name, "/")
        if (!dir.exists(output_dir)) {
          output_dir_creator(output_dir)
        }

    
    for (type in types) {
      
      if (length(types) > 1) {
        output_dir <- paste0(output_dir_og, regression_name, "/", type, "/")
        if (!dir.exists(output_dir)) {
          output_dir_creator(output_dir)
        }
      } 
      
      for (subset in subsets) {
        patenting_products <- age_data_filter(data, threshold_young, subset)
        
        if (type == "patenting_firms") {
          patenting_products <- patenting_products[firmid %in% patenting_prod_firmids]
          if(grepl("patent", y)){
            next
          }
        }
        
        if (length(subsets) > 1) {
          output_dir <- paste0(output_dir, subset, "/")
          if (!dir.exists(output_dir)) {
            output_dir_creator(output_dir)
          }
        } 
        
        
        print(paste0(y, " ~ ", x))
        
        models <- setNames(vector("list", nrow(model_table)), model_table$title)
        for (j in seq_len(nrow(model_table))) {
          title <- model_table$title[j]
          restriction <- model_table$restriction[j]
          weight_flag <- as.logical(model_table$weight_flag[j])
          weight <- ifelse(weight_flag, formulas$weight[i], NA)
          additional_controls <- model_table$additional_controls[j]

          # Add additional controls if specified
          if (!is.na(additional_controls) && additional_controls != "") {
            formula <- update(formula, paste(". ~ . +", additional_controls))
          }

          if (is.na(model_table$restriction[j])) {
            # "All" case
            models[[j]] <- feols(formula, data, weights = if (!is.na(weight)) data[[weight]] else NULL)
          } else {
            models[[j]] <- feols(formula, data[eval(parse(text = restriction))], weights = if (!is.na(weight)) data[eval(parse(text = restriction)), get(weight)] else NULL) # nolint          }
          }
        }
        

                
        # Remove underscores from coefficient names for LaTeX export for all models
        all_coef_names <- unique(unlist(lapply(models, function(m) names(m$coefficients))))
        coef_map <- setNames(gsub("_", " ", all_coef_names), all_coef_names)

        # Escape underscores in fixed effect names for LaTeX
        all_fe_names <- unique(unlist(lapply(models, function(m) {
          if (!is.null(m$fixef_vars)) m$fixef_vars else character(0)
        })))
        fe_map_latex <- setNames(gsub("_", "\\\\_", all_fe_names), all_fe_names)

        modelsummary(
          models,
          coef_map = coef_map_latex,
          output = paste0(output_dir, description_temp, ".tex"),
          label = paste0(regression_name, "_", description_temp),
          stars = TRUE,
          title = "", # tools::toTitleCase(paste0(gsub("_", " ", y), " on ", gsub("_", " ", gsub("\\*", "_", x)), " - Sample: ", subset, " within ", gsub("_", " ", type))),
          gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
          escape = FALSE,
          notes = NULL,
          float = "H"
        )
        # Remove underscores from coefficient and fixed effect names for non-LaTeX output (e.g., HTML, txt)
        # If you want to export to HTML or txt, use coef_map and fe_map instead of coef_map_latex and fe_map_latex
        # Example:
        # modelsummary(
        #   models,
        #   coef_map = coef_map,
        #   group_map = fe_map,
        #   output = paste0(output_dir, description_temp, ".html"),
        #   ...
        # )

        # If you want to replace underscores with spaces in the output file name and label:
        # output_file <- paste0(output_dir, gsub("_", " ", description_temp), ".tex")
        # output_label <- paste0(regression_name, "_", gsub("_", " ", description_temp))
        # modelsummary(..., output = output_file, label = output_label, ...)        
        if (graphs) {
          tidy_models <- imap(models, function(model, model_name) {
            if (str_detect(x, "\\*")) {
              tidy(model, conf.int = T) %>%
                filter(str_detect(term, ".*:.*") | str_detect(term, paste(strsplit(x, "\\*")[[1]], collapse = "|")) |
                         str_detect(term, paste(trimws(strsplit(x, "\\+")[[1]]), collapse = "|"))) %>%
                mutate(model_label = model_name)
            } else {
              tidy(model, conf.int = T) %>%
                filter(str_detect(term, x) |
                         str_detect(term, paste(trimws(strsplit(x, "\\+")[[1]]), collapse = "|"))) %>%
                mutate(model_label = model_name)
            }
          })
          
          results_df <- bind_rows(tidy_models)
          setDT(results_df)
          results_df[, group := fifelse(
            grepl("Y.", model_label), "Young",
            fifelse(
              grepl("SS", model_label), "Superstar",
              fifelse(grepl("M.", model_label), "Mature", "All")
            )
          )]
          
          # results_df[, c("matched_var", "term_clean") := {
          #   matched <- NA_character_
          #   for (v in vars_interactions) {
          #     if (str_ends(term, v)) {
          #       matched <- v
          #       break
          #     }
          #   }
          #   cleaned <- if (!is.na(matched)) str_remove(term, paste0(matched)) else "term"
          #   list(matched, cleaned)
          # }, by = seq_len(nrow(results_df))]
          
          # pattern <- paste(paste0(":", vars_interactions), collapse = "|")
          # results_df[, term_clean := str_remove_all(term, pattern)]
          # # results_df<-results_df[!is.na(matched_var)]
          
          results_df$model_label <- factor(results_df$model_label, levels = names(models))
          
          for (coefficient in unique(results_df$term)) {
            results_df_temp <- results_df[term == coefficient]
            
            group_levels <- unique(results_df_temp$group)
            color_values <- setNames(c(scales::hue_pal()(length(group_levels) - 1), "black"), group_levels)
            
            ggplot(results_df_temp, aes(x = model_label, y = estimate, ymin = conf.low, ymax = conf.high, color = group)) +
              geom_pointrange() +
              geom_hline(yintercept = 0, linetype = "dashed") +
              scale_color_manual(values = color_values) +
              labs(
                x = tools::toTitleCase(paste0("Subset")),
                y = paste("Estimate (with 95% CI)"),
                subtitle = tools::toTitleCase(paste0("Variable: ", gsub(" window", "  W",  gsub(":", " ? ", gsub("_", " ", coefficient) )))))+
              theme(legend.position = "none")
            # title=tools::toTitleCase(paste0(gsub("_", " ", gsub("window", "W", y)),
            #                                 " = ",
            #                                 gsub("_", " ", gsub("_window", " W",  gsub("\\*", "? ", x))))),
            
            ggsave(paste0(output_dir, description_temp, ".png"), height = 4, width = 7)
          }
        }

     


        if (!is.null(disag_reg_parameters)) {
          for (k in seq_along(nrow(disag_reg_parameters))) {
            disag_y <- disag_reg_parameters$y[k]
            disag_x <- disag_reg_parameters$x[k]
            disag_controls <- disag_reg_parameters$controls[k]
            disag_fe <- disag_reg_parameters$fe[k]
            disag_restriction <- disag_reg_parameters$restriction[k]
            disag_var <- disag_reg_parameters$disag_var[k]
            disag_weight <- disag_reg_parameters$weight[k]

            # Check if the vector version of the disag_controls exactly matches the vector version of the controls
            if (disag_y == y &
              unlist(strsplit(disag_x, "\\+")) %>%
                sort() %>%
                identical(sort(unlist(strsplit(disag_x, "\\+")))) &
              unlist(strsplit(disag_controls, "\\+")) %>%
                sort() %>%
                identical(sort(unlist(strsplit(controls, "\\+")))) &
              unlist(strsplit(disag_fe, "\\+")) %>%
                sort() %>%
                identical(sort(unlist(strsplit(fe, "\\+"))))) {{ model_industry <- dynamic_reg_reallocation(
              # data restricted by disag_restriction if disag_restriction is not blank or na, data otherwise
              data = data[eval(parse(text = ifelse(is.na(disag_restriction) | disag_restriction == "", "TRUE", disag_restriction)))],
              y = disag_y,
              x = c(unlist(strsplit(disag_x, "\\+")), unlist(strsplit(disag_controls, "\\+"))),
              fix_eff = disag_fe,
              weight_var = if (is.na(disag_weight) | disag_weight != "") disag_weight else NULL,
              disag_var = disag_var,
              n_lags_bw = 0,
              n_lags_fw = 0
            )



            # model_industry <- merge(model_industry, br_industry_HHI[, .(median_HHI = median(HHI_industry)), by = NACE_BR],
            #   by.x = "filter",
            #   by.y = disag_reg_parameters$disag_var,
            #   all.x = T
            # ) %>% .[, sector := substr(filter, 1, 2)]

            # ggplot(model_industry, aes(x = median_HHI, y = log(log_empl_bar_HHI_industry), color = as.factor(substr(filter, 1, 2)))) +
            #   geom_point(alpha = 0.5) +
            #   labs(
            #     x = "Median HHI",
            #     y = "Coefficient for log_empl_bar*HHI_industry",
            #     subtitle = "Mature Large Firms",
            #     color = "Sector"
            #   )
            # ggsave(paste0(output_dir, y, "_", formula_description, ".png"), height = 6, width = 8)

            # model_industry <- model_industry[nobs > 5]

            fwrite(model_industry, paste0(
              output_dir, "data_for_image_",
              remove_special_chars(disag_y), "_",
              remove_special_chars(gsub("\\*", "_", disag_x)), "_",
              remove_special_chars(gsub("\\+", "_", disag_restriction)), "_",
              ".csv"
            )) }}
            print(paste0(output_dir, "regressions_ipcr_addition_", y, "_", x, ".tex"))
          }
        }
        }
        }
        }
        }
