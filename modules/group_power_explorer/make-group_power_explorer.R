library(targets)
library(ravepipeline)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
._._env_._.$pipeline <- pipeline_from_path(".")
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R", 
  full.names = TRUE
)), function(f) {
  source(f, local = ._._env_._., chdir = TRUE)
})
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_selected_pipelines = targets::tar_target_raw("selected_pipelines", 
        quote({
            settings[["selected_pipelines"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_pipeline_table = targets::tar_target_raw("pipeline_table", 
        quote({
            settings[["pipeline_table"]]
        }), deps = "settings"), load_group_data = targets::tar_target_raw(name = "group_data", 
        command = quote({
            .__target_expr__. <- quote({
                require(data.table)
                .datatable.aware = TRUE
                pipeline_table <- as.data.frame(pipeline_table)
                subjects_needed <- unique(subset(pipeline_table, 
                  label %in% selected_pipelines)$subject)
                all_pipe_res <- lapply(subjects_needed, function(sbj) {
                  print(sbj)
                  subject <- raveio::RAVESubject$new(project_name = project_name, 
                    subject_code = sbj)
                  pipe_table_row <- as.data.frame(subset(pipeline_table, 
                    subject == sbj))[1, ]
                  pipe_path <- file.path(subject$pipeline_path, 
                    pipe_table_row$pipeline_name, pipe_table_row$directory)
                  dfga <- file.path(pipe_path, "shared", "objects", 
                    "data_for_group_analysis")
                  if (file.exists(dfga)) {
                    return(readRDS(dfga))
                  }
                  return(NULL)
                })
                ravedash::logger("Got initial pipe res", level = "debug")
                all_pipe_res <- all_pipe_res[!sapply(all_pipe_res, 
                  is.null)]
                electrode_details <- lapply(all_pipe_res, function(apr) {
                  ee <- subset(apr$electrode_information, Electrode %in% 
                    unique(apr$omnibus_data$Electrode))
                  ee$Subject = apr$analysis_settings_clean[[1]]$subject_code
                  ee$Project = apr$analysis_settings_clean[[1]]$project_name
                  ee[, c("Subject", "Project", setdiff(names(ee), 
                    c("Subject", "Project")))]
                })
                names_in_common <- Reduce(intersect, lapply(electrode_details, 
                  names))
                electrode_details <- rbind_list(lapply(electrode_details, 
                  `[`, names_in_common))
                ravedash::logger("Got electrode details", level = "debug")
                flatten_as <- function(as) {
                  as$frequency %<>% paste0(collapse = "-")
                  as$censor_enabled = FALSE
                  as$censor_window = "none"
                  if (as$censor_info$enabled) {
                    as$censor_window <- paste0(as$censor_info$window, 
                      collapse = "-")
                    as$censor_enabled <- TRUE
                  }
                  as$censor_info <- NULL
                  as$frequency_dd <- NULL
                  as$time %<>% paste0(collapse = "-")
                  data.frame(as)
                }
                all_analysis_settings <- lapply(all_pipe_res, 
                  function(apr) {
                    lapply(apr$analysis_settings_clean, flatten_as) %>% 
                      rbind_list
                  }) %>% rbind_list
                all_data <- lapply(all_pipe_res, function(apr) {
                  od <- apr$omnibus_data
                  od[c("Time", "Freq", "Event", "currently_selected")] <- NULL
                  od$Subject <- apr$analysis_settings_clean[[1]]$subject_code
                  od
                }) %>% rbind_list
                all_labels <- sapply(all_pipe_res, function(apr) {
                  names(apr$over_time_by_electrode_data)
                }) %>% unlist %>% c %>% unique
                available_range <- sapply(all_labels, function(lbl) {
                  by_sbj <- lapply(all_pipe_res, function(apr) {
                    if (is.null(apr$over_time_by_electrode_data[[lbl]])) {
                      return(NULL)
                    }
                    range(apr$over_time_by_electrode_data[[lbl]]$x)
                  }) %>% rbind_list
                  apply(by_sbj, 2, closest_to_zero)
                }, simplify = FALSE)
                over_time_by_condition_data_long <- sapply(all_labels, 
                  function(lbl) {
                    first_res <- all_pipe_res[[1]]$over_time_by_electrode_data[[lbl]]
                    eldata <- lapply(all_pipe_res, function(apr) {
                      if (is.null(apr$over_time_by_electrode_data[[lbl]])) {
                        return(NULL)
                      }
                      time_ind <- apr$over_time_by_electrode_data[[lbl]]$x %within% 
                        available_range[[lbl]]
                      apr$over_time_by_electrode_data[[lbl]]$data[time_ind, 
                        ]
                    }) %>% cbind_list
                    res <- list()
                    nms <- c("xlab", "label", "has_trials", "analysis_group", 
                      "analysis_window")
                    res[nms] = first_res[nms]
                    res$x = first_res$x[all_pipe_res[[1]]$over_time_by_electrode_data[[lbl]]$x %within% 
                      available_range[[lbl]]]
                    res$data <- t(apply(eldata, 1, m_se))
                    res$ylab <- first_res$zlab
                    lbl_tokens <- stringr::str_split(lbl, stringr::fixed("."), 
                      simplify = TRUE)
                    res$time_window_label = lbl_tokens[1]
                    res$data_label = lbl_tokens[2]
                    res$range <- range(plus_minus(res$data))
                    return(res)
                  }, simplify = FALSE)
                otbc_names <- apply(stringr::str_split(all_labels, 
                  stringr::fixed("."), simplify = TRUE), 2, unique, 
                  simplify = FALSE)
                over_time_by_condition_data <- vector("list", 
                  length = length(otbc_names[[2]]))
                names(over_time_by_condition_data) = otbc_names[[2]]
                for (ii in otbc_names[[2]]) {
                  for (jj in otbc_names[[1]]) {
                    over_time_by_condition_data[[ii]][[jj]] = over_time_by_condition_data_long[[paste(jj, 
                      ii, sep = ".")]]
                  }
                }
                unires <- lapply(all_pipe_res, function(apr) {
                  df <- as.data.frame(t(apr$omnibus_stats))
                  df$Project = apr$analysis_settings_clean[[1]]$project_name
                  df$Subject = apr$analysis_settings_clean[[1]]$subject_code
                  df$Electrode = as.integer(colnames(apr$omnibus_stats))
                  first_ind <- which(names(df) %in% c("Project", 
                    "Subject", "Electrode"))
                  df[, c(first_ind, (1:ncol(df))[-first_ind])] %>% 
                    set_rownames(NULL)
                })
                merged_unires <- unires[[1]]
                if (length(unires) > 1) {
                  for (ii in 2:length(unires)) merged_unires %<>% 
                    merge(unires[[ii]], all = TRUE)
                }
                group_data <- list(electrodes = electrode_details, 
                  data = all_data, univariate_results = merged_unires, 
                  analysis_settings = all_analysis_settings, 
                  over_time_by_condition_data = over_time_by_condition_data)
                group_data$all_data_by_el <- as.data.frame(data.table::data.table(all_data)[, 
                  list(y = mean(y)), keyby = c("Electrode", "Factor1", 
                    "AnalysisLabel")])
            })
            tryCatch({
                eval(.__target_expr__.)
                return(group_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "group_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "group_data", target_expr = quote({
                {
                  require(data.table)
                  .datatable.aware = TRUE
                  pipeline_table <- as.data.frame(pipeline_table)
                  subjects_needed <- unique(subset(pipeline_table, 
                    label %in% selected_pipelines)$subject)
                  all_pipe_res <- lapply(subjects_needed, function(sbj) {
                    print(sbj)
                    subject <- raveio::RAVESubject$new(project_name = project_name, 
                      subject_code = sbj)
                    pipe_table_row <- as.data.frame(subset(pipeline_table, 
                      subject == sbj))[1, ]
                    pipe_path <- file.path(subject$pipeline_path, 
                      pipe_table_row$pipeline_name, pipe_table_row$directory)
                    dfga <- file.path(pipe_path, "shared", "objects", 
                      "data_for_group_analysis")
                    if (file.exists(dfga)) {
                      return(readRDS(dfga))
                    }
                    return(NULL)
                  })
                  ravedash::logger("Got initial pipe res", level = "debug")
                  all_pipe_res <- all_pipe_res[!sapply(all_pipe_res, 
                    is.null)]
                  electrode_details <- lapply(all_pipe_res, function(apr) {
                    ee <- subset(apr$electrode_information, Electrode %in% 
                      unique(apr$omnibus_data$Electrode))
                    ee$Subject = apr$analysis_settings_clean[[1]]$subject_code
                    ee$Project = apr$analysis_settings_clean[[1]]$project_name
                    ee[, c("Subject", "Project", setdiff(names(ee), 
                      c("Subject", "Project")))]
                  })
                  names_in_common <- Reduce(intersect, lapply(electrode_details, 
                    names))
                  electrode_details <- rbind_list(lapply(electrode_details, 
                    `[`, names_in_common))
                  ravedash::logger("Got electrode details", level = "debug")
                  flatten_as <- function(as) {
                    as$frequency %<>% paste0(collapse = "-")
                    as$censor_enabled = FALSE
                    as$censor_window = "none"
                    if (as$censor_info$enabled) {
                      as$censor_window <- paste0(as$censor_info$window, 
                        collapse = "-")
                      as$censor_enabled <- TRUE
                    }
                    as$censor_info <- NULL
                    as$frequency_dd <- NULL
                    as$time %<>% paste0(collapse = "-")
                    data.frame(as)
                  }
                  all_analysis_settings <- lapply(all_pipe_res, 
                    function(apr) {
                      lapply(apr$analysis_settings_clean, flatten_as) %>% 
                        rbind_list
                    }) %>% rbind_list
                  all_data <- lapply(all_pipe_res, function(apr) {
                    od <- apr$omnibus_data
                    od[c("Time", "Freq", "Event", "currently_selected")] <- NULL
                    od$Subject <- apr$analysis_settings_clean[[1]]$subject_code
                    od
                  }) %>% rbind_list
                  all_labels <- sapply(all_pipe_res, function(apr) {
                    names(apr$over_time_by_electrode_data)
                  }) %>% unlist %>% c %>% unique
                  available_range <- sapply(all_labels, function(lbl) {
                    by_sbj <- lapply(all_pipe_res, function(apr) {
                      if (is.null(apr$over_time_by_electrode_data[[lbl]])) {
                        return(NULL)
                      }
                      range(apr$over_time_by_electrode_data[[lbl]]$x)
                    }) %>% rbind_list
                    apply(by_sbj, 2, closest_to_zero)
                  }, simplify = FALSE)
                  over_time_by_condition_data_long <- sapply(all_labels, 
                    function(lbl) {
                      first_res <- all_pipe_res[[1]]$over_time_by_electrode_data[[lbl]]
                      eldata <- lapply(all_pipe_res, function(apr) {
                        if (is.null(apr$over_time_by_electrode_data[[lbl]])) {
                          return(NULL)
                        }
                        time_ind <- apr$over_time_by_electrode_data[[lbl]]$x %within% 
                          available_range[[lbl]]
                        apr$over_time_by_electrode_data[[lbl]]$data[time_ind, 
                          ]
                      }) %>% cbind_list
                      res <- list()
                      nms <- c("xlab", "label", "has_trials", 
                        "analysis_group", "analysis_window")
                      res[nms] = first_res[nms]
                      res$x = first_res$x[all_pipe_res[[1]]$over_time_by_electrode_data[[lbl]]$x %within% 
                        available_range[[lbl]]]
                      res$data <- t(apply(eldata, 1, m_se))
                      res$ylab <- first_res$zlab
                      lbl_tokens <- stringr::str_split(lbl, stringr::fixed("."), 
                        simplify = TRUE)
                      res$time_window_label = lbl_tokens[1]
                      res$data_label = lbl_tokens[2]
                      res$range <- range(plus_minus(res$data))
                      return(res)
                    }, simplify = FALSE)
                  otbc_names <- apply(stringr::str_split(all_labels, 
                    stringr::fixed("."), simplify = TRUE), 2, 
                    unique, simplify = FALSE)
                  over_time_by_condition_data <- vector("list", 
                    length = length(otbc_names[[2]]))
                  names(over_time_by_condition_data) = otbc_names[[2]]
                  for (ii in otbc_names[[2]]) {
                    for (jj in otbc_names[[1]]) {
                      over_time_by_condition_data[[ii]][[jj]] = over_time_by_condition_data_long[[paste(jj, 
                        ii, sep = ".")]]
                    }
                  }
                  unires <- lapply(all_pipe_res, function(apr) {
                    df <- as.data.frame(t(apr$omnibus_stats))
                    df$Project = apr$analysis_settings_clean[[1]]$project_name
                    df$Subject = apr$analysis_settings_clean[[1]]$subject_code
                    df$Electrode = as.integer(colnames(apr$omnibus_stats))
                    first_ind <- which(names(df) %in% c("Project", 
                      "Subject", "Electrode"))
                    df[, c(first_ind, (1:ncol(df))[-first_ind])] %>% 
                      set_rownames(NULL)
                  })
                  merged_unires <- unires[[1]]
                  if (length(unires) > 1) {
                    for (ii in 2:length(unires)) merged_unires %<>% 
                      merge(unires[[ii]], all = TRUE)
                  }
                  group_data <- list(electrodes = electrode_details, 
                    data = all_data, univariate_results = merged_unires, 
                    analysis_settings = all_analysis_settings, 
                    over_time_by_condition_data = over_time_by_condition_data)
                  group_data$all_data_by_el <- as.data.frame(data.table::data.table(all_data)[, 
                    list(y = mean(y)), keyby = c("Electrode", 
                      "Factor1", "AnalysisLabel")])
                }
                group_data
            }), target_depends = c("pipeline_table", "selected_pipelines", 
            "project_name")), deps = c("pipeline_table", "selected_pipelines", 
        "project_name"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_compatibility = targets::tar_target_raw(name = "compatibility_report", 
        command = quote({
            .__target_expr__. <- quote({
                compatibility_report <- ""
            })
            tryCatch({
                eval(.__target_expr__.)
                return(compatibility_report)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "compatibility_report", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "compatibility_report", target_expr = quote({
                {
                  compatibility_report <- ""
                }
                compatibility_report
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    run_model = targets::tar_target_raw(name = "model_results", 
        command = quote({
            .__target_expr__. <- quote({
                require(lme4)
                analysis_function = "lmer"
                analysis_formula <- "y ~ AnalysisLabel * Factor1 + (1|Subject/Electrode)"
                aFUN <- match.fun(analysis_function)
                aFormula <- as.formula(analysis_formula)
                model_results <- aFUN(aFormula, data = group_data$data)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(model_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "model_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "model_results", target_expr = quote({
                {
                  require(lme4)
                  analysis_function = "lmer"
                  analysis_formula <- "y ~ AnalysisLabel * Factor1 + (1|Subject/Electrode)"
                  aFUN <- match.fun(analysis_function)
                  aFormula <- as.formula(analysis_formula)
                  model_results <- aFUN(aFormula, data = group_data$data)
                }
                model_results
            }), target_depends = "group_data"), deps = "group_data", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    assess_model = targets::tar_target_raw(name = "model_fit_statistics", 
        command = quote({
            .__target_expr__. <- quote({
                require(car)
                model_fit_statistics <- Anova(model_results)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(model_fit_statistics)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "model_fit_statistics", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "model_fit_statistics", target_expr = quote({
                {
                  require(car)
                  model_fit_statistics <- Anova(model_results)
                }
                model_fit_statistics
            }), target_depends = "model_results"), deps = "model_results", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    generate_post_hocs = targets::tar_target_raw(name = "post_hoc_results", 
        command = quote({
            .__target_expr__. <- quote({
                require(emmeans)
                post_hoc_str <- "pairwise ~ Factor1"
                post_hoc_formula <- as.formula(post_hoc_str)
                pFormula <- as.formula(post_hoc_formula)
                post_hoc_results <- emmeans(object = model_results, 
                  specs = post_hoc_formula)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(post_hoc_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "post_hoc_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "post_hoc_results", target_expr = quote({
                {
                  require(emmeans)
                  post_hoc_str <- "pairwise ~ Factor1"
                  post_hoc_formula <- as.formula(post_hoc_str)
                  pFormula <- as.formula(post_hoc_formula)
                  post_hoc_results <- emmeans(object = model_results, 
                    specs = post_hoc_formula)
                }
                post_hoc_results
            }), target_depends = "model_results"), deps = "model_results", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    generate_second_level_post_hocs = targets::tar_target_raw(name = "second_level_post_hoc_results", 
        command = quote({
            .__target_expr__. <- quote({
                second_level_post_hoc_results <- NULL
            })
            tryCatch({
                eval(.__target_expr__.)
                return(second_level_post_hoc_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "second_level_post_hoc_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "second_level_post_hoc_results", 
            target_expr = quote({
                {
                  second_level_post_hoc_results <- NULL
                }
                second_level_post_hoc_results
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"))
