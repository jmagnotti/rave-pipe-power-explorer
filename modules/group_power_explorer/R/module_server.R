
module_server <- function(input, output, session, ...){

  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL,
    update_3dviewer = NULL,
    update_over_time_plot = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  local_data$active_analysis_settings <- NULL
  local_data$active_analysis_settings_clean <- NULL
  local_data$results <- NULL

  brain_proxy <- threeBrain::brain_proxy("brain_viewer", session = session)

  # get server tools to tweak
  server_tools <- get_default_handlers(session = session)

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      shidashi::clear_notifications(class = "pipeline-error")

      # set pipeline inputs

      ravedash::shiny_alert2(title = "Running Analysis", text = ravedash::be_patient_text(), auto_close = FALSE, buttons = FALSE)

      on.exit({
        Sys.sleep(0.5)
        ravedash::close_alert2(session = session)
      }, add = TRUE, after = TRUE)

      #' Run pipeline without blocking the main session
      #' The trick to speed up is to set
      #' `async=TRUE` will run the pipeline in the background
      #' `shortcut=TRUE` will ignore the dependencies and directly run `names`
      #' `names` are the target nodes to run
      #' `scheduler="none"` will try to avoid starting any schedulers and
      #' run targets sequentially. Combined with `callr_function=NULL`,
      #' scheduler's overhead can be removed.
      #' `type="smart"` will start `future` plan in the background, allowing
      #' multicore calculation
      results <- pipeline$run(
        as_promise = FALSE,
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        async = FALSE,
        shortcut = FALSE,
        names = c(
          'group_data', 'model_results', 'model_fit_statistics', 'post_hoc_results'
        )
      )
      ravedash::logger("Scheduled: ", pipeline$pipeline_name,
                       level = 'debug', reset_timer = TRUE)


      ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
                       level = 'debug')



      # local_data <- list(
      #   results = results
      # )
      #
      local_data$results <- results

      ot_data <- local_data$results$group_data$over_time_by_condition_data

      # update the range of available times
      newly_available_time <- get_recursive_summary(
        ot_data, 'x'
      )
      shiny::updateSliderInput(session = session, inputId = 'over_time_by_condition_plot_range',
                               value = range(newly_available_time),
                               min = min(newly_available_time),
                               max = max(newly_available_time)
      )


      local_reactives$update_outputs <- Sys.time()
      local_reactives$update_3dviewer <- Sys.time()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      # TODO: reset UIs to default

      # get available analysis settings

      aas <- pipeline$read('group_data')$analysis_settings

      aas_labels <- unique(
        unname(apply(aas[,c('label', 'event', 'time', 'frequency')], 1, function(tt) {
          sprintf("%s|%s|%ss|%sHz", tt[1], tt[2], tt[3], tt[4])
        }))
      )

      current_selection <- input$selected_analysis_settings
      shiny::updateSelectInput(inputId = 'selected_analysis_settings',
                               choices = aas_labels, selected = current_selection %OF% aas_labels)


      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )


  # listen to changes in the selected analysis changes
  shiny::bindEvent(
    ravedash::safe_observe({

      # just copy over the current choice, don't do anything unless the user presses RAVE
      local_data$active_analysis_settings <- input$selected_analysis_settings

      # make sure we're only working with current analysis settings
      as_want <- sapply(stringr::str_split(local_data$active_analysis_settings, stringr::fixed('|')), `[[`, 1)
      local_data$active_analysis_settings_clean <- as_want

      ravedash::logger('AS Want: ', paste0(collapse='--', as_want), level='debug')

      adbe <- aas <- pipeline$read('group_data')$all_data_by_el

      # use all_data_by_el because it's smaller than `data`
      as_ind <- adbe$AnalysisLabel %in% as_want

      # update the available condition groups once we have some analysis settings
      potential_vars <- c('Factor1', 'Factor2', 'AnalysisLabel')

      avail_fe <- sapply(potential_vars, function(pv) {
        re <- NULL
        if(!is.null(adbe[[pv]])) {

          ravedash::logger('Looking for ', pv)

          k <- sum(0 < table(adbe[[pv]][as_ind]))

          if(k > 1) {
            re <- pv
          }
        }
        re
      }, USE.NAMES = FALSE) %>% unlist

      ravedash::logger('Found FEs: ', paste0(collapse='--', avail_fe), level='debug')

      current_fe <- input$selected_fixed_effects

      current_fe <- avail_fe[avail_fe %in% current_fe]

      sel <- if(length(current_fe) > 0) {
        current_fe
      } else {
        avail_fe[[1]]
      }

      shiny::updateSelectInput(inputId = 'selected_fixed_effects', choices = avail_fe, selected = sel)

    }), input$selected_analysis_settings, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # changes to over_time_by_condition plot options
  shiny::bindEvent(
    ravedash::safe_observe({
      po <- pe_graphics_settings_cache$get('over_time_by_condition_plot_options')

      po$condition_switch = input$over_time_by_condition_switch
      po$plot_range <- input$over_time_by_condition_plot_range

      pe_graphics_settings_cache$set('over_time_by_condition_plot_options', po)

      local_reactives$update_over_time_plot = Sys.time()

    }), input$over_time_by_condition_switch, input$over_time_by_condition_plot_range,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Register outputs
  ravedash::register_output(
    outputId = "brain_viewer",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({

      ravedash::logger('Trying to update 3dViewer', level='debug')

      force(local_reactives$update_3dviewer)

      # build template brain
      # local_data$active_analysis_settings <- aas_labels[[1]]
      data <- local_data$results$group_data

      shiny::validate(shiny::need(
        !is.null(data), label = 'Group data'
      ))

      shiny::validate(shiny::need(
        nrow(data$univariate_results) > 0, message = 'No univariate data found'
      ))

      # analysis settings we want, use sapply here because there may be more than 1!
      as_want <- local_data$active_analysis_settings_clean #sapply(stringr::str_split(local_data$active_analysis_settings, stringr::fixed('|')), `[[`, 1)
      as_dont_want <- unique(data$analysis_settings$label[! data$analysis_settings$label %in% as_want])

      # columns_needed <- stringr::str_detect(colnames(data$univariate_results), as_dont_want)

      as_dont_want <- c(as_dont_want, 'overall', 'currently_selected')

      columns_to_keep = !apply(
        MARGIN = 1, FUN = any,
        sapply(as_dont_want, stringr::str_detect, string=colnames(data$univariate_results))
      )

      subjects_needed <- unique(data$univariate_results$Subject)

      all_brains <- dipsaus::lapply_async2(subjects_needed, function(sbj) {
        subject <- raveio::RAVESubject$new(
          project_name = data$univariate_results$Project[1],
          subject_code = sbj
        )

        els_needed <- unique(subset(data$univariate_results, Subject == sbj)$Electrode)

        # here we want to show only electrodes that are available in the given
        # pipeline
        br <- raveio::rave_brain(subject)

        rt <- br$electrodes$raw_table
        rt <- subset(rt, Electrode %in% els_needed)

        br$electrodes$set_electrodes(rt)

        br$electrodes$set_values(
          data$univariate_results[data$univariate_results$Subject==sbj,columns_to_keep]
        )

        return(br)
      })

      brain <- threeBrain::merge_brain(
        .list = all_brains,
        template_subject = ravepipeline::raveio_getopt("threeBrain_template_subject")
      )

      if(is.null(brain)) {
        return(threeBrain::threejs_brain(title = "No 3D model found"))
      }

      if(FALSE) {

        # ravedash::logger(dput(local_data$results$omnibus_results$stats), level='debug')

        # df <- data.frame(t(local_data$results$omnibus_results$stats))

        # fix some column names
        # Avoid changing `df` multiple times
        # names(df) = stringr::str_replace_all(names(df), '\\.\\.\\.', ' vs ')
        cnames <- stringr::str_replace_all(names(df), '\\.\\.\\.', ' vs ')

        # names(df) = stringr::str_replace_all(names(df), '\\.', ' ')
        cnames <- stringr::str_replace_all(cnames, '\\.', ' ')

        # names(df) = stringr::str_replace_all(names(df), '\\ $', '')
        cnames <- stringr::str_replace_all(cnames, '\\ $', '')
        names(df) <- cnames

        df$Electrode = as.integer(rownames(df))
        res <- build_palettes_and_ranges_for_omnibus_data(df)

        brain$set_electrode_values(df)

        brain$render(outputId = "brain_viewer", session = session,
                     palettes=res$palettes, value_ranges=res$val_ranges,
                     control_display = FALSE, side_display=FALSE,
                     timestamp=FALSE)
      }

      brain$render(outputId = "brain_viewer", session = session,
                   # palettes=res$palettes, value_ranges=res$val_ranges,
                   control_display = FALSE, side_display=FALSE,
                   timestamp=FALSE)
    })
  )


  ravedash::register_output(
    outputId = "over_time_by_condition",
    render_function = shiny::renderPlot({
      force(local_reactives$update_outputs)
      force(local_reactives$update_over_time_plot)

      data <- local_data$results$group_data

      shiny::validate(shiny::need(
        !is.null(data), label = 'Group data'
      ))

      # force(local_reactives$update_line_plots)
      # force(local_reactives$update_over_time_plot)

      # make sure we're only showing certain analysis settings

      as_want <- local_data$active_analysis_settings_clean

      for(ii in seq_along(data$over_time_by_condition_data)) {
        not_wanted <- which(! (names(data$over_time_by_condition_data[[ii]]) %in% as_want))
        if(any(not_wanted)) {
          data$over_time_by_condition_data[[ii]][not_wanted] = NULL
        }
      }

      plot_over_time_by_condition(over_time_by_condition_data = data$over_time_by_condition_data,
        plot_options = pe_graphics_settings_cache$get('over_time_by_condition_plot_options')
      )
    })
  )

}
