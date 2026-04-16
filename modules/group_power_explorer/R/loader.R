# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){


  project_choices <- raveio::get_projects(refresh = FALSE)

  last_used_project <- pipeline$get_settings('project_name', project_choices[1])


  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = 'max-width:1444px;',
    input_ui = {
      # project & subject
      ravedash::input_card(
        title = "Data Selection",
        class_header = "",

        # ravedash::flex_group_box(
        # title = "",

        # shidashi::flex_item(
        ravedash::flex_group_box(
          title = 'Find pipelines',
          shidashi::flex_item(
            shiny::selectInput(inputId = ns('project_name'), label = 'Project to analyze', choices = project_choices,
                               selected = last_used_project, multiple = FALSE)

          ),
          shidashi::flex_break(),
          shidashi::flex_item(
            shiny::actionButton(ns('scan_for_pipelines'),
                                label = 'Scan for pipelines', icon = ravedash::shiny_icons$check)
          ),
          shidashi::flex_break(),

          shidashi::flex_item(
            # shiny::p("If multiple pipelines with the same label are found, the pipeline with the most recent export data will be used."),
            shiny::p("You can multi-select pipelines and we'll do our best to merge them."),
            shiny::p("Pipeline labels are listed alongside the number of subjects with each label")
            # add in a "Strict" checkbox here that ensure settings are comparable
          ),
          shidashi::flex_break(),
          shidashi::flex_item(
            shiny::selectInput(inputId = ns('pipeline_to_load'), label = 'Pipeline to load',
                               choices = character(),
                               selected = character(), multiple = TRUE)

          )
        ),
        ravedash::flex_group_box(
          title = 'Template brain',
          shidashi::flex_item(
            dipsaus::actionButtonStyled(inputId = ns('refresh_loader_viewer'),
                                        label = 'Load/Refresh Brain', type='info')
          )
        ),

        footer = shiny::tagList(
          dipsaus::actionButtonStyled(
            inputId = ns("loader_ready_btn"),
            label = "Load data",
            type = "primary",
            width = "100%"
          )
        )

      )
    },
    output_ui = {
      ravedash::output_card(
        title = "3D Viewer",
        class_body = "no-padding min-height-650 height-650",
        ravedash::output_gadget_container(
          threeBrain::threejsBrainOutput(
            outputId = ns("loader_brain_viewer"), height = "100%"
          )
        )

      )
    }
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  local_data <- fastmap::fastmap()

  local_data$mset(
    'pipeline_table' = list(),
    'project_name' = '',
    'selected_pipelines' = c()
  )

  local_reactives <- shiny::reactiveValues(
    current_project_name = NULL,
    pipeline_table_updated = NULL,
    update_3dviewer = NULL
  )

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      # settings <- component_container$collect_settings(
      #   ids = c(
      #     "loader_project_name",
      #     "loader_subject_code",
      #     "loader_electrode_text",
      #     "loader_epoch_name",
      #     "loader_reference_name"
      #   )
      # )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = local_data$as_list())

      # --------------------- Run the pipeline! ---------------------

      # Calculate the progress bar
      # tarnames <- pipeline$target_table$Names
      # count <- length(tarnames) + length(dipsaus::parse_svec(loader_electrodes$current_value)) + 4

      # Pop up alert to prevent user from making any changes (auto_close=FALSE)
      # This requires manually closing the alert window
      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "..."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      # Run the pipeline target `repository`
      # Use `as_promise=TRUE` to make result as a promise
      res <- pipeline$run(
        as_promise = TRUE,
        names = "group_data",
        scheduler = "none",
        type = "smart",  # parallel
        # async = TRUE,
        callr_function = NULL,
        progress_quiet = TRUE
      )

      # The `res` contains a promise that might not have finished yet,
      # so register functions to run when the promise is resolved
      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravedash::logger("Data has been loaded loaded")

          # Close the alert
          dipsaus::close_alert2()
        },


        # this is what should happen when pipeline fails
        onRejected = function(e){

          # Close the alert
          dipsaus::close_alert2()

          # Immediately open a new alert showing the error messages
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the data:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )
    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # listen to updates on the project selector
  shiny::bindEvent(
    ravedash::safe_observe({

      ravedash::logger("Project name is: ", input$project_name)
      local_reactives$current_project_name = input$project_name

      local_data$set('project_name', local_reactives$current_project_name)

      # get available pipelines and have a progress bar
      prog <- raveio::progress_with_logger("Looking for subjects...", max = 2)

      prog$inc('loading project details')
      rp <- raveio::as_rave_project(local_reactives$current_project_name)

      subjects <- rp$subjects()
      prog$inc('Enumerating subjects')
      prog$close()

      prog <- raveio::progress_with_logger("Scanning for pipelines",
                                           max = length(subjects))

      on.exit({
        prog$close()
      })

      re <- dipsaus::lapply_async2(subjects, function(subject_code) {

        subject <- raveio::RAVESubject$new(project_name = rp$name,
                                           subject_code = subject_code, strict = FALSE)
        prog$inc(detail = subject$subject_code)
        subject$list_pipelines(pipeline_name = 'power_explorer',
                               check = TRUE, all = FALSE)
      })

      local_data$set('pipeline_table', value = data.table::rbindlist(re))

      local_reactives$pipeline_table_updated <- Sys.time()

    }),
    input$scan_for_pipelines, ignoreNULL=TRUE, ignoreInit=TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({

      pipe_table <- local_data$get('pipeline_table')

      if(!data.table::is.data.table(pipe_table) || nrow(pipe_table) < 1) {
        dipsaus::shiny_alert2(title = 'No pipelines found', text = "Try another project", icon="warning")
      } else {
        ch <- table(pipe_table$label)
        vals <- paste0(paste(names(ch), ch, sep=' ('), ')')
        shiny::updateSelectInput(inputId = 'pipeline_to_load', choices=vals, selected=vals[1])
      }

    }), local_reactives$pipeline_table_updated, ignoreNULL=TRUE, ignoreInit=TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      selected_pipelines <- input$pipeline_to_load

      if(is.null(selected_pipelines) || !nzchar(selected_pipelines[[1]])) {
        local_data$set('selected_pipelines', c())

      } else {
        # detect location of last space in the name, should be the one we added
        ind <- unlist(lapply(stringr::str_locate_all(selected_pipelines, stringr::fixed(' ')), function(ss) {
          ss[nrow(ss),1]
        }))

        clean_names <- mapply(function(nm, len) {
          substr(nm, 1, len-1)
        }, selected_pipelines, ind)

        ravedash::logger('setting pipeline names: ', paste0(collapse=',', clean_names), level='debug')
        local_data$set('selected_pipelines', clean_names)
      }


    }), input$pipeline_to_load, ignoreNULL = TRUE, ignoreInit = TRUE
  )




  shiny::bindEvent(
    ravedash::safe_observe({

      local_reactives$update_3dviewer <- Sys.time()

    }), input$refresh_loader_viewer, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  ravedash::register_output(
    outputId = "loader_brain_viewer",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({

      force(local_reactives$update_3dviewer)

      pipe_table <- local_data$get('pipeline_table')

      if(data.table::is.data.table(pipe_table) && nrow(pipe_table) > 0) {

        selected_pipelines <- local_data$get('selected_pipelines')

        subjects_needed <- subset(pipe_table, label %in% selected_pipelines)$subject

        if(length(subjects_needed) > 0) {

          prog <- raveio::progress_with_logger("Loading template brain...", max = 2*length(subjects_needed))
          on.exit({prog$close()})

          all_brains <- lapply(subjects_needed, function(sbj) {
            subject <- raveio::RAVESubject$new(
              project_name = local_data$get('project_name'),
              subject_code = sbj
            )
            prog$inc(sprintf('loading pipeline %s', sbj))

            pipe_table_row <- subset(pipe_table, subject == sbj)[1,,drop=FALSE]

            pipe_path <- file.path(subject$pipeline_path, pipe_table_row$pipeline_name,
                                   pipe_table_row$directory
            )

            pipe_result <- ravepipeline::pipeline_from_path(pipe_path)
            ga <- pipe_result$read('data_for_group_analysis')
            els_needed <- unique(ga$omnibus_data$Electrode)

            # here we want to show only electrodes that are available in the given
            # pipeline
            prog$inc(sprintf('building brain %s', sbj))
            br <- raveio::rave_brain(subject)

            rt <- br$electrodes$raw_table
            rt <- subset(rt, Electrode %in% els_needed)

            br$electrodes$set_electrodes(rt)

            return(br)
          })
          prog$close()
          on.exit({})

          template <- threeBrain::merge_brain(
            .list = all_brains,
            template_subject = ravepipeline::raveio_getopt("threeBrain_template_subject")
          )

          template$plot(
            control_display = FALSE, side_display=FALSE,
            timestamp=FALSE)

        } else {
          return(threeBrain::threejs_brain(title = "Could not build template brain"))
        }
      } else {
        return(threeBrain::threejs_brain(title = "Could not build template brain"))
      }
    })
  )
}
