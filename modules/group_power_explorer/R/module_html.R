

module_html <- function(){

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,
            ravedash::input_card(
              title = "Analysis Settings",class_header = "shidashi-anchor",

              shiny::p("Select from available analysis settings. Compatible settings will be merged, incompatible settings will be treated as estimable terms in the model."),
              shiny::selectInput(ns('selected_analysis_settings'), label='Available settings', multiple = TRUE,
                                 choices = c()),

              shiny::checkboxInput(ns('enable_custom_subject_variables'), 'Add participant-level variables', value = FALSE),
              shiny::conditionalPanel(
                condition = 'input["group_power_explorer-enable_custom_subject_variables"] == 1',
                shiny::hr(),
                shiny::p("Select participant level variables for between-subject groupings/covariates"),
                shiny::selectInput(ns('selected_participant_variables'), label='Available patient variables', multiple = TRUE, choices = c()),
                shiny::fileInput(ns('upload_more_participant_variables'), label='Uploaded additional patient-level data', multiple = FALSE)
              ),

              shiny::checkboxInput(ns('enable_custom_electrode_variables'), 'Add electrode-level variables', value = FALSE),
              shiny::conditionalPanel(
                condition = 'input["group_power_explorer-enable_custom_electrode_variables"] == 1',
                shiny::hr(),
              shiny::p("Select electrode-level variables to create ROI-style groupings/covariates"),
              shiny::selectInput(ns('selected_roi_variables'), label='Available electrode variables', multiple = TRUE, choices = c()),
              shiny::fileInput(ns('upload_more_roi_variables'), label='Uploaded additional electrode-level data', multiple = TRUE)
              ),

              shiny::hr(),
              shiny::p("Select from trial-level variables built with Power Explorer"),
              shiny::selectInput(ns('selected_fixed_effects'), label='Select condition variables', multiple = TRUE, choices = c()),
              shiny::fileInput(ns('upload_more_fixed_effects'), label='Uploaded additional trial-level data', multiple = TRUE)
            ),
            ravedash::input_card(
              title = "Model specification", class_header = "shidashi-anchor",

              dipsaus::actionButtonStyled(ns('btn_magic_model'), label = 'Auto', icon = ravedash::shiny_icons$magic, type = 'info'),

              shiny::selectInput(ns('model_function'), "Model type",
                                 choices=c('linear model (lm)', 'linear mixed-effects model (lmer)', 'generalized lme (glmer)'),
                                 selected = 'linear mixed-effects model (lmer)'),

              shiny::textInput(ns('model_formula'), "Model formula", placeholder = 'y ~ Factor1 + (1|Subject/Electrode)')
            ),
            ravedash::input_card(
              title = "Post-hoc tests", class_header = "shidashi-anchor",

              dipsaus::actionButtonStyled(ns('btn_magic_posthoc'), label = 'Auto', icon = ravedash::shiny_icons$magic, type = 'info'),

              shiny::selectInput(ns('posthoc_function'), "Post hoc type",
                                 choices=c('All pairwise comparisons', 'Stratify by patient type', 'Stratify by electrode type',
                                           'Stratify by analysis type'),
                                 selected = 'All pairwise comparisons'),

              shiny::textInput(ns('posthoc_formula'), "Post-hoc formula", placeholder = 'pairwise ~ Factor1 | AnalysisLabel')
            )
          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,

            # ---- Output tab-set: Brain Viewers -------------------------------
            ravedash::output_cardset(
              inputId = ns('brain_viewers'), title = 'Brain Viewers',
              class_body = "no-padding min-height-500 height-500 resize-vertical",
              tools = list(
              ),
              append_tools = FALSE,
              `Results Viewer` =shiny::div(class='position-relative fill',
                                           ravedash::output_gadget_container(
                                             threeBrain::threejsBrainOutput(
                                               outputId = ns("brain_viewer"), height = "100%"
                                             )
                                           )
              )
            ),
            # ---- Output tab-set: Over Time -----------------------------------
            ravedash::output_cardset(
              inputId = ns('over_time_tabset'),
              title='Over Time',
              class_body="no-padding fill-width",
              append_tools = FALSE,
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("over_time_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("over_time_tabset_camera")
                )
              ),

              # ---- Output tab: Over Time > By Condition ----------------------
              `By Condition` = shiny::tagList(
                shiny::div(
                  # opens a fluid container
                  class = "container-fluid",
                  shiny::conditionalPanel(
                    condition = "input['over_time_tabset_config']%2 == 1",
                    ns = ns,
                    shiny::fluidRow(
                      shiny::column(
                        width = 3L,
                        shiny::selectInput(
                          inputId = ns('over_time_by_condition_switch'),
                          label='Plot type', selected = 'Combine conditions',
                          choices = c('Combine conditions', 'Combine events',
                                      'Combine all', 'Separate all')
                        )),
                      shiny::column(offset = 1,
                                    width = 4L,
                                    shiny::sliderInput(
                                      inputId = ns('over_time_by_condition_plot_range'),
                                      label='Plot range', value = c(0,1),
                                      min =0, max=1, step = 0.01, dragRange = TRUE
                                    )
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_condition'),
                      min_height = 400)
                  )
                )
              ),
              `By Electrode` = shiny::tagList(
                shiny::div(
                  class = "fill-width no-padding min-height-400 resize-vertical",
                  # make_heatmap_control_panel(prefix = 'otbt', config = 'over_time_tabset_config'),
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_electrode'),
                      min_height = 400)
                  )
                )
              )
            ),

            # ---- Output tab-set: By Condition ------------------------------------
            ravedash::output_cardset(
              inputId = ns('by_condition_tabset'),
              title='By Condition',
              class_body='',
              append_tools = FALSE,
              tools = list(
                clipboardOutput(outputId=ns('by_condition_tabset_clipboard'), as_card_tool = TRUE, message='copy data'),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("by_condition_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("by_condition_tabset_camera")
                )
              ),
              `By Trial` = #shiny::tagList(
                shiny::div(
                  # id='makeinline',
                  # class='height-400 fill-width container-fluid',
                  # class = "no-padding",
                  {shiny::conditionalPanel(
                    "input['group_power_explorer-by_condition_tabset_config'] %2 == 1",
                    shiny::fluidRow(
                      shiny::column(
                        width=2, shiny::selectInput(ns('btp_basic_unit'),
                                                    label = 'Points are: ',
                                                    choices=c('Trials', 'Electrodes'))
                      ),
                      shiny::column(width=1, style='text-align: left; margin-top:37px; margin-left:0px',
                                    shiny::checkboxInput(ns('bcbt_show_outliers'), "Show Outliers", value = TRUE)
                      ),
                      shiny::column(
                        width=3,
                        shiny::selectInput(ns("btp_types"), label = 'Plot types',
                                           multiple = TRUE,
                                           choices = c('jitter points', 'means', 'ebar polygons', 'sd polygons',
                                                       'points', 'connect points',
                                                       'densities', 'density polygons',
                                                       'bars', 'borders', 'ebars'),
                                           selected=c('jitter points', 'means', 'ebar polygons')
                        )),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_xvar"),
                                                       "X-axis", choices=c('First Factor',
                                                                           'Analysis Group'))
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_gvar"),
                                                       "Group by", choices=c('none', 'Analysis Group', 'First Factor')),
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_panelvar"),
                                                       "Panel by", choices=c('none', 'Analysis Group',
                                                                             'First Factor'))
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(width=3,
                                    shiny::numericInput(ns('btp_pt.alpha'), 'Point alpha (opacity)', value = 100, min=0, max=100)
                      ),
                      shiny::column(width=3,
                                    shiny::numericInput(ns('btp_pt.cex'), 'Point scaling', value = 1, min=0.1, max=10, step = .1)
                      ),
                      shiny::column(width=3,
                                    shiny::numericInput(ns('scale_pbtbc'), 'Plot width scaling', value = 1, min=0.1, max=10, step = .1)
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_highlight_clicks"), selected='labels',
                                                       "Highlight Clicks", choices=c('points', 'lines',
                                                                                     'labels'), multiple = TRUE)
                      ),
                      shiny::column(width=1,
                                    shiny::selectInput(ns("btp_highlight_text_location"), selected='top',
                                                       "Highlight pos", choices=c('center', 'bottom', 'left', 'top', 'right'))
                      )
                    )
                  )},
                  shiny::fluidRow(
                    shiny::column(width=7,
                                  # shiny::div(class='col-sm-8',
                                  # ravedash::output_gadget_container(
                                  ravedash::plotOutput2(outputId = ns('by_condition_by_trial'),
                                                        click = shiny::clickOpts(ns('btbc_click'), clip=TRUE),
                                                        dblclick =ns('btbc_dblclick')
                                                        # hover=ns('btbc_hover')
                                  ),
                    ),
                    #),
                    shiny::column(width=5,
                                  # shiny::div(class='col-sm-4',
                                  # ravedash::output_gadget_container(
                                  DT::dataTableOutput(outputId = ns('by_condition_by_trial_clicks'))
                                  # )
                    )
                    # )
                  )
                ),
              # ), # end of By trial tag list
              `Overall model test` = shiny::div(style='margin:20px', class="",
                                                shiny::htmlOutput(ns('by_condition_statistics'))
              ),
              `Conditions vs. Baseline` = shiny::div(style='margin:20px',
                                                     class="",
                                                     shiny::fluidRow(
                                                       shiny::column(width = 4,
                                                                     shiny::selectInput(ns('bcs_choose_emmeans'),
                                                                                        "Which means to display?", choices=c('All possible')))
                                                     ),

                                                     shiny::htmlOutput(ns('by_condition_statistics_emmeans'))
              ),
              `Pairwise comparisons` = shiny::div(style='margin:20px',
                                                  class="",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 5,
                                                                  shiny::selectInput(ns('bcs_choose_contrasts'),
                                                                                     "Which contrasts to display?", selected = 'All-possible pairwise',
                                                                                     choices=c('All-possible pairwise',
                                                                                               'Stratified contrasts (more power!)',
                                                                                               'ITX Contrasts (diff of diff)')
                                                                  )
                                                    ),
                                                    shiny::column(width=4,
                                                                  shiny::conditionalPanel('input["power_explorer-bcs_choose_contrasts"] != "All-possible pairwise"',
                                                                                          shiny::selectInput(ns('bcs_choose_specific_contrast'),
                                                                                                             'Choose layer/grouping', choices='')))
                                                  ),
                                                  shiny::htmlOutput(ns('by_condition_statistics_contrasts'))
              )
            )

          )
        )
      )
    )
  )
}
