#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#
rm(list=ls())
library(shiny)
seed = 12345
set.seed(seed)

#### Load required packages #### 
library(tidyverse)
library(reshape2)   # For melting data
library(ggrepel)    # For plotting
library(gridExtra)  # For plotting
library(ellipse)    # For plotting
library(scales)     # For dollar signs and commas
library(patchwork)  # For combining ggplot2 figures
library(dampack)    
library(darthtools) 
library(EnvStats)
library(truncnorm)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(bslib)
library(writexl)
library(readxl)
library(knitr)
library(kableExtra)
library(shinycssloaders)


#### Load supplementary functions #### 
source("allfuns_final.R", local = TRUE)
source("he_model_acc.R") #modifiable inputs for HE model
source("bi_model_acc.R") #modifiable inputs for BIA model
source("eg_model_acc.R") #modifiable inputs for workload calculations


ui <- page_navbar(
  theme = bs_theme(bootswatch = "flatly", bg = "#E5F0FB", primary = "#021122", secondary = "#6C88A5",
                   success = "#5e7b9a", font_scale = NULL,
                   fg = "#000"),
  id = "nav",
  tags$style(HTML("
    .navbar {
      min-height: 40px; /* Reduce height of the navbar */
    }
    .navbar-nav > li > a, .navbar-brand {
      padding-top: 5px;
      padding-bottom: 5px;
      height: 30px; /* Adjust link and brand height */
      line-height: 30px; /* Align text vertically */
    }
  ")),
  nav_panel(title = tags$h5(tags$b("Information and instructions")),
            
            # Wrap image and text inside a flex container
            div(
              style = "display: flex; align-items: center;",
              
              # Image
              tags$img(
                src = "box_image.jpg",
                width = "70px",
                height = "auto",
                class = "me-3",  
                alt = "JHlogo"
              ),
              
              # Text
              tags$h4(
                style = "margin-left: 10px;",  
                tags$b("CVRM-Box economic model for implementation in Dutch primary care")
              )
            ),
            card(
              card_header(
                class= "bg-light bg-gradient bg-opacity-75",
                tags$h6(tags$b("Model overview and instructions for use"))
              ),
              full_screen = TRUE,
              card_body(
                style = "background-color: white;",
                tags$p("This is a dedicated app intended for users and decision-makers to interact with and modify the economic model in the publication:"), 
                tags$p(tags$a(href="https://www.google.com",
                              icon("mouse-pointer", style="color:#2196c4"),
                              tags$strong("[pending: Reference to future manuscript]",
                                          style="color:#2196c4"),
                              target="_blank")
                ),
                p("Note: the user interface works best on a desktop as opposed to a smartphone."),
                tags$p("We only provide a brief description of the economic model below. Therefore, please read the manuscript before interacting with this app as it contains important information necessary to understand how/which modifications should be made and interpreted."),
                tags$p(tags$strong("Model structure")),
                p("A Markov model (Figure below) was used to simulate the health states and transitions patients may experience during their modelled lifetime. Three cardiovascular events were modelled: non-fatal MI, non-fatal stroke, or fatal cardiovascular event. Additionally, the ‘Post-recurrent’ states were included to account for the greater risk and consequences of recurrent non-fatal events. As the costs and burden of stroke are generally greater than those of MI, the model does not allow for transitions from stroke to MI states. Finally, patients could experience a fatal cardiovascular event or die of other causes. Patients with no CVD history enter the model in the ‘At risk’ state, while patients with a CVD history enter the model in the post-MI (64%) and post-stroke (36%) states."),
                p("The effect of CVRM-Box was modelled as a reduction in the absolute risk of MACE (i.e., lower transition probabilities) resulting from an expected reduction in BP and smoking with the intervention (Table 1). The reduction in BP was equivalent to the adjusted difference in BP estimated in the matched prospective study of CVRM-Box (manuscript Table 1 and supplementary eTable 1). Smoking cessation was assumed at 13% of patients with a wide credible interval of 2%-18%."),
                img(src="model_diagram.jpg", width = 700, height = 200),
                tags$p(tags$strong("Interventions")),
                p("CVRM-Box"), 
                p("Eligible patients undergo a consultation with an assistant who introduces “The Box”, which includes a wireless BP monitor, a smart body scale analyzer, and an activity tracker. During this session, patients receive training on device usage and are introduced to a dedicated lifestyle app. Patients conduct weekly independent BP and weight measurements, and receive guidance on interpreting their results and customizing their weight and activity goals. Measurements automatically appear in the lifestyle app and are transmitted to the GPs' electronic records every three months for review by the PN. Yearly check-ups at the GP include a local BP measurement and laboratory examination. Extra check-ups are scheduled when necessary."), 
                p("Care as usual (CaU)"),  
                p("CaU was defined as the current cardiovascular risk management practice within the primary integrated care programs for individuals with increased cardiovascular risk or a history of CVD. This protocolized program focuses on lifestyle modifications, the treatment of BP and cholesterol levels, and is primarily conducted by the PN. Yearly check-ups at the GP include a local BP measurement and laboratory examination. Most visits (95%) are with the PN, while some visits (5%) take place with the GP when necessary."),
                br(),
                tags$b("Aknowledgements"),
                p("We would like to aknowledge Alarid-Escudero F, Krijkamp E, Enns EA, et al. for open-sourcing their work (referenced below), which helped us develop the economic model."),
                p(tags$a(href="https://journals.sagepub.com/doi/10.1177/0272989X221121747",
                         icon("mouse-pointer", style="color:#2196c4"),
                         tags$strong("Alarid-Escudero F, Krijkamp E, Enns EA, et al. A Tutorial on Time-Dependent Cohort State-Transition Models in R Using a Cost-Effectiveness Analysis Example. Medical Decision Making. 2023;43(1):21-41.",
                                     style="color:#2196c4"),
                         target="_blank")
                ),
                p("We would also like to aknowledge Smith and Schneider for open-sourcing their work (referenced below), which we used to develop this Shiny interface."),
                p(tags$a(href="https://wellcomeopenresearch.org/articles/5-69/v2",
                         icon("mouse-pointer", style="color:#2196c4"),
                         tags$strong("Smith RA and Schneider PP. Making health economic models Shiny: A tutorial [version 2; peer review: 2 approved]. Wellcome Open Res 2020, 5:69",
                                     style="color:#2196c4"),
                         target="_blank")
                ),
                br(),
                tags$p("Code for this model can be downloaded from Github repository by clicking",
                       tags$a(href="https://github.com/jmheij/CVRM-box-cua",
                              "here.",
                              style="color:#2196c4"),
                       target="_blank"),
                br(),
                p("THIS SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."),
              )
            )
  ),
  nav_panel(title = tags$h5(tags$strong("Cost-effectiveness Model Results")), 
            layout_sidebar(
              sidebar = sidebar(
                he_model_acc,
                title="Controls",
                width=300
              ),
              p("Please ensure you have set the desired model specifications on the sidebar. Then, click 'Run model'. The calculation may take some time."),
              actionButton(inputId = "run_model", label   = "Run model"),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header(
                    class= "bg-light bg-gradient bg-opacity-75",
                    tags$h6("Results Figures")
                  ),
                  full_screen = TRUE,
                  card_body(
                    style = "background-color: white;",
                    p(tags$strong("Cost-effectiveness acceptability curve")),
                    imageOutput(outputId = 'SO_ceac'),
                    p(tags$strong("Cost-effectiveness plane")),
                    imageOutput(outputId = 'SO_scatter')#,
                  )
                ),
                card(
                  card_header(
                    class= "bg-light bg-gradient bg-opacity-75",
                    tags$h6("Results Tables")
                  ),
                  full_screen = TRUE,
                  card_body(
                    style = "background-color: white;",
                    p(tags$strong("Summary of Results")),
                    tableOutput(outputId = "SO_icer_table"),
                    hr(),
                    p(tags$strong("Life expectancy (LE) and costs of added life years")),
                    tableOutput(outputId = "SO_LEs"),
                    textOutput(outputId = "SO_added_LE_costs")
                  )
                )
              )
            ),
  ),
  nav_panel(title = tags$h5(tags$strong("Budget impact analysis")), 
            layout_sidebar(
              sidebar = sidebar(
                actionButton(inputId = "run_BI_model", label = "Run BI model"),
                bi_model_acc,
                title="Controls",
                width=300
              ),
              card(
                height = "40%",
                full_screen = TRUE,
                card_header(
                  class= "bg-light bg-gradient bg-opacity-75",
                  tags$h6("Population size settings")
                ),
                card_body(
                  style = "background-color: white;",
                  p("First, expand this card (see bottom right of this cell)."),
                  p("Below, set/modify the eligible population sizes for each year that are assumed for regional and national level rollout."),
                  p("Note that depending on the subgroup you choose in the sidebar, you should update the below numbers accordingly."),
                  fluidRow(
                    column(6,
                           tags$b("Regional Scaling Perspective"),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 1", style = "margin-right: 10px;"),
                             numericInput("SI_BI_reg_yr1", label = NULL, value = round(15170*0.55,0), min = 0, max = 10000000, step = 1)
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 2", style = "margin-right: 10px;"),
                             numericInput("SI_BI_reg_yr2", label = NULL, value = round(910*0.55,0), min = 0, max = 10000000, step = 1)
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 3", style = "margin-right: 10px;"),
                             numericInput("SI_BI_reg_yr3", label = NULL, value = round(960*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 4", style = "margin-right: 10px;"),
                             numericInput("SI_BI_reg_yr4", label = NULL, value = round(713*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 5", style = "margin-right: 10px;"),
                             numericInput("SI_BI_reg_yr5", label = NULL, value = round(895*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           )
                    ),
                    column(6,
                           tags$b("National Scaling Perspective"),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 1", style = "margin-right: 10px;"),
                             numericInput("SI_BI_nat_yr1", label = NULL, value = round(3486930*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 2", style = "margin-right: 10px;"),
                             numericInput("SI_BI_nat_yr2", label = NULL, value = round(206350*0.55,0), min = 0, max = 10000000, step = 1)
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 3", style = "margin-right: 10px;"),
                             numericInput("SI_BI_nat_yr3", label = NULL, value = round(227277*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 4", style = "margin-right: 10px;"),
                             numericInput("SI_BI_nat_yr4", label = NULL, value = round(163890*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           ),
                           div(
                             style = "display: flex; align-items: center;",
                             tags$label("Year 5", style = "margin-right: 10px;"),
                             numericInput("SI_BI_nat_yr5", label = NULL, value = round(207587*0.55,0), min = 0, max = 10000000, step = 1)
                             
                           )
                    )
                  ),
                  p("When ready, click 'Run BI model' on the sidebar")
                )
              ),
              card(
                height = "60%",
                full_screen = TRUE,
                card_header(
                  class= "bg-light bg-gradient bg-opacity-75",
                  tags$h6("Summary of Budget Impact Analysis Results")
                ),
                card_body(
                  style = "background-color: white;",
                  p("The results for the specified budget impact model are shown below. The calculation may take some time."),
                  fluidRow(
                    column(6,
                           plotOutput(outputId = "SO_BI_reg_plot")),
                    column(6,
                           plotOutput(outputId = "SO_BI_nat_plot"))
                  )
                )
              )
            )
  ),
  nav_panel(title = tags$h5(tags$strong("Workload reduction")), 
            layout_sidebar(
              sidebar = sidebar(
                eg_model_acc,
                title="Controls",
                width=360
              ),
              card(
                height = "20%",
                full_screen = TRUE,
                card_header(
                  class= "bg-light bg-gradient bg-opacity-75",
                  tags$h6("Instructions")
                ),
                card_body(
                  style = "background-color: white;",
                  p("Please refer to the manuscript text and supplementary eMethods to understand how workload reductions were calculated. The results shown in this tab are per general practitioner (GP) and practice nurse (PN) in a 'reference primary care practice' in the Netherlands, with an eligible CVD population of n=231 patients. Here, users may modify these and other assumptions (sidebar).")
                )
              ),
              card(
                height = "80%",
                full_screen = TRUE,
                card_header(
                  class= "bg-light bg-gradient bg-opacity-75",
                  tags$h6("Calculation Results")
                ),
                card_body(
                  style = "background-color: white;",
                  tags$style(HTML("
    #SO_EG_table table {
      background-color: white !important;
    }
    #SO_EG_table th, 
    #SO_EG_table td {
      background-color: white !important; /* Override inherited background color */
    }
    #SO_EG_table {
      background-color: white !important;
      padding: 10px;
    }
  ")),
                  uiOutput(outputId = "SO_EG_table")
                )
              )
            )
  )
)


#### Define server ####
server <- function(input, output){  
  #bs_themer()
  # when action button pressed ...
  observeEvent(input$run_model,
               ignoreNULL = T, 
               {
                 showPageSpinner(type=1)
                 # Run model function with Shiny inputs
                 reslist = f_wrapper(
                   n_age_init = input$SI_n_age_init, 
                   perspective = input$SI_perspective, 
                   prop_male = input$SI_prop_male, 
                   sbp_diff_controlled = rnormA(n=1000, mean = input$SI_sbp_diff_controlled, sd = input$SI_sbp_diff_controlled_SE), 
                   sbp_diff_uncontrolled = rnormA(n=1000, mean = input$SI_sbp_diff_uncontrolled, sd = input$SI_sbp_diff_uncontrolled_SE), 
                   c_init_strA = input$SI_c_device_strA + input$SI_c_device_shipping + input$SI_c_personnel_strA,
                   c_strA_controlled = rgammaA(1000, mean=input$SI_c_strA_controlled_mean, sd= (input$SI_c_strA_controlled_mean*0.25)),
                   c_strA_uncontrolled = rgammaA(1000, mean=input$SI_c_strA_uncontrolled_mean, sd= (input$SI_c_strA_uncontrolled_mean*0.25)),
                   c_SOC_controlled = rgammaA(1000, mean=input$SI_c_SOC_controlled_mean, sd= (input$SI_c_SOC_controlled_mean*0.15)),
                   c_SOC_uncontrolled = rgammaA(1000, mean=input$SI_c_SOC_uncontrolled_mean, sd= (input$SI_c_SOC_uncontrolled_mean*0.15)),
                   subgroup = input$SI_subgroup,
                   year_reduction_effect = input$SI_year_reduction_effect,
                   percentage_reduction_effect = input$SI_percentage_reduction_effect/100,
                   d_e = input$SI_d_e/100,
                   d_c = input$SI_d_c/100)
                 
                 
                 #-- CREATE COST EFFECTIVENESS TABLE --#
                 # renderTable continuously updates table
                 output$SO_icer_table <- renderTable({
                   df_res_table <- data.frame( # create dataframe
                     Option =  c("Treatment","No Treatment"), 
                     QALYs  =   c(paste0(round(mean(as.matrix(reslist$output$l_psa$effectiveness[2])),2), 
                                         " (",
                                         round(quantile(as.matrix(reslist$output$l_psa$effectiveness[2]), probs = 0.025, names = F),2),
                                         " to ",
                                         round(quantile(as.matrix(reslist$output$l_psa$effectiveness[2]), probs = 0.975, names = F),2),
                                         ")"),
                                  paste0(round(mean(as.matrix(reslist$output$l_psa$effectiveness[1])),2),
                                         " (",
                                         round(quantile(as.matrix(reslist$output$l_psa$effectiveness[1]), probs = 0.025, names = F),2),
                                         " to ",
                                         round(quantile(as.matrix(reslist$output$l_psa$effectiveness[1]), probs = 0.975, names = F),2),
                                         ")")
                     ),
                     Costs  =  c(paste0(round(mean(as.matrix(reslist$output$l_psa$cost[2])),0), 
                                        " (",
                                        round(quantile(as.matrix(reslist$output$l_psa$cost[2]), probs = 0.025, names = F),0),
                                        " to ",
                                        round(quantile(as.matrix(reslist$output$l_psa$cost[2]), probs = 0.975, names = F),0),
                                        ")"),
                                 paste0(round(mean(as.matrix(reslist$output$l_psa$cost[1])),0),
                                        " (",
                                        round(quantile(as.matrix(reslist$output$l_psa$cost[1]), probs = 0.025, names = F),0),
                                        " to ",
                                        round(quantile(as.matrix(reslist$output$l_psa$cost[1]), probs = 0.975, names = F),0),
                                        ")")),
                     Inc.QALYs = c(paste0(round(mean(as.matrix(reslist$output$l_psa$effectiveness[2]) - as.matrix(reslist$output$l_psa$effectiveness[1])),2), 
                                          " (",
                                          round(quantile(as.matrix(reslist$output$l_psa$effectiveness[2]) - as.matrix(reslist$output$l_psa$effectiveness[1]), probs = 0.025, names = F),2),
                                          " to ",
                                          round(quantile(as.matrix(reslist$output$l_psa$effectiveness[2]) - as.matrix(reslist$output$l_psa$effectiveness[1]), probs = 0.975, names = F),2),
                                          ")"),
                                   NA),
                     Inc.Costs = c(paste0(round(mean(as.matrix(reslist$output$l_psa$cost[2]) - as.matrix(reslist$output$l_psa$cost[1])),0), 
                                          " (",
                                          round(quantile(as.matrix(reslist$output$l_psa$cost[2]) - as.matrix(reslist$output$l_psa$cost[1]), probs = 0.025, names = F),0),
                                          " to ",
                                          round(quantile(as.matrix(reslist$output$l_psa$cost[2]) - as.matrix(reslist$output$l_psa$cost[1]), probs = 0.975, names = F),0),
                                          ")"),
                                   NA),
                     ICER = c(reslist$output$df_cea_psa$ICER[2], NA)
                   ) # close data-frame
                   df_res_table
                 }) # table plot end.
                 output$SO_ceac <- renderPlot({reslist$output$gg_ceac})
                 output$SO_scatter <- renderPlot({reslist$output$gg_scatter})
                 output$SO_trace_all <- renderPlot({reslist$output$trace_all})
                 # browser()
                 output$SO_time_in_states_table <- renderTable(reslist$output$time_in_states)
                 output$SO_time_in_states_plot <- renderPlot({
                   # Get the data frame from reactive expression
                   time_in_states_df <- as.data.frame(reslist$output$time_in_states)
                   time_in_states_df[2:8] <- lapply(time_in_states_df[2:8], function(x) as.numeric(as.character(x)))
                   maxval <- max(time_in_states_df[2:8])
                   # Convert data to long format for ggplot2
                   time_in_states_df_long <- time_in_states_df %>%
                     pivot_longer(cols = -Option, names_to = "State", values_to = "Percentage")%>%
                     mutate(State = factor(State, levels = colnames(time_in_states_df)[2:8]))
                   
                   # Create the barplot
                   ggplot(time_in_states_df_long, aes(x = State, y = Percentage, fill = Option)) +
                     geom_bar(stat = "identity", position = "dodge") +
                     labs(title = "",
                          x = "Model State",
                          y = "Percentage time spent") +
                     geom_text(aes(label = Percentage, y = Percentage), 
                               position = position_dodge(width = 0.9), vjust = -0.2, size = 4) +
                     theme_minimal() +
                     theme(
                       legend.position = c(0.5, 0.9),
                       legend.title = element_blank(),
                       legend.text = element_text(size = 14),
                       axis.text.x = element_text(size = 14, angle = 25, hjust = 1),  # Rotate x labels for better readability
                       axis.text.y = element_text(size = 14),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 16),
                       plot.title = element_text(size = 18, face = "bold"),
                       plot.margin = margin(10, 10, 10, 10)  # Add margins around the plot
                     ) +
                     ylim(0, maxval + 5)
                 })
                 output$SO_LEs <- renderTable(reslist$output$LEs)
                 output$SO_added_LE_costs <- renderText(paste0(c("Costs of Added life years (mean [95% CI]) = €", round(mean(reslist$output$PAID_final$intervention - reslist$output$PAID_final$control),0), "(", round(quantile(reslist$output$PAID_final$intervention - reslist$output$PAID_final$control, probs=0.025),0), ",", round(quantile(reslist$output$PAID_final$intervention - reslist$output$PAID_final$control, probs=0.975),0), ")") ))
               
                 hidePageSpinner()
                 
                 }# Observe event end inner
  ) # Observe event end
  
  ###Budget impact
  observeEvent(input$run_BI_model,
               ignoreNULL = T, 
               {
                 showPageSpinner(type = 1)
                 # Run model function with Shiny inputs
                 reslist_BI = f_wrapper_BI(n_age_init = input$SI_BI_n_age_init, 
                                           perspective = "healthcare",
                                           prop_male = 0.531,
                                           c_init_strA = input$SI_BI_c_device_strA + input$SI_BI_c_device_shipping + input$SI_BI_c_personnel_strA,
                                           sbp_diff_controlled = rnormA(n=1000, mean = input$SI_BI_sbp_diff_controlled, sd = input$SI_BI_sbp_diff_controlled_SE), 
                                           sbp_diff_uncontrolled = rnormA(n=1000, mean = input$SI_BI_sbp_diff_uncontrolled, sd = input$SI_BI_sbp_diff_uncontrolled_SE), 
                                           c_strA_controlled = rgammaA(1000, mean=input$SI_BI_c_strA_controlled_mean, sd= (input$SI_BI_c_strA_controlled_mean*0.25)),
                                           c_strA_uncontrolled = rgammaA(1000, mean=input$SI_BI_c_strA_uncontrolled_mean, sd= (input$SI_BI_c_strA_uncontrolled_mean*0.25)),
                                           c_SOC_controlled = rgammaA(1000, mean=input$SI_BI_c_SOC_controlled_mean, sd= (input$SI_BI_c_SOC_controlled_mean*0.15)),
                                           c_SOC_uncontrolled = rgammaA(1000, mean=input$SI_BI_c_SOC_uncontrolled_mean, sd= (input$SI_BI_c_SOC_uncontrolled_mean*0.15)),
                                           subgroup = input$SI_BI_subgroup,
                                           BI_reg_n_yr1 = input$SI_BI_reg_yr1,
                                           BI_reg_n_yr2 = input$SI_BI_reg_yr2,
                                           BI_reg_n_yr3 = input$SI_BI_reg_yr3,
                                           BI_reg_n_yr4 = input$SI_BI_reg_yr4,
                                           BI_reg_n_yr5 = input$SI_BI_reg_yr5,
                                           BI_nat_n_yr1 = input$SI_BI_nat_yr1,
                                           BI_nat_n_yr2 = input$SI_BI_nat_yr2,
                                           BI_nat_n_yr3 = input$SI_BI_nat_yr3,
                                           BI_nat_n_yr4 = input$SI_BI_nat_yr4,
                                           BI_nat_n_yr5 = input$SI_BI_nat_yr5,
                                           year_reduction_effect = 1,
                                           percentage_reduction_effect = 0)
                 
                 output$SO_BI_reg_plot <- renderPlot({
                   # Create dataframe
                   df_BI_reg_final <- data.frame(
                     Option =  c("Treatment", "No Treatment", "Treatment BI"),
                     Total.cost.mean.reg = c(
                       round(mean(as.matrix(reslist_BI$output$df_BI_reg_strA$total_BI + input$SI_BI_one_time_option_reg)/1000000), 2),
                       round(mean(as.matrix(reslist_BI$output$df_BI_reg_SOC$total_BI)/1000000), 2),
                       round(mean((as.matrix(reslist_BI$output$df_BI_reg_strA$total_BI + input$SI_BI_one_time_option_reg) - as.matrix(reslist_BI$output$df_BI_reg_SOC$total_BI))/1000000), 2)
                     ),
                     Total.cost.lower.reg = c(
                       round(quantile(as.matrix(reslist_BI$output$df_BI_reg_strA$total_BI + input$SI_BI_one_time_option_reg), probs = 0.025, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_reg_SOC$total_BI), probs = 0.025, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_reg_strA$total_BI + input$SI_BI_one_time_option_reg) - as.matrix(reslist_BI$output$df_BI_reg_SOC$total_BI), probs = 0.025, names = F)/1000000, 2)
                     ),
                     Total.cost.upper.reg = c(
                       round(quantile(as.matrix(reslist_BI$output$df_BI_reg_strA$total_BI + input$SI_BI_one_time_option_reg), probs = 0.975, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_reg_SOC$total_BI), probs = 0.975, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_reg_strA$total_BI + input$SI_BI_one_time_option_reg) - as.matrix(reslist_BI$output$df_BI_reg_SOC$total_BI), probs = 0.975, names = F)/1000000, 2)
                     )
                   )
                   
                   #browser()
                   # Create the plot
                   ggplot(df_BI_reg_final, aes(x = Option, y = Total.cost.mean.reg, fill = Option)) +
                     geom_bar(aes(y = Total.cost.mean.reg), stat = "identity", alpha = 0.7) +
                     geom_text(aes(label = paste0("€",sprintf("%.2f", Total.cost.mean.reg), " M"), y = Total.cost.mean.reg), 
                               position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
                     labs(y = "Cost (Millions of Euros)", title = "Regional scaling") +
                     theme_minimal(base_size = 18) +  # Increase base size for all text
                     theme(
                       plot.title = element_text(size = 22, face = "bold"),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 20),
                       axis.text = element_text(size = 18, angle = 35, hjust = 1),
                       legend.text = element_text(size = 18)
                     )+
                     scale_fill_manual(values=c("#A4C4E7","#A4C4E7","#D35400"))+
                     scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +  # Adjust expand to add space
                     guides(fill="none")
                 })
                 
                 output$SO_BI_nat_plot <- renderPlot({
                   # Create dataframe
                   df_BI_nat_final <- data.frame(
                     Option =  c("Treatment", "No Treatment", "Treatment BI"),
                     Total.cost.mean.nat = c(
                       round(mean(as.matrix(reslist_BI$output$df_BI_nat_strA$total_BI + input$SI_BI_one_time_option_nat)/1000000), 2),
                       round(mean(as.matrix(reslist_BI$output$df_BI_nat_SOC$total_BI)/1000000), 2),
                       round(mean((as.matrix(reslist_BI$output$df_BI_nat_strA$total_BI + input$SI_BI_one_time_option_nat) - as.matrix(reslist_BI$output$df_BI_nat_SOC$total_BI))/1000000), 2)
                     ),
                     Total.cost.lower.nat = c(
                       round(quantile(as.matrix(reslist_BI$output$df_BI_nat_strA$total_BI + input$SI_BI_one_time_option_nat), probs = 0.025, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_nat_SOC$total_BI), probs = 0.025, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_nat_strA$total_BI + input$SI_BI_one_time_option_nat) - as.matrix(reslist_BI$output$df_BI_nat_SOC$total_BI), probs = 0.025, names = F)/1000000, 2)
                     ),
                     Total.cost.upper.nat = c(
                       round(quantile(as.matrix(reslist_BI$output$df_BI_nat_strA$total_BI + input$SI_BI_one_time_option_nat), probs = 0.975, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_nat_SOC$total_BI), probs = 0.975, names = F)/1000000, 2),
                       round(quantile(as.matrix(reslist_BI$output$df_BI_nat_strA$total_BI + input$SI_BI_one_time_option_nat) - as.matrix(reslist_BI$output$df_BI_nat_SOC$total_BI), probs = 0.975, names = F)/1000000, 2)
                     )
                   )
                   # Create the plot
                   ggplot(df_BI_nat_final, aes(x = Option, y = Total.cost.mean.nat, fill = Option)) +
                     geom_bar(aes(y = Total.cost.mean.nat), stat = "identity", alpha = 0.7) +
                     geom_text(aes(label = paste0("€",sprintf("%.2f", Total.cost.mean.nat), " M"), y = Total.cost.mean.nat), 
                               position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
                     labs(y = "Cost (Millions of Euros)", title = "National scaling") +
                     theme_minimal(base_size = 18) +  # Increase base size for all text
                     theme(
                       plot.title = element_text(size = 22, face = "bold"),
                       axis.title.x = element_blank(),
                       axis.title.y = element_text(size = 20),
                       axis.text = element_text(size = 18, angle = 35, hjust = 1),
                       legend.text = element_text(size = 18)
                     )+
                     scale_fill_manual(values=c("#A4C4E7","#A4C4E7","#D35400"))+
                     scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +  # Adjust expand to add space
                     guides(fill="none")
                 })
               hidePageSpinner()
                 } #observe event inner
  ) # end observe event
  
  # Define a reactive expression for efficiency gains output
  efficiency_gains_output <- reactive({
    fun_efficiency_gains(controlled_prop = 1-input$SI_EG_uncontrolled_prop,
                         uncontrolled_prop = input$SI_EG_uncontrolled_prop,
                         EG_duration_GP_long = input$SI_EG_duration_GP_long,
                         EG_duration_GP_short = input$SI_EG_duration_GP_short,
                         EG_duration_POH_long = input$SI_EG_duration_POH_long,
                         EG_duration_POH_short = input$SI_EG_duration_POH_short,
                         cvd_pop_catchment = input$SI_EG_cvd_pop_catchment, 
                         n_gp_ref = input$SI_EG_n_gp_ref,
                         n_POH_ref = input$SI_EG_n_POH_ref)
  })
  
  # Render the table to display efficiency gain values
  output$SO_EG_table <- renderUI({
    # Get the list of results from the reactive expression
    efficiency_gains <- efficiency_gains_output()
    
    # Convert the list to a data frame for rendering in the table
    df_EG_table <- data.frame(
      Category = c("Long patient contacts (20-30 minutes)", "", "Short patient contacts (10-15 minutes)", "", "Remote consultations", ""),
      Measure = c("Hours spent per GP, annually", "Hours spent per PN, annually", 
                  "Hours spent per GP, annually", "Hours spent per PN, annually", 
                  "Frequency, per GP, annually", "Frequency, per PN, annually"),
      Controlled_Intervention = c(efficiency_gains$tot_GP_long_intervention_controlled[1], efficiency_gains$tot_POH_long_intervention_controlled[1], efficiency_gains$tot_GP_short_intervention_controlled[1], efficiency_gains$tot_POH_short_intervention_controlled[1], efficiency_gains$tot_eGP_intervention_controlled[1], efficiency_gains$tot_ePOH_intervention_controlled[1]),
      Controlled_CaU = c(efficiency_gains$tot_GP_long_soc_controlled[1], efficiency_gains$tot_POH_long_soc_controlled[1], efficiency_gains$tot_GP_short_soc_controlled[1], efficiency_gains$tot_POH_short_soc_controlled[1], efficiency_gains$tot_eGP_soc_controlled[1], efficiency_gains$tot_ePOH_soc_controlled[1]),
      Controlled_Change = c(efficiency_gains$diff_GP_long_controlled[1], efficiency_gains$diff_POH_long_controlled[1], efficiency_gains$diff_GP_short_controlled[1], efficiency_gains$diff_POH_short_controlled[1], efficiency_gains$diff_eGP_controlled[1], efficiency_gains$diff_ePOH_controlled[1]),
      Uncontrolled_Intervention = c(efficiency_gains$tot_GP_long_intervention_uncontrolled[1], efficiency_gains$tot_POH_long_intervention_uncontrolled[1], efficiency_gains$tot_GP_short_intervention_uncontrolled[1], efficiency_gains$tot_POH_short_intervention_uncontrolled[1], efficiency_gains$tot_eGP_intervention_uncontrolled[1], efficiency_gains$tot_ePOH_intervention_uncontrolled[1]),
      Uncontrolled_CaU = c(efficiency_gains$tot_GP_long_soc_uncontrolled[1], efficiency_gains$tot_POH_long_soc_uncontrolled[1], efficiency_gains$tot_GP_short_soc_uncontrolled[1], efficiency_gains$tot_POH_short_soc_uncontrolled[1], efficiency_gains$tot_eGP_soc_uncontrolled[1], efficiency_gains$tot_ePOH_soc_uncontrolled[1]),
      Uncontrolled_Change = c(efficiency_gains$diff_GP_long_uncontrolled[1], efficiency_gains$diff_POH_long_uncontrolled[1], efficiency_gains$diff_GP_short_uncontrolled[1], efficiency_gains$diff_POH_short_uncontrolled[1], efficiency_gains$diff_eGP_uncontrolled[1], efficiency_gains$diff_ePOH_uncontrolled[1])
    )
    
    # Create a styled table using kable
    p <- kable(df_EG_table, "html", align = 'c', col.names = c("", "", "Intervention", "CaU", 
                                                               "Increase (+) or reduction (-) with intervention", 
                                                               "Intervention", "CaU", 
                                                               "Increase (+) or reduction (-) with intervention")) %>%
      add_header_above(c(" " = 2, "Controlled BP" = 3, "Uncontrolled BP" = 3)) %>%
      kable_styling(full_width = F, position = "center", bootstrap_options = c("striped", "hover", "condensed"))
    HTML(p)  
  })
} # Server end

# Run the application 
shinyApp(ui = ui, server = server)
