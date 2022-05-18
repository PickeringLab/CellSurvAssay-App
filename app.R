library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(CellSurvAssay)
library(readxl)
library(writexl)
library(ggplot2)
library(colourpicker)
library(shinythemes)
library(shinydisconnect)
library(svglite)


methods <- c("ml", "ls", "franken")
method_names <- c("Maximum Likelihood", "Least Squares", "Franken")
method_codes <- setNames(methods, method_names)

pemethods <- c("fit", "fix")
pemethod_names <- c("Fit", "Fix")
pemethod_codes <- setNames(pemethods, pemethod_names)

gg_themes <- list("Black & White" = theme_bw(), 
                  "Test" = theme_test(), 
                  "Classic" = theme_classic(),
                  "Grey" = theme_grey(), 
                  "Linedraw" = theme_linedraw(), 
                  "Light" = theme_light(), 
                  "Dark" = theme_dark(), 
                  "Minimal" = theme_minimal(),
                  "Void" = theme_void())

font_faces <- c("plain", "bold", "italic", "bold.italic")
font_face_names <- c("Plain", "Bold", "Italic", "Bold & Italic")
font_face_codes <- setNames(font_faces, font_face_names)


linetypes <- 1:6
linetype_names <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
linetype_codes <- setNames(linetypes, linetype_names)


justtype_codes <- c("center", "left", "right")



ui <- function(request) {
  fluidPage(theme = shinytheme("yeti"),
            
            responsive = TRUE,
            
            disconnectMessage(text = "An error occurred. Please refresh the page and try again."),
            
            useShinyjs(),
            
            fluidRow(
              column(12,
                titlePanel(title = div("CellSurvAssay: Clonogenic Survival Analysis in R made easy!", img(src = "MDA_master.png", height = 46.15, width = 450, align = "right")), windowTitle = "CellSurvAssay - App")
              )
            ),
            
            tabsetPanel(
              tabPanel("Home",
                       fluidRow(
                         column(12,
                                HTML('<center><img src="CellSurvAssay.png" width = 90% height = 50.63%></center>')
                                ))),
              tabPanel("Import data",
                       sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           fileInput("datatab", "Upload your data (.csv/.tsv/.xlsx) ...", 
                                     accept = c(".xlsx", ".csv", ".tsv")),
                           h4("Format of dataset to be uploaded"),
                           helpText("The following columns, with these exact names, should be present in your data set. 
                                    The sequence of the columns or presence of other extra columns don't affect the analysis. Below is an example."),
                           helpText("1. “cline” - distinguishes the curves in the data frame"),
                           helpText("2. “Exp” - discriminates replicates within each curve"),
                           helpText("3. “dose” - applied radiation dose"),
                           helpText("4. “ncells” - no. of cells seeded"),
                           helpText("5. “ncolonies” - no. of counted colonies"),
                           tableOutput("CASP8_table"),
                           helpText("Click the button below to download an example dataset:"),
                           fluidRow(
                             column(8,
                                    downloadButton("ex_data", "Example data", icon = icon("angle-double-down"), class = "btn-lrg", width = "100%",
                                                   style = "color: white; background-color: steelblue"))
                           )
                         ),
                         mainPanel(
                           dataTableOutput("datatable"),
                           br(),
                           fluidRow(
                             column(10,
                                    offset = 6,
                                    h4("... and check here if your dataset is properly uploaded."))
                           ),
                           fluidRow(
                             column(10, offset = 0,
                                    span(h4(strong(textOutput("error_msg"), style = "color:red"))))
                           )
                         )
                       )
              ),
              tabPanel("Fit LQ Model",
                       sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           selectInput("cline", "Choose a cell line to fit the model ...", "", selected = NULL),
                           h4("Choose methods"),
                           radioButtons("method", "Method of estimating parameters", choices = method_codes, inline = FALSE),
                           radioButtons("pemethod", "Method of calculating plating efficiency", choices = pemethod_codes, inline = FALSE),
                           fluidRow(
                             column(4, offset = 8,
                                    downloadButton("lq_details", "Download", icon = icon("angle-double-down"), class = "btn-lrg", width = "100%",
                                                   style = "color: white; background-color: steelblue"))
                           ),
                           hr(),
                           h4("A few notes about the LQ model"),
                           p(),
                           withMathJax(),
                           helpText("The model that is fit here is:
                          $$S = S(D) = e^{c + \\alpha D + \\beta D^2}$$
                          Please note that due to the positive formulation of the model, the parameters
                          \\(\\alpha\\), \\(\\beta\\), and \\(c\\) take negative values in the fit results.
                          Please check Help for more details.")
                           
                         ),
                         mainPanel(
                           verbatimTextOutput("lqmodel")
                         )
                       )
                       
              ),
              tabPanel("Plot CS Curve",
                       sidebarLayout(
                         sidebarPanel(
                           id = "default",
                           width = 4,
                           selectInput("cline_plot", "Choose cell line(s) to plot the curve(s) ...", "", multiple = TRUE),
                           h4("Choose methods"),
                           radioButtons("plot_method", "Method of estimating parameters", choices = method_codes, inline = TRUE),
                           radioButtons("plot_pemethod", "Method of calculating plating efficiency", choices = pemethod_codes, inline = TRUE),
                           hr(),
                           h4("Customize overall plot architecture"),
                           fluidRow(
                             column(12,
                                    sliderInput("ylim", "Range of Y-axis", value = c(0.01, 1.1), min = 0.0001, max = 1.25, step = 0.001)
                             )
                           ),
                           fluidRow(
                             column(8,
                                    textInput("ybreaks", "Breaks in Y-axis:", placeholder = "Separate values by comma...")
                             ),
                             column(4,
                                    sliderInput("size", "Plot viewer size", min = 1, max = 5, value = 3, step = 1, width = "100%")
                             )
                           ),
                           fluidRow(
                             column(6,
                                    selectInput("plot_cols", "Pick color(s)", multiple = TRUE, choices = colors())
                             ),
                             column(6,
                                    selectInput("plot_theme", "Pick a plot theme", choices = names(gg_themes))
                             )
                           ),
                           fluidRow(
                             column(4, 
                                    selectInput("curve_type", "Curve style", choices = linetype_codes, selected = 1)
                             ),
                             column(4,
                                    numericInput("curve_width", "Curve width", min = 0.5, max = 10, step = 0.5, value = 1.5)
                             ),
                             column(4,
                                    selectInput("seg_type", "Segment style", choices = linetype_codes, selected = 1)
                             )
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("seg_width", "Segment width", min = 0.5, max = 10, step = 0.5, value = 1)
                             ),
                             column(4,
                                    selectInput("point_type", "Point shape", choices = 0:25, selected = 16)
                             ),
                             column(4,
                                    numericInput("point_size", "Point size", min = 0.5, max = 10, step = 0.5, value = 3.5)
                             )
                           ),
                           hr(),
                           h4("Customize plot title"),
                           fluidRow(
                             column(8,
                                    textInput("title", "Plot title", width = "100%") 
                             ),
                             column(4,
                                    selectInput("title_pos", "Alignment", width = "100%", choices = justtype_codes, selected = 0.5))
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("title_size", "Font size", value = 20, min = 10, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("title_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("title_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           hr(),
                           h4("Customize plot subtitle"),
                           fluidRow(
                             column(8,
                                    textInput("sub", "Plot subtitle")
                             ),
                             column(4,
                                    selectInput("sub_pos", "Alignment", width = "100%", choices = justtype_codes, selected = 0.5))
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("sub_size", "Subtitle: Font size", value = 15, min = 8, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("sub_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("sub_face", "Face", choices = font_face_codes, selected = "italic")
                             )
                           ),
                           hr(),
                           h4("Customize plot legend"),
                           fluidRow(
                             column(4,
                                    textInput("ltitle", "Legend title", value = "")),
                             column(4,
                                    selectInput("ltitle_al", "Alignment", width = "100%", choices = justtype_codes, selected = "left")),
                             column(4,
                                    selectInput("leg_pos", "Position", width = "100%", choices = c("inside", "outside", "none")))
                           ),
                           fluidRow(
                             column(4,
                                    selectInput("leg_fill", "Background", choices = c("", colors()), selected = "")),
                             column(4,
                                    selectInput("leg_bor", "Border color", choices = colors(), selected = "white")),
                             column(4,
                                    numericInput("leg_size", "Border width", min = 0, max = 10, step = 0.5, value = 0))
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("ltitle_size", "Title: Font size", value = 20, min = 8, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("ltitle_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("ltitle_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("ltext_size", "Text: Font size", value = 18, min = 8, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("ltext_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("ltext_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           hr(),
                           h4("Customize X-axis"),
                           fluidRow(
                             column(6,
                                    textInput("xlab", "X-axis label", value = "Dose (Gy)")),
                             column(6,
                                    selectInput("rem_x", "Remove minor X grids", choices = c("no", "yes"), selected = "yes"))
                           ),
                           fluidRow(
                             column(5,
                                    selectInput("emph_x", "Emphasize major X grids", choices = c("no", "yes"))),
                             column(4,
                                    selectInput("emph_x_col", "Grid color", choices = colors(), selected = "gray")),
                             column(3,
                                    numericInput("emph_x_wid", "Width", min = 0.5, max = 10, step = 0.5, value = 1))
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("xlab_size", "Label: Font size", value = 16, min = 10, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("xlab_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("xlab_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("xtext_size", "Tick labels: Font Size", value = 14, min = 8, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("xtext_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("xtext_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           hr(),
                           h4("Customize Y-axis"),
                           fluidRow(
                             column(6,
                                    textInput("ylab", "Y-axis label", value = "Surviving Fraction")),
                             column(6,
                                    selectInput("rem_y", "Remove minor Y grids", choices = c("no", "yes"), selected = "yes"))
                           ),
                           fluidRow(
                             column(5,
                                    selectInput("emph_y", "Emphasize major Y grids", choices = c("no", "yes"))),
                             column(4,
                                    selectInput("emph_y_col", "Grid color", choices = colors(), selected = "gray")),
                             column(3,
                                    numericInput("emph_y_wid", "Width", min = 0.5, max = 10, step = 0.5, value = 1))
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("ylab_size", "Label: Font size", value = 16, min = 10, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("ylab_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("ylab_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           fluidRow(
                             column(4,
                                    numericInput("ytext_size", "Text: Font size", value = 14, min = 8, max = 30, step = 0.5)
                             ),
                             column(4,
                                    selectInput("ytext_color", "Color", choices = colors(), selected = "black")
                             ),
                             column(4,
                                    selectInput("ytext_face", "Face", choices = font_face_codes, selected = "bold")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    actionButton("plot_button", "Plot!", width = "100%", icon = icon("chart-line"),
                                                 style = "color: white; background-color: steelblue; border-color: #2e6da4"))
                           ),
                           br(),
                           fluidRow(
                             column(12,
                                    bookmarkButton())
                           ),
                           hr(),
                           h4("Save Plot"),
                           fluidRow(
                             column(4,
                                    numericInput("save_height", "Height (inches):", value = 4, min = 1, max = 30, step = 0.1)
                             ),
                             column(4,
                                    numericInput("save_width", "Width (inches):", value = 5, min = 1, max = 30, step = 0.1)
                             ),
                             column(4,
                                    selectInput("save_type", "File type:", choices = c("pdf", "svg", "jpg", "png"))
                             )
                           ),
                           fluidRow(
                             column(4,
                                    actionButton("reset_all", "Reset All", icon = icon("redo-alt"), width = "100%", 
                                                 style = "color: white; background-color: steelblue")
                             ),
                             column(4, offset = 4,
                                    downloadButton("download", "Download", icon = icon("angle-double-down"), class = "btn-lrg", width = "100%",
                                                   style = "color: white; background-color: steelblue"))
                           )
                         ),
                         mainPanel(
                           fluidRow(),
                           fluidRow(
                             column(2,
                             ),
                             column(10,
                                    plotOutput("plot", width = 500, height = 600)
                             )
                           )
                           
                         )
                       )),
              tabPanel("Compare Curves",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("comp_cline1", "Choose the first cell line to compare ...", ""),
                           selectInput("comp_cline2", "Choose the second cell line to compare ...", ""),
                           h4("Choose methods"),
                           radioButtons("comp_method", "Method for estimating parameters", choices = method_codes, inline = FALSE),
                           radioButtons("comp_pemethod", "Method for calculating plating efficiency", choices = pemethod_codes, inline = FALSE),
                           fluidRow(
                             column(4, offset = 8,
                                    downloadButton("compare_details", "Download", icon = icon("angle-double-down"), class = "btn-lrg", width = "100%",
                                                   style = "color: white; background-color: steelblue"))
                           )
                         ),
                         mainPanel(
                           verbatimTextOutput("compare_curves")
                         )
                       )),
              tabPanel("Calculate DER",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("der_cline1", "Choose the numerator cell line ...", ""),
                           selectInput("der_cline2", "Choose the denominator cell line ...", ""),
                           numericInput("surv_frac", "Enter the survival fraction:", value = 0.25, min = 0, max = 1, step = 0.01),
                           h4("Choose methods"),
                           radioButtons("der_method", "Method for estimating parameters", choices = method_codes, inline = FALSE),
                           radioButtons("der_pemethod", "Method for calculating plating efficiency", choices = pemethod_codes, inline = FALSE),
                           fluidRow(
                             column(4, offset = 8,
                                    downloadButton("der_results", "Download", icon = icon("angle-double-down"), class = "btn-lrg", width = "100%",
                                                   style = "color: white; background-color: steelblue")))
                         ),
                         mainPanel(
                           verbatimTextOutput("der")
                         )
                       )),
              tabPanel("Help",
                       navlistPanel(widths = c(3, 9),
                                    tabPanel("Import data",
                                             hr(),
                                             p("This tab allows you to upload your data and check if it satisfies the minimum requirements to run the analysis.
                                    The app only accepts .tsv, .csv, or .xlsx file types. The following columns, with these exact names, should be present in your data set. The sequence of these columns or the presence of other extra columns don't affect the analysis."),
                                             p("1. ", strong("cline"), ": distinguishes the curves in the data frame"),
                                             p("2. ", strong("Exp"), ": discriminates replicates within each curve"),
                                             p("3. ", strong("dose"), ":  applied radiation dose"),
                                             p("4. ", strong("ncells"), ": no. of cells seeded"),
                                             p("5. ", strong("ncolonies"), ": no. of counted colonies"),
                                             div("Note: If you get an error message after uploading your data saying a mismatch in format, that means the names of your columns do not match the requirements.
                                      Please do not proceed before rectifying this.", style = "color:darkblue"),
                                             br(),
                                             p("The ", strong("Example data"), " button allows you to download an example dataset that can be used to get to know the features of the app before you have your own data.
                                               Below is the same data presented in an interactive table:"),
                                             dataTableOutput("example_datatable")),
                                    tabPanel("Fit LQ Model",
                                             hr(),
                                             p("This tab allows you to fit the linear quadratic model for the chosen cell type. It allows you to choose a cell type among the ones present in your dataset, 
                                    and also lets you choose a method each for estimating the parameters and calculating the plating efficiencies. For more details on these methods, please continue reading below, and you can also refer to ", em("cite our paper"), ". 
                                               The ", strong("Download button"), " helps you download the statistical results as a text file."),
                                             p("The linear-quadratic model has been ubiquitous and often the method of choice for depicting the effect of radiation for both researchers and clinicians (McMahon, 2018). This popular yet quite simple model is given by:"),
                                             withMathJax(),
                                             p("$$S = S(D)/S(0) = e^{- \\alpha D - \\beta D^2}$$"),
                                             p("Here, cell survival \\(S\\) is a function of dose \\(D\\) of the insulting agent to which the cells are exposed. \\(S(0)\\) is the plating efficiency, the surviving fraction of untreated or unirradiated cells, and \\(S(D)\\) is that for treated cells.
                                    \\(\\alpha\\) and \\(\\beta\\) are parameters that describe the cell’s radiosensitivity. Typically, \\(S(0)\\) is considered fixed and kept on the left side of the equation (Franken, Rodermond, Stap, Haveman, & van Bree, 2006). 
                                    However, CellSurvAssay, which is built around the R package CFAssay (Braselmann et al., 2015), treats colonies from untreated cells as random observations, similar to colonies from treated cells, and keeps \\(S(0)\\) on the right side of the equation. 
                                    Moreover, it formulates the equation differently and reverses the signs, similar to CFAssay, resulting in the following equation: "),
                                             p("$$S = S(D) = e^{c + \\alpha D + \\beta D^2}$$"),
                                             p("Here, \\(c = log(S(0))\\) is the intercept and varies between different experiments."),
                                             div("Note: Due to the positive formulation of the model, the parameters \\(c\\), \\(\\alpha\\), and \\(\\beta\\) take negative values in the fit results (Braselmann et al., 2015)", style = "color:darkblue"),
                                             br(),
                                             p("Though this method (Method of calculating plating efficiency = 'Fit') of treating the data from untreated cells as random observations is statistically preferable, 
                                    the option for implementing the conventional normalization method (Method of calculating plating efficiency = 'Fix') is present as well for assessment."),
                                             p("Also, the commonly applied Least-Squares method for estimating parameters of the linear-quadratic model necessitates the data to be normally distributed, while colony numbers being discrete values follow a Poisson distribution, 
                                    making the Maximum Likelihood method preferable statistically. However, both these options for estimating parameters are present, along with an additional option of a weighted least-squares method named Franken, as described by (Franken et al., 2006)."),
                                             hr(),
                                             h4("References"),
                                             hr(),
                                             p("Braselmann, H., Michna, A., Heß, J., & Unger, K. (2015). CFAssay: statistical analysis of the colony formation assay. Radiat Oncol, 10, 223. doi:10.1186/s13014-015-0529-y"),
                                             p("Franken, N. A., Rodermond, H. M., Stap, J., Haveman, J., & van Bree, C. (2006). Clonogenic assay of cells in vitro. Nat Protoc, 1(5), 2315-2319. doi:10.1038/nprot.2006.339"),
                                             p("McMahon, S. J. (2018). The linear quadratic model: usage, interpretation and challenges. Phys Med Biol, 64(1), 01tr01. doi:10.1088/1361-6560/aaf26a")),
                                    tabPanel("Plot CS Curve",
                                             hr(),
                                             p("This tab allows you to plot the cell survival curves, customize them, and download the figures as per your requirements and specifications.
                                               To plot the curves, you have to select a single, or multiple, cell type(s) from the drop down menu, and select your methods of choice. Please visit the help page for LQ model to know more about the various methods."),
                                             p("Please note that you have to click the ", strong("'Plot' button"), " each time you plot or update the customization settings. Also, the sequence in which you choose the cell lines is the one that is maintained in your figure legend, and also when you pick your colors.
                                               So, for example, if you want your control cell line to be at the top of the figure legend, choose it first."),
                                             p("Below, we go over each customization option in detail:"),
                                             hr(),
                                             h4("Customize overall plot architecture"),
                                             hr(),
                                             p(strong("Range of Y-axis"), ": This dual-ended slider helps to set the limits of the Y-axis of the figure. Clicking and moving a slider head changes the limit of the axis on that end. Also, clicking on one slider head and using your arrow keys for its movement helps make precise adjustments.
                                               The Y-axis of your figure can range from 0.0001 to 1.25, and the slider can be precise to the unit of 0.001. This customization can be used to make your curves fit better into the plot and make your figure comparable to other figures with the same Y-axis limits."),
                                             p(strong("Breaks in Y-axis"), ": This option allows you to customize the breaks/ticks that appear on the Y-axis of the figure. To enter customized Y-axis breaks, simply type the numbers in, each separated by commas. This customization also makes your figure comparable to other figures with the same Y-axis breaks and limits. 
                                               When nothing is entered, these are chosen by default by the plotting function ", code("ggplot()"), "."),
                                             p(strong("Plot viewer size"), ": This slider simply zooms in and out of your plot. Please note that this option ", em("does not"), " change the actual size of your figure."),
                                             p(strong("Pick color(s)"), ": This drop-down menu allows you to customize the colors of your cell survival curves. The sequence in which the colors are chosen matters as the first color is assigned to the first chosen cell line, and so on. Also, the number of colors chosen should be at least equal to, or greater than, 
                                               the number of cell lines picked, or else the plotting function fails and gives you an error. When no colors are chosen, the plotting function chooses colors by random, and these might change every time the 'Plot' button is clicked."),
                                             p(strong("Pick a plot theme"), ": Allows you to pick one of the several ", code("ggplot2"), " themes that changes the overall appearance of the plot. You can visit ", a(href = "https://statisticsglobe.com/ggplot2-themes-r", "this page"), " to learn more about these themes. The Black & White theme is chosen by default."),
                                             p(strong("Curve style"), ": Allows you to choose the line type of the cell survival curves. Visit ", a(href = "https://r-charts.com/base-r/line-types/", "this page"), " to get an idea about the various line type options available."),
                                             p(strong("Curve width"), ": Allows you to customize the width of the cell survival curves. Increasing the number increases the widths of these curves, and vice versa."),
                                             p(strong("Segment style"), ": Allows you to choose the line type of the vertical segments present in the figure. Read 'Curve style'."),
                                             p(strong("Segment width"), ": Allows you to customize the width of the vertical segments present in the figure. Increasing the number increases the widths of these segments, and vice versa."),
                                             p(strong("Point shape"), ": Allows you to to customize the shape of the data points in the figure. Visit ", a(href = "https://r-charts.com/base-r/pch-symbols/", "this page"), " for an idea regarding the different shape options available."),
                                             p(strong("Point size"), ": Allows you to change the size of the data points."),
                                             hr(),
                                             h4("Customize plot title"),
                                             hr(),
                                             p(strong("Plot title"), ": Enter a title to be added to your figure"),
                                             p(strong("Alignment"), ": Allows you to choose the alignment of your plot title. It can be center, left, or right aligned"),
                                             p(strong("Font size"), ": Allows you to customize the font size of the title"),
                                             p(strong("Color"), ": Allows you to customize the font color of your title"),
                                             p(strong("Face"), ": Allows you to customize the font face of your title. It can be bold, italic, bold-italic, or plain."),
                                             hr(),
                                             h4("Customize plot subtitle"),
                                             hr(),
                                             p("The customization options work exactly similar to the section 'Customize plot title', but for the subtitle. Please refer to that section for details."),
                                             hr(),
                                             h4("Customize plot legend"),
                                             hr(),
                                             p(strong("Legend title"), ": Allows you to add a legend title."),
                                             p(strong("Alignment"), ": Allows you to customize the alignment of the legend title."),
                                             p(strong("Position"), ": Allows you to choose whether the legend will be inside or outside the figure. 'inside' places it inside the figure at the bottom left of the curves, while 'outside' places it outside the figure on the right."),
                                             p(strong("Background"), ": Allows you to change the color of the background of the legend."),
                                             p(strong("Border color"), " and ", strong("Border width"), ": Allows you to put the legend inside a rectangular box. ", strong("Border color"), " allows you to select the color of the border of the box, while ", strong("Border width"), " allows you to choose the width of the border."),
                                             p(strong("Title: Font size"), ", ", strong("Color"), ", ", strong("Face"), ": Allows you to customize the legend title font size, color and face, respectively, similar to plot title."),
                                             p(strong("Text: Font size"), ", ", strong("Color"), ", ", strong("Face"), ": Allows you to customize the legend text font size, color and face, respectively, similar to plot title."),
                                             hr(),
                                             h4("Customize X-axis"),
                                             hr(),
                                             p(strong("X-axis label"), ": Allows you give a label for your X-axis."),
                                             p(strong("Remove minor X grids"), ": Allows you to remove or add minor lines in your X-axis."),
                                             p(strong("Emphasize major X grids"), ": Allows you to customize the major lines in your X-axis. When chosen 'yes', ", strong("Grid color"), " and ", strong("Width"), " can be used to customize the major X-axis lines."),
                                             p(strong("Grid color"), " and ", strong("Width"), ": Allows you to change the color and width of the major lines in your X-axis, when ", strong("Emphasize major X grids"), " is set to 'yes'."),
                                             p(strong("Label: Font size"), ", ", strong("Color"), ", ", strong("Face"), ": Allows you to customize the X-axis label font size, color and face, respectively, similar to plot title."),
                                             p(strong("Tick labels: Font size"), ", ", strong("Color"), ", ", strong("Face"), ": Allows you to customize the X-axis tick label font size, color and face, respectively, similar to plot title. Tick labels are the labels of the breaks that appear on your X or Y axis."),
                                             hr(),
                                             h4("Customize Y-axis"),
                                             hr(),
                                             p("All the options here work similar to the section 'Customize X-axis', but for the Y-axis. Please refer to that section for more details."),
                                             hr(),
                                             h4("Plot"),
                                             hr(),
                                             p("Click on this button everytime you want to:"),
                                             p("1. plot a figure, or"),
                                             p("2. update a figure when you have modified any of the settings. Please note that none of the settings are updated until you click this button."),
                                             hr(),
                                             h4("Bookmark"),
                                             hr(),
                                             p("The bookmark button provides you with a personal link which you can save as a bookmark in your browser, or in a text or word file. This link allows you to load the app with the customization settings that you had set when the bookmark link was retrieved, instead of the default ones.  
                                               It can be helpful as: "),
                                             p("1. it helps you to not repeat your preferred plotting settings everytime you load the app, and"),
                                             p("2. you can share this link with others to let them know the settings that you used for your analysis."),
                                             hr(),
                                             h4("Save Plot"),
                                             hr(),
                                             p("This section helps you to download the last figure that you plotted in your own specifications."),
                                             p(strong("Height"), ": Allows you to set a height of the figure, in inches."),
                                             p(strong("Width"), ": Allows you to set a width of the figure, in inches."),
                                             p(strong("File type"), ": Allows you to choose a file type for your figure. It can be a pdf, jpg, or png."),
                                             p(strong("Download button"), ": Click on it to download the figure in your provided specifications."),
                                             hr(),
                                             h4("Reset All"),
                                             hr(),
                                             p("Resets all the customization settings in the page to the default ones.")),
                                    tabPanel("Compare Curves",
                                             hr(),
                                             p("This page allows you to compare two linear-quadratic cell survival curves using ANOVA."),
                                             p("The null hypothesis is that the parameters \\(\\alpha\\) and \\(\\beta\\) of both the models are independent of the two curves, while the alternate hypothesis is that the parameters are different."),
                                             p("You have to choose the two ", em("different"), " cell lines you want to compare, and your methods of choice. Please refer to the 'Fit LQ Model' help page for more details on these methods."),
                                             p("The ", strong("Download button"), " helps you download the statistical results as a text file.")),
                                    tabPanel("Calculate DER",
                                             hr(),
                                             p("Clonogenic Assay can determine cell survival fractions in combination treatments as well, but the additional treatment might influence the proliferation rate and modify the radiation dose-survival curve (Franken et al., 2006).
                                               Hence, in such situations, we must calculate the Dose Enhancement Ratio (DER), also known as Sensitizer Enhancement Ratio, Dose Modifying Factor, Dose Modifying Ratio, or Radiosenstitivity Enhancement Factor (Subiel, Ashmore, & Schettino, 2016), as a parameter to quantify the differences between survival curves."),
                                             p("While calculating the DER, the ratio of the radiation dose at a certain survival level for radiation alone to that for the combined treatment should be calculated and not the surviving fractions at a specific radiation dose (Franken et al., 2006). This ratio can be written as:"),
                                             p("$$ DER = DER_{control} / DER_{treatment} $$"),
                                             p("You have to choose your numerator (control) cell line, denominator (treatment) cell line, the survival fraction for which you want to calculate the DER,and your methods of choice. Please refer to the 'Fit LQ Model' help page for more details on the different methods."),
                                             hr(),
                                             h4("References"),
                                             hr(),
                                             p("Franken, N. A., Rodermond, H. M., Stap, J., Haveman, J., & van Bree, C. (2006). Clonogenic assay of cells in vitro. Nat Protoc, 1(5), 2315-2319. doi:10.1038/nprot.2006.339"),
                                             p("Subiel, A., Ashmore, R., & Schettino, G. (2016). Standards and Methodologies for Characterizing Radiobiological Impact of High-Z Nanoparticles. Theranostics, 6(10), 1651-1671. doi:10.7150/thno.15019")),
                                    tabPanel("Troubleshooting",
                                             hr(),
                                             h4("Troubleshooting"),
                                             hr(),
                                             p("1. ", em("After importing your data, you get an error message '", strong("Error! Your column names don't match with the requirements! Please check the left panel or Help page for the data format necessary for the analysis."), "':")),
                                             p("This means that one or more of the column names in your data don't match with the required column names. Please refer to the help page of 'Import Data' for more details regarding that."),
                                             br(),
                                             p("2. ", em("The app suddenly crashes and you get a message '", strong("An error occurred. Hint: Check format of uploaded data. Please refresh the page and try again."), em("':"))),
                                             p("This means that your app encountered something that caused it to disconnect from the server.
                                               Unfortunately, Shiny sometimes behaves this way when it encounters an error. It's very likely that the column names in your data don't match with the required ones. If that's the case, rectifying the column names should do the trick. The ", strong("Import Data"), " page should have provided an error message as well, if that's the case. 
                                               Please refer to the ", strong("Help"), " pages for more details."),
                                             br(),
                                             p("3. ", em("While plotting the CS curves, you get an error message '", strong("Insufficient values in manual scale. x needed but only y provided."), em("':"))),
                                             p("It's quite likely that the number of colors you have chosen is less than the number of cell types you have selected. Once the numbers match, you should be good to go."))
                       )),
              tabPanel("About",
                       navlistPanel(widths = c(3, 9),
                                    tabPanel("CellSurvAssay",
                                             hr(),
                                             h4("CellSurvAssay"),
                                             hr(),
                                             p(strong("CellSurvAssay"), " consists of a couple of tools that can be used to perform Clonogenic Survival Analysis in R very easily and efficiently. These two tools are:"),
                                             p("1. ", em("CellSurvAssay R package"), ": This helps even beginner R users to perform the analysis in R, while maintaining the flexibility of a package. To know more details about the R package, visit ", a(href = "https://pickeringlab.github.io/CellSurvAssay/", "here"), "."),
                                             p("2. ", em("CellSurvAssay Shiny app"), ": This is a web application that helps users with no experience in R to perform the analysis, in R. The app is based on the CellSurvAssay R package and can be accessed ", a(href = "https://pickeringlab.shinyapps.io/CellSurvAssay-App/", "here"), "."),
                                             hr(),
                                             h4("Purpose of the CellSurvAssay Shiny App"),
                                             hr(),
                                             p("The CellSurvAssay Shiny web app uses the CellSurvAssay R package in the background, which is built around the ", a(href = "https://bioconductor.org/packages/release/bioc/html/CFAssay.html", "CFAssay"), " R package 
                                               that can be used to perform Cell Survival Assay analysis in R. However, the CellSurvAssay app has it’s own purposes and advantages:"),
                                             p("1. it makes performing Clonogenic Survival Analysis in R incredibly user-friendly and efficient, even for users who have no experience in R and don't have the luxury of time to learn it,"),
                                             p("2. it arranges all the commonly used steps of clonogenic assay analysis in one location and automates the steps very similar to other available automated software,"),
                                             p("3. it utilizes ", code("ggplot()"), " to plot the cell suvival curves, and builds better quality figures than other available R packages,"),
                                             p("4. it is less time consuming and more convenient for the user, as it accepts the raw data for the analysis, and calculates the plating efficiencies by itself,"),
                                             p("5. it offers various method options for parameter estimation and calculating plating efficiencies, unlike most other available software tools, and"),
                                             p("6. as R is being utilized, the methodology stays open and the results reproducible.")),
                                    tabPanel("Pickering lab",
                                             HTML('<center><img src="Pickering_lab.jpg" width = "720" height = "526.32"></center>'),
                                             br(),
                                             p(style = "text-align: center;", "Pickering Lab, Feb 2020"),
                                             br(),
                                             p("The Pickering Lab investigates the translational genomics of head and neck cancer. We are in the Department of Head & Neck Surgery at The University of Texas MD Anderson Cancer Center."),
                                             p("Our research studies start with the head and neck cancer genome and investigate the biology of the genomic alterations with the goal to improve outcomes for cancer patients.")
                                             ),
                                    tabPanel("Contact us",
                                             hr(),
                                             h4("Authors and Maintainers"),
                                             hr(),
                                             p("Arunangshu Sarkar, B.D.S., M.S.", a(href = 'mailto:arunangshu.sarkar@ucdenver.edu?subject=CellSurvAssay-App', "[email Arunangshu]")),
                                             p("Curtis Pickering, Ph.D. ", a(href = 'mailto:CRPickering@mdanderson.org?subject=CellSurvAssay-App', "[email Curtis]")),
                                             br(),
                                             hr(),
                                             h4("Contributors"),
                                             hr(),
                                             p("Frederico Omar Gleber-Netto, D.D.S., Ph.D. ", a(href = 'mailto:FONetto@mdanderson.org?subject=CellSurvAssay-App', "[email Frederico]")),
                                             p("David Molkentine, Ph.D. ", a(href = 'mailto:molkentined@upmc.edu?subject=CellSurvAssay-App', "[email David]")),
                                             p("Heath Skinner, M.D., Ph.D.", a(href = 'mailto:skinnerh@upmc.edu?subject=CellSurvAssay-App', "[email Heath]")),
                                             p("Burak Uzunparmak, M.D., Ph.D.", a(href = 'mailto:BUzunparmak@mdanderson.org?subject=CellSurvAssay-App', "[email Burak]")),
                                             p("Thomaia Pamplin, M.S.", a(href = 'mailto:TPamplin@mdanderson.org?subject=CellSurvAssay-App', "[email Thomaia]"))),
                                    tabPanel("Cite us")))
            )
  )
  
} 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  datainput <- reactive({
    req(input$datatab)
    
    ext <- tools::file_ext(input$datatab$name)
    switch(ext,
           xlsx = importData(input$datatab$datapath),
           csv = importData(input$datatab$datapath, filetype = "csv"),
           tsv = importData(input$datatab$datapath, filetype = "tsv"),
           validate("Invalid file; Please upload a .xlsx, .csv or .tsv file")
           )
  })
  
  table_casp <- CASP8_data[c(1,2,4,5,13,14,16,17),]
  
  output$CASP8_table <- renderTable(table_casp)
  
  output$datatable <- renderDataTable(datainput(), options = list(pageLength = 10))
  
  ex_data <- CASP8_data
  output$example_datatable <- renderDataTable(ex_data, options = list(pageLength = 5))
  
  observeEvent(datainput(), {
      cline_choices <- unique(datainput()$cline)
      comp_cline1 <- input$comp_cline1
      updateSelectInput(inputId = "cline", choices = cline_choices)
      updateSelectInput(inputId = "cline_plot", choices = cline_choices)
      updateSelectInput(inputId = "comp_cline1", choices = cline_choices)
      updateSelectInput(inputId = "comp_cline2", choices = cline_choices, selected = cline_choices[2])
      updateSelectInput(inputId = "der_cline1", choices = cline_choices)
      updateSelectInput(inputId = "der_cline2", choices = cline_choices, selected = cline_choices[2])
      })
  
  output$error_msg <- renderText({
    j <- 0
    for (i in 1:5) {
      if (!(c("cline", "Exp", "dose", "ncells", "ncolonies")[i] %in% colnames(datainput()))) {
        j <- j + 1
      }
    }
    if (j > 0) {
      return("Error! Your column names don't match with the requirements! 
             Please check the left panel or Help page for the data format necessary for the analysis.")
    }
  })
  
  output$ex_data <- downloadHandler(
    filename = function() {
      paste("CASP8data.xlsx")
    },
    content = function(file) {
      ex_data <- CASP8_data
      write_xlsx(ex_data, file)
    }
  )
  
  output$lqmodel <- renderPrint({
      req(input$cline)
      lqmodelFit(datainput(), input$cline, method = input$method, PEmethod = input$pemethod)
      })
  
  output$lq_details <- downloadHandler(
      filename = function() {
        paste("lqmodel_",input$cline, "_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        lqmodel <- capture.output(lqmodelFit(datainput(), input$cline, method = input$method, PEmethod = input$pemethod))
        writeLines(paste(lqmodel, sep = "\n"), file)
      })
  
  p <- reactiveValues()
  
  
  observeEvent(input$plot_button, {
      req(input$cline_plot)
      theme_plot <- reactive({gg_themes[[input$plot_theme]]})
      clines <- reactive({input$cline_plot})
      p$plot <- ggplotCSCurve(datainput(), clines(), method = input$plot_method, PEmethod = input$plot_pemethod,
                              colors = input$plot_cols, ylim = input$ylim, theme = theme_plot(), ybreaks = extract_breaks(input$ybreaks),
                              title = input$title, subtitle = input$sub, xlab = input$xlab, ylab = input$ylab,
                              title_size = input$title_size, title_color = input$title_color, title_face = input$title_face, title_align = input$title_pos,
                              sub_size = input$sub_size, sub_color = input$sub_color, sub_face = input$sub_face, sub_align = input$sub_pos,
                              xlab_size = input$xlab_size, xlab_color = input$xlab_color, xlab_face = input$xlab_face,
                              ylab_size = input$ylab_size, ylab_color = input$ylab_color, ylab_face = input$ylab_face,
                              legend_title = input$ltitle, ltitle_size = input$ltitle_size, ltitle_color = input$ltitle_color,
                              ltitle_face = input$ltitle_face, ltitle_align = input$ltitle_al, legend_pos = input$leg_pos,
                              legend_back = input$leg_fill, legend_border = input$leg_bor, legend_border_width = input$leg_size,
                              ltext_size = input$ltext_size, ltext_color = input$ltext_color, ltext_face = input$ltext_face, 
                              xtext_size = input$xtext_size, xtext_color = input$xtext_color, xtext_face = input$xtext_face,
                              ytext_size = input$ytext_size, ytext_color = input$ytext_color, ytext_face = input$ytext_face, 
                              rem_minor_x = input$rem_x, rem_minor_y = input$rem_y, emph_major_x = input$emph_x, emph_major_y = input$emph_y,
                              major_x_col = input$emph_x_col, major_x_width = input$emph_x_wid, major_y_col = input$emph_y_col, major_y_width = input$emph_y_wid,
                              curve_type = extract_breaks(input$curve_type), curve_width = extract_breaks(input$curve_width),
                              segment_type = extract_breaks(input$seg_type), segment_width = extract_breaks(input$seg_width), 
                              point_shape = extract_breaks(input$point_type), point_size = extract_breaks(input$point_size)
                              )
      

      
      })
  
  output$plot <- renderPlot(
      width = function() {60 * (input$size + 7)},
      height = function() {50 * (input$size + 7)},
      res = 96,
      {
        p$plot
      })
  
  output$download <- downloadHandler(
      filename = function() {
        save_ext <- input$save_type
        switch(save_ext,
               jpg = paste("myplot", Sys.Date(), ".jpg", sep=""),
               png  = paste("myplot", Sys.Date(), ".png", sep=""),
               pdf = paste("myplot", Sys.Date(), ".pdf", sep=""),
               svg = paste("myplot", Sys.Date(), ".svg", sep=""))
      },
      content = function(file) {
        ggsave(file, plot = p$plot, width = input$save_width, height = input$save_height, units = "in")
      })
  
  output$compare_curves <- renderPrint({
      req(input$comp_cline1)
      req(input$comp_cline2)
      compareCurves(datainput(), input$comp_cline1, input$comp_cline2, method = input$comp_method, PEmethod = input$comp_pemethod)
    })
  
  output$compare_details <- downloadHandler(
      filename = function() {
        paste(input$comp_cline1, "_vs_", input$comp_cline2, "_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        compare_model <- capture.output(compareCurves(datainput(), input$comp_cline1, input$comp_cline2, method = input$comp_method, PEmethod = input$comp_pemethod))
        writeLines(paste(compare_model, sep = "\n"), file)
      })
  
  output$der <- renderPrint({
      req(input$der_cline1)
      req(input$der_cline2)
      calculateDER(datainput(), input$der_cline1, input$der_cline2, method = input$der_method, PEmethod = input$der_pemethod, S = input$surv_frac)
    })
  
  
  output$der_results <- downloadHandler(
    filename = function() {
      paste(input$der_cline1, "_", input$der_cline2, "_", input$surv_frac, ".txt", sep = "")
    },
    content = function(file) {
      der_output <- capture.output(calculateDER(datainput(), input$der_cline1, input$der_cline2, S = input$surv_frac, method = input$der_method, PEmethod = input$der_pemethod))
      writeLines(paste(der_output, sep = "\n"), file)
    })
  
  extract_breaks <- function(text) {
      if (text == "") {
        waiver()
      } else {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
      }
    }
  
  observeEvent(input$reset_all, {
      reset("default")
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
