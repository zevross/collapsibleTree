library(shiny)
library(shinySelect)
library(dplyr)
# library(d3Tree)
# library(collapsibleTree)
devtools::load_all("../../../../collapsibleTree")
library(data.table)
library(data.tree)
library(datasets)
library(htmltools)


#   ____________________________________________________________________________
#   functions                                                               ####

#' for `{reactable}` custom filters, see
#' https://glin.github.io/reactable/articles/custom-filtering.html
# data_list_filter_1 <- function(table_id, style = "width: 100%; height: 28px;") {
#   function(values, name) {
#     data_list_id <- sprintf("%s-%s-list", table_id, name)
#     tagList(
#       tags$input(
#         type = "text",
#         list = data_list_id,
#         oninput = sprintf(
#           "Reactable.setFilter('%s', '%s', event.target.value || undefined)",
#           table_id,
#           name
#         ),
#         "aria-label" = sprintf("Filter %s", name),
#         style = style
#       ),
#       tags$datalist(
#         id = data_list_id,
#         lapply(unique(values), function(value) tags$option(value = value))
#       )
#     )
#   }
# }

# data_list_filter_2 <- function(table_id, style = "width: 100%; height: 28px;") {
#   function(values, name) {
#     tagList(
#       tags$select(
#         # Set to undefined to clear the filter
#         onchange = sprintf(
#           "Reactable.setFilter('%s', '%s', event.target.value || undefined)",
#           table_id,
#           name
#         ),
#         # "All" has an empty value to clear the filter, and is the default option
#         tags$option(value = "", "All"),
#         lapply(unique(values), tags$option),
#         "aria-label" = sprintf("Filter %s", name),
#         style = style
#       )
#     )
#   }
# }


#   ____________________________________________________________________________
#   load mock data dictionary                                               ####

data_dict <- readRDS("data_dict.Rds")
data_tree <- data_dict %>%
  count(
    domain,
    recipient,
    assessment,
    instrument
  ) %>%
  mutate(n = paste0(as.character(n), " variables")) %>%
  as.data.table()

variables <- names(data_tree)
rootName <- "ABCD"


#   ____________________________________________________________________________
#   Shiny                                                                   ####

##  ............................................................................
##  ui                                                                      ####

ui <- fluidPage(
  fluidRow(
    # left side of page
    column(
      5,
      column(
        8,
        style = "margin-top: 8px;",
        shinySelect::selectControlInput(
          "Hierarchy",
          "Tree Hierarchy",
          choices = variables,
          multiple = TRUE,
          sortable = TRUE,
          selected = c(
            "domain",
            "recipient",
            "instrument",
            "n"
          )
        )
      ),
      # d3treeOutput(
      #   outputId = "d3",
      #   width = '1200px',
      #   height = '1200px'
      # )
      collapsibleTree::collapsibleTreeOutput(
        "d3",
        width = '1200px',
        height = '1200px'
      )
    ),

    # right side of page
    column(
      7,
      style = "margin-top: 10px;",
      selectizeInput(
        "Selected",
        "Columns to display",
        choices = c(
          Domain = "domain",
          Recipient = "recipient",
          `Variable Name` = "var_name",
          Table = "table",
          Instrument = "instrument",
          Assessment = "assessment",
          `Variable Label` = "var_label",
          Type = "type",
          `Variable Label (Short)` = "var_label_short",
          `NDA Table` = "nda_table",
          `NDA Variable Name` ="nda_var_name"
        ),
        multiple = TRUE,
        selected = c(
          "var_name",
          "table",
          "instrument",
          "var_label",
          "type",
          "nda_table",
          "nda_var_name"
        ),
        options = list(plugins = list('drag_drop', 'remove_button'))
      ),
      reactable::reactableOutput(
        'filteredTableOut',
        width = "100%",
        height = "1200px"
      )
    )
  )
)


##  ............................................................................
##  server                                                                  ####

server <- function(input, output, session) {

  network <- reactiveValues(
    click = data.frame(name = NA, value = NA, depth = NA, id = NA)
  )

  observeEvent(input$d3_update, {

    browser()

    network$nodes <- unlist(input$d3_update$.nodesData$data)
    # activeNode <- input$d3_update$.activeNode
    # if (!is.null(activeNode))
    #   network$click <- jsonlite::fromJSON(activeNode)
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # output$clickView <- renderTable({
  #   req({as.data.table(network$click)})
  #
  #   # browser()
  #
  # }, caption = 'Last Clicked Node', caption.placement = 'top')

  toListen <- reactive({

    # browser()

    list(network$nodes,input$Selected)
  })

  filteredTable <- eventReactive(toListen(), {

    # filteredTable <- reactiveVal(NULL)

    # observeEvent(input$d3_update, {

    # browser()

    if (is.null(network$nodes)) {
      data_dict
      columns <- names(data_dict)
    } else{
      columns <- names(data_dict)
      if (!is.null(input$Selected)) {
        columns <- input$Selected
      }

      # browser()

      filterStatements <- d3Tree::tree.filter(network$nodes, data_tree)
      filterStatements$FILTER <- gsub(
        pattern = rootName,
        replacement = variables[1],
        x = filterStatements$FILTER
      )
      # remove n=... filter
      filterStatements$FILTER <- gsub(
        pattern = "&n=='.*'",
        replacement = "",
        x = filterStatements$FILTER
      )
      network$filterStatements <- filterStatements
      data_dict <- data_dict %>%
        filter_(
          paste0(
            network$filterStatements$FILTER,
            collapse = " | "
          )
        ) %>%
        select(columns)

    }

    column_def <- list(
      table = reactable::colDef(show = FALSE),
      var_name = reactable::colDef(
        name = "Variable Name",
        minWidth = 150,
        cell = function(value, index) {
          table <- data_dict$table[index]
          table <- if (!is.na(table)) table else "Unknown"
          div(
            div(
              style = list(
                fontFamily = "Hasklug Nerd Font Mono",
                fontWeight = 600
              ),
              value
            ),
            div(style = list(fontSize = 12), table)
          )
        }
      ),
      var_label = reactable::colDef(
        name = "Variable Label",
        minWidth = 300
      ),
      nda_var_name = reactable::colDef(
        name = "NDA Variable Name",
        style = list(
          fontColor = "gray"
        ),
      ),
      nda_table = reactable::colDef(
        name = "NDA Table"
      ),
      domain = reactable::colDef(
        name = "Domain"
      ),
      recipient = reactable::colDef(
        name = "Recipient"
      ),
      instrument = reactable::colDef(
        name = "Instrument"
      ),
      assessment = reactable::colDef(
        name = "Assessment"
      ),
      var_label_short = reactable::colDef(
        name = "Variable Label (Short)"
      ),
      type = reactable::colDef(
        name = "Type"
      )
    )

    column_def <- column_def[names(column_def) %in% columns]

    # browser()

    reactable::reactable(
      data_dict,
      columns = column_def,
      # defaultColDef = reactable::colDef(
      #   filterInput = function(values, name) {
      #     if (name %in% c("instrument", "table")) {
      #       data_list_filter_1("table-select")(values, name)
      #     } else if (name %in% c("domain", "recipient", "assessment", "type")) {
      #       data_list_filter_2("table-select")(values, name)
      #     }
      #   }
      # ),
      theme = reactable::reactableTheme(
        cellStyle = list(
          display = "flex",
          flexDirection = "column",
          justifyContent = "center"
        ),
        stripedColor = "#f0f5f9",
        highlightColor = "#f0f5f9",
        searchInputStyle = list(width = "100%")
      ),
      language = reactable::reactableLang(
        searchPlaceholder = paste0(
          "Enter string to filter all columns of the data dictionary at ",
          "once or use the column-specific filter options below..."
        )
      ),
      selection = "multiple",
      onClick = "select",
      striped = FALSE,
      highlight = TRUE,
      compact = TRUE,
      searchable = TRUE,
      filterable = TRUE,
      showSortable = TRUE,
      resizable = TRUE,
      wrap = FALSE,
      defaultPageSize = 50,
      elementId = "table-select"
    )
    # }

    # browser()

    # filteredTable(out)

    # out

  }, ignoreNULL = FALSE)

  # output$d3 <- renderD3tree({
  #   if (is.null(input$Hierarchy)) {
  #     selectedCols <- variables
  #   } else{
  #     selectedCols <- input$Hierarchy
  #   }
  #
  #   d3tree(
  #     data = list(
  #       root = df2tree(
  #         struct = data_tree[, .SD, .SDcols=selectedCols][, "dummy.col" := ''],
  #         rootname = rootName
  #       ),
  #       layout = 'collapse'
  #     ),
  #     activeReturn = c('name', 'value', 'depth', 'id'),
  #     height = 18
  #   )
  # })

  output$d3 <- collapsibleTree::renderCollapsibleTree({
    if (is.null(input$Hierarchy)) {
      selectedCols <- variables
    } else{
      selectedCols <- input$Hierarchy
    }

    browser()

    collapsibleTree::collapsibleTree(
      df = data_tree[, .SD, .SDcols=selectedCols][, "dummy.col" := ''],
      hierarchy = selectedCols,
      root = "ABCD",
      # height = 18,

      tooltip = TRUE,
      # zoomable = TRUE,
      # collapsed = TRUE,

      inputId = "d3_update"

      #### DEFAULTS ####
      # inputId = NULL,
      # attribute = "leafCount",
      # aggFun = sum,
      # fill = "lightsteelblue",
      # fillByLevel = TRUE,
      # linkLength = NULL, # (i.e. `automatic`)
      # fontSize = 10,
      # nodeSize = NULL,
      # width = NULL,
      # heigth = NULL
    )

  })

  # output$filteredTableOut <- reactable::renderReactable({
  #
  #   if (is.null(network$nodes)) {
  #     out <- data_dict
  #   } else{
  #     columns <- names(data_dict)
  #     if (!is.null(input$Selected)) {
  #       columns <- input$Selected
  #     }
  #
  #     browser()
  #
  #     filterStatements <- d3Tree::tree.filter(network$nodes, data_tree)
  #     filterStatements$FILTER <- gsub(
  #       pattern = rootName,
  #       replacement = variables[1],
  #       x = filterStatements$FILTER
  #     )
  #     # remove n=... filter
  #     filterStatements$FILTER <- gsub(
  #       pattern = "&n=='.*'",
  #       replacement = "",
  #       x = filterStatements$FILTER
  #     )
  #     network$filterStatements <- filterStatements
  #     data_dict <- data_dict %>%
  #       filter_(
  #         paste0(
  #           network$filterStatements$FILTER,
  #           collapse = " | "
  #         )
  #       ) %>%
  #       select(columns)
  #
  #     column_def <- list(
  #       table = reactable::colDef(show = FALSE),
  #       var_name = reactable::colDef(
  #         name = "Variable Name",
  #         minWidth = 150,
  #         cell = function(value, index) {
  #           table <- data_dict$table[index]
  #           table <- if (!is.na(table)) table else "Unknown"
  #           div(
  #             div(
  #               style = list(
  #                 fontFamily = "Hasklug Nerd Font Mono",
  #                 fontWeight = 600
  #               ),
  #               value
  #             ),
  #             div(style = list(fontSize = 12), table)
  #           )
  #         }
  #       ),
  #       var_label = reactable::colDef(
  #         name = "Variable Label",
  #         minWidth = 300
  #       ),
  #       nda_var_name = reactable::colDef(
  #         name = "NDA Variable Name",
  #         style = list(
  #           fontColor = "gray"
  #         ),
  #       ),
  #       nda_table = reactable::colDef(
  #         name = "NDA Table"
  #       ),
  #       domain = reactable::colDef(
  #         name = "Domain"
  #       ),
  #       recipient = reactable::colDef(
  #         name = "Recipient"
  #       ),
  #       instrument = reactable::colDef(
  #         name = "Instrument"
  #       ),
  #       assessment = reactable::colDef(
  #         name = "Assessment"
  #       ),
  #       var_label_short = reactable::colDef(
  #         name = "Variable Label (Short)"
  #       ),
  #       type = reactable::colDef(
  #         name = "Type"
  #       )
  #     )
  #
  #     column_def <- column_def[names(column_def) %in% columns]
  #
  #     # browser()
  #
  #     out <- reactable::reactable(
  #       data_dict,
  #       columns = column_def,
  #       # defaultColDef = reactable::colDef(
  #       #   filterInput = function(values, name) {
  #       #     if (name %in% c("instrument", "table")) {
  #       #       data_list_filter_1("table-select")(values, name)
  #       #     } else if (name %in% c("domain", "recipient", "assessment", "type")) {
  #       #       data_list_filter_2("table-select")(values, name)
  #       #     }
  #       #   }
  #       # ),
  #       theme = reactable::reactableTheme(
  #         cellStyle = list(
  #           display = "flex",
  #           flexDirection = "column",
  #           justifyContent = "center"
  #         ),
  #         stripedColor = "#f0f5f9",
  #         highlightColor = "#f0f5f9",
  #         searchInputStyle = list(width = "100%")
  #       ),
  #       language = reactable::reactableLang(
  #         searchPlaceholder = paste0(
  #           "Enter string to filter all columns of the data dictionary at ",
  #           "once or use the column-specific filter options below..."
  #         )
  #       ),
  #       selection = "multiple",
  #       onClick = "select",
  #       striped = FALSE,
  #       highlight = TRUE,
  #       compact = TRUE,
  #       searchable = TRUE,
  #       filterable = TRUE,
  #       showSortable = TRUE,
  #       resizable = TRUE,
  #       wrap = FALSE,
  #       defaultPageSize = 50,
  #       elementId = "table-select"
  #     )
  #   }
  #
  #   out
  # })

  output$filteredTableOut <- reactable::renderReactable({
    # browser()

    filteredTable()
  })

}


##  ............................................................................
##  app                                                                     ####

shinyApp(ui = ui, server = server)
