# specific (gui)-options for csv-import
output$ui_import_csv <- renderUI({
  rb1 <- radioButtons("import_csv_header", label=h5(translate("Does the first row contain the variable names?")), choices=c(TRUE,FALSE), inline=TRUE)
  rb2 <- radioButtons("import_csv_sep", label=h5(translate("Select the field separator")), choices=c(Comma=",", Semicolon=";", Tab="\t"), inline=TRUE)
  return(fluidRow(
    column(6, rb1, align="center"),
    column(6, rb2, align="center")))
})

# specific (gui)-options for r-dataframe import
output$ui_import_rdf <- renderUI({
  selDF <- selectInput("sel_choose_df", label=NULL, choices=available_dfs,
    selected=input$sel_choose_df, width="50%")
  btn <- myActionButton("btn_chooose_df",label=translate("Load data"), "primary")
  return(fluidRow(
    column(12, h5(translate("Select a test dataset or any object in your current workspace"), align="center")),
    column(12, div(selDF, align="center")),
    column(12, p(btn, align="center"))))
})

# specific (gui)-options for sas import
output$ui_import_sas <- renderUI({
  return(NULL)
})

# specific (gui)-options for spss import
output$ui_import_spss <- renderUI({
  return(NULL)
})

# specific (gui)-options for stata import
output$ui_import_stata <- renderUI({
  return(NULL)
})

output$ui_import_data_main <- renderUI({
  cur_error <- lastError()
  btn <- myActionButton("btn_reset_inputerror",label=("Try again!"), "primary")
  if (!is.null(lastError())) {
    return(fluidRow(
      column(12, h4(translate("Importing data resulted in an error!")), align="center"),
      column(12, verbatimTextOutput("ui_lasterror")),
      column(12, btn, align="center")))
  }

  val <- obj$cur_selection_import
  if (val=="btn_import_data_1") {
    val <- "rdf"
  }
  if (val=="btn_import_data_2") {
    val <- "rdata"
  }
  if (val=="btn_import_data_3") {
    val <- "spss"
  }
  if (val=="btn_import_data_4") {
    val <- "sas"
  }
  if (val=="btn_import_data_5") {
    val <- "csv"
  }
  if (val=="btn_import_data_6") {
    val <- "stata"
  }
  out <- fluidRow(
    column(width = 12, offset = 0, h3(translate("Uploading microdata")), class="wb-header"),
    column(width = 12, offset = 0, p(translate("Load the dataset to be anonymized.")), class="wb-header-hint")
  )

  if (val %in% c("R","csv","spss","sas","rdata","stata")) {
    # convert characters automatically to factors
    rb1 <- radioButtons("rb_convert_c_to_f", label=h5(translate("Convert string variables (character vectors) to factor variables?")), choices=c(TRUE, FALSE), inline=TRUE)
    rb2 <- radioButtons("rb_drop_all_missings", label=h5(translate("Drop variables with only missing values (NA)?")), choices=c(TRUE, FALSE), inline=TRUE)

    out <- list(out, fluidRow(column(12, h5(translate("Set additional options for the data import"), align="center"))))

    out <- list(out, fluidRow(
      column(6, rb1, align="center"),
      column(6, rb2, align="center")))

    if (val == "csv") {
      allowed <- c(".txt",".csv")
      out <- list(out, uiOutput("ui_import_csv"))
    }
    if (val == "spss") {
      allowed <- c(".sav")
      out <- list(out, uiOutput("ui_import_spss"))
    }
    if (val == "sas") {
      allowed <- c(".sas7bdat")
      out <- list(out, uiOutput("ui_import_sas"))
    }
    if (val == "rdata") {
      allowed <- c(".rdata")
    }
    if (val == "stata") {
      allowed <- c(".dta")
      out <- list(out, uiOutput("ui_import_stata"))
    }

    out <- list(out, fluidRow(
      column(12, p(translate("Note: the selected file is loaded immediately upon selecting. Set the above options before selecting the file.")), align="center")
    ))

    fI <- fileInput("file1", h5(paste0(translate("Select file (allowed types are"), " '", paste0(allowed, collapse="', '"),"')")),
      width="75%", accept=allowed, buttonLabel=translate("Browse..."))
    out <- list(out, fluidRow(column(12, fI, align="center")))
  } else {
    out <- list(out, uiOutput("ui_import_rdf"))
  }
  out
})

output$ui_import_data_sidebar_left <- renderUI({
  output$ui_sel_resbtns_import <- renderUI({
    cc <- c(paste0(translate("Testdata"), "/", translate("internal data")), paste0(translate("R-dataset"), " (.rdata)"), paste0(translate("SPSS-file"), " (.sav)"), 
            paste0(translate("SAS-file"), " (.sas7bdat)"), paste0(translate("CSV-file"), " (.csv, .txt)"), paste0(translate("STATA-file"), " (.dta)"))
    out <- fluidRow(column(12, h4(translate("Select data source"))))
    for (i in 1:length(cc)) {
      id <- paste0("btn_import_data_", i)
      if (obj$cur_selection_import==id) {
        style <- "primary"
      } else {
        style <- "default"
      }
      out <- list(out, fluidRow(
        # column(12, bsButton(id, label=cc[i], block=TRUE, size="extra-small", style=style), tags$br())
        column(12, bsButton(id, label=cc[i], block=TRUE, size="extra-small", style=style))
      ))
    }
    out
  })

  # required observers that update the color of the active button!
  eval(parse(text=genObserver_menus(pat="btn_import_data_", n=1:6, updateVal="cur_selection_import")))
  return(uiOutput("ui_sel_resbtns_import"))
})
output$ui_import_data <- renderUI({
  fluidRow(
    column(2, uiOutput("ui_import_data_sidebar_left"), class="wb_sidebar"),
    column(10, uiOutput("ui_import_data_main")))
})

output$ui_show_changed_labels <- renderUI({
  # Show changed labels
  txtChangedLabels1 <- translate("Some strings in the variable names, variable labels and/or value labels in the loaded dataset do not comply with UTF-8 encoding. This application requires strings to be UTF-8 encoded or UTF-8 compatible. The application has attempted to convert the non-compliant characters to UTF-8. ")
  txtChangedLabels2 <- translate("Below is a summary of the strings containing characters that are not compliant with UTF-8 encoding after been automatically converted to UTF-8 encoding. While the auto conversion process attempts to do the best it can to fix the issues, results are often uneven. To avoid the auto conversion of characters it is advised that the problems first be corrected in the original dataset and then reloaded into this application. Doing so will prevent unnecessary loss of information. ")
  txtChangedLabels3 <- translate("Alternatively, you can continue and allow the application to auto convert the characters. The summary below shows the strings after auto conversion. Continuing with the converted characters may result in changed variable names and variable labels in the exported anonymized dataset. ")
  txtChangedLabels4 <- translate("Note: The application will not allow the automatic conversion of values contained in string variables as this may result in changes to the actual data. In this case the data will need to be fixed before reloading into the application.")
  btn1 <- bsButton("btn_reset_inputdata_xx", label=(translate("Reset inputdata")), block=TRUE, style="default", size="extra-small")
  btn2 <- bsButton("btn_acc_utf8_conv", label=(translate("Continue with converted strings")), block=TRUE, style="default", size="extra-small")
  out <- fluidRow(
      column(12, h3(translate("Strings not in UTF-8 encoding"), align="center")),
      column(12, txtChangedLabels1, align="center"),
      column(12, tags$br()),
      column(12, txtChangedLabels2, align="center"),
      column(12, tags$br()),
      column(12, txtChangedLabels3, align="center"),
      column(12, tags$br()),
      column(12, txtChangedLabels4, align="center"),
      column(12, tags$br())
      )
  if (is.null(attr(obj$inputdata, "nonUTF")[[2]])) { # only show accept button if no changes to actual values are made
    out <- list(out, fluidRow(column(6, btn1), column(6, btn2)))
  } else {
    out <- list(out, fluidRow(column(12, btn1, align="center")))
  }
  if (!is.null(attr(obj$inputdata, "nonUTF")[[1]])) {
    df1 <- as.data.frame(attr(obj$inputdata, "nonUTF")[[1]][, 2])
    colnames(df1) <- c(translate("Converted variable name"))
    out <- list(out, fluidRow(
      column(12, h5(translate("Changed variable names"), align="center")),
      column(12, renderTable(df1, colnames = TRUE), align = "center")
    ))
  }
  if (length(attr(obj$inputdata, "nonUTF"))==3) {
    if (!is.null(attr(obj$inputdata, "nonUTF")[[3]])) {
      df2 <- as.data.frame(attr(obj$inputdata, "nonUTF")[[3]][,c(1,3)])
      colnames(df2) <- c(translate("Variable name"), translate("Converted variable label"))
      out <- list(out, fluidRow(
        column(12, h5(translate("Changed variable labels"), align="center")),
        column(12, renderTable(df2, colnames = TRUE), align = "center")
      ))
    }
  }
  if (!is.null(attr(obj$inputdata, "nonUTF")[[2]])) {
    df3 <- as.data.frame(attr(obj$inputdata, "nonUTF")[[2]][,c(1,3)])
    colnames(df3) <- c(translate("Variable name"), translate("Converted value label"))
    out <- list(out, fluidRow(
      column(12, h5(translate("Changed value labels"), align="center")),
      column(12, renderTable(df3), align = "center")
    ))
  }
  out
})

output$ui_inputdata <- renderUI({
  if (is.null(obj$inputdata)) {
    uiOutput("ui_import_data")
  } else if(obj$utf8) {
    uiOutput("ui_show_changed_labels")
  } else {
    uiOutput("ui_modify_data")
  }
})
