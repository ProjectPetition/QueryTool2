library(shiny)
library(dplyr)
library(reshape2)
library(ggplot2)
library(choroplethr)
library(choroplethrZip)
library(zipcode)
## library(lubridate)
## library(tseries)
Sys.setlocale('LC_ALL','C')

config = readLines("config.txt")
defaultdb = config[1]
user = config[2]
pass = config[3]

wtp = src_mysql(dbname = defaultdb, user = user,
    password = pass)
dbs = DBI::dbGetQuery(wtp$con, "show databases")
data(zipcode)
data(df_pop_state)

shinyServer(function(input, output) {
  output$databases = renderUI({
    selectInput("database", "Database:", dbs$Database,
                selected = defaultdb)
  })
  db = reactive({
    if ("database" %in% names(input)) {
      src_mysql(input$database, user = user, password = pass)
    } else {
      NA
    }
  })
  output$tables = renderUI({
    if (is.na(db())) {
      selectInput("table", "Table:", choices = NULL)
    } else {
      selectInput("table", "Table:", db() %>% src_tbls())
    }
  })
  datasetInput = reactive({
    if ("table" %in% names(input) &&
          input$table %in% (db() %>% src_tbls())) {
      db() %>% tbl(input$table)
    } else {
      NULL
    }
  })
  output$filters = renderUI({
    if (!is.na(datasetInput())) {
      classes = datasetInput() %>% head() %>% sapply(class)
      chars = which(classes == "character")
      colnames = names(classes)[chars]
      if (length(chars) > 0) {
        fields = list()
        for (n in 1:length(chars)) {
          ui_name = paste0(input$table, "-filter-", colnames[n])
          ui_text = paste0("Search ", colnames[n], ":")
          fields[[n]] = textInput(ui_name, ui_text)
        }
        tag("Filters", fields)
      }
    }
  })
  whereClause = reactive({
    if (length(grep("filter", names(input))) > 0) {
      filters = names(input)[grep("filter", names(input))]
      colnames = gsub("^.*filter-", "", filters)
      tables = gsub("-filter.*$", "", filters)
      conditions = rep("", length(colnames))
      for (n in 1:length(colnames)) {
        query = input[[filters[n]]]
        if (query != "" && tables[n] == input$table)
          conditions[n] =
            paste0(colnames[n], " %like% '%", query, "%'")
      }
      conditions = conditions[conditions != ""]
      all_conditions = paste(conditions, collapse = " & ")
    }
    if (all_conditions == "") {
    } else {
      all_conditions
    }
  })
  datasetFiltered = reactive({
    if (!is.null(whereClause())) {
      datasetInput() %>% filter_(whereClause())
    } else {
      datasetInput()
    }
  })
  output$text1 = renderText({
    if (!is.null(whereClause())) {
      paste0("Head of '", input$table, "' table from database '",
             input$database, "', ", whereClause(), ":")
    } else {
      paste0("Head of '", input$table, "' table from database '",
             input$database, "':")
    }
  })
  output$mainTable = renderTable({
    datasetFiltered() %>% head()
  })
  output$downloadData = downloadHandler(
      filename = function() { 
        paste0(input$table, '.csv') 
      },
      content = function(file) {
        write.csv(datasetFiltered(), file, row.names = F)
      }
  )

  # Petitions page
  output$text2 = renderText({ 
    paste0("Daily signature counts:")
  })
  ids = reactive({
    strsplit(input$petID, ",")[[1]]
  })
  datasetInput2 = reactive({
    ids = ids()
    if (length(ids) == 1) {
      wtp %>% tbl("wtp_data_signatures") %>%
        filter(petition_id == ids) %>%
          mutate(date = date(from_unixtime(created))) %>%
            group_by(petition_id, date) %>%
              summarize(count = n())
    } else {
      wtp %>% tbl("wtp_data_signatures") %>%
        filter(petition_id %in% ids) %>%
          mutate(date = date(from_unixtime(created))) %>%
            group_by(petition_id, date) %>%
              summarize(count = n())
    }
  })
  petCreated = reactive({
    ids = ids()
    if (length(ids) == 1) {
      tmp = wtp %>% tbl("wtp_data_petitions") %>%
        filter(id == ids) %>%
          mutate(date = date(from_unixtime(created))) %>%
            select(id, date) %>% collect()
    } else {
      tmp = wtp %>% tbl("wtp_data_petitions") %>%
        filter(id %in% ids) %>%
          mutate(date = date(from_unixtime(created))) %>%
            select(id, date) %>% collect()
    }
    tmp$date = as.Date(tmp$date)
    tmp
  })
  petition_df = reactive({
    created = petCreated()
    df = datasetInput2() %>% collect()
    df$day = as.Date(df$date) -
      created$date[match(df$petition_id, created$id)] + 1
    print(class(df$count))
    df$day = as.integer(df$day)
    df$count = as.integer(df$count)
    dcast(df, day ~ petition_id, value.var = "count") %>%
      filter(day <= 30)
  })
  output$petitionTable = renderTable({
    if (input$tsbutton > 0) isolate(petition_df())
  })
  output$plot <- renderPlot({
    if (input$tsbutton > 0) {
      df = melt(isolate(petition_df()), id = "day",
          variable.name = "petition_id",
          value.name = "count")
      print(head(df))
      p = ggplot(df, aes(x = day, y = count,
          group = petition_id))
      p + geom_line(aes(color = petition_id)) +
        labs(title = "Signatures per Day",
             x = "Day", y = "Count") +
               theme(title = element_text(size = 18),
                     axis.title = element_text(size = 16),
                     legend.title = element_text(size = 16),
                     axis.text = element_text(size = 12),
                     legend.text = element_text(size = 12)) +
                       guides(color = guide_legend(title = "Petition ID"))
    }
  })
  output$downloadData2 = downloadHandler(
      filename = function() { 
        paste0("30daycount_",
               paste(ids(), collapse = "_"),
               '.csv') 
      },
      content = function(file) {
        write.csv(petition_df(), file, row.names = F)
      }
  )

  # Geography
  output$geoText = renderText({
    if (input$geobutton > 0)
      paste0("Signatures per ",
             tolower(isolate(input$geoType)), ":")
  })
  geoIds = reactive({
    if (input$geobutton > 0 || input$mapbutton > 0)
      strsplit(isolate(input$geopetID), ",")[[1]]
  })
  datasetGeo = reactive({
    ids = geoIds()
    tmp = wtp %>% tbl("wtp_data_signatures")
    if (length(ids) == 1) {
      tmp = tmp %>% filter(petition_id == ids)
    } else {
      tmp = wtp %>% filter(petition_id %in% ids) 
    }
    if (input$geoType == "ZIP Code") {
      tmp = tmp %>%
        group_by(petition_id, zip) %>%
          dplyr::summarize(count = n()) %>% collect()
      newdf =
        data.frame(petition_id = rep(tmp$petition_id[1], nrow(zipcode)),
                   zip = zipcode$zip, state = zipcode$state)
      newdf$count = tmp$count[match(newdf$zip, tmp$zip)]
      newdf$count[is.na(newdf$count)] = 0
      newdf$count = as.integer(newdf$count)
      newdf
    } else {
      states = unique(zipcode$state)
      state_names = tolower(state.name[match(states, state.abb)])
      tmp = tmp %>% collect()
      tmp$state = zipcode$state[match(tmp$zip, zipcode$zip)]
      state_sigs = table(tmp$state)
      newdf =
        data.frame(petition_id = rep(tmp$petition_id[1], length(states)),
                   state = states,
                   count = state_sigs[states])
      newdf$per_capita = newdf$count /
        df_pop_state$value[match(state_names, df_pop_state$region)]
      newdf
    }
  })
  output$geoTable = renderTable({
    ## petition_df()
    if (input$geobutton > 0 || input$mapbutton > 0) {
      if (input$geoType == "ZIP Code") {
        datasetGeo() %>% head()
      } else {
        datasetGeo()
      }
    }
  })
  output$map = renderPlot({
    if (input$mapbutton > 0) {
      df = datasetGeo()
      if (input$geoType == "ZIP Code") {
        df$region = df$zip
        df$value = df$count
        zip_choropleth(df, legend = "Signatures")
      } else if (input$geoType == "State") {
        df$region = tolower(state.name[match(df$state, state.abb)])
        df$value = df$per_capita
        df = na.omit(df)
        state_choropleth(df, legend = "Signatures Per Capita")
      }
    }
  })
  output$geoDownload = downloadHandler(
      filename = function() {
        region = gsub(" ", "", tolower(input$geoType))
        paste0("signatureby", region, "_",
               paste(ids(), collapse = "_"), ".csv") 
      },
      content = function(file) {
        write.csv(datasetGeo(), file, row.names = F)
      }
  )
})
