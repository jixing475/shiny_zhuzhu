library(tidyverse)
here <- here::here
select <- dplyr::select
library(casewhen)
parse_number_cw <- create_case_when(
  str_detect(x, "%") ~ parse_number(x) * 0.01,
  str_detect(x, "^") ~ parse_number(x) * 10^as.numeric(str_extract(x ,pattern = "(?<=\\^)[0-9]+")),
  is.character(x) ~ as.numeric(x),
  vars = "x")

table_atom <- function(path="zhuzhu.txt"){
  raw <- read_file(path)
  raw_split <- 
    raw %>% 
    str_split(";") %>% 
    .[[1]] %>% 
    .[!.==""] 
  
  who_is_twice <- 
    raw_split %>% 
    regmatches(gregexpr(":", .)) %>% 
    map(~length(.x)) %>% 
    unlist()
  
  dat <- if_else(who_is_twice==2, str_extract(raw_split, "(?<=:).+"), raw_split) 
  
  dat_table <- 
    dat %>% 
    data.frame(raw_data = .) %>% 
    separate(col = raw_data, into = c("variable","value"), sep = ":") %>% 
    mutate(var_zh  = str_extract(variable, ".+(?=\\()")) %>% # extract 
    mutate(var_en  = str_extract(variable, "(?<=\\().+?(?=\\))")) %>% 
    mutate(unit  = str_replace(value, "[0-9.%×^]+", "") %>% str_replace("(↓|↑)", "")) %>% 
    mutate(value_text = str_extract(value, "[0-9.%×^]+") ) %>% 
    mutate(value_numb = if_else(!is.na(as.numeric(value_text)), as.numeric(value_text), parse_number_cw(value_text))) 
  
  #Remove special characters
  dat_table$var_en <- 
    dat_table$var_en %>% 
    str_replace_all("(%|#|=)", "")
  
  #guess_encoding(charToRaw("who am i"))
  #guess_encoding(charToRaw("我是谁"))
  dat_table$encoding <- 
    dat_table$unit %>%
    map(~ charToRaw(.x) %>% guess_encoding()) %>% 
    map(~ .x[[1]]) %>% 
    unlist() 
  
  #dat_table <-  
  #dat_table %>% 
  #mutate(value_numb = ifelse(var_en=="ID", value, value_numb)) %>% 
  #mutate(value_numb = ifelse(var_en=="date", value, value_numb)) %>% 
  #mutate(value_numb = if_else(encoding=="UTF-8", unit, value_numb)) 
  res_tmp <-
    dat_table %>% 
    select(value_numb) %>% 
    t() %>% 
    as.data.frame()
  colnames(res_tmp) <- dat_table$var_en
  # 给每单位的变量赋值
  for (i in 1:length(dat_table$encoding)) {
    if(dat_table$encoding[i]=="UTF-8"){
      res_tmp[1, i] <- dat_table$value[i]
    }
  }
  
  res_tmp$ID <- dat_table %>% filter(var_en=="ID") %>% select(value) %>% .[[1]]
  res_tmp$date <- dat_table %>% filter(var_en=="date") %>% select(value) %>% .[[1]]
  rownames(res_tmp) <- NULL
#  which_is_numb <- map(res_tmp, is.numeric) %>% unlist()
#  for (i in 1:length(which_is_numb)) {
#    if(which_is_numb[i] & dat_table$unit[i]!=""){
#      colnames(res_tmp)[i] <- 
#        str_c(colnames(res_tmp)[i], 
#              "(", 
#              dat_table$unit[i] %>% str_replace("^/", ""),
#              ")")
#    }
#  }
  return(res_tmp)
}
  
#==== zer_DT_table ====
library(tidyverse)
zero_DT_table <- partial(DT::datatable,  extensions = c('Buttons', 'FixedColumns', 'Scroller'), 
                         options = list(fixedColumns = TRUE, 
                                        scrollY = 400,
                                        scrollX = TRUE,
                                        scroller = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('colvis', 'copy', 'print', 'csv','excel', 'pdf'),
                                        columnDefs = list(
                                          list(targets = c(3,4,5), visible = FALSE)
                                        )),
                         filter = 'bottom')

# Module UI function
dataFileInput <- function(id, label = "Data File") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label)
  )
}


# Module server function
dataFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    rio::import(userFile()$datapath)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
}


multiFileInput <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("files"), label="Upload txt Files Here", multiple = TRUE)
  )
}

multiFile <- function(input, output, session){
  dataframe <- reactive({
    input$files$datapath %>%
      #set_names() %>% #给向量添加名字
      map(table_atom) %>% 
      #imap(~ transform(.x, filename = basename(.y))) %>% 
      do.call(bind_rows,.) 
  })
  return(dataframe)
}
