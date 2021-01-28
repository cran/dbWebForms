#' Add single quotes to strings, useful for converting R strings into SQL formatted strings.
#'
#' @param x A string.
#' @param char_only TRUE/FALSE, if TRUE, adds quotes only if is.character(x) is TRUE.
#' @return A string, with single quotes added to match postgreSQL string formatting.
#' @examples
#' quoteText("Sample quotes.")
quoteText <- function(x, char_only = TRUE){

  if(char_only == TRUE){
    if(is.character(x) == TRUE){
      return(paste0("'", gsub("\'", "''", x), "'"))
    }else{
      return(x)
    }
  }else{
    return(paste0("'", gsub("\'", "''", x), "'"))
  }

}

#' Query INFORMATION_SCHEMA or equivalent SQL meta data to obtain column names and types for a table.
#'
#' @param con A database connection that can be passed to DBI::dbSendQuery/DBI::dbGetQuery.
#' @param sql A string, the type of SQL database con is connected to; must be one of c("MariaDB", "Microsoft SQL Server", "MySQL", "PostgreSQL", "SQLite").
#' @param table_catalog A string, the catalog (usually a database name) name of the SQL table to return column meta data for. Not used if sql = "SQLite".
#' @param table_schema A string, the schema name of the SQL table to return column meta data for.
#' @param table_name A string, the name of the SQL table to return column meta data for.
#' @return A data.table, two columns, "column_name" has the names of the columns in the specified SQL table and "data_type" has the data types for each column.
#' If con is NULL, returns the SQL string for querying the meta data but does not execute the statement.
#' @examples
#' dbTableInfo(
#' con = NULL,
#' sql = "PostgreSQL",
#' table_catalog = "db1",
#' table_schema = "public",
#' table_name = "table1"
#' )
dbTableInfo <- function(con = NULL, sql = c("MariaDB", "Microsoft SQL Server", "MySQL", "PostgreSQL", "SQLite"), table_catalog = NULL, table_schema = NULL, table_name){

  if(sql %in% c("MariaDB", "Microsoft SQL Server", "MySQL", "PostgreSQL")){

    x <- paste0(
      "SELECT tbl.column_name column_name, tbl.data_type data_type FROM INFORMATION_SCHEMA.columns tbl WHERE ",
      if(is.null(table_catalog) == FALSE){paste0("tbl.table_catalog = ", quoteText(table_catalog))}else{""},
      if(is.null(table_schema) == FALSE){paste0(if(is.null(table_catalog) == FALSE){" AND "}else{""}, "tbl.table_schema = ", quoteText(table_schema))}else{""},
      paste0(if(is.null(table_catalog) == FALSE | is.null(table_schema) == FALSE){" AND "}else{""}, "tbl.table_name = ", quoteText(table_name)),
      " ORDER BY tbl.ordinal_position"
    )

    if(is.null(con) == FALSE){

      x <- as.data.table(DBI::dbGetQuery(con, x))

    }

  }else if(sql == "SQLite"){

    x <- paste0(
      "PRAGMA ",
      if(is.null(table_schema) == FALSE){paste0(table_schema, ".")}else{""},
      "table_info(", table_name, ");"
    )

    if(is.null(con) == FALSE){

      sort_col <- c("cid")

      x <- as.data.table(DBI::dbGetQuery(con, x))

      setorderv(x, sort_col)

      x <- x[, c("name", "type")]

      colnames(x) <- c("column_name", "data_type")

    }

  }

  if(is.null(con) == FALSE){

    x <- x[, lapply(.SD, tolower)]

  }

  return(x)

}

#' Convert strings to title case, splitting strings into separate words based on a separator.
#'
#' @param x A string.
#' @param split A string, used to split x into constituent words to be converted to title case.
#' @return A string, converted to title case with split words separated with a space character.
#' @examples
#' namesToLabels("date_of_birth", split = "_")
namesToLabels <- function(x, split = "_"){
  return(unlist(lapply(lapply(strsplit(x, split), stringi::stri_trans_totitle), paste0, collapse = " ")))
}

#' Convert SQL data types to likely HTML input types
#'
#' @param db_type A string, the SQL data type to convert to HTML input type.
#' @return A character vector, the HTML input type and attributes likely associated with the SQL data type.
#' @examples
#' dbTypeToHTMLInputType("int")
dbTypeToHTMLInputType <- function(db_type){

  db_type <- gsub("\\(.*.\\)", "", db_type)

  if(db_type %in% c(
    "int",
    "integer",
    "tinyint",
    "smallint",
    "mediumint",
    "bigint",
    "unsigned big int",
    "int2",
    "int8"
    )
  ){
    x <- c(type = "number", step = 1)
  }else if(db_type %in% c(
    "decimal",
    "smallmoney",
    "money",
    "numeric",
    "double",
    "double precision",
    "real",
    "float"
    )
  ){
    x <- c(type = "number")
  }else if(db_type %in% c(
    "boolean"
    )
  ){
    x <- c(type = "number", min = 0, max = 1, step = 1)
  }else if(db_type %in% c(
    "timestamp",
    "timestamp with time zone",
    "timestamp without time zone",
    "date",
    "datetime",
    "datetime2",
    "smalldatetime"
    )
  ){
    x <- c(type = "date")
  }else if(db_type %in% c(
    "time",
    "time with time zone",
    "time without time zone")
  ){
    x <- c(type = "time")
  }else{
    x <- c(type = "text")
  }

  return(
    x
  )

}

#' Based on the columns in a table, produces a R function with parameters for each column that produces a HTML form when called.
#'
#' @param filepath A string, the file path including the name and file type extension where the output will be written.
#' @param function_name A string, the name of the function to be written to the file path.
#' @param form_method A string, the method attribute for the HTML form tag, likely "GET" or "POST".
#' @param add_csrf_param TRUE/FALSE, if TRUE, adds a parameter for passing a value to a hidden input with a name of "crsf_token".
#' @param form_class A string, the class attribute for the HTML form tag.
#' @param input_class A string, the class attribute for the div tag wrapping the HTML inputs.
#' @param submit_class A string, the class attribute for the HTML button tag used to submit the form.
#' @param submit_label A string, the label attribute for the HTML button tag used to submit the form.
#' @param required_span_class A string, the class attribute for the HTML span tag optionally added to required fields.
#' @param required_span_label A string, the message for the HTML span tag optionally added to required fields.
#' @param x A data.table, should have a column of input ids (used for input names as well), and a column of HTML input types.
#' @param id_col A string, the column of x containing the HTML input ids.
#' @param type_col A string, the column of x containing the HTML input types (usually created by calling dbTypeToHTMLInputType()) .
#' @param labels A named character vector, names are the ids of the inputs and values are the labels to use. If a column is not specified here,
#' the default label is the result of namesToLabels() called for each input id.
#' @param select A character vector, ids included here will become select tags rather than input tags and a function parameter will be added to pass options.
#' @param exclude A character vector, ids included here will not be included as parameters for the resultant function.
#' @param optional A character vector, ids included here will not be marked as required inputs in the relevant HTML tags.
#' @param custom_input_types A named list of character vectors, names are the column ids, character vectors must have an entry named type with a value for
#' the HTML input type to be used and additional attributes can be included as subsequent named values in the character vector.
#' @return A character vector, the HTML input type and attributes likely associated with the SQL data type.
#' @examples
#' createHTMLFormFunction(
#' filepath = paste0(tempdir(), "/", "testHTMLFormFunction.R"),
#' function_name = "example_function",
#' form_method = "POST",
#' add_csrf_param = TRUE,
#' form_class ="pure-form pure-form-aligned",
#' input_class = "pure-control-group",
#' submit_class = "pure-button pure-button-primary",
#' submit_label = "Save",
#' required_span_class = "pure-form-message-inline",
#' required_span_label = "Required field",
#' x = as.data.table(
#' list(
#' ids = c("user_id", "user_name", "first_name", "last_name", "state"),
#' types = c("number", "text", "text", "text", "text")
#' )
#' ),
#' id_col = "ids",
#' type_col = "types",
#' labels = c(user_name = "Account Name"),
#' select = c("state"),
#' exclude = c("user_id"),
#' optional = c("first_name", "last_name"),
#' custom_input_types = list(user_name = c(type = "email", minlength = 5))
#' )
createHTMLFormFunction <- function(
  filepath,
  function_name,
  form_method = "POST",
  add_csrf_param = TRUE,
  form_class = NULL,
  input_class = NULL,
  submit_class = NULL,
  submit_label = "Save",
  required_span_class = NULL,
  required_span_label = NULL,
  x,
  id_col,
  type_col,
  labels = c(),
  select = c(),
  exclude = c(),
  optional = c(),
  custom_input_types = list()
){

  label_col <- "label_name"
  input_type_col <- "input_type"
  meta_input_col <- "meta_input"

  x <- x[!(base::get(id_col) %in% exclude), ]

  x[!(base::get(id_col) %in% names(labels)), paste0(label_col) :=  namesToLabels(base::get(id_col))]

  if(length(labels) > 0){

    for(i in 1:length(labels)){

      x[base::get(id_col) == names(labels)[i], paste0(label_col) := labels[i]]

    }

  }

  if(length(select) > 0){

    select_ids <- select
    select_function_inputs <- paste0("select_options_", select_ids)

  }else{
    select_ids <- select
    select_function_inputs <- NULL
  }

  x[(base::get(id_col) %in% select), paste0(input_type_col) := NA]

  for(i in 1:nrow(x)){

    i_id_col <- x[i, base::get(id_col)]
    i_label_name <- x[i, base::get(label_col)]
    i_input_type <- unlist(x[i, base::get(input_type_col)])

    if(is.na(i_input_type) == TRUE){

      if(i_id_col %in% optional){

        if(length(names(select) > 0)){
          i_meta_input <- paste0(
            'div(class = input_class, ',
            paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), "),
            paste0("select(id = ", quoteText(i_id_col), ", ", "name = ", quoteText(i_id_col), if(names(select[i_id_col]) == "multiple"){', multiple = "TRUE"'}else{''}, ", ", paste0("select_options_", i_id_col), ")"),
            ")"
          )
        }else{
          i_meta_input <- paste0(
            'div(class = input_class, ',
            paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), "),
            paste0("select(id = ", quoteText(i_id_col), ", ", "name = ", quoteText(i_id_col), ", ", paste0("select_options_", i_id_col), ")"),
            ")"
          )
        }

      }else{

        if(length(names(select) > 0)){
          i_meta_input <- paste0(
            'div(class = input_class, ',
            paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), "),
            paste0("select(required = TRUE, id = ", quoteText(i_id_col), ", ", "name = ", quoteText(i_id_col), if(names(select[i_id_col]) == "multiple"){', multiple = "TRUE"'}else{''}, ", ", paste0("select_options_", i_id_col), ")"),
            ', if(is.null(required_span_label) == FALSE){span(class = required_span_class, required_span_label)}else{""}',
            ")"
          )
        }else{
          i_meta_input <- paste0(
            'div(class = input_class, ',
            paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), "),
            paste0("select(required = TRUE, id = ", quoteText(i_id_col), ", ", "name = ", quoteText(i_id_col), ", ", paste0("select_options_", i_id_col), ")"),
            ', if(is.null(required_span_label) == FALSE){span(class = required_span_class, required_span_label)}else{""}',
            ")"
          )
        }

      }

    }else{

      if(i_id_col %in% names(custom_input_types)){

        i_custom_type <- custom_input_types[[i_id_col]]

        if(i_id_col %in% optional){
          i_meta_input <- paste0(
            'div(class = input_class, ',
            if(!("hidden" %in% i_custom_type)){paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), ")}else{""},
            paste0("input(name = ", quoteText(i_id_col), ", ", paste0(paste(names(i_custom_type), quoteText(i_custom_type), sep = " = "), collapse = ", "), ", value = ", i_id_col, ")"),
            ")"
          )
        }else{
          i_meta_input <- paste0(
            'div(class = input_class, ',
            if(!("hidden" %in% i_custom_type)){paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), ")}else{""},
            paste0("input(required = TRUE, name = ", quoteText(i_id_col), ", ", paste0(paste(names(i_custom_type), quoteText(i_custom_type), sep = " = "), collapse = ", "), ", value = ", i_id_col, ")"),
            if(!("hidden" %in% i_custom_type)){
              ', if(is.null(required_span_label) == FALSE){span(class = required_span_class, required_span_label)}else{""}'
              }else{""},
            ")"
          )
        }

      }else{

        if(i_id_col %in% optional){
          i_meta_input <- paste0(
            'div(class = input_class, ',
            paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), "),
            paste0("input(name = ", quoteText(i_id_col), ", ", paste0(paste(names(i_input_type), quoteText(i_input_type), sep = " = "), collapse = ", "), ", value = ", i_id_col, ")"),
            ")"
          )
        }else{
          i_meta_input <- paste0(
            'div(class = input_class, ',
            paste0("label(for_attr = ", quoteText(i_id_col), ", ", quoteText(i_label_name), "), "),
            paste0("input(required = TRUE, name = ", quoteText(i_id_col), ", ", paste0(paste(names(i_input_type), quoteText(i_input_type), sep = " = "), collapse = ", "), ", value = ", i_id_col, ")"),
            ', if(is.null(required_span_label) == FALSE){span(class = required_span_class, required_span_label)}else{""}',
            ")"
          )
        }

      }

    }

    x[i, paste0(meta_input_col) := i_meta_input]

  }

  meta_string <- paste0(
    function_name, " <- function(\n",
    "form_id = NULL,\n",
    "form_for = NULL,\n",
    "form_action = NULL,\n",
    "form_method = ", quoteText(form_method), ",\n",
    if(add_csrf_param == TRUE){'csrf_token,\n'}else{""},
    "form_class = ", quoteText(form_class), ",\n",
    "input_class = ", quoteText(input_class), ",\n",
    "submit_class = ", quoteText(submit_class), ",\n",
    "submit_label = ", quoteText(submit_label), ",\n",
    "required_span_class = ", quoteText(required_span_class), ",\n",
    "required_span_label = ", quoteText(required_span_label), ",\n",
    paste0(paste0(c(x[, base::get(id_col)], select_function_inputs), collapse = " = NULL,\n"), " = NULL\n"),
    ")",
    "{\n",
    "return(\n",
    'form(class = form_class, ',
    'name = form_id, id = form_id, action = form_action, method = form_method, ',
    'fieldset(\ninput(type = "hidden", name = "form_id", value = form_id), input(type = "hidden", name = "form_for", value = form_for),\n',
    if(add_csrf_param == TRUE){'input(type = "hidden", name = "csrf_token", value = csrf_token),\n'}else{""},
    paste0(unlist(x[, base::get(meta_input_col)]), collapse = ",\n"),
    ",",
    'div(class = input_class, ',
    'button(type = "submit", class = submit_class, id = paste0("submit_", form_id), submit_label))))',
    "\n)\n",
    "}\n"
  )

  writeLines(paste("library('html5')", meta_string, sep = "\n\n"), filepath)

  return(paste0("File written to ", filepath))

}

#' Based on the columns in a table, produces a R function that returns a data.table of HTTP parameters extracted from a list of HTPP params. You can create such a list easily using serverUtils::paramList.
#'
#' @param filepath A string, the file path including the name and file type extension where the output will be written.
#' @param function_name A string, the name of the function to be written to the file path.
#' @param x A data.table, should have a column of input ids (used for input names as well), and a column of HTML input types.
#' @param id_col A string, the column of x containing the HTML input ids.
#' @param type_col A string, the column of x containing the HTML input types.
#' @param exclude A character vector, ids included here will not be included as parameters for the resultant function.
#' @return A data.table, the HTML input type and attributes likely associated with the SQL data type.
#' @examples
#' createFetchParamsFunction(
#' filepath = paste0(tempdir(), "/", "testHTMLFormFunction.R"),
#' function_name = "fetch_example_function",
#' x = as.data.table(
#' list(
#' ids = c("user_id", "user_name", "first_name", "last_name", "state"),
#' types = c("number", "text", "text", "text", "text")
#' )
#' ),
#' id_col = "ids",
#' type_col = "types",
#' exclude = c("user_id")
#' )
createFetchParamsFunction <- function(
  filepath,
  function_name,
  x,
  id_col,
  type_col,
  exclude = c()
){

  x <- x[!(base::get(id_col) %in% exclude), ]

  ids <- x[, base::get(id_col)]

  db_types <- x[, base::get(type_col)]

  meta_string <- paste0(

    function_name, " <- function(params",
    ")\n{\n",
    "x <- list(", paste0(paste0(quoteText(ids), ' = if(length(params[[', quoteText(ids), ']]) > 0){params[[', quoteText(ids), ']]}else{""}'), collapse = ", \n"),")\n",
    "return(list(table = as.data.table(x), types = c(", paste0(paste(quoteText(ids), quoteText(db_types), sep = " = "), collapse = ", "), ")))\n}"
  )

  writeLines(paste("library('data.table')", meta_string, sep = "\n\n"), filepath)

  return(paste0("File written to ", filepath))

}
