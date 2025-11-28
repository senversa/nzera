# Plumber API for nzera Models

library(plumber)
library(nzera)

#* @apiTitle nzera API
#* @apiDescription API for New Zealand Environmental Risk Assessment models

#* Get list of available models
#* @get /models
function() {
  list_models() # returns tibble with id/title/version/status/owners/doc_path
}

#* Get model metadata
#* @param model_name Name of the model
#* @get /models/<model_name>/metadata
function(model_name) {
  metadata <- get_model_metadata(model_name)
  return(metadata)
}

#* Get model schema
#* @param model_name Name of the model
#* @get /models/<model_name>/schema
function(model_name) {
  schema <- get_model_schema(model_name)
  return(schema)
}

#* Run a model
#* @param model_name Name of the model
#* @param input_data:object Input data for the model
#* @post /models/<model_name>/run
#* Run a model
#* @post /models/<model_name>/run
function(model_name, input_data, params = list(), config = list()) {
  df <- tryCatch(
    as.data.frame(input_data, stringsAsFactors = FALSE),
    error = function(e) {
      plumber::abort(
        http_status = 400,
        message = "input_data must be a table/list of records"
      )
    }
  )

  schema <- get_model_schema(model_name)

  tryCatch(
    validate_input(df, schema, strict = TRUE),
    error = function(e) plumber::abort(http_status = 400, message = e$message)
  )

  tryCatch(
    run_model(
      model_id = model_name,
      data = df,
      params = params,
      config = config
    ),
    error = function(e) plumber::abort(http_status = 500, message = e$message)
  )
}


#* Health check
#* @get /health
function() {
  list(
    status = "healthy",
    timestamp = Sys.time()
  )
}
