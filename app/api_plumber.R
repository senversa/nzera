# Plumber API for nzera Models

library(plumber)
library(nzera)

#* @apiTitle nzera API
#* @apiDescription API for New Zealand Environmental Risk Assessment models

#* Get list of available models
#* @get /models
function() {
  registry <- list_models()
  return(registry$id)
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
function(model_name, input_data) {
  # Validate input
  validate_model_input(input_data, model_name)

  # Get and run model
  model <- get_model(model_name)
  results <- model(input_data)

  # Validate output
  validate_model_output(results, model_name)
  return(results)
}

#* Health check
#* @get /health
function() {
  list(
    status = "heathy",
    timestamp = Sys.time()
  )
}
