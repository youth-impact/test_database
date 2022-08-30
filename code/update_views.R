source(file.path('code', 'setup.R'))

msg = tryCatch(
  update_views(auth, params),
  error = function(e) trimws(as.character(e)))

glue::glue('MESSAGE="{msg}"')
