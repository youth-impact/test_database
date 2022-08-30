source(file.path('code', 'setup.R'))

msg = tryCatch(
  update_views(auth, params),
  error = function(e) trimws(as.character(e)))

glue::glue('message="{msg}"')
# TODO: if msg != 0, send email via github actions
