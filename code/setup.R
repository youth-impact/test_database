library('data.table')
library('googledrive')
library('googlesheets4')
library('rsurveycto')
library('yaml')

paramsDir = 'params'
params = read_yaml(file.path(paramsDir, 'params.yaml'))

########################################

if (Sys.getenv('GOOGLE_TOKEN') == '') {
  drive_auth()
} else {
  drive_auth(path = Sys.getenv('GOOGLE_TOKEN'))}

gs4_auth(token = drive_token())

########################################

if (Sys.getenv('SCTO_AUTH') == '') {
  auth_file = file.path(paramsDir, 'scto_auth.txt')
} else {
  auth_file = withr::local_tempfile()
  writeLines(Sys.getenv('SCTO_AUTH'), auth_file)}

auth = scto_auth(auth_file)

########################################

set_facilitators = function(
    auth, dataset_id, file_url, sheet = 'facilitators') {
  facilitators = scto_read(auth, dataset_id)
  write_sheet(facilitators, file_url, sheet)}


get_tables = function(
    file_url, sheets = c('facilitators', 'groups', 'show_columns', 'viewers')) {
  tables = lapply(sheets, function(x) setDT(read_sheet(file_url, x)))
  names(tables) = sheets
  setorderv(tables$groups, 'group_id')
  tables$show_columns[is.na(column_label), column_label := column_name]
  tables$viewers = unique(tables$viewers)
  return(tables)}


get_tables_validity = function(x) {
  cols = c('column_name', 'column_label')
  group_cols = setdiff(colnames(x$show_columns), cols)
  viewer_cols = c('viewer_name', 'viewer_email', 'group_id')

  r = if (!identical(colnames(x$groups), c('group_id', 'file_url'))) {
    'Column names of the `groups` sheet are not "group_id" and "file_url".'
  } else if (any(apply(x$groups, 2, uniqueN) != nrow(x$groups))) {
    'At least one column of the `groups` sheet contains duplicated values.'
  } else if (!identical(
    as_id(x$groups$file_url), drive_get(x$groups$file_url)$id)) {
    # might just throw an error
    paste('At least one row of the `file_url` column of the `groups`',
          'sheet does not correspond to a valid spreadsheet file.')
  } else if (!identical(colnames(x$show_columns)[1:2], cols)) {
    paste('The first two columns of the `show_columns` sheet',
          'are not named "column_name" and "column_label".')
  } else if (!setequal(colnames(x$facilitators), x$show_columns$column_name)) {
    paste('Column names of the `facilitators` sheet do not match',
          'the rows of `column_name` in the `show_columns` sheet.')
  } else if (!setequal(x$groups$group_id, group_cols)) {
    paste('Values of `group_id` of the `groups` sheet do not match the',
          'column names (from C column onward) of the `show_columns` sheet.')
  } else if (anyDuplicated(x$show_columns$column_label) != 0) {
    paste('The `column_label` column of the `show_columns`',
          'sheet contains duplicated values.')
  } else if (anyNA(x$show_columns[, ..group_cols])) {
    'The `show_columns` sheet contains missing values.'
  } else if (!all(as.matrix(x$show_columns[, ..group_cols]) %in% 0:1)) {
    'The `show_columns` sheet contains values other than 0 and 1.'
  } else if (!setequal(colnames(x$viewers), viewer_cols)) {
    paste('Columns names of the `viewers` sheet are not',
          '"viewer_name", "viewer_email", and "group_id".')
  } else if (!setequal(x$viewers$group_id, group_cols)) {
    paste('Values of `group_id` of the `viewers` sheet',
          'do not match those of the `groups` sheet.')
  } else {
    0}
  return(r)}


set_status = function(file_url, msg, sheet = 'status') {
  status = data.table(
    last_checked_utc = Sys.time(),
    message = msg)
  write_sheet(status, file_url, sheet = sheet)
  range_autofit(file_url, sheet = sheet)}

########################################

drive_share_get = function(file_id) {
  a1 = drive_get(file_id)
  a2 = a1$drive_resource[[1L]]$permissions
  a3 = data.table(
    email = sapply(a2, `[[`, 'emailAddress'),
    user_id = sapply(a2, `[[`, 'id'),
    role = sapply(a2, `[[`, 'role'))
  return(a3)}


drive_share_add = function(file_id, emails, role = 'reader') {
  for (email in unique(emails)) {
    drive_share(file_id, role = role, type = 'user', emailAddress = email)}
  invisible(drive_get(id = file_id))}


drive_share_remove = function(file_id, user_ids) {
  # https://developers.google.com/drive/api/v3/reference/permissions/delete
  for (user_id in unique(user_ids)) {
    req = gargle::request_build(
      path = 'drive/v3/files/{fileId}/permissions/{permissionId}',
      method = 'DELETE',
      params = list(fileId = file_id, permissionId = user_id),
      token = drive_token())
    res = request_make(req)}
  invisible(drive_get(id = file_id))}

########################################

set_views = function(x, file_prefix = 'facilitator_database_view_') {
  for (i in 1:nrow(x$groups)) {
    file_id = as_id(x$groups$file_url[i])
    group_id_now = x$groups$group_id[i]

    # rename file
    drive_rename(file_id, paste0(file_prefix, group_id_now), overwrite = TRUE)

    # update contents
    idx = x$show_columns[[group_id_now]] == 1
    cols_now = x$show_columns$column_name[idx]
    facs = x$facilitators[, ..cols_now]
    setnames(facs, cols_now, x$show_columns$column_label[idx])

    write_sheet(facs, file_id, sheet = 1)
    range_autofit(file_id, sheet = 1)

    # update permissions
    viewers_now = x$viewers[group_id == group_id_now]
    viewers_old = drive_share_get(file_id)[role == 'reader']
    viewers_add = viewers_now[!viewers_old, on = c('viewer_email' = 'email')]
    viewers_del = viewers_old[!viewers_now, on = c('email' = 'viewer_email')]

    drive_share_add(file_id, viewers_add$viewer_email)
    drive_share_remove(file_id, viewers_del$user_id)}

  invisible(0)}


update_views = function(auth, params) {
  # update the main file with the latest version of the dataset from surveycto
  set_facilitators(auth, params$dataset_id, params$main_file_url)

  # get the current and previous versions of all tables
  tables_old = get_tables(params$mirror_file_url)
  tables_new = get_tables(params$main_file_url)

  # check validity of tables
  msg = get_tables_validity(tables_new)
  if (msg != 0) {
    set_status(params$main_file_url, msg)
    return(msg)}

  # check whether anything has changed
  tables_eq = sapply(names(tables_new), function(i) {
    isTRUE(all.equal(tables_new[[i]], tables_old[[i]]))})

  if (all(tables_eq)) {
    set_status(params$main_file_url, 'No updates necessary.')
    return(0)}

  # update the views
  set_views(tables_new)
  msg = 'Successfully updated views.'
  set_status(params$main_file_url, msg)

  # update the mirror file
  lapply(names(tables_new), function(i) {
    write_sheet(tables_new[[i]], params$mirror_file_url, i)})

  return(msg)}


get_env_output = function(
    msg, file_url, sheet = 'maintainers', colname = 'email') {
  maintainers = read_sheet(file_url, sheet)
  emails = paste(maintainers[[colname]], collapse = ', ')
  r = glue::glue('MESSAGE={msg}\nEMAIL_TO={emails}')
  return(r)}
