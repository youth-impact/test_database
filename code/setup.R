library('cli')
library('data.table')
library('glue')
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

set_dataset = function(auth, dataset_id, file_id) {
  d = scto_read(auth, dataset_id)
  # formdef_version could be integer64, which write_sheets can't handle
  cols = which(sapply(d, bit64::is.integer64))
  for (col in cols) {
    set(d, j = col, value = as.character(d[[col]]))}
  write_sheet(d, file_id, 'dataset')}


get_tables = function(
    file_id,
    sheets = c('dataset', 'groups', 'show_columns', 'sorting', 'viewers')) {
  tables = lapply(sheets, function(x) setDT(read_sheet(file_id, x)))
  names(tables) = sheets
  if (nrow(tables$groups) > 0) setorderv(tables$groups, 'group_id')
  if (nrow(tables$show_columns) > 0) {
    tables$show_columns[is.na(column_label), column_label := column_name]}
  tables$viewers = unique(tables$viewers)
  return(tables)}


compare_tables = function(x, y) {
  # dataset, groups, show_columns, sorting, viewers
  ignore_row_order = c(TRUE, TRUE, FALSE, FALSE, TRUE)
  ignore_col_order = c(TRUE, TRUE, FALSE, TRUE, TRUE)
  eq = sapply(1:length(x), function(i) {
    isTRUE(all.equal(
      x[[i]], y[[i]], check.attributes = FALSE,
      ignore.col.order = ignore_col_order[i],
      ignore.row.order = ignore_row_order[i]))})
  names(eq) = names(x)
  return(eq)}


get_sorting_validity = function(sorting, dataset, dataset_id) {
  sorting = copy(sorting)
  r = if (!setequal(colnames(sorting), c('column_name', 'column_value'))) {
    paste('Column names of the `sorting` sheet are not',
          '"column_name" and "column_value".')
  } else if (!all(sorting$column_name %in% colnames(dataset))) {
    glue('The `column_name` column of the `sorting` sheet contains ',
         'values that are not column names of the `{dataset_id}` dataset.')
  } else {
    ast = c('*ascending*', '*descending*')
    n = table(sorting[column_value %in% ast]$column_name)

    d1 = sorting[!(column_value %in% ast)]
    cols = unique(d1$column_name)
    d2 = unique(melt(
      dataset[, ..cols], measure.vars = cols, variable.factor = FALSE,
      variable.name = 'column_name', value.name = 'column_value'))

    if (any(n > 1)) {
      paste('In the `sorting` sheet, "*ascending*" or "*descending*"',
            'is not the only `column_value` for a given `column_name`.')
    } else if (nrow(d1) > 0 && nrow(fsetdiff(d1, d2)) > 0) {
      glue('The `sorting` sheet contains combinations of `column_name` and ',
           '`column_value` not present in the `{dataset_id}` dataset.')
    } else {
      0}}

  return(r)}


get_tables_validity = function(x, dataset_id) {
  # dataset_id = names(x)[1L]
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
  # } else if (!setequal(colnames(x$dataset), x$show_columns$column_name)) {
  #   glue('Values of `column_name` in the `show_columns` sheet do not match ',
  #        'the column names of the `{dataset_id}` dataset.')
  } else if (!setequal(x$groups$group_id, group_cols)) {
    paste('Values of `group_id` of the `groups` sheet do not match the',
          'column names (from C column onward) of the `show_columns` sheet.')
  } else if (anyDuplicated(x$show_columns$column_name) != 0) {
    paste('The `column_name` column of the `show_columns`',
          'sheet contains duplicated values.')
  } else if (!all(x$show_columns$column_name %in% colnames(x$dataset))) {
    glue('The `column_name` column of the `show_columns` sheet contains values',
         ' not present in the column names of the `{dataset_id}` dataset.')
  } else if (anyDuplicated(x$show_columns$column_label) != 0) {
    paste('The `column_label` column of the `show_columns`',
          'sheet contains duplicated values.')
  } else if (anyNA(x$show_columns[, ..group_cols])) {
    'The `show_columns` sheet contains missing values.'
  } else if (!all(as.matrix(x$show_columns[, ..group_cols]) %in% 0:1)) {
    'The `show_columns` sheet contains values other than 0 and 1.'
  } else if (!setequal(colnames(x$viewers), viewer_cols)) {
    paste('Column names of the `viewers` sheet are not',
          '"viewer_name", "viewer_email", and "group_id".')
    # } else if (!setequal(x$viewers$group_id, group_cols)) {
  } else if (!all(x$viewers$group_id %in% group_cols)) {
    paste('Values of `group_id` of the `viewers` sheet',
          'do not match those of the `groups` sheet.')
  } else {
    get_sorting_validity(x$sorting, x$dataset, dataset_id)}
  return(r)}


set_status = function(file_id, msg, sheet = 'status') {
  status = data.table(
    last_checked_utc = Sys.time(),
    message = msg)
  write_sheet(status, file_id, sheet = sheet)
  range_autofit(file_id, sheet = sheet)}

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
    res = googledrive::request_make(req)}
  invisible(drive_get(id = file_id))}


get_background = function(file_id, sheet, range, nonwhite = TRUE) {
  bg = setDT(range_read_cells(file_id, sheet, range, cell_data = 'full'))
  for (p in c('red', 'green', 'blue')) {
    y = lapply(bg$cell, function(z) z$effectiveFormat$backgroundColor[[p]])
    y = sapply(y, function(z) if (is.null(z)) 0 else z)
    set(bg, j = p, value = y)}
  bg[, cell := NULL][]
  if (nonwhite) bg = bg[!(red == 1 & green == 1 & blue == 1)]
  return(bg)}

########################################

set_background = function(file_id, background) {
  # only for setting one color per entire column
  bod_base = '{
  "repeatCell": {
    "range": {
      "startColumnIndex": (start_col),
      "endColumnIndex": (start_col + 1)
    },
    "cell": {
      "userEnteredFormat": {
        "backgroundColor": {
          "red": (red),
          "green": (green),
          "blue": (blue)
        }
      }
    },
    "fields": "userEnteredFormat.backgroundColor"
    }
  }'

  background = copy(background)
  background[, bod := glue(bod_base, .envir = .SD, .open = '(', .close = ')')]
  bod = sprintf('{"requests": [%s]}', paste(background$bod, collapse = ',\n'))

  req = googlesheets4::request_generate(
    'sheets.spreadsheets.batchUpdate', list(spreadsheetId = file_id))
  req$body = bod
  res = googlesheets4::request_make(req)
  invisible(res)}

########################################

sort_dataset = function(d, sorting) {
  d = copy(d)

  ast = c('*ascending*', '*descending*')
  cols_tmp = unique(sorting[!(column_value %in% ast)]$column_name)
  for (col in cols_tmp) {
    levs_tmp = sorting[column_name == col]$column_value
    levs = c(levs_tmp, setdiff(unique(d[[col]]), levs_tmp))
    d[, y := factor(y, levs), env = list(y = col)]}

  v = sorting[
    , .(ord = 1 - 2 * any(column_value == '*descending*')),
    by = column_name]
  setorderv(d, v$column_name, v$ord)
  return(d)}


get_view_prefix = function(file_id) {
  r = drive_get(file_id)$name
  r = gsub('main$', 'view', r)
  return(r)}


set_views = function(x, bg, prefix) {
  dataset = sort_dataset(x$dataset, x$sorting)
  bg = copy(bg)[, column_name := x$show_columns$column_name[row - 1L]]

  for (i in 1:nrow(x$groups)) {
    file_id = as_id(x$groups$file_url[i])
    group_id_now = x$groups$group_id[i]

    # rename file
    drive_rename(file_id, glue('{prefix}_{group_id_now}'), overwrite = TRUE)

    # update contents
    idx = x$show_columns[[group_id_now]] == 1
    cols_now = x$show_columns$column_name[idx]
    dataset_now = dataset[, ..cols_now]
    setnames(dataset_now, cols_now, x$show_columns$column_label[idx])

    write_sheet(dataset_now, file_id, sheet = 1)
    range_autofit(file_id, sheet = 1)

    # update formatting
    bg_now = bg[column_name %in% cols_now]
    bg_now[, start_col := match(column_name, cols_now) - 1L]
    set_background(file_id, bg_now)

    # update permissions
    viewers_now = x$viewers[group_id == group_id_now]
    viewers_old = drive_share_get(file_id)[role == 'reader']
    viewers_add = viewers_now[!viewers_old, on = c('viewer_email' = 'email')]
    viewers_del = viewers_old[!viewers_now, on = c('email' = 'viewer_email')]

    drive_share_add(file_id, viewers_add$viewer_email)
    drive_share_remove(file_id, viewers_del$user_id)}

  invisible(0)}

########################################

update_views = function(auth, params) {
  main_id = as_id(params$main_file_url)
  mirror_id = as_id(params$mirror_file_url)
  cli_alert_success('Created file ids from file urls.')

  # update the main file with the latest version of the dataset from surveycto
  set_dataset(auth, params$dataset_id, main_id)
  cli_alert_success('Updated dataset sheet in main file.')

  # get the current and previous versions of relevant tables
  tables_old = get_tables(mirror_id)
  cli_alert_success('Fetched tables from mirror file.')
  tables_new = get_tables(main_id)
  cli_alert_success('Fetched tables from main file.')

  # check validity of tables
  msg = get_tables_validity(tables_new, params$dataset_id)
  cli_alert_success('Checked validity of tables.')
  if (msg != 0) {
    set_status(main_id, msg)
    cli_alert_success('Set status sheet of main file.')
    return(msg)}

  # check whether anything has changed (ignores fill color)
  tables_eq = compare_tables(tables_new, tables_old)
  cli_alert_success('Compared main and mirror tables.')
  if (all(tables_eq)) {
    set_status(main_id, 'No updates necessary.')
    cli_alert_success('Set status sheet of main file.')
    return(0)}

  # update the views
  bg = get_background(main_id, 'show_columns', 'A2:A')
  cli_alert_success('Got background colors.')
  view_prefix = get_view_prefix(main_id)
  cli_alert_success('Got prefix for view files.')
  set_views(tables_new, bg, view_prefix)
  cli_alert_success('Wrote tables to view files.')

  msg_end = paste(names(tables_eq)[!tables_eq], collapse = ', ')
  msg_end = gsub()
  msg = glue('Successfully updated views based on changes to {msg_end}.')
  set_status(main_id, msg)
  cli_alert_success('Set status sheet of main file.')

  # update the mirror file
  r = lapply(names(tables_new)[!tables_eq], function(i) {
    write_sheet(tables_new[[i]], mirror_id, i)})
  cli_alert_success('Wrote tables to mirror file.')

  return(msg)}


get_env_output = function(
    msg, file_url, sheet = 'maintainers', colname = 'email') {
  maintainers = read_sheet(file_url, sheet)
  emails = paste(maintainers[[colname]], collapse = ', ')
  r = glue('MESSAGE={msg}\nFILE_URL={file_url}\nEMAIL_TO={emails}')
  return(r)}
