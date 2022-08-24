source(file.path('code', 'setup.R'))

########################################

facilitators = scto_read(auth, 'cases_facilitators')
groups = setDT(read_sheet(params$file_id, 'groups'))
columns = setDT(read_sheet(params$file_id, 'show_columns'))
viewers = setDT(read_sheet(params$file_id, 'viewers'))

########################################

setorderv(groups, 'group_id')
viewers = unique(viewers)

if (any(apply(groups, 2, uniqueN) != nrow(groups))) {
  stop('At least one column of the `groups` sheet contains duplicated values.')}

if (!identical(colnames(facilitators), columns$column_name)) {
  stop(paste('Columns of `facilitators` are not identical to',
             'rows of `column_name` in the `show_columns` sheet.'))}

if (!identical(groups$group_id, colnames(columns)[2:ncol(columns)])) {
  stop(paste('Rows of `group_id` in the `groups` sheet inconsistent with',
             'column names (B column onward) in the `show_columns` sheet.'))}

if (any(is.na(columns[, !'column_name']))) {
  stop('The `show_columns` sheet contains missing values.')}

if (!all(as.matrix(columns[, !'column_name']) %in% 0:1)) {
  stop('The `show_columns` sheet contains values other than 0 and 1.')}

########################################

file_pre = 'facilitator_database_view_'

for (i in 1:nrow(groups)) {
  file_id = as_id(groups$file_id[i])
  group_id_now = groups$group_id[i]

  # rename file
  drive_rename(file_id, paste0(file_pre, group_id_now), overwrite = TRUE)

  # update contents
  cols_now = columns$column_name[columns[[group_id_now]] == 1]
  facs = facilitators[, ..cols_now]
  write_sheet(facs, file_id, sheet = 1)
  range_autofit(file_id, sheet = 1)

  # update permissions
  viewers_now = viewers[group_id == group_id_now]
  viewers_old = drive_share_get(file_id)[role == 'reader']
  viewers_add = viewers_now[!viewers_old, on = c('viewer_email' = 'email')]
  viewers_del = viewers_old[!viewers_now, on = c('email' = 'viewer_email')]

  drive_share_add(file_id, viewers_add$viewer_email)
  drive_share_remove(file_id, viewers_del$user_id)}
