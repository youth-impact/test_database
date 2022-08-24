source(file.path('code', 'setup.R'))

file_id = params$file_id

########################################

facilitators = scto_read(auth, 'cases_facilitators')
groups = setDT(read_sheet(file_id, 'groups'))
columns_old = setDT(read_sheet(file_id, 'show_columns'))

########################################

setorderv(groups, 'group_id')

if (any(apply(groups, 2, uniqueN) != nrow(groups))) {
  stop('At least one column of the `groups` sheet contains duplicated values.')}

########################################

columns_new = data.table(
  column_name = colnames(facilitators),
  matrix(nrow = ncol(facilitators), ncol = nrow(groups),
         dimnames = list(c(), groups$group_id)))

########################################

sheet_old = 'show_columns_old'

if (identical(columns_old$column_name, columns_new$column_name) &&
    identical(colnames(columns_old), colnames(columns_new))) {
  needs_update = 'no'
  if (sheet_old %in% sheet_names(file_id)) {
    sheet_delete(file_id, sheet = sheet_old)}
} else {
  needs_update = 'yes'
  if (!(sheet_old %in% sheet_names(file_id))) {
    sheet_add(file_id, sheet_old)}
  write_sheet(columns_old, file_id, sheet = sheet_old)
  range_autofit(file_id, sheet = sheet_old)
  write_sheet(columns_new, file_id, sheet = 'show_columns')
  range_autofit(file_id, sheet = 'show_columns')}

########################################

status = data.table(
  `Last populated (UTC)` = Sys.time(),
  `show_columns out of date` = needs_update)

write_sheet(status, file_id, sheet = 'status')
range_autofit(file_id, sheet = 'status')
