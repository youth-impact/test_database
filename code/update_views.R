library('data.table')
# library('glue')
library('googledrive')
library('googlesheets4')
# library('rsurveycto')
library('yaml')

paramsDir = 'params'
# dataDir = 'data'
outputDir = 'output'

if (!dir.exists(outputDir)) dir.create(outputDir)

params = read_yaml(file.path(paramsDir, 'params.yaml'))

########################################

if (Sys.getenv('GOOGLE_TOKEN') == '') {
  drive_auth()
} else {
  drive_auth(path = Sys.getenv('GOOGLE_TOKEN'))}

gs4_auth(token = drive_token())

########################################

facilitators = setDT(read_sheet(params$master_sheet_id, 'facilitators'))
viewers = setDT(read_sheet(params$master_sheet_id, 'viewers'))
pgs = setDT(read_sheet(params$master_sheet_id, 'perm_groups'))

########################################

for (i in 1:nrow(pgs)) {
  sheet_id = as_id(pgs$sheet_id[i])
  group_id = pgs$perm_group_id[i]

  drive_rename(
    sheet_id, paste0('facilitator_db_prototype_', group_id), overwrite = TRUE)

  facilitators_now = facilitators[perm_group_id == group_id]
  write_sheet(facilitators_now[, !'perm_group_id'], sheet_id, sheet = 1)
  range_autofit(sheet_id, sheet = 1)

  viewers_now = viewers[perm_group_id == group_id]
  for (email in viewers_now$viewer_email) {
    drive_share(sheet_id, role = 'reader', type = 'user', emailAddress = email)}}
