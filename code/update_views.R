library('data.table')
library('glue')
library('googledrive')
library('googlesheets4')
# library('rsurveycto')
library('yaml')

paramsDir = 'params'
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

facilitators = setDT(read_sheet(params$master_file_id, 'facilitators'))
viewers = setDT(read_sheet(params$master_file_id, 'viewers'))
pgs = setDT(read_sheet(params$master_file_id, 'perm_groups'))

########################################

for (i in 1:nrow(pgs)) {
  file_id = as_id(pgs$file_id[i])
  group_id = pgs$perm_group_id[i]

  # rename the file
  drive_rename(
    file_id, paste0('facilitator_db_prototype_', group_id), overwrite = TRUE)

  # update the contents
  facilitators_now = facilitators[perm_group_id == group_id]
  write_sheet(facilitators_now[, !'perm_group_id'], file_id, sheet = 1)
  range_autofit(file_id, sheet = 1)

  # remove all reader permissions
  a1 = drive_reveal(file_id, 'permissions')
  a2 = a1$permissions_resource[[1L]]$permissions
  a3 = data.table(
    email = sapply(a2, `[[`, 'emailAddress'),
    user_id = sapply(a2, `[[`, 'id'),
    role = sapply(a2, `[[`, 'role'))
  viewers_old = a3[role == 'reader']

  for (user_id in viewers_old$user_id) {
    # https://developers.google.com/drive/api/v3/reference/permissions/delete
    req = gargle::request_build(
      path = 'drive/v3/files/{fileId}/permissions/{permissionId}',
      method = 'DELETE',
      params = list(fileId = file_id, permissionId = user_id),
      token = drive_token())
    res = request_make(req)}

  # add reader permissions
  viewers_now = viewers[perm_group_id == group_id]
  for (email in viewers_now$viewer_email) {
    drive_share(file_id, role = 'reader', type = 'user', emailAddress = email)}}
