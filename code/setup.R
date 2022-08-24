library('data.table')
library('glue')
library('googledrive')
library('googlesheets4')
library('rsurveycto')
library('yaml')

paramsDir = 'params'
outputDir = 'output'

if (!dir.exists(outputDir)) dir.create(outputDir)

params = read_yaml(file.path(paramsDir, 'params.yaml'))
params$file_id = as_id(params$file_id)

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

drive_share_get = function(file_id) {
  a1 = drive_reveal(file_id, 'permissions')
  a2 = a1$permissions_resource[[1L]]$permissions
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
