// https://developers.google.com/apps-script/guides/triggers#onedite
// https://developers.google.com/apps-script/reference/url-fetch/url-fetch-app
// https://stackoverflow.com/questions/66154780/send-a-post-request-using-google-apps-script
// https://stackoverflow.com/questions/58359417/you-do-not-have-permission-to-call-urlfetchapp-fetch

function atEdit(e) {
  var sheets = ['groups', 'show_columns', 'sorting', 'viewers', 'surveycto'];
  var sheet_now = e.range.getSheet().getName();

  if (sheets.includes(sheet_now)) {
    var s = e.source.getSheetByName('github')
    var repo_owner = s.getRange('A2').getValue()
    var repo_name = s.getRange('B2').getValue()
    var url = 'https://api.github.com/repos/' + repo_owner + '/' + repo_name + '/dispatches';

    var pat_file = DriveApp.getFileById('1xOU3teX79HsTTljvdjfn9vUH4__iVoyy');
    var pat = pat_file.getBlob().getDataAsString();
    var auth = 'Bearer ' + pat;

    var headers = {
      'Accept': 'application/vnd.github+json',
      'Authorization': auth
    };
    var data = {
      'event_type': 'webhook'
    };
    var options = {
      'method': 'post',
      'contentType': 'application/json',
      'headers': headers,
      'payload': JSON.stringify(data)
    };
    UrlFetchApp.fetch(url, options);
  }
}
