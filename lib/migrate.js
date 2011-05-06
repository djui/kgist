var fs       = require('fs'),
    dirty    = require('dirty'),
    pygments = require('./pygments');

const dbPath = '../data/gist.db';
path.existsSync(dbPath) || fs.writeFileSync(dbPath, ''); // BUGFIX

function migrate() {
  var backupFile = backup(dbPath);
  var dbOld = dirty(backupFile);
  var dbNew = dirty(dbPath);
  migrationStep0(dbOld, dbNew);
  migrationStep1(dbNew, dbNew);
  // ...
}

function backup(dbFile) {
  var backupFile = dbFile+'.backup';
  copyFileSync(dbFile, backupFile);
  return backupFile;
}

exports.migrate = migrate;

////////////////////////////////////////////////////////////////////////////////
// Migration steps
////////////////////////////////////////////////////////////////////////////////

// Compaction
function migrationStep0(dbOld, dbNew) {
  dbOld.forEach(function (key, doc) {
      dbNew.set(key, doc);
  });
}

// Re-highlight code syntax to apply pygments options changes
function migrationStep1(dbOld, dbNew) {  
  dbOld.forEach(function (key, doc) {
    var languageAlias = pygments.getAlias(doc.language);
    pygments.highlight(doc.code, languageAlias, function (highlightedCode) {
      doc.hl_code = highlightedCode;
      dbNew.set(key, doc);
    });
  });
}

////////////////////////////////////////////////////////////////////////////////
// Helper
////////////////////////////////////////////////////////////////////////////////

function copyFileSync(src, dest) {
  fs.writeFileSync(dest, fs.readFileSync(src, 'utf8'));
}
