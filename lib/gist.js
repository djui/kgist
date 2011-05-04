var fs = require('fs'),
    path = require('path'),
    dirty = require('dirty');

////////////////////////////////////////////////////////////////////////////////
// Configuration
////////////////////////////////////////////////////////////////////////////////

const HashLength  = 4;
const HashCharset = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const dbPath = 'data/gist.db';
var db = {};

path.existsSync(dbPath) || fs.writeFileSync(dbPath, ''); // BUGFIX
db = dirty(dbPath);

var host_ip = '';
hostIP();

////////////////////////////////////////////////////////////////////////////////
// Database
////////////////////////////////////////////////////////////////////////////////

function get(key) {
  var doc = db.get(key);

  return doc;
}

function getAllIds() {
  var docs = [];
  
  db.forEach(function (key, val) {
    if (val.archived) return;
    docs.push(key);
  });
  
  return docs;
}

function getRecent() { // within a week from now
  var now = new Date();
  var since = now.setDate(now.getDate()-7);
  var recentDocs = [];
  
  db.forEach(function (key, val) {
    if (val.archived) return;
    if (new Date(val.ctime) >= since) {
      recentDocs.push(val);
    }
  });
  
  var sortedRecentDocs = recentDocs.sort(sortByCTime);
  return sortedRecentDocs.reverse();
}

function create(doc, callback) {
  if (!doc.code) callback(new Error('Invalid snippet!'));
  
  var key = uniqueKey();
  
  if (!doc.filename) doc.filename = key;
  if (!doc.language) doc.language = 'Text only';
  if (!validExpire(doc.expires)) doc.expires = '';
  doc.ctime = new Date().toUTCString();

  update(key, doc, callback);
}

function update(key, doc, callback) {
  doc.id = key;
  db.set(key, doc, function () {
    callback(undefined, key)
  });
}

function archive(key, callback) {
  var doc = get(key);
  doc.archived = true;
  update(key, doc, callback);
}

function exists(key) {
  var doc = get(key);
  
  if (!doc) return false;
  else return true;
}

function validId(key) {
  var pattern = '^['+HashCharset+']{'+HashLength+'}$';
  
  return (new RegExp(pattern)).test(key);
}

function isArchived(key) {
  var doc = get(key);
  if (!doc) return false;
  
  return !!doc.archived;
}

function hasExpired(key) {
  var doc = get(key);
  if (!doc) return false;
  
  var expireDate = calcExpireDate(doc.ctime, doc.expires);
  if (expireDate == 'never') return false;

  var now = new Date();
  if (now < expireDate) return false;
  else return true;
}

function calcExpireDate(strDate, duration) {
  if (!duration || duration == 'never') return 'never';

  var date = new Date(strDate);
  
  switch (duration) {
    case '1h': return new Date(date.setHours(date.getHours()+1));
    case '1d': return new Date(date.setDate(date.getDate()+1));
    case '1w': return new Date(date.setDate(date.getDate()+7));
    case '1m': return new Date(date.setMonth(date.getMonth()+1));
    case '1y': return new Date(date.setFullYear(date.getFullYear()+1));
    default: throw new Error('Invalid duration: ' + duration);
  }
}

function url(key) {
  return 'http://'+host_ip+'/'+key;
}

function clone(doc) {
  // Bugfix to simulate a "clone"
  var docClone = {};
  
  docClone.id = doc.id;
  docClone.ctime = doc.ctime;
  docClone.filename = doc.filename;
  docClone.description = doc.description;
  docClone.language = doc.language;
  docClone.code = doc.code;
  docClone.hl_code = doc.hl_code;
  docClone.author = doc.author;
  docClone.expires = doc.expires;
  docClone.irc = doc.irc;
  
  return docClone;
}

exports.get = get;
exports.getAllIds = getAllIds;
exports.getRecent = getRecent;
exports.create = create;
exports.update = update;
exports.archive = archive;
exports.exists = exists;
exports.validId = validId;
exports.isArchived = isArchived;
exports.hasExpired = hasExpired;
exports.calcExpireDate = calcExpireDate;
exports.url = url;
exports.clone = clone;
exports.migrate = migrate;

////////////////////////////////////////////////////////////////////////////////
// Migration steps
////////////////////////////////////////////////////////////////////////////////

function migrate() {
  migrationBackup('step1');
  migrationStep1();
}

function migrationBackup(name) {
  var src = db.path;
  var dest = src+'_migrationbackup-before-'+name;
  
  fs.writeFileSync(dest, fs.readFileSync(src, 'utf8'));
}

// Re-highlight code syntax to apply pygments options changes
function migrationStep1() {
  var pygments = require('./pygments');
  
  db.forEach(function (key, doc) {
    var languageAlias = pygments.getAlias(doc.language);
    pygments.highlight(doc.code, languageAlias, function (highlightedCode) {
      doc.hl_code = highlightedCode;
      db.rm(key, function () {
        db.set(key, doc);
      });
    });
  });
}

////////////////////////////////////////////////////////////////////////////////
// Helper
////////////////////////////////////////////////////////////////////////////////

function uniqueKey() {
  var counter = 10;
  var key = '';

  do {
    if (counter-- == 0) throw new Error('Can\'t generate unique key');
    key = generateHash(HashCharset, HashLength);
  } while (!!db.get(key));

  return key;
}

function generateHash(charset, length) {
  var hash = "";
  for (var i=1; i<=length; i++)
    hash += charset.charAt(Math.floor(Math.random() * charset.length));
  return hash;
}

function validExpire(duration) {
  return /^[0-9]+[hdwmy]$/.test(duration);
}

function sortByCTime(a, b) {
  var x = a.ctime;
  var y = b.ctime;
  return ((x < y) ? -1 : ((x > y) ? 1 : 0));
}

function hostIP() {
  var os = require('os');
  var dns = require('dns');
  
  dns.resolve4(os.hostname(), function (err, IPs) {
    if (err) throw err;
    host_ip = IPs[0];
  });
}
