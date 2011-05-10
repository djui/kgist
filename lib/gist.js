var fs = require('fs'),
    path = require('path'),
    dirty = require('dirty'),
    Pygments = require('./pygments');

////////////////////////////////////////////////////////////////////////////////
// Configuration
////////////////////////////////////////////////////////////////////////////////

const HASH_LENGTH  = 4;
const HASH_CHARSET = "0123456789";
const HASH_PATTERN = '['+HASH_CHARSET+']{'+HASH_LENGTH+'}';

// FIXME This is double and should be handed over by the server
const DATA_PATH    = path.resolve('./data'); // absolute path
const DB_PATH      = DATA_PATH+'/gist.db';

var db = {};

path.existsSync(DB_PATH) || fs.writeFileSync(DB_PATH, ''); // BUGFIX
db = dirty(DB_PATH);

////////////////////////////////////////////////////////////////////////////////
// Database
////////////////////////////////////////////////////////////////////////////////

function get(key, callback) {
  if (!validId(key)) callback(new Error('Gist id invalid'));
  else if (!exists(key)) callback(new Error('Gist not found'));
  else if (isArchived(key)) callback(new Error('Gist is archived'));
  else if (hasExpired(key)) {
    archive(key, function (err) {
      if (err) callback(err);
      callback(new Error('Gist has expired'));
    });
  } else callback(undefined, db.get(key));
}

function getRecent() {
  var now = new Date();
  var since = now.setDate(now.getDate()-7); // within a week from now
  var recents = [];
  
  db.forEach(function (key, val) {
    if (val.archived) return;
    if (val.ctime >= since) {
      recents.push(val);
    }
  });
  
  var sortedRecents = recents.sort(sortByCTime);
  return sortedRecents.reverse();
}

function create(doc, callback) {
  if (!doc.code) callback(new Error('Invalid snippet!'));
  
  var key = uniqueKey();
  
  if (!doc.language) doc.language = 'Text only';
  if (!doc.filename) doc.filename = generateFilename(doc.language);
  if (!validExpire(doc.expires)) doc.expires = '';
  doc.ctime = new Date().getTime();

  update(key, doc, callback);
}

function update(key, doc, callback) {
  doc.id = key;
  db.set(key, doc, function () {
    callback(undefined, key)
  });
}

function archive(key, callback) {
  get(key, function (err, gist) {
    if (err) callback(err);
    else {
      gist.archived = true;
      update(key, gist, callback);
    }
  });
}

function exists(key) {
  var doc = db.get(key);
  
  if (!doc) return false;
  else return true;
}

function isArchived(key) {
  var doc = db.get(key);
  if (!doc) return false;
  
  return !!doc.archived;
}

function hasExpired(key) {
  var doc = db.get(key);
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

function generateUrl(host, key) {
  return 'http://'+host+'/'+key;
}

function filter(doc) {
  // Bugfix to simulate a "clone" and also to filter unintented form fields
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
exports.getRecent = getRecent;
exports.create = create;
exports.update = update;
exports.archive = archive;
exports.exists = exists;
exports.isArchived = isArchived;
exports.hasExpired = hasExpired;
exports.calcExpireDate = calcExpireDate;
exports.generateUrl = generateUrl;
exports.filter = filter;
exports.validRepo = validRepo;
exports.HASH_PATTERN = HASH_PATTERN;

////////////////////////////////////////////////////////////////////////////////
// Helper
////////////////////////////////////////////////////////////////////////////////

function uniqueKey() {
  var counter = 10;
  var key = '';

  do {
    if (counter-- == 0) throw new Error('Can\'t generate unique key');
    key = generateHash(HASH_CHARSET, HASH_LENGTH);
  } while (!!db.get(key));

  return key;
}

function validId(key) {
  return (new RegExp('^'+HASH_PATTERN+'$')).test(key);
}

function validRepo(repo) {
  return (new RegExp('^'+HASH_PATTERN+'\.git$')).test(repo);
}

function generateHash(charset, length) {
  var hash = '';
  
  for (var i=1; i<=length; i++) {
    var char = charset.charAt(Math.floor(Math.random() * charset.length));
    if (i == 1 && char == '0') i--;
    else hash += char;
  }
  
  return hash;
}

function generateFilename(language) {
  var extension = Pygments.getExtension(language);
  
  return 'gistfile'+extension;
}

function validExpire(duration) {
  return /^[0-9]+[hdwmy]$/.test(duration);
}

function sortByCTime(a, b) {
  var x = a.ctime;
  var y = b.ctime;
  return ((x < y) ? -1 : ((x > y) ? 1 : 0));
}
