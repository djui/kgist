/*
FIXME Not really nice to have this asynchronize without event emitters: First...

 1. get host ip, then...
 2. load db, then...
 3. start irc bot, then...
 4. start server listen

Or have every start process emit and done event and when all are emitted, start
server listen.
*/

var fs       = require('fs'),
    path     = require('path'),
    express  = require('express'),
    hbs      = require('hbs'),
 // showdown = require('showdown').Showdown,
    jerk     = require('jerk'),
    Gist     = require('./lib/gist'),
    Pygments = require('./lib/pygments'),
    Git      = require('./lib/git'),
    tests    = require('./tests/tests'),
    repl     = require('repl').start();

////////////////////////////////////////////////////////////////////////////////
// Configuration
////////////////////////////////////////////////////////////////////////////////

const HOST         = '0.0.0.0'; // INADDR_ANY
const PORT         = process.env.PORT || 8001;

const DATA_PATH    = path.resolve('./data'); // absolute path
const REPO_PATH    = DATA_PATH+'/git';
const DB_PATH      = DATA_PATH+'/gist.db';

const IRC_SERVER   = 'irc.hq.kred';
const IRC_CHANNELS = ['#tech'];

const DEFAULT_LANGUAGE = 'Erlang';

var server;
var hostIP;
var ircBot;
var ircBotOptions = { server:   IRC_SERVER
                    , channels: IRC_CHANNELS
                    , nick:     'gistbot'
                    };

////////////////////////////////////////////////////////////////////////////////
// Initialization
////////////////////////////////////////////////////////////////////////////////

path.existsSync(DATA_PATH) || fs.mkdirSync(DATA_PATH, 0755);
path.existsSync(REPO_PATH) || fs.mkdirSync(REPO_PATH, 0755);
path.existsSync(DB_PATH)   || fs.writeFileSync(DB_PATH, '');

server = express.createServer();
server.configure(function () {
  var oneYear = 1*365*24*60*60*1000;
  server.set('view engine', 'hbs');
  server.set('view options', {cache: false, compile: false});
  server.register('.html', hbs);
  server.use(express.logger({format: ':method :status :url :response-timems '
                                   + ':user-agent'}));
  server.use(express.errorHandler({showStack: true, dumpExceptions: true}));
  server.use(express.static(__dirname+'/public', {maxAge: oneYear}));
  server.use(express.bodyParser());
  server.use(express.methodOverride());
  server.use(express.cookieParser());
});

////////////////////////////////////////////////////////////////////////////////
// Server routes
////////////////////////////////////////////////////////////////////////////////

server.get('/',                 index);
server.get('/new',              new_page);
server.get('/:gistId',          show);
//server.get('/:gistId/edit',   edit);
server.get('/:gistId/raw',      show_raw);
server.get('/:gistId/download', download);
//server.put('/:gistId',        update);
server.post('/',                create);
server.del('/:gistId',          destroy);

// For git clone
server.get(new RegExp('('+Gist.HASH_PATTERN+'\.git)(\/.*)'), repo_clone);

// For testing
server.get('/500', function (req, res) {
  throw new Error('This is a 500 Error');
});

// For the rest
server.get('/*', function (req, res) {
  throw new NotFound;
});

////////////////////////////////////////////////////////////////////////////////
// Server routes' validations
////////////////////////////////////////////////////////////////////////////////

server.param('gistId', function (req, res, next, id) {
  Gist.get(id, function (err, gist) {
    if (err) throw new NotFound;
    
    req.gist = gist;
    next();
  });
});

////////////////////////////////////////////////////////////////////////////////
// Server error handling
////////////////////////////////////////////////////////////////////////////////

process.on('uncaughtException', function (err) {
  console.log('Caught exception: ' + err);
});

server.error(function (err, req, res, next) {
  if (err instanceof NotFound) {
    res.render('404.html', {status: 404, layout: false});
  } else {
    res.render('500.html', {status: 500, layout: false});
    console.log(err.stack);
  }
});

function NotFound(msg) {
  this.name = 'NotFound';
  Error.call(this, msg);
  Error.captureStackTrace(this, arguments.callee);
}

////////////////////////////////////////////////////////////////////////////////
// IRC Bot
////////////////////////////////////////////////////////////////////////////////

function formatIrcMessage(author, gistId) {
  var from = author || 'Someone anonymously';
  var url = Gist.generateUrl(hostIP, PORT, gistId);
  
  return from+' created a gist under '+url;
}

////////////////////////////////////////////////////////////////////////////////
// Starting / Stopping
////////////////////////////////////////////////////////////////////////////////

getHostIP(function (err, IP) {
  hostIP = err ? HOST : IP;
});

server.listen(PORT, HOST, function () {
  console.log('Listening at http://'+HOST+':'+PORT);
});

ircBot = jerk(function (j) {}).connect(ircBotOptions);
//var ircChannel = ircBot.join('#tech');

function stop() {
  console.log('Stopping...');
  
  ircBot.quit('');
  server.close();
  process.exit();
}

repl.context.migrate = Gist.migrate;
repl.context.run_tests = tests.run_tests;
repl.context.stop = stop;

////////////////////////////////////////////////////////////////////////////////
// Main fuctionality
////////////////////////////////////////////////////////////////////////////////

function index(req, res) {
  res.redirect('/new');
}

function new_page(req, res) {
  var cookie_author = req.cookies.author;
  var recentGists = Gist.getRecent();
  var emptyGist = { 'author': cookie_author
                  , 'language': DEFAULT_LANGUAGE
                  };
  res.render('gist_new.html', {'gists': recentGists, 'gist': emptyGist});
}

function create(req, res) {
  var gist = req.body;
  
  // Sanitize
  var errors = [];
  
  // Has code?
  if (!gist.code) errors.push({'error': 'Missing code snippet'});
  // Filename valid?
  if (gist.filename && !validFilename(gist.filename))
    errors.push({'error': 'Invalid filename'});

  if (errors.length) {
    res.render('gist_new.html', {'messages': errors});
    return;
  }
  
  // Highlight the code syntax
  var languageAlias = Pygments.getAlias(gist.language);
  Pygments.highlight(gist.code, languageAlias, function (err, hlCode) {
    if (err) gist.hl_code = code;
    else gist.hl_code = hlCode;
    
    // Save the gist
    Gist.create(gist, function (err2, gistId) {
      if (err2) throw err2;

      // Create git repo
      Git.init(gistId, REPO_PATH, function (err3) {
        if (err3) throw err3;
        
        // Add gist to repo and commit it
        // TODO Tricky, because '--bare' only accepts push's not commits
        
        // Update git repo server info
        Git.updateServerInfo(path.join(REPO_PATH, gistId+'.git'), function (err4) {
          if (err4) throw err4;
          
          // Send irc message
          assertValidIrcChannel(gist.irc, function (err5, channel) {
            if (err5) { /* TODO add to error messages on create page */ }
            // TODO Implement to say in public
            // ircBot.say(channel, formatIrcMessage(gist.author, gistId));
          });
        
          res.cookie('author', gist.author, {maxAge: 900000});
          res.redirect('/'+gistId);
        });
      });
    });
  });
}

function show(req, res) {
  var gist0 = req.gist;
  var recentGists = Gist.getRecent();
  var gist = Gist.filter(gist0);
  
  if (!gist0.description) gist.description = '-';
  if (!gist0.author) gist.author = 'anonymous';
  gist.expires = relativeDate(Gist.calcExpireDate(gist0.ctime, gist0.expires));
  gist.http_clone_link = Gist.generateUrl(hostIP, PORT, gist0.id)+'.git';
  
  res.render('gist_view.html', {'gist': gist, 'gists': recentGists});
}

function show_raw(req, res) {
  var gist = req.gist;
  
  res.send(gist.code, {'Content-Type': 'text/plain'});
}

function download(req, res) {
  var gist = req.gist;
  var mime = Pygments.getMime(gist.language);
  
  res.send(gist.code, {'Content-Type': mime});
}

function destroy(req, res) {
  var gist = req.gist;
  Gist.archive(gist.id, function (err) {
    if (err) throw err;
    
    Git.remove(path.join(REPO_PATH, gist.id+',git'), function (err2) {
      if (err) throw err;
      
      res.redirect('home');
    });
  });
}

// Every time a repo changes, you need to run this command on your git repo
// server side:
//
//     $ git update-server-info
//
// Then, on the client side, if you do a `git clone` git will try the following
// fallback order to retreive the references:
// 
//     $ git clone http://127.0.0.1:8001/
//     
//     GET /info/refs?service=git-upload-pack
//     GET /info/refs
//     GET /HEAD
function repo_clone(req, res) {
  var gistRepo = req.params[0];
  if (!Gist.validRepo(gistRepo))
    new Error('Invalid gist repository');

  var objectPath = req.params[1];
  var service = req.query.service;
  
  switch (service) {
  case 'git-receive-pack': // git push
    throw new NotFound;
  case 'git-upload-pack': // git clone
    // throw new NotFound;
  default: // git fallback
    serveGitObject(res, gistRepo, objectPath);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////

function serveGitObject(res, gistRepo, objectPath) {
  var file = path.join(REPO_PATH, gistRepo, objectPath);
  // FIXME Set connect's middleware module `static` to allow more listeners:
  // res.connection.setMaxListeners(100);
  // Git clone uses many parallel connections
  res.download(file);
}

function assertValidIrcChannel(channel, callback) {
  if (/^[0-9a-zA-Z_-]+$/.test(channel)) callback(null, '#'+channel);
  else if (/^#[0-9a-zA-Z_-]+$/.test(channel)) callback(null, channel);
  else callback(new Error('Invalid IRC channel'));
}

function validFilename(filename) {
  if (/^[^\/?*:;{}\\]{1,256}$/.test(filename)) return true;
  else return false;
}

function relativeDate(date) {
  if (date == 'never') return 'never';
  
  var diff = date - new Date();

  var years = Math.floor(diff / (1000*60*60*24*30*12));
  var months = Math.floor(diff / (1000*60*60*24*30));
  var days = Math.floor(diff / (1000*60*60*24));
  var hours = Math.floor(diff / (1000*60*60));
  var minutes = Math.floor(diff / (1000*60));
  
  if (diff == 0) return 'now';
  else if (diff > 0) {
    if (years) return 'in '+years+' years';
    else if (months) return 'in '+months+' months';
    else if (days) return 'in '+days+' days';
    else if (hours) return 'in '+hours+' hours';
    else if (minutes) return 'in '+minutes+' minutes';
    else return 'in less than a minute';
  } else {
    if (years) return Math.abs(years)+' years ago';
    else if (months) return Math.abs(months)+' months ago';
    else if (days) return Math.abs(days)+' days ago';
    else if (hours) return Math.abs(hours)+' hours ago';
    else if (minutes) return Math.abs(minutes)+' minutes ago';
    else return 'less than a minute ago';
  }
}

function getHostIP(callback) {
  var os = require('os');
  var dns = require('dns');
  
  dns.resolve4(os.hostname(), function (err, IPs) {
    if (err) callback(err);
    else callback(null, IPs[0]);
  });
}
