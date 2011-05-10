/*******************************************************************************
FIXME Not really nice to have this asynchronize without event emitters: First...

 1. get host ip, then...
 2. load db, then...
 3. start irc bot, then...
 4. start server listen

Or have every start process emit and done event and when all are emitted, start
server listen.
*/

var fs       = require('fs'),
    express  = require('express'),
    hbs      = require('hbs'),
 // showdown = require('showdown').Showdown,
    jerk     = require('jerk'),
    Gist     = require('./lib/gist'),
    pygments = require('./lib/pygments'),
    tests    = require('./tests/tests'),
    repl     = require('repl').start();

////////////////////////////////////////////////////////////////////////////////
// Server configuration
////////////////////////////////////////////////////////////////////////////////

const IRC_SERVER   = 'irc.hq.kred';
const IRC_CHANNELS = ['#tech'];
const HOST         = '0.0.0.0'; // INADDR_ANY
const PORT         = process.env.PORT || 8001;

var server = express.createServer();
server.configure(function () {
  var oneYear = 1*365*24*60*60*1000;
  server.set('view engine', 'hbs');
  server.set('view options', {cache: false, compile: false});
  server.register('.html', hbs);
  server.use(express.logger({format: ':method :status :url :response-time ms'}));
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

// For testing
server.get('/500', function (req, res) {
  throw new Error('This is a 500 Error');
});

server.get('/*', function (req, res) {
  throw new NotFound;
});

////////////////////////////////////////////////////////////////////////////////
// Server routes' validations
////////////////////////////////////////////////////////////////////////////////

server.param('gistId', function (req, res, next, id) {
  Gist.get(id, function (err, gist) {
    if (err) return next(err);
    req.gist = gist;
    next();
  });
});

////////////////////////////////////////////////////////////////////////////////
// Server error handling
////////////////////////////////////////////////////////////////////////////////

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

var ircBotOptions = { server:   IRC_SERVER
                    , channels: IRC_CHANNELS
                    , nick:     'gistbot'
                    }

function formatIrcMessage(author, gistId) {
  var from = author || 'Someone anonymously';
  var url = Gist.generateUrl(host, gistId);
  
  return from+' created a gist under '+url;
}

////////////////////////////////////////////////////////////////////////////////
// Starting / Stopping
////////////////////////////////////////////////////////////////////////////////

var host_ip = ''; hostIP();

server.listen(PORT, HOST, function () {
  console.log('Listening at http://'+HOST+':'+PORT);
});

var ircBot = jerk(function (j) {}).connect(ircBotOptions);
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
  res.render('gist_new.html', {'author': cookie_author, 'gists': recentGists});
}

function create(req, res) {
  var gist = req.body;
  
  // Sanitize
  var errors = [];
  
  // Has code?
  if (!gist.code) errors.push({'error': 'Missing code snippet'});
  // Filename valid?
  if (gist.filename && !validFilename(gist.filename)) messages.push({'error': 'Invalid filename'});

  if (errors.length) {
    res.render('gist_new.html', {'messages': errors});
    return;
  }
  
  // Highlight the code syntax
  var languageAlias = pygments.getAlias(gist.language);
  pygments.highlight(gist.code, languageAlias, function (highlightedCode) {
    gist.hl_code = highlightedCode;
    
    // Save the gist
    Gist.create(gist, function (err, gistId) {
      if (err) throw err;
      
      // Send irc message
      var ircChannel = assureIrcChannel(gist.irc);
      if (ircChannel) {
        ircBot.say(ircChannel, formatIrcMessage(gist.author, gistId));
      }
      
      res.cookie('author', gist.author, { maxAge: 900000 });
      res.redirect('/'+gistId);
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
  
  res.render('gist_view.html', {'gist': gist, 'gists': recentGists});
}

function show_raw(req, res) {
  var gist = req.gist;
  
  res.send(gist.code, {'Content-Type': 'text/plain'});
}

function download(req, res) {
  var gist = req.gist;
  var mime = pygments.getMime(gist.language);
  
  res.send(gist.code, {'Content-Type': mime});
}

function destroy(req, res) {
  var gist = req.gist;
  Gist.archive(gist.id, function (err) {
    res.redirect('home');
  });
}

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////

function assureIrcChannel(channel) {
  if (/^[0-9a-zA-Z_-]+$/.test(channel)) return '#'+channel;
  else if (/^#[0-9a-zA-Z_-]+$/.test(channel)) return channel;
  else return undefined;
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

function hostIP() {
  var os = require('os');
  var dns = require('dns');
  
  dns.resolve4(os.hostname(), function (err, IPs) {
    if (err) throw err;
    host_ip = IPs[0];
  });
}
