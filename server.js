var fs       = require('fs'),
    express  = require('express'),
    hbs      = require('hbs'),
 // showdown = require('showdown').Showdown,
    jerk     = require('jerk'),
    snippet  = require('./lib/snippet'),
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

server.get('/',             index);
server.get('/new',          new_page);
server.post('/',            create);
server.get('/:id',          show);
//server.get('/:id/edit',     edit);
server.get('/:id/raw',      show_raw);
server.get('/:id/download', download);
//server.put('/:id',          update);
server.del('/:id',          destroy);

// For testing
server.get('/500', function (req, res) {
  throw new Error('This is a 500 Error');
});

server.get('/*', function (req, res) {
  throw new NotFound;
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
                    , nick:     'snippetbot'
                    }

function formatIrcMessage(author, snippetId) {
  var from = author || 'Someone anonymously';
  var url = snippet.url(snippetId);
  
  return from+' created a code snippet under '+url;
}

////////////////////////////////////////////////////////////////////////////////
// Starting / Stopping
////////////////////////////////////////////////////////////////////////////////

server.listen(PORT, HOST, function () {
  console.log('Listening on http://' + HOST + ':' + PORT);
});

repl.context.run_tests = tests.run_tests;
if (process.argv[2] == "test") tests.run_tests();

var ircBot = jerk(function (j) {}).connect(ircBotOptions);
//var ircChannel = ircBot.join('#tech');

function stop() {
  console.log('Stopping...');
  
  ircBot.quit('');
  server.close();
  process.exit();
}
repl.context.stop = stop;

////////////////////////////////////////////////////////////////////////////////
// Main fuctionality
////////////////////////////////////////////////////////////////////////////////

function index(req, res) {
  var docs = snippet.getRecent();
  res.render('index.html', {'snippets': docs});
}

function new_page(req, res) {
  var cookie_author = req.cookies.author;
  res.render('gist_new.html', {author: cookie_author});
}

function create(req, res) {
  var doc = req.body;
  // No code, no snippet!
  if (!doc.code) {
    res.redirect('home');
    return;
  }
  
  // Highlight the code syntax
  var languageAlias = pygments.getAlias(doc.language);
  pygments.highlight(doc.code, languageAlias, function (highlightedCode) {
    doc.hl_code = highlightedCode;
    
    // Save the snippet
    snippet.create(doc, function (err, snippetId) {
      if (err) throw err;
      
      // Send irc message
      var ircChannel = assureIrcChannel(doc.irc);
      if (ircChannel) {
        ircBot.say(ircChannel, formatIrcMessage(doc.author, snippetId));
      }
      
      res.cookie('author', doc.author, { maxAge: 900000 });
      res.redirect('/'+snippetId);
    });
  });
}

function show(req, res) {
  var doc = assertValidId(req, res);
  if (!doc) return;
  
  var snippetDoc = snippet.clone(doc);
  
  if (!doc.description) snippetDoc.description = '-';
  if (!doc.author) snippetDoc.author = 'anonymous';
  snippetDoc.expires = relativeDate(snippet.calcExpireDate(doc.ctime, doc.expires));
  
  res.render('gist_view.html', {'snippet': snippetDoc});
}

function show_raw(req, res) {
  var doc = assertValidId(req, res);
  if (!doc) return;
  
  res.send(doc.code, {'Content-Type': 'text/plain'});
}

function download(req, res) {
  var doc = assertValidId(req, res);
  if (!doc) return;
  
  var mime = pygments.getMime(doc.language);
  
  res.send(doc.code, {'Content-Type': mime});
}

function destroy(req, res) {
  var snippetId = req.params.id;
  snippet.archive(snippetId, function (err) {
    res.redirect('home');
  });
}

function assertValidId(req, res) {
  var snippetId = req.params.id;
  
  if (!snippet.validId(snippetId)) { // Invalid ID
    res.redirect('home');
    return undefined
  }
  else if (!snippet.exists(snippetId)) { // Not found
    res.redirect('home');
    return undefined
  }
  else if (snippet.isArchived(snippetId)) { // Archived
    res.redirect('home');
    return undefined
  }
  else if (snippet.hasExpired(snippetId)) { // Expired
    snippet.archive(snippetId, function (err) {
      if (err) throw err;
      res.redirect('home');
    });
    return undefined;
  } else return snippet.get(snippetId);
}

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////

function assureIrcChannel(channel) {
  if (/^[0-9a-zA-Z_-]+$/.test(channel)) return '#'+channel;
  else if (/^#[0-9a-zA-Z_-]+$/.test(channel)) return channel;
  else return undefined;
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
