var fs = require('fs'),
    path = require('path'),
    express = require('express'),
    hbs = require('hbs'),
    dirty = require('dirty'),
    pygmentize = require('./lib/pygmentize'),
    showdown = require('./lib/showdown').Showdown,
    port = process.env.PORT || 8001;

////////////////////////////////////////////////////////////////////////////////
// Server configuration
////////////////////////////////////////////////////////////////////////////////

var server = express.createServer();
server.configure(function() {
  var oneYear = 1*365*24*60*60*1000;
  server.set('view engine', 'hbs');
  server.use(express.logger({format: ':method :status :url :response-time ms'}));
  server.use(express.errorHandler({showStack: true, dumpExceptions: true}));
  server.use(express.static(__dirname+'/public', {maxAge: oneYear}));
  server.register('.html', hbs);
});

server.listen(port);
console.log('Listening on http://0.0.0.0:' + port);
var repl = require('repl').start();

////////////////////////////////////////////////////////////////////////////////
// Routes
////////////////////////////////////////////////////////////////////////////////

pygmentize.highlight('-module(test)', 'erlang', console.log);
console.log(showdown.makeHtml('# test\n\ntest `code`'));

server.get('/', function(req, res) {
  res.render('index.html', {'calendar': week});
});

server.get('/list', function(req, res) {
  // var week = req.params.week_id;
  res.render('index.html');
});

server.get('/recent', function(req, res) {
  res.render('index.html');
});

// ...

// For testing
server.get('/500', function(req, res) {
  throw new Error('This is a 500 Error');
});

server.get('/*', function(req, res) {
  throw new NotFound;
});

////////////////////////////////////////////////////////////////////////////////
// Error handling
////////////////////////////////////////////////////////////////////////////////

server.error(function(err, req, res, next) {
  if (err instanceof NotFound) {
    res.render('404.html', {status: 404, layout: false});
  } else {
    res.render('500.html', {status: 500, layout: false});
  }
});

function NotFound(msg) {
  this.name = 'NotFound';
  Error.call(this, msg);
  Error.captureStackTrace(this, arguments.callee);
}

////////////////////////////////////////////////////////////////////////////////
// Database
////////////////////////////////////////////////////////////////////////////////

var dbPath = 'data/kist.db';
path.existsSync(dbPath) || fs.writeFileSync(dbPath, ''); // BUGFIX
var db = dirty(dbPath);

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

var assert = require('assert');
repl.context.run_tests = function() {
  // ...
  console.log("\033[1;32mPassed\033[0m.");
}
