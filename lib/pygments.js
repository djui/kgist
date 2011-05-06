var fs    = require('fs'),
    spawn = require('child_process').spawn;

var languages = []; // Dynamically populate on init from languages.json
var styles = ['monokai', 'manni', 'perldoc', 'borland', 'colorful',
              'default', 'murphy', 'vs', 'trac', 'tango', 'fruity',
              'autumn', 'bw', 'emacs', 'vim', 'pastie', 'friendly',
              'native'];

function init() {
  fs.readFile('lib/languages.json', 'utf8', function (err, data) {
    if (err)
      throw err
    else
      languages = JSON.parse(data);
  });
}

function getLanguages() {
  return languages;
}

function getAlias(language) {
  for (var i = languages.length; i--; )
    if (languages[i].name == language)
      return languages[i].alias[0];
  return 'text';
}

function getMime(language) {
  for (var i = languages.length; i--; )
    if (languages[i].name == language)
      return languages[i].mime[0];
  return 'text/plain';
}

function getExtension(language) {
  var defaultExt = '';
  for (var i = languages.length; i--; )
    if (languages[i].name == language)
      return (languages[i].ext[0] || defaultExt).substr(1);
  
  return defaultExt;
}

function highlight(source, language, callback) {
  var output = '';
  var pygments = spawn('pygmentize', ['-l', language,
                                      '-f', 'html',
                                      '-P', 'encoding=utf-8',
                                      '-P', 'linenos=inline',
                                      '-P', 'linenospecial=2',
                                      '-P', 'lineanchors=linum',
                                      '-P', 'anchorlinenos=true'
                                      ]);
  pygments.stderr.addListener('data', function (error) {
    console.log("@");
    if (error) return console.error(error);
  });

  pygments.stdout.addListener('data', function (result) {
    if (result) return output += result;
  });

  pygments.addListener('exit', function () {
    return callback(output);
  });

  pygments.stdin.write((function () {
    return source;
  }()));

  return pygments.stdin.end();
};

init();

exports.highlight = highlight;
exports.getLanguages = getLanguages;
exports.getAlias = getAlias;
exports.getMime = getMime;
exports.getExtension = getExtension;
