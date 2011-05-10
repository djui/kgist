var fs    = require('fs'),
    spawn = require('child_process').spawn;

const PYGMENTIZE_CMD = 'pygmentize';
const styles = ['monokai', 'manni', 'perldoc', 'borland', 'colorful',
              'default', 'murphy', 'vs', 'trac', 'tango', 'fruity',
              'autumn', 'bw', 'emacs', 'vim', 'pastie', 'friendly',
              'native'];
var languages = []; // Dynamically populate on init from languages.json

function init() {
  fs.readFile('lib/languages.json', 'utf8', function (err, data) {
    if (err) throw err
    else languages = JSON.parse(data);
  });
}

function highlight(source, language, callback) {
  var output = '';
  var pygments = spawn(PYGMENTIZE_CMD, ['-l', language,
                                        '-f', 'html',
                                        '-P', 'encoding=utf-8',
                                        '-P', 'linenos=inline',
                                        '-P', 'linenospecial=2',
                                        '-P', 'lineanchors=linum',
                                        '-P', 'anchorlinenos=true'
                                       ]);
  pygments.stderr.on('data', function (err) {
    if (err) throw err;
  });

  pygments.stdout.on('data', function (data) {
    if (data) return output += data;
  });

  pygments.on('exit', function (code) {
    return callback(code, output);
  });

  pygments.stdin.write((function () {
    return source;
  }()));

  return pygments.stdin.end();
};

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

init();

exports.highlight = highlight;
exports.getLanguages = getLanguages;
exports.getAlias = getAlias;
exports.getMime = getMime;
exports.getExtension = getExtension;
