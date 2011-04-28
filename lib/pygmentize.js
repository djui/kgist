var fs    = require('fs'),
    spawn = require('child_process').spawn;

function highlight(source, language, callback) {
  var output = '';
  var pygments = spawn('pygmentize', ['-l', language,
                                      '-f', 'html',
                                      '-O', 'encoding=utf-8']);

  pygments.stderr.addListener('data', function(error) {
    if (error) return console.error(error);
  });
  pygments.stdout.addListener('data', function(result) {
    if (result) return output += result;
  });
  pygments.addListener('exit', function() {
    return callback(output);
  });
  pygments.stdin.write((function() {
    return source;
  }()));

  return pygments.stdin.end();
};

exports.highlight = highlight;

// pygmentize.highlight('-module(test)', 'erlang', console.log);
