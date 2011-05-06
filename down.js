var http = require('http');

http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Server in maintenance mode ;-(\n');
}).listen(8001, '0.0.0.0', function () {
  console.log('Server running at http://0.0.0.0:8001/');
});
