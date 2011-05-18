var cluster = require('cluster');

cluster('./app')
  .in('development')
    .set('workers', 1)
    .use(cluster.logger('logs', 'debug'))
    .use(cluster.debug())
    .listen(8001)
  .in('production')
    .use(cluster.logger('logs'))
    .listen(80)
  .in('all')
    .use(cluster.stats())
    .use(cluster.pidfiles('pids'))
    .use(cluster.cli())
    .use(cluster.repl(8002));
