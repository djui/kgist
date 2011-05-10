var fs   = require('fs'),
    exec = require('child_process').exec;

const GIT_CMD = 'git';

function init(repoName, cwd, callback) {
  exec(GIT_CMD+' init --bare '+repoName+'.git', {'cwd': cwd},
       function (err, stdout, stderr) {
         callback(err, stdout);
       });
};

function updateServerInfo(cwd, callback) {
  exec(GIT_CMD+' update-server-info', {'cwd': cwd},
       function (err, stdout, stderr) {
         callback(err, stdout);
       });
};

function remove(repoDir, callback) {
  fs.rmdir(repoDir, function (err) {
    callback(err);
  });
}

exports.init = init;
exports.updateServerInfo = updateServerInfo;
exports.remove = remove;
