var git = require('nodegit');

// Read a repository
git.repo('.git', function(err, repo) {
  // Success is always 0, failure is always an error string
  if (err) { throw err; }

  // Use the master branch
  repo.branch('master', function(err, branch) {
    if (err) { throw err; }
    
    // Iterate over the revision history
    var history = branch.history();
    
    history.on('commit', function(err, commit) {
      // Print out `git log` emulation
      console.log('commit ' + commit.sha);
      console.log(commit.author.name + '<' + commit.author.email + '>');
      console.log(commit.time);
      console.log('\n');
      console.log(commit.message);
      console.log('\n');
    });
  });
});
