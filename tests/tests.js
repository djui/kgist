var assert   = require('assert'),
    showdown = require('showdown').Showdown,
    pygments = require('../lib/pygments');
    
function run_tests() {
  pygmentize_test();
  showdown_test();
  // ...
  console.log("\033[1;32mPassed\033[0m.");
}

function pygmentize_test() {
  pygments.highlight('-module(test)', 'erlang', function(result) {
    assert.equal('<div class="highlight"><pre><span class="p">-</span>'+
                 '<span class="ni">module</span><span class="p">(</span>'+
                 '<span class="n">test</span><span class="p">)</span>'+
                 '\n</pre></div>\n',
                 result);
  });
}

function showdown_test() {
  assert.equal('<h1>test</h1>\n\n<p>test <code>code</code></p>',
               showdown.makeHtml('# test\n\ntest `code`'));
}

exports.run_tests = run_tests;
