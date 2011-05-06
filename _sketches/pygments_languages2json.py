import pygments.lexers as pl
import locale
import json

wantedLanguages = [ "Bash"
                  , "Clojure"
                  , "CoffeeScript"
                  , "Common Lisp"
                  , "CSS"
                  , "Diff"
                  , "Erlang"
                  , "Gettext Catalog"
                  , "Gnuplot"
                  , "Go"
                  , "Haskell"
                  , "HTML"
                  , "INI"
                  , "IRC logs"
                  , "Java"
                  , "JavaScript"
                  , "Makefile"
                  , "MySQL"
                  , "Perl"
                  , "PHP"
                  , "Python"
                  , "Python 3"
                  , "reStructuredText"
                  , "Ruby"
                  , "S" # R
                  , "Scala"
                  , "SQL"
                  , "TeX"
                  , "Text only"
                  , "VimL"
                  , "XML"
                  , "XQuery"
                  , "XSLT"
                  ]

UnfilteredLexers = pl.get_all_lexers()
Lexers = []
for l in UnfilteredLexers:
    if l[1] == ('basemake',): continue # Bugfix: we want unique keys (Makefile)
    if not l[0] in wantedLanguages: continue
    if l[0] == 'S' and l[2][0] == '*.S': # Prioritize R over S
        l[0] = 'R'
        l[2][0] = '*.R'
        l[2][1] = '*.S'
    Lexers.append(l)
sortLambda = lambda x: locale.strxfrm(x[0].lower())
sortedLexers = sorted(Lexers, key=sortLambda)
newLexers = []
for l in sortedLexers:
    newLexers.append(dict({'name':  l[0], 
                           'alias': l[1],
                           'ext':   l[2],
                           'mime':  l[3]}))
JSON = json.dumps(newLexers) # indent=2
print JSON
