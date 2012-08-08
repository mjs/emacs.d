/*
 Copyright (c) 2002 Douglas Crockford  (www.JSLint.com) Rhino Edition
 This is the Rhino companion to fulljslint.js modified for use within BATS.
 */

 // -- BEGIN CUSTOMIZATION -------------------------------------------
 // Modify path to fulljslint.js if necessary.
 load(environment["user.home"] + '/.emacs.d/jslib/fulljslint.js');
  // Customize jslint options
 var options = {
     bitwise: true,
     browser: true,
     eqeqeq: true,
     immed: true,
     indent: true,
     maxlen: 120,
     rhino: true
 };

 // -- END CUSTOMIZATION -------------------------------------------
 (function (a) {
     var e, i, input;
     if (!a[0]) {
         print("Usage: jslint.js file.js");
         quit(1);
     }
     input = readFile(a[0]);
     if (!input) {
         print("jslint: Couldn't open file '" + a[0] + "'.");
         quit(1);
     }
     print('Running jslint - ' + (new Date()) + '\n');
     if (!JSLINT(input, options)) {
         for (i = 0; i < JSLINT.errors.length; i += 1) {
             e = JSLINT.errors[i];
             if (e) {
                 print('Lint at line ' + e.line + ' character ' +
                         e.character + ': ' + e.reason);
                 print((e.evidence || '').
                         replace(/^\s*(\S*(\s+\S+)*)\s*$/, "$1"));
                 print('');
             }
         }
         quit(2);
     } else {
         print("jslint: No problems found in " + a[0]);
         quit();
     }
 }(arguments));
