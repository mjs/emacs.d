;;; vimpulse.el --- emulates Vim's most useful features, including Visual mode

;; ### For setup instructions, see "Installation" below. ###

;; Copyright (C) 2007 Brad Beveridge.
;; Copyright (C) 2007, 2009 Alessandro Piras.
;; Copyright (C) 2008 Frank Fischer.
;; Copyright (C) 2009 Jason Spiro <http://www.jspiro.com>.
;; Copyright (C) 2010 Vegard Øye.
;;
;; Version: 0.3.0+svn
;; Keywords: emulations
;; Human-Keywords: vim, visual-mode, rsi, ergonomics, Emacs pinky finger
;; URL: http://emacswiki.org/emacs/vimpulse.el
;; Contact: <implementations-list at lists.ourproject.org>.
;;          You don't have to subscribe.  We usually reply within a
;;          few days and CC our replies back to you.  If we don't,
;;          check the archives:
;;          http://tinyurl.com/implementations-list
;; SVN and bug tracker: http://www.assembla.com/spaces/vimpulse/
;; Send patches to: http://my-trac.assembla.com/vimpulse/report/1
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Works well with GNU Emacs 21.4 and 22.0.
;;                Causes problems with undo, but has no other problems,
;;                on XEmacs 21.4.19.
;;                Please send us compatibility info about other versions.
;; Maintainer: Alessandro Piras but I am looking for a co-maintainer
;;             or for a new maintainer to take over.  Contact the list.
;; Current developers: Alessandro Piras (maintainer),
;;                     Vegard Øye <vegard_oye at hotmail dot com>
;; Related: viper.el, viper-in-more-modes.el
;;
;; Thanks to our
;;        Old Maintainer: Jason Spiro
;; we'll miss you as a maintainer :)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse emulates Vim's most useful features, including Visual
;; mode and (still partially) text objects.
;; Vimpulse is a set of modifications to viper, the minor mode
;; that emulates Vi.  Vimpulse is not a minor mode; as soon as it is
;; loaded, viper will start working in a more Vim-like way.
;;
;; Vimpulse is beta software.  It seems to work quite well already
;; though.  Patches and feature requests welcome.
;;
;; NEWS 19/06/2009: The visual mode is greatly improved, we also
;;                  feature an improved paren matching. It can
;;                  become slow (i didn't check, but it's likely)
;;                  if you work with very big files, so deactivate it
;;                  if you notice sluggishness (see point 5 of the
;;                  installation instructions).

;;; Installation:

;; 0. If you checked out from SVN, do `make all' to produce
;;    vimpulse-big.el.  If on Windows, you can run compile.bat.
;;
;; 1. Copy vimpulse.el (or vimpulse-big.el) to somewhere in your
;;    load-path, e.g., your site-lisp directory.
;;
;; 2. Comment out and add the following line to your .emacs file:
;;
;;        (require 'vimpulse)
;;
;;    If .emacs does not exist, create it.  If you use Windows,
;;    see http://www.gnu.org/software/emacs/windows/faq3.html.
;;
;; That's all the required steps.  The rest is optional:
;;
;; 3. If using GNU Emacs, download and install
;;    http://wonderworks.com/download/redo.el to get C-r
;;    (redo key) to work.  redo.el is included with XEmacs.
;;
;; 4. For block selection (C-v), download and install
;;    http://www.emacswiki.org/emacs/rect-mark.el.
;;
;; We would appreciate it if you e-mailed us saying whether you liked
;; or disliked Vimpulse and why, and which new features you would like
;; to see.  Also, if you stop using Emacs or Vimpulse, please tell us
;; why, so we can try to improve it!

;;; Usage:

;; To use Visual mode, press v in vi (command) mode. Then use the
;; motion commands to select the region.  Press d to delete, c to
;; change, r to replace, or y to copy.  You can use p to paste. For
;; line selection, press V instead of v.  Then you can copy and paste
;; whole lines.  For block selection, press C-v.  Now you can copy and
;; paste the selected rectangle.  In block selection, you may use I or
;; A to insert or append text before or after the selection on each
;; line.
;;
;; Other features:
;;
;; The extended documentation is still in its early stages, but you
;; can view drafts at
;;
;; http://my-trac.assembla.com/vimpulse/wiki/Documentation
;;
;; Also see the definition of `viper-vi-global-user-map' right near
;; the beginning of the code.  You'll see a list of keys and what
;; function each one calls.  The documentation that comes with Vim
;; -- which is also online at http://vimdoc.sf.net/ -- may also be
;; helpful.
;;
;; Tips:
;;
;; - Vimpulse makes C-r run `redo' in command mode, but you can
;;   still get reverse i-search by pressing C-s and then C-r.

;;; Bugs:

;; (We would appreciate it very much if you report bugs.)
;;
;; Known bugs:
;;
;; - cw with a count doesn't work the same as Vim when the point
;;   is just before a space between words
;;     - Fix plan: try cw with a count, then try dwi with a count; or,
;;       ask on a relevant forum how the commands differ; or, check
;;       how it works in Vi / Vim then check the Vim manual for more
;;       info; then, decide how to best fix.
;;         - Vim's behavior seems unconsistent in this case.
;;           I think making the fix optional would be best, as I (at least)
;;           like more vimpulse behavior in this case (defaulting to
;;           standard vim behavior though)
;;
;; - Undo has problems in XEmacs.

;;; Development and documentation TODOs:

;; - Make sure I have added all stuff in Brad's viper additions and
;;   from my collection, then start documenting already.  Once there
;;   are even the simplest of docs (a nice key map) people will have a
;;   far easier time using vimpulse and so I bet more will contribute.
;;
;; - The / key should allow isearch that works like Vim's, or until
;;   that's implemented, it should at least remap / to isearch-forward
;;   or viper-isearch-forward.  This should be an option that should
;;   be disabled by default.  For now, have viper load .vimrc and
;;   check for vim-specific option strings like "set incsearch".  If
;;   anyone complains, rethink that plan.
;;
;; - Folding.  This should be implemented as a separate lisp library
;;   usable for even non-viper users.  Which foldmethods to do first?
;;   I personally only use foldmethod=marker, and even that only rarely.
;;
;; - i_C-(I forgot the letter) should do (copy-from-above-command 1)
;;   from misc.el.
;;
;; - Add :set spell / :set nospell that uses flyspell-mode.
;;
;; - Add support for tabs.el, a tabs mode that works sensibly (get it
;;   from Emacs Lisp List).
;;     - Minimum needed: :tabedit, :tabnext, :tabprevious.
;;     - Since I'm emulating Vim, emulate its tab pages feature.  So a
;;       tab page should be able to hold one or more buffers.
;;
;; - Add Customize option to let users stop C-r from being redo?
;;
;; - E-mail and try to get redo.el included with GNU Emacs (since I
;;   won't include redo.el here since nobody else includes it in their
;;   Lisp files either).
;;
;; - Copy more features from Brad's work in darcs and from vimpact
;;   into vimpulse.
;;
;; - Doc: look in google chat log, find description of one-char-off
;;   bug, see if it applies to this or to the not-yet-released
;;   viper-x, and if to this, mention under Bugs.
;;
;; - Doc: fix ref to "your home directory": Windows users don't have
;;   one.
;;
;; - Doc: list all new keys (and maybe all differences from viper) in
;;   Usage section.
;;
;; - Doc: describe all new keys in Usage section; can look at Vim
;;   manual for ideas.
;;
;; - Modify how tramp works so it also automatically handles URLs
;;   typed in the netrw syntax, i.e., http:// etc.  But first ask tramp
;;   upstream if they could please make those changes themselves.
;;
;; - Add CTRL-O for jumping back in the jumplist and CTRL-I for
;;   jumping forwards (for undoing one CTRL-O).  I wonder if emacs'
;;   tags functionality allows a jumplist.  I wonder if viper does
;;   tags like nvi does.
;;     - The global mark ring is not what I want.
;;     - Try code.google.com/p/ejumplist/source/browse/trunk/jumplist.el
;;
;; - On my PC (I run Ubuntu), if you start plain Vim then press CTRL-O
;;   many times, it starts opening recently opened files.  Is that
;;   useful?  Should vimpulse have persistent jump table functionality
;;   like that, and if so, should it use recentf or vim's .viminfo
;;   file or some tag functionality in emacs?  How will it interact
;;   with the fact that in emacs it's not traditional to suddenly
;;   close files without warning?
;;
;; - Make sentence movement work like in Vim.  I wonder if this can be
;;   done by setting viper options.
;;     - In Vim, according to :help sentence, end of sentence is:
;;         - '.', '?', or '!'
;;         - then (optionally) one or more '"', ''', ')', and ']'
;;           characters
;;         - then a newline, space, or tab.
;;         - A paragraph or section boundary is also a sentence
;;           boundary, but I bet viper handles that, and if it doesn't,
;;           it should.
;;             - A paragraph begins after each truly empty line (no
;;               whitespace chars on it) or after certain col-1 nroff
;;               macros.  A sentence begins after a form feed (^L), or
;;               certain nroff macros, in column 1.
;;             - The characters '{' and '}' sometimes affect paragraph
;;               definitions.  See :help paragraph.
;;     - In Viper, on the other hand, I bet sentences are like in vi,
;;       where Tabs aren't whitespace, and you need at least two spaces
;;       after the punctuation mark.
;;
;; - Make "dgg" work.
;;
;; - Make :q and :q! work as in Vim.
;;
;; - Implement smartcase searching.
;;
;; - Try to get vimpulse included with upstream viper; also, ideally,
;;   if you pressed "v" in viper, viper would offer to load vimpulse.
;;   (likely to happen?  Consider that Michael Kifer, the viper
;;   maintainer, told me he doesn't need vim keys.  Then again, maybe
;;   I could convince him that it's worth it to ship vim keys, for
;;   other people's benefit).
;;
;; - E-mail ridip <rdp@inthefaith.net> and ask him for his vimpulse
;;   contribs and his DVORAK stuff.
;;
;; - E-mail to Tromey for upload into ELPA?  we'd have to redo this
;;   when a new major version comes out.  Or maybe we should just
;;   contribute some auto-ELPA-management code.  By the way, should we
;;   move vimpulse into CVS somewhere?
;;
;; - Maybe merge all feature requests that anyone has ever sent into a
;;   "Feature requests" section here.

;;; Development plans:

;; The design plan for Vimpulse is for it to only emulate features
;; that are in Vim.  Unfortunately, other new features do not belong
;; in Vimpulse unless you can get the Vim people to implement those
;; features too.

;;; Undecided development questions:

;; - In vimpulse, like in real vim, C-r only does redo in command
;;   mode; in insert mode it does something else.  (In vimpulse that
;;   "something else" is reverse i-search.)  Should it do reverse
;;   i-search in insert mode too?
;;
;; - When you press "v" for visual mode, Vimpulse modifies the mode
;;   section of the modeline, so it reads e.g. "(Emacs-Lisp visual)".
;;   Shouldn't it do something to the <V> indicator instead?
;;
;; - In Vim, when a line starts with a "// " or ";; " comment and I
;;   press enter, Vim extends the comment onto the next line.  What
;;   Vim function is it that does this?  Is the function enabled in
;;   plain vanilla Vim 7 as shipped by vim.org?  (Check by seeing how
;;   it works on Vim for Windows running on either Windows or Wine.)
;;   Is it mostly useful or mostly annoying?  Is it worth implementing
;;   in Emacs considering there are other easy ways to create
;;   comments?
;;
;; - In v / V mode, Vim makes sure there is always at least 1 char /
;;   line selected.  IMO it provides nice feedback as to whether
;;   visual mode is on or not.  Is this worth implementing?  This is
;;   especially important for the block mode because currently it's
;;   impossible to select the last character in a line.
;;       19/06/2009: we only need to fix block mode.
;;
;; - Sometimes when you use C (viper-change-to-eol) or other change
;;   commands, Jason's new viper-exec-change function shows a message
;;   like "Deleted 50 characters" as a side effect.  Is that annoying?
;;     - Update 1 month later:  I hardly notice the message.
;;     - Dear users:  Do you think I should disable the message?
;;
;; - I want to allow buffer-switching without using the C-x key, since
;;   C-x b RET an extremely large amount of times per day is
;;   uncomfortable for my right pinky which presses RET.  There's
;;   already :b which seems to just invoke switch-to-buffer.  Is this
;;   right?  Is it bad if I make vimpulse emulate set autowrite=on
;;   then write new multi-buffer code?  What should the code's user
;;   interface be like?  I really should switch back to Vim for a day,
;;   learn more about how it deals with multiple buffers at once (and
;;   maybe also with tab pages) and emulate whatever of Vim's is most
;;   convenient.  What do you think of all the above?\
;;     - update: IIRC :set hidden lets you switch buffers w/o saving
;;     - update from Sebastien Rocca Serra: :set wildmenu plus
;;       tab-completion makes :b very pleasant to use when you have
;;       50+ buffers open.  Wildmenu is almost like iswitchb or ido.
;;     - I wonder how well that stuff works with just a few buffers open.
;;
;; - Simulate Vim's set virtualedit=onemore option to make C-x C-e
;;   possible w/o first advancing to next line?
;;
;; - Would it be bad to edit users' .viminfo files without asking
;;   permission, or should some variable have to be customized on to do
;;   such a thing?
;;
;; - Should gj and gk do longlines-style movement like in Vim?  I
;;   really must resolve my Windows vs. Unix line-length hangups by
;;   Googling or asking before I even think about this.
;;
;; - Is there any need to implement Vim's new
;;   [count]dk-can-go-past-top-of-file-without-error functionality (to
;;   me, no need) or any related functionality?
;;
;; - What to do about xemacs?  It doesn't ship with woman.  I wonder
;;   if woman is in some xemacs package?

;;; Change Log:
;;
;; Version 0.3.0+svn
;;  [laynor@gmail.com:]
;;  - Added some small fixes, and promoted the experimental stuff to
;;    stable, as it seems to work well and not loading it caused problems.
;;
;; Version 0.3.0
;;  [laynor@gmail.com:]
;;  - [NEW] register support on text object commands
;;  - [NEW] issuing : when visual selecting has a behavior closer to vim's
;;  [jasonspiro3@gmail.com:]
;;  - [FIX]: The Enter key now does what it should do -- insert a
;;    newline -- even when longlines-mode is on.
;;  - Comment changes.
;;
;; Version 0.2.6.9
;;  [laynor@gmail.com:]
;; - [FIX & NEW] Text Objects support fixed and integrated with viper. Now
;;   count works (i.e. you can do 3caw and it works correctly), and it's
;;   possible to repeat the commands with ".".
;;
;; Version 0.2.6.8
;;  [laynor@gmail.com:]
;; - [NEW]: Text object support: paren blocks, sentences, word, Words, quoted expressions,
;;   paragraphs. Delete and change commands.
;;   Example commands : diw ci( das etc.
;; - [FIX]: It's now possible to exit visual mode by pressing the ESC key or ^[
;;
;; Version 0.2.6.7
;;  [jasonspiro3@gmail.com:]
;;  - No code changes.
;;  - Fixed up "thanks" section below to mention Mieszko <sillyfox at yahoo.com>'s full
;;    name.  He wrote a small patch which was included long ago.  I must have
;;    forgotten to include it in the changelog.
;;
;; Version 0.2.6.6
;;  [laynor@gmail.com:]
;; - Fixed pasting in visual mode, works like in vim now (Experimental, see point 6.
;;   installation instructions)
;;
;; Version 0.2.6.5
;;  [laynor@gmail.com:]
;; - Fixed some major suckage with the change command. Still alpha, comments welcome,
;;   to use it see the installation instructions, point 6. (it's still experimental)
;; - Cleaned namespace, hope there are no hidden bugs
;; - Fixed loading on emacs snapshot
;;
;; Version 0.2.6.4
;;  [laynor@gmail.com:]
;;  This can probably be considered a major release.
;;  - [NEW and FIX] Rewritten visual mode, v and V variants (no changes to visual block still)
;;    It doesnot use the region like before: highlighting is done thru overlays,
;;    and the region is set inside the command code, before calling the viper
;;    commands. = in visual mode calls vimpulse-visual-indent-command. The visual mode
;;    (apart form block mode) looks and feels like vim.
;;  - [NEW] Enhanced paren matching. Moving the cursor on a closing paren in normal mode
;;    now highlights the opening paren.
;;  - [NEW] Pressing RET in insert mode automatically indents the new line.
;;  - [NEW] ^[ works
;;  - [FIX] a<ESC> leaves the cursor in the same location as it was before (it advanced the
;;    cursor 1 character before - viper-exit-insert-state's fault)
;;  - [FIX] cW doesn't suck anymore at the end of a line
;;
;; Version 0.2.6.3:
;;  [frank.fischer@s2001.tu-chemnitz.de:]
;;  - Support more visual-block-mode features: insert, append, delete, yank, change.
;;  - Change some vimpulse-functions and some viper-functions to handle
;;    block-mode properly.
;;  - Update documentation to reflect visual-block-mode.
;;  - The '=' command in visual-mode calls 'indent-region'.
;;
;; Version 0.2.6.2:
;;  [jasonspiro3@gmail.com:]
;;  - Improved XEmacs compatibility.
;;  - Small documentation improvements.
;;
;; Version 0.2.6.1:
;;  [jasonspiro3@gmail.com:]
;;  - Removed duplicate definition of vimpulse-detect-mark-deactivate
;;    and duplicate add-hook call to add the hook.  I must have added
;;    the extra copies by accident when doing my last big merge; now
;;    they are gone.
;;
;; Version 0.2.6.0:
;;  [jasonspiro3@gmail.com:]
;;  - Merged a patch for the function that powers * and #. Based on
;;    Ryoichi's patch and a cleaned-up version of Weihua's patch --
;;    thanks.  Now * and # will search for entire symbol at point,
;;    including underscores, not just word at point.
;;  - Todo addition.
;;
;; Version 0.2.5.1:
;;  [jasonspiro3@gmail.com:]
;;  - Redefined viper-adjust-undo to do nothing.  This way, in
;;    insert mode, typing then moving the cursor then typing more
;;    counts as two separately undoable actions instead of one.
;;    Thanks to Weihua JIANG and to max_ from IRC #emacs for the idea.
;;  - Small extra TODO.
;;
;; Version 0.2.5.0:
;;  [jasonspiro3@gmail.com:]
;;  - I've ignored my local changes for too long.  Here they are:
;;  - added keybindings from a Usenet post by Samuel Padgett
;;  - made change (cw, etc.) commands work more like Vim (my code)
;;  - I removed (setq ex-cycle-other-window nil); although it is very
;;    useful, it merely works around a problem with Viper.  I plan to
;;    discuss it with the Viper maintainer instead.
;;  - other changes and bugfixes from various people
;;
;; Version 0.2.0.3:
;;  [jasonspiro3@gmail.com:]
;;  - Added Brad's viper-jump-to-tag-at-point
;;
;; Version 0.2.0.2:
;;  [jasonspiro3@gmail.com:]
;;  - Small C-w keys and doc fixes.
;;
;; Version 0.2.0.1:
;;  [cppjavaperl:]
;;  - Added support for block visual mode (i.e. rectangle selection).
;;  - Made C-p look for matches *prior* to the cursor, added C-n
;;    binding to look for matches *before* the cursor.  This works
;;    more like Vim does.
;;  [jasonspiro3@gmail.com:]
;;  - Since vimpulse has no website, I added a prominent
;;    pointer at the top to the installation instructions.
;;
;; Version 0.2.0.0: Brad merged in several changes, including:
;;  - exit visual mode when the mark deactivates
;;  - changed the window manipulation to be global
;;  - added gf (goto file at point)
;;  - added \C-] and \C-t, tag jump & pop
;;  - added a helper function for defining keys
;;  - commented out show-paren-function, what is it meant to do?
;;
;; Version 0.1.0.1: No code changes.  Small documentation changes,
;; including updates on moving-left bug.
;;
;; Version 0.1: Initial release.

;;; Acknowledgements:

;; Special thanks to Brad Beveridge, the original author of Vimpulse.
;;
;; Thanks to:
;;
;; cppjavaperl <cppjavaperl@yahoo.com>
;; Frank Fischer <frank.fischer@s2001.tu-chemnitz.de>
;; John <jn at ngedit.com>
;; John J Foerch <jjfoerch@earthlink.net>
;; José Alfredo Romero L. <escherdragon at gmail.com>
;; Mieszko <sillyfox at yahoo.com>
;; Ryoichi Kanetaka <ryoichi.kanetaka at gmail.com>
;; Samuel Padgett
;; Stephen Bach <stephen at sjbach.com>
;; Stian S.
;; Toby Cubitt
;; Wang Xin
;; Weihua JIANG <weihua.jiang at gmail.com>
;;
;; and all the other people who have sent in bug reports or feedback.
;; Also, thanks to Michael Kifer and all those who have contributed to
;; viper-mode.
;;
;; We love patches.  Would you like to see your name here?  Please
;; send code and/or documentation patches to the maintainer.  Ideas,
;; comments, and test results are appreciated too.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; Load Viper
(unless (boundp 'viper-mode)
  (setq viper-mode t))
(require 'viper)

;; Load redo.el if available. Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21
;; doesn't ship with APEL included.
(unless (featurep 'redo)
  (load "redo" t))

;; Load rect.el and rect-mark.el if available
(require 'rect)
(unless (featurep 'rect-mark)
  (load "rect-mark" t))

;; Customization group for Vimpulse
(defgroup vimpulse nil
  "Vim emulation within Emacs."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'vimpulse-)

(defcustom vimpulse-experimental t
  "Whether or not to use experimental features.
Turned on by default, so you will give feedback :P"
  :group 'vimpulse
  :type  'boolean)

;; The secrets discovered from untold diggings among
;; the ruins of Customize code
(defun vimpulse-custom-value-p (symbol)
  "Non-nil if SYMBOL has a customized value."
  (or (get symbol 'customized-value)
      (get symbol 'customized-face)
      (get symbol 'saved-value)))

(defmacro vimpulse-custom-setq (sym val)
  "Set the customized value of SYM to VAL."
  `(prog1 (setq ,sym ,val)              ; return VAL
     (when (get ',sym 'custom-autoload)
       (custom-load-symbol ',sym))
     (put ',sym 'customized-value (list (custom-quote ,val)))))

(defmacro vimpulse-custom-setq-default (symbol value)
  "Set the customized default value of SYMBOL to VALUE."
  `(prog1 ,value                        ; return VALUE
     (when (get ',symbol 'custom-autoload)
       (custom-load-symbol ',symbol))
     (put ',symbol 'standard-value (list (custom-quote ,value)))))

(defmacro vimpulse-setq (sym val)
  "Set SYM to VAL, defaults included, unless SYM is customized.
SYM is unquoted. Returns VAL."
  `(cond
    ;; Customized value: just set custom standard value
    ((vimpulse-custom-value-p ',sym)
     (vimpulse-custom-setq-default ,sym ,val))
    ;; Customized variable: set custom and regular values
    ((custom-variable-p ',sym)
     (vimpulse-custom-setq-default ,sym ,val)
     (vimpulse-custom-setq ,sym ,val)
     (setq-default ,sym ,val)
     (setq ,sym ,val))
    ;; Regular variable; set default and local values
    (t
     (setq-default ,sym ,val)
     (setq ,sym ,val))))

;; Carefully set Viper/woman variables
(defun vimpulse-initialize-variables ()
  "Set various non-Vimpulse variables, unless customized."
  ;; Can backspace past start of insert/line
  (vimpulse-setq viper-ex-style-editing nil)
  ;; Don't create new frame for manpages
  (vimpulse-setq woman-use-own-frame nil)
  ;; Don't prompt upon K key (manpage display)
  (vimpulse-setq woman-use-topic-at-point t)
  ;; Make cursor color consistent
  (vimpulse-setq viper-insert-state-cursor-color
                 viper-vi-state-cursor-color)
  ;; Cursor moves backwards when exiting Insert state
  (vimpulse-setq viper-ESC-moves-cursor-back t)
  ;; Not in Vim: C-h is indispensable in Emacs
  (vimpulse-setq viper-want-ctl-h-help t)
  ;; Refresh Viper settings
  (viper-change-state-to-vi))

(if (and (boundp 'after-init-time) after-init-time)
    (vimpulse-initialize-variables)
  (add-hook 'after-init-hook 'vimpulse-initialize-variables))

(provide 'vimpulse-dependencies)

(defcustom vimpulse-want-change-state nil
  "Whether commands like \"cw\" invoke Replace state, vi-like.
The default is to delete the text and enter Insert state,
like in Vim."
  :group 'vimpulse
  :type  'boolean)

(defadvice viper-change
  (around vimpulse-want-change-state activate)
  "Disable Replace state if `vimpulse-want-change-state' is nil."
  (cond
   (vimpulse-want-change-state
    ad-do-it)
   (t
    ;; We don't want Viper's Replace mode when changing text;
    ;; just delete and enter Insert state.
    (kill-region beg end)
    (viper-change-state-to-insert))))

(defcustom vimpulse-goto-line t
  "Goto line with \"G\" like in Vim."
  :group 'vimpulse
  :type  'boolean)

(defun vimpulse-goto-line (arg)
  "Go to ARG's line; without ARG go to end of buffer.
Works like Vim's \"G\"."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq ?c com) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-max)))
     (t
      (goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

(when vimpulse-goto-line
  (fset 'viper-goto-line 'vimpulse-goto-line))

(defun vimpulse-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then modifications to this
major mode may not take effect until the buffer switches state to Vi,
Insert or Emacs.  If this happens, add `viper-change-state-to-emacs' to this
major mode's hook.  If no such hook exists, you may have to put an advice on
the function that invokes the major mode.  See `viper-set-hooks' for hints.

The above needs not to be done for major modes that come up in Vi or Insert
state by default."
  (let ((alist
	 (cond ((eq state 'vi-state)
                'viper-vi-state-modifier-alist)
	       ((eq state 'insert-state)
                'viper-insert-state-modifier-alist)
	       ((eq state 'emacs-state)
                'viper-emacs-state-modifier-alist)
               ((eq state 'visual-state)
                'vimpulse-visual-state-modifier-alist)))
	elt)
    (if (setq elt (assoc mode (eval alist)))
	(set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))
    (viper-normalize-minor-mode-map-alist)
    (viper-set-mode-vars-for viper-current-state)))

(fset 'viper-modify-major-mode 'vimpulse-modify-major-mode)

;;
;; Thanks to the anonymous poster for the idea on how to modify the viper
;; function to add the di da ci and ca partial commands.
;;
(defcustom vimpulse-text-objects t
  "Text objects support, on by default."
  :group 'vimpulse
  :type  'boolean)

;; REDEFINITION OF VIPER FUNCTION
;;
;; `viper-prefix-arg-com', originally defined in viper-cmd.el, does
;; much of the work of reading keyboard input and chosing the
;; appropriate command. As an ugly way of getting basic "delete inner
;; parentheses" functionality, we extend it here with entries for our
;; custom `vimpulse-di' and `vimpulse-ci' functions (defined below).
;;
;; This should be done in a cleaner way. Michael Kifer gives some
;; hints in viper.el:
;;
;;     Some of the code that is inherited from VIP-3.5 is rather
;;     convoluted. Instead of viper-command-argument, keymaps should
;;     bind the actual commands. E.g., "dw" should be bound to a
;;     generic command viper-delete that will delete things based on
;;     the value of last-command-char. This would greatly simplify the
;;     logic and the code.
;;
;; For (some) brewity, Kifer's comments are removed. The added lines
;; are annotated with ";; MODIFICATION".

(defun vimpulse-prefix-arg-com (char value com)
  (let ((cont t)
        cmd-info
        cmd-to-exec-at-end)
    (while (and cont
                (viper-memq-char char
                                 (list ?q ?i ?a ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ;;modified ?i ?a ?q
                                       viper-buffer-search-char)))
      (if com
          ;; this means that we already have a command character, so we
          ;; construct a com list and exit while.  however, if char is "
          ;; it is an error.
          (progn
            ;; new com is (CHAR . OLDCOM)
            (if (viper-memq-char char '(?# ?\")) (error "Viper bell"))
            (setq com (cons char com))
            (setq cont nil))
        ;; If com is nil we set com as char, and read more.  Again, if char is
        ;; ", we read the name of register and store it in viper-use-register.
        ;; if char is !, =, or #, a complete com is formed so we exit the while
        ;; loop.
        (cond ((viper-memq-char char '(?! ?=))
               (setq com char)
               (setq char (read-char))
               (setq cont nil))
              ((viper= char ?#)
               ;; read a char and encode it as com
               (setq com (+ 128 (read-char)))
               (setq char (read-char)))
              ((viper= char ?\")
               (let ((reg (read-char)))
                 (if (viper-valid-register reg)
                     (setq viper-use-register reg)
                   (error "giovanni"))
                 (setq char (read-char))))
              (t
               (setq com char)
               (setq char (read-char))))))
    (if (atom com)
        ;; `com' is a single char, so we construct the command argument
        ;; and if `char' is `?', we describe the arg; otherwise
        ;; we prepare the command that will be executed at the end.
        (progn
          (setq cmd-info (cons value com))
          (while (viper= char ?U)
            (viper-describe-arg cmd-info)
            (setq char (read-char)))
          ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
          ;; we execute it at the very end
          (or (viper-movement-command-p char)
              (viper-digit-command-p char)
              (viper-regsuffix-command-p char)
              (viper= char ?!) ; bang command
              (viper= char ?g) ; the gg command (like G0)
              (viper= char ?=) ; the == command
              (error "Viper bell"))
          (setq cmd-to-exec-at-end
                (viper-exec-form-in-vi
                 `(key-binding (char-to-string ,char)))))
      ;; as com is non-nil, this means that we have a command to execute
      (if (viper-memq-char (car com) '(?r ?R))
          ;; execute apropriate region command.
          (let ((char (car com)) (com (cdr com)))
            (setq prefix-arg (cons value com))
            (if (viper= char ?r)
                (viper-region prefix-arg)
              (viper-Region prefix-arg))
            ;; reset prefix-arg
            (setq prefix-arg nil))
        ;; otherwise, reset prefix arg and call appropriate command
        (setq value (if (null value) 1 value))
        (setq prefix-arg nil)
        (cond
         ;; If we change ?C to ?c here, then cc will enter replacement mode
         ;; rather than deleting lines.  However, it will affect 1 less line
         ;; than normal.  We decided to not use replacement mode here and
         ;; follow Vi, since replacement mode on n full lines can be achieved
         ;; with nC.
         ((equal com '(?q . ?d)) (vimpulse-test-function value))
         ((equal com '(?a . ?d)) (vimpulse-delete-text-objects-command value ?a)) ; da<x>
         ((equal com '(?a . ?c)) (vimpulse-change-text-objects-command value ?a)) ; ca<x>
         ((equal com '(?a . ?y)) (vimpulse-yank-text-objects-command value ?a)) ; ya<x>
         ((equal com '(?i . ?d)) (vimpulse-delete-text-objects-command value ?i)) ; di<x>
         ((equal com '(?i . ?c)) (vimpulse-change-text-objects-command value ?i)) ; ci<x>
         ((equal com '(?i . ?y)) (vimpulse-yank-text-objects-command value ?i)) ; yi<x>
         ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
         ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
         ((equal com '(?d . ?y)) (viper-yank-defun))
         ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
         ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
         ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
         ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
         ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
         ;; gg  acts as G0
         ((equal (car com) ?g)   (viper-goto-line 0))
         (t (error "Viper bell")))))
    (if cmd-to-exec-at-end
        (progn
          (setq last-command-event
                (viper-copy-event
                 (if (featurep 'xemacs) (character-to-event char) char)))
          (condition-case err
              (funcall cmd-to-exec-at-end cmd-info)
            (error
             (error "%s" (error-message-string err))))))))

(when vimpulse-text-objects
  (fset 'viper-prefix-arg-com 'vimpulse-prefix-arg-com))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefining viper-ex to get a similar behavior to vim when ;;;
;;; issuing ":" when visual selecting.                        ;;;
;;; NOTE: this is a kludge.                                   ;;;
;;;       Vimpulse eats 'y and 'z marks to emulate vim's      ;;;
;;;       behavior instead of introducing '< and '>, because  ;;;
;;;       introducing them would introduce even more kludges  ;;;
;;;       like this one.                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vimpulse-visual-ex t
  "Ex support for visual selections, on by default."
  :group 'vimpulse
  :type  'boolean)

(defun vimpulse-ex (arg &optional string)
  (interactive "P")
  (or string
      (setq ex-g-flag nil
            ex-g-variant nil))
  (let* ((map (copy-keymap minibuffer-local-map))
         (address nil)
         (cont t)
         (dot (point))
         (initial-str
          (when (and vimpulse-visual-mode
                     (not (eq 'block vimpulse-visual-mode)))
            "'y,'z"))
         reg-beg reg-end
         reg-beg-line reg-end-line
         prev-token-type com-str)
    (viper-add-keymap viper-ex-cmd-map map)
    (if arg
        (progn
          (viper-enlarge-region (mark t) (point))
          (if (> (point) (mark t))
              (setq reg-beg (mark t)
                    reg-end (point))
            (setq reg-end (mark t)
                  reg-beg (point)))
          (save-excursion
            (goto-char reg-beg)
            (setq reg-beg-line (1+ (count-lines (point-min) (point)))
                  reg-end-line
                  (+ reg-beg-line (count-lines reg-beg reg-end) -1)))))
    (if reg-beg-line
        (setq initial-str (format "%d,%d" reg-beg-line reg-end-line)))
    (setq com-str
          (if string
              (concat initial-str string)
            (viper-read-string-with-history
             ":"
             initial-str
             'viper-ex-history
             ;; no default when working on region
             (if initial-str
                 nil
               (car viper-ex-history))
             map
             (if initial-str
                 " [Type command to execute on current region]"))))
    (save-window-excursion
      ;; just a precaution
      (setq viper-ex-work-buf (get-buffer-create viper-ex-work-buf-name))
      (set-buffer viper-ex-work-buf)
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type nil
          ex-addresses nil)
    (while cont
      (viper-get-ex-token)
      (cond ((memq ex-token-type '(command end-mark))
             (if address (setq ex-addresses (cons address ex-addresses)))
             (viper-deactivate-mark)
             (let ((cmd (ex-cmd-assoc ex-token ex-token-alist)))
               (if (null cmd)
                   (error "`%s': %s" ex-token viper-BadExCommand))
               (ex-cmd-execute cmd)
               (if (or (ex-cmd-is-mashed-with-args cmd)
                       (ex-cmd-is-one-letter cmd))
                   (setq cont nil)
                 (save-excursion
                   (save-window-excursion
                     (setq viper-ex-work-buf
                           (get-buffer-create viper-ex-work-buf-name))
                     (set-buffer viper-ex-work-buf)
                     (skip-chars-forward " \t")
                     (cond ((looking-at "|")
                            (forward-char 1))
                           ((looking-at "\n")
                            (setq cont nil))
                           (t
                            (error
                             "`%s': %s"
                             ex-token
                             viper-SpuriousText))))))))
            ((eq ex-token-type 'non-command)
             (error "`%s': %s" ex-token viper-BadExCommand))
            ((eq ex-token-type 'whole)
             (setq address nil)
             (setq ex-addresses
                   (if ex-addresses
                       (cons (point-max) ex-addresses)
                     (cons (point-max) (cons (point-min) ex-addresses)))))
            ((eq ex-token-type 'comma)
             (if (eq prev-token-type 'whole)
                 (setq address (point-min)))
             (setq ex-addresses
                   (cons (if (null address) (point) address) ex-addresses)))
            ((eq ex-token-type 'semi-colon)
             (if (eq prev-token-type 'whole)
                 (setq address (point-min)))
             (if address (setq dot address))
             (setq ex-addresses
                   (cons (if (null address) (point) address) ex-addresses)))
            (t (let ((ans (viper-get-ex-address-subr address dot)))
                 (if ans (setq address ans)))))
      (setq prev-token-type ex-token-type))))

(when vimpulse-visual-ex
  (fset 'viper-ex 'vimpulse-ex))

(provide 'vimpulse-viper-function-redefinitions)

;;;;
;;;; This file contains helper functions that
;;;; a) Can be useful for the end user
;;;; b) Can be useful for the contributor, thus avoiding
;;;;    duplication of functionalities.
;;;;

(defun vimpulse-is-whitespace (pos)
  "Returns true if the character at `pos' is whitespace, nil otherwhise"
  (equal (char-syntax (char-after pos)) 32))

;; Define a helper function that sets up the viper keys in a given map.
;; This function is useful for creating movement maps or altering existing
;; maps
(defun vimpulse-set-movement-keys-for-map (map)
  (define-key map "\C-d" 'viper-scroll-up)
  (define-key map "\C-u" 'viper-scroll-down)
  (define-key map "j" 'viper-next-line)
  (define-key map "k" 'viper-previous-line)
  (define-key map "l" 'viper-forward-char)
  (define-key map "h" 'viper-backward-char))

;; EXAMPLE, the following lines enable Vim style movement in help
;; and dired modes.
;; create a movement map and set the keys
;;
;; (setq vimpulse-movement-map (make-sparse-keymap))
;; (vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;; (viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map)
;; (viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)

(defmacro vimpulse-region-command (function)
  "Convert Emacs region commands to visual selection commands.
See the comments in the source for an example on how to use it."
  `(lambda ()
     ,(format "Apply `%s' on visual selection." function)
     (interactive)
     (,function (vimpulse-visual-start) (vimpulse-visual-end))
     (vimpulse-visual-mode -1)))

;; The macro vimpulse-region-commands works with any Emacs command
;; that operates with a region and takes as arguments the beginning
;; and end of the region. For example, the `comment-region' and
;; `uncomment-region' commands:
;;
;;     (comment-region beg end &optional arg)
;;     (uncomment-region beg end &optional arg)
;;
;; You can define new bindings for commenting and uncommenting the
;; region as easily as:
;;
;;     (define-key viper-vi-global-user-map "\\\]"
;;       (vimpulse-region-command comment-region))
;;     (define-key viper-vi-global-user-map "\\,"
;;       (vimpulse-region-command uncomment-region))

(provide 'vimpulse-utils)

;;;;
;;;; This file provides the functions `vimpulse-map', `vimpulse-imap'
;;;; and `vimpulse-vmap', which mimic :map, :imap and :vmap in Vim, as
;;;; well as `vimpulse-define-key', a general-purpose function for
;;;; binding keys in a "modal" way.
;;;;
;;;; BACKGROUND
;;;;
;;;; The :map, :imap and :vmap commands of Vim let one make two key
;;;; mappings starting with the same sequence of characters without one
;;;; overwriting the other. For example:
;;;;
;;;;     :imap aa foo
;;;;     :imap aaa bar
;;;;
;;;; When Vim has read "aa" in Insert mode, it will wait for another
;;;; character to decide whether to insert "foo" or "bar". If the user
;;;; types "a", "bar" is inserted; if another letter, "foo" plus that
;;;; letter.
;;;;
;;;; Compare with the analogous use of Emacs' `global-set-key' function:
;;;;
;;;;     (global-set-key "aa" 'foo)
;;;;     (global-set-key "aaa" 'bar)
;;;;
;;;; Here, the first binding is simply overwritten by the more specific
;;;; second. The end result is that "aaa" is bound to `bar', while any
;;;; other sequence starting with "aa" is not bound to anything.
;;;;
;;;; This file provides a set of Vim-like or "modal" functions for
;;;; making new key bindings "on top of" previous bindings. They are
;;;; `vimpulse-map', `vimpulse-imap' and `vimpulse-vmap', which mimic
;;;; Vim's commands, and `vimpulse-define-key', a general function for
;;;; specifying the keymap. Returning to the example:
;;;;
;;;;     (vimpulse-imap "aa" 'foo)
;;;;     (vimpulse-imap "aaa" 'bar)
;;;;
;;;; This will bind "aaa" to `bar', and "aa" + any other key to `foo'.
;;;; The syntax is the same as that of `global-set-key'. The key
;;;; sequence may be specified as a string, like above, as a vector
;;;; (like [?a ?b ?c]), or as a call to `kbd' (like (kbd "a b c")).
;;;;
;;;; To make a binding in vi (command) mode, use `vimpulse-map';
;;;; in Insert mode, `vimpulse-imap'; in Visual mode, `vimpulse-vmap'.
;;;; The more general `vimpulse-define-key' function lets one specify
;;;; the keymap to store the binding in, as when using `define-key':
;;;;
;;;;     (vimpulse-define-key keymap "abc" 'command)
;;;;
;;;; IMPLEMENTATION
;;;;
;;;; The code depends on a little-known GNU Emacs feature called
;;;; "default key bindings". A default key binding is a binding ending
;;;; with the Lisp symbol t, which roughly stands for "any other key".
;;;; Default bindings allow a keymap to bind all possibilities without
;;;; having to enumerate them. For example, we may bind the sequence
;;;; "AB" + any key as such:
;;;;
;;;;     (global-set-key (kbd "A B <t>") 'foo)
;;;;
;;;; This means that "ABA" will execute `foo', as will "ABB", "ABC",
;;;; and so on. For more on default key bindings, see the GNU Emacs
;;;; Lisp Reference Manual, chapter 22.3: "Format of Keymaps".
;;;;
;;;; What is done by functions like `vimpulse-define-key' and
;;;; `vimpulse-map' (which depends on the former) is to generate these
;;;; default bindings automatically. If "AB" is already bound to `foo'
;;;; and we modally bind "ABC" to `bar', the old binding is first
;;;; replaced by a default binding, as if we issued the following:
;;;;
;;;;     (global-set-key (kbd "A B") nil) ; delete old binding
;;;;     (global-set-key (kbd "A B <t>") 'foo)
;;;;     (global-set-key (kbd "A B C") 'bar)
;;;;
;;;; Then, "ABC" runs `bar', while "AB" + any other key than C
;;;; runs `foo'.
;;;;
;;;; This almost gets us where we want with regard to Vimpulse, but not
;;;; quite. The problem is that quite a few commands must necessarily
;;;; read and parse keyboard input to decide what to do. For instance,
;;;; Viper binds "d" to the general command `viper-command-argument',
;;;; which, depending on the next key-presses, deletes a line, two
;;;; words, or any motion entered by the user. What happens if we decide
;;;; to modally bind, say, "dq" to a custom command `foo' of our own?
;;;;
;;;;     (global-set-key (kbd "d") nil) ; delete old binding
;;;;     (global-set-key (kbd "d <t>") 'viper-command-argument)
;;;;     (global-set-key (kbd "d q") 'foo)
;;;;
;;;; Now, if the user enters "dq", `foo' is called. But when the user
;;;; enters "dw" to delete a word, `viper-command-argument' is called
;;;; only after the "w" is entered. This destroys the logic of the
;;;; command, which depends on "d" being the last key-press (stored in
;;;; `last-command-event') before "w" is read through `read-char'. It
;;;; obviously won't work as intended with a single "w" missing a
;;;; preceding "d", which is what it sees.
;;;;
;;;; So, we need to find a way to pass "d" and "w" along in the proper
;;;; manner; that is, to make the default binding appear the same as the
;;;; old binding it replaces. This is done by `vimpulse-modal-pre-hook',
;;;; which unreads "w" (so it can be read again) and changes
;;;; `last-command-event' to "d". Of course, this behavior is only
;;;; needed for default key bindings, and only for default key bindings
;;;; made by the modal binding functions. To that end, every time
;;;; `vimpulse-define-key' makes a default binding, the binding is
;;;; listed in `vimpulse-modal-alist' for future reference. Checking
;;;; against the list, `vimpulse-modal-pre-hook' only does its thing if
;;;; the current binding comes back positive.
;;;;
;;;; XEmacs is somewhat fuzzy about its command loop variables, not
;;;; allowing direct modification of `last-command-event'. However,
;;;; shadowing it with a `let' binding is possible, and a wrap-around
;;;; advice of the current command is employed to accomplish this. Also,
;;;; XEmacs does not have default key bindings in quite the same way as
;;;; GNU Emacs; `vimpulse-def-binding' takes care of the differences.
;;;;
;;;; LIMITATIONS
;;;;
;;;; Vim has a `timeout' option which lets one specify the time in
;;;; milliseconds that is waited for a key code or mapped key sequence
;;;; to complete. Emacs, on the other hand, will wait indefinitely. This
;;;; behavior is probably not implementable.
;;;;

;; VARIABLES
;;
;; This is mostly for dealing with default key bindings.
(defvar vimpulse-last-command-event nil
  "Value for overwriting `last-command-event'.
Used by `vimpulse-modal-pre-hook'.")

(defvar vimpulse-modal-alist '()
  "Key bindings for which `vimpulse-modal-pre-hook' is active.
That is, `last-command-event' and `read-char' work differently
for these bindings. The format is (KEY-VECTOR . COMMAND).")

;; ADVICE
;;
;; For XEmacs, construct a wrap-around advice of the current command
;; for shadowing the read-only command loop variables with a
;; `let' binding.
(defmacro vimpulse-advice-command (command)
  "Make wrap-around advice for shadowing `last-command-event'.
XEmacs does not allow us to change its command loop variables
directly, but shadowing them with a `let' binding works."
  `(defadvice ,command (around vimpulse-modal activate)
     "Shadow `last-command-event' with a `let' binding."
     (cond
      (vimpulse-last-command-event
       (let* ((last-command-event
               (character-to-event vimpulse-last-command-event))
              (last-command-char vimpulse-last-command-event)
              (last-input-event last-command-event)
              (last-input-char last-command-char))
         ad-do-it))
      (t
       ad-do-it))))

;; GENERAL FUNCTIONS
;;
;; Makes dealing with vectors and key sequences a little easier.
(defun vimpulse-truncate (vector length &optional offset)
  "Return a copy of VECTOR truncated to LENGTH.
If LENGTH is negative, skip last elements of VECTOR.
If OFFSET is specified, skip first elements of VECTOR."
  ;; If LENGTH is too large, trim it
  (when (> length (length vector))
    (setq length (length vector)))
  ;; If LENGTH is negative, convert it to the positive equivalent
  (when (> 0 length)
    (setq length (+ (length vector) length)))
  (when (> 0 length)
    (setq length 0))
  (if offset
      (setq length (- length offset))
    (setq offset 0))
  (let ((result (make-vector length t)) idx)
    (dotimes (idx length result)
      (aset result idx (aref vector (+ idx offset))))))

(defun vimpulse-strip-prefix (key-sequence)
  "Strip any prefix argument keypresses from KEY-SEQUENCE.
This is useful for deriving a \"standard\" key-sequence from
`this-command-keys', to be looked up in `vimpulse-modal-alist'."
  (let* ((offset 0)
         (temp-sequence (vconcat key-sequence))
         (key (aref temp-sequence offset))
         (length (length temp-sequence)))
    ;; If XEmacs, get rid of the event object type
    (and (featurep 'xemacs) (eventp key)
         (setq key (event-to-character key)))
    ;; Any keys bound to `universal-argument', `digit-argument' or
    ;; `negative-argument' or bound in `universal-argument-map'
    ;; are considered prefix keys.
    (while (and (or (memq (key-binding (vector key) t)
                          '(universal-argument
                            digit-argument
                            negative-argument))
                    (lookup-key universal-argument-map
                                (vector key)))
                (setq offset (1+ offset))
                (< offset length))
      (setq key (aref temp-sequence offset))
      (and (featurep 'xemacs) (eventp key)
           (setq key (event-to-character key))))
    (vimpulse-truncate temp-sequence length offset)))

(defun vimpulse-modal-check (key-sequence)
  "Return t if KEY-SEQUENCE defaults to `this-command',
but only for bindings listed in `vimpulse-modal-alist'."
  (let ((temp-sequence (vimpulse-strip-prefix key-sequence)))
    (setq temp-sequence (vimpulse-truncate temp-sequence -1))
    (and this-command ; may be nil
         (eq this-command
             (cdr (assoc temp-sequence vimpulse-modal-alist))))))

(defun vimpulse-modal-remove (key-vector &optional recursive)
  "Delete entry with KEY-VECTOR from `vimpulse-modal-alist'.
If RECURSIVE is non-nil, also delete entries whose key-vectors
start with KEY-VECTOR."
  (if recursive
      (let (entry)
        (dolist (entry vimpulse-modal-alist)
          (when (equal key-vector
                       (vimpulse-truncate (car entry)
                                          (length key-vector)))
            (setq vimpulse-modal-alist
                  (delq entry vimpulse-modal-alist)))))
    (assq-delete-all key-vector vimpulse-modal-alist)))

(defun vimpulse-xemacs-def-binding
  (keymap key def &optional modal-binding define-func)
  "Make a default binding in XEmacs. If MODAL-BINDING is
non-nil, advice DEF by means of `vimpulse-advice-command'."
  (let ((temp-sequence (vconcat key))
        (submap (lookup-key keymap key)))
    (unless define-func (setq define-func 'define-key))
    (and modal-binding (commandp def)
         (eval `(vimpulse-advice-command ,def)))
    (and (< 1 (length temp-sequence))
         (eq t (aref temp-sequence (1- (length temp-sequence))))
         (setq temp-sequence (vimpulse-truncate temp-sequence -1)))
    ;; The following is from
    ;; http://tracker.xemacs.org/XEmacs/its/msg2021
    (unless (keymapp submap)
      (setq submap (make-sparse-keymap)))
    (set-keymap-default-binding submap def)
    (funcall define-func keymap temp-sequence submap)))

(defun vimpulse-def-binding
  (keymap key def &optional modal-binding define-func)
  "Make a default binding in GNU Emacs or XEmacs,
whichever is appropriate. If MODAL-BINDING is non-nil,
the binding is listed in `vimpulse-modal-alist'."
  (let ((temp-sequence (vconcat key)))
    (unless define-func (setq define-func 'define-key))
    (cond
     ((featurep 'xemacs)
      (vimpulse-xemacs-def-binding
       keymap temp-sequence def modal-binding define-func))
     (t
      (unless (eq t (aref temp-sequence (1- (length temp-sequence))))
        (setq temp-sequence (vconcat temp-sequence [t])))
      (funcall define-func keymap temp-sequence def)))
    (when modal-binding
      (add-to-list 'vimpulse-modal-alist
                   (cons (vimpulse-truncate temp-sequence -1) def)))))

;; HOOK RUN BEFORE EACH COMMAND IS EXECUTED
;;
;; If the current command is a default key binding made by the modal
;; binding functions, we need to unread the last input events and
;; change some command loop variables to give the command the
;; impression of its "old" binding.
(defun vimpulse-modal-pre-hook ()
  "Update `vimpulse-last-command-event' and `unread-command-events'.
If the current key-sequence defaults to a shorter key-sequence,
the difference is stored in these two variables, to be passed on
via the `last-command-event' variable and the `read-char'
functions, respectively."
  (setq vimpulse-last-command-event nil)
  (let ((key-sequence (vconcat (this-command-keys))))
    ;; If XEmacs, get rid of the event object type
    (when (featurep 'xemacs)
      (setq key-sequence (events-to-keys key-sequence)))
    (while (and (< 1 (length key-sequence))
                (vimpulse-modal-check key-sequence))
      ;; Unread last event
      (setq vimpulse-last-command-event
            (elt key-sequence
                 (- (length key-sequence) 1)))
      (when (featurep 'xemacs)
        (setq vimpulse-last-command-event
              (character-to-event vimpulse-last-command-event)))
      (add-to-list 'unread-command-events vimpulse-last-command-event)
      ;; Change command loop variables
      (setq vimpulse-last-command-event
            (elt key-sequence
                 (- (length key-sequence) 2)))
      (unless (featurep 'xemacs) ; if XEmacs, do this with advice
        (setq last-command-event vimpulse-last-command-event)
        (setq last-command-char  vimpulse-last-command-event)
        (setq last-input-event   vimpulse-last-command-event)
        (setq last-input-char    vimpulse-last-command-event))
      (setq key-sequence
            (vimpulse-truncate key-sequence -1)))))

;; HOOK RUN AFTER EACH COMMAND IS EXECUTED
;;
;; This merely ensures `vimpulse-last-command-event' is reset.
(defun vimpulse-modal-post-hook ()
  "Erase `vimpulse-last-command-event'."
  (setq vimpulse-last-command-event nil))

(add-hook 'pre-command-hook  'vimpulse-modal-pre-hook)
(add-hook 'post-command-hook 'vimpulse-modal-post-hook)

;; MODAL BINDING FUNCTIONS
;;
;; `vimpulse-define-key' is general; `vimpulse-map', `vimpulse-imap'
;; and `vimpulse-vmap' imitate Vim's :map, :imap and :vmap,
;; respectively.
(defun vimpulse-define-key
  (keymap key def &optional dont-list define-func)
  "Modally bind KEY to DEF in KEYMAP.
\"Modally\" means that if a subset of the key sequence is already
bound, a default binding is made so that the new binding won't
overwrite the old. E.g., if we want to modally bind \"A B C\" to
`foo', and \"A B\" is already bound to `bar', the end result is

    \"A B C\"   => `foo'
    \"A B <t>\" => `bar'

which means that \"A B D\", for example, defaults to `bar'. (For
more on default bindings, see `define-key'.) The default binding
gets listed in `vimpulse-modal-alist', so that, from the
command's point of view, it appears exactly the same as the
binding it replaced. To override this, use DONT-LIST.
DEFINE-FUNC specifies a function to be used in place of
`define-key'.

NOTE: If \"A B\" was not bound in KEYMAP, but in some other map
which is active only in a certain state (say, Insert mode), this
function could detect that binding only if called in the same
state. The functions `vimpulse-map', `vimpulse-imap' and
`vimpulse-vmap' take care of this."
  (let (temp-sequence current-binding previous-binding i)
    ;; For each subset of KEY (stored in `temp-sequence'), check
    ;; the binding (stored in `current-binding'); if it isn't bound,
    ;; use `previous-binding'.
    (setq define-func (or define-func 'define-key)
          key (vconcat key))
    (cond
     ;; nil unbinds the key-sequence
     ((not def)
      (funcall define-func keymap key def)
      (while (and (< 1 (length key))
                  (not (lookup-key keymap key)))
        (vimpulse-modal-remove key t)
        (setq key (vimpulse-truncate key -1))))
     ;; undefined also unbinds, but less forcefully
     ((eq 'undefined def)
      (if (keymapp (lookup-key keymap key))
          (vimpulse-def-binding keymap key nil t define-func)
        (funcall define-func keymap key def))
      (vimpulse-modal-remove key))
     ;; Regular binding: convert previous bindings to default bindings
     (t
      (dotimes (i (1- (length key)))
        (setq temp-sequence (vimpulse-truncate key (1+ i)))
        (setq current-binding (lookup-key keymap temp-sequence t))
        (when (or (numberp current-binding) (not current-binding))
          (setq current-binding
                (or (key-binding temp-sequence t) previous-binding)))
        (setq previous-binding current-binding)
        ;; If `current-binding' is a keymap, do nothing, since our modal
        ;; binding can exist happily as part of that keymap. However, if
        ;; `current-binding' is a command, we need to make room for the
        ;; modal binding by creating a default binding.
        (unless (keymapp current-binding)
          (setq temp-sequence (vconcat temp-sequence [t]))
          (setq current-binding (lookup-key keymap temp-sequence t))
          (when (or (numberp current-binding) (not current-binding))
            (setq current-binding
                  (or (key-binding temp-sequence t) previous-binding))
            (define-key keymap
              (vimpulse-truncate temp-sequence -1) nil)
            (vimpulse-def-binding keymap
                                  temp-sequence
                                  current-binding
                                  (not dont-list)
                                  define-func))
          (setq previous-binding current-binding)))
      ;; Defaults are taken care of; we may now bind the key.
      ;; If a longer binding starting with KEY exists,
      ;; make a default binding so it's not overwritten.
      (if (keymapp (lookup-key keymap key))
          (vimpulse-def-binding
           keymap key def (not dont-list) define-func)
        (funcall define-func keymap key def))))))

(defun vimpulse-map (key def &rest modes)
  "Modally bind KEY to DEF in vi (command) state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-map \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-map \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (let ((old-state viper-current-state)
        (old-cursor cursor-type)
        mode map)
    (viper-change-state-to-vi)
    (cond
     (modes
      (dolist (mode modes)
        (if (eq t mode)
            (vimpulse-define-key viper-vi-global-user-map key def)
          (setq map
                (or (cdr (assoc mode viper-vi-state-modifier-alist))
                    (make-sparse-keymap)))
          (vimpulse-define-key map key def)
          (viper-modify-major-mode mode viper-current-state map))))
     (t
      (vimpulse-define-key viper-vi-basic-map key def)))
    (save-excursion
      (viper-change-state old-state)
      (setq cursor-type old-cursor))))

(defun vimpulse-imap (key def &rest modes)
  "Modally bind KEY to DEF in Insert state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-imap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-imap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (let ((old-state viper-current-state)
        (old-cursor cursor-type)
        mode map)
    (viper-change-state-to-insert)
    (cond
     (modes
      (dolist (mode modes)
        (if (eq t mode)
            (vimpulse-define-key viper-insert-global-user-map key def)
          (setq map
                (or (cdr (assoc mode viper-insert-state-modifier-alist))
                    (make-sparse-keymap)))
          (vimpulse-define-key map key def)
          (viper-modify-major-mode mode viper-current-state map))))
     (t
      (vimpulse-define-key viper-insert-basic-map key def)))
    (save-excursion
      (viper-change-state old-state)
      (setq cursor-type old-cursor))))

(defun vimpulse-vmap (key def &rest modes)
  "Modally bind KEY to DEF in Visual state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-vmap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-vmap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (let ((old-state viper-current-state)
        (old-cursor cursor-type)
        mode map)
    (vimpulse-change-state-to-visual)
    (cond
     (modes
      (dolist (mode modes)
        (if (eq t mode)
            (vimpulse-define-key
             vimpulse-visual-global-user-map key def)
          (setq map
                (or (cdr (assoc mode vimpulse-visual-state-modifier-alist))
                    (make-sparse-keymap)))
          (vimpulse-define-key map key def)
          (viper-modify-major-mode mode viper-current-state map))))
     (t
      (vimpulse-define-key vimpulse-visual-basic-map key def)))
    (save-excursion
      (viper-change-state old-state)
      (setq cursor-type old-cursor))))

(defun vimpulse-map! (key def &rest modes)
  "Bind KEY to DEF in vi (command) state and Visual state."
  (vimpulse-map key def modes)
  (vimpulse-vmap key def modes))

(provide 'vimpulse-modal)

;;;;
;;;; Utility code
;;;;
(defvar vimpulse-viper-movement-cmds
  '(viper-backward-Word viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-command-argument
    viper-digit-argument viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-line-to-bottom viper-line-to-middle
    viper-line-to-top viper-next-line viper-previous-line
    viper-scroll-down-one viper-scroll-down viper-scroll-up
    viper-scroll-up-one viper-window-bottom viper-window-middle
    viper-window-top vimpulse-goto-first-line
    vimpulse-search-backward-for-symbol-at-point
    vimpulse-search-forward-for-symbol-at-point
    vimpulse-jump-backward vimpulse-jump-forward
    vimpulse-visual-toggle-normal vimpulse-visual-toggle-line
    vimpulse-visual-toggle-block)
  "List of Viper/Vimpulse movement commands.")

(defvar vimpulse-core-movement-cmds
  '(viper-backward-char
    viper-next-line
    viper-previous-line
    viper-forward-char)
  "List of Viper \"core\" movement commands.
These should be present in every mode, to avoid confusion.")

(defun vimpulse-augment-keymap
  (map augment-alist &optional replace)
  "Augment MAP with bindings from AUGMENT-ALIST.
If REPLACE is non-nil, bindings in MAP may be overwritten.
AUGMENT-ALIST has the format ((KEY . DEF) ...),
where KEY and DEF are passed to `define-key'."
  (let (binding key def num)
    (dolist (binding augment-alist)
      (setq key (car binding)
            def (cdr binding)
            num (lookup-key map key))
      (cond
       (replace
        (when (numberp num)
          (define-key map (vimpulse-truncate key num) nil))
        (define-key map key def))
       (t
        (when (numberp num)
          (setq num (lookup-key map (vimpulse-truncate key num))))
        (unless num
          (define-key map key def)))))))

(defun vimpulse-add-vi-bindings (map cmds &optional replace)
  "Add vi bindings for CMDS to MAP."
  (let (cmd vimap pmap keys key)
    (setq pmap (make-sparse-keymap))
    (dolist (cmd cmds map)
      (dolist (vimap (list viper-vi-intercept-map
                           viper-vi-local-user-map
                           viper-vi-global-user-map
                           viper-vi-kbd-map
                           viper-vi-diehard-map
                           viper-vi-basic-map))
        (setq keys (where-is-internal cmd vimap))
        (dolist (key keys)
          (when (or (not (lookup-key pmap key))
                    (numberp (lookup-key pmap key)))
            (vimpulse-augment-keymap map
                                     `((,key . ,cmd))
                                     replace)
            ;; To prioritize between maps in `vimap',
            ;; we keep track of bindings by augmenting `pmap'.
            (vimpulse-augment-keymap pmap
                                     `((,key . ,cmd)))))))))

(defun vimpulse-add-movement-cmds (map &optional replace)
  "Add Viper/Vimpulse movement commands to MAP.
The commands are taken from `vimpulse-viper-movement-cmds' and looked
up in vi keymaps. If REPLACE is non-nil, may overwrite bindings
in MAP."
  (vimpulse-add-vi-bindings map vimpulse-viper-movement-cmds replace))

;; The default for this function is to replace rather than augment,
;; as core navigation should be present everywhere.
(defun vimpulse-add-core-movement-cmds (map &optional augment)
  "Add \"core\" movement commands to MAP, forcefully.
The commands are taken from `vimpulse-core-movement-cmds'.
If AUGMENT is non-nil, don't overwrite bindings in MAP."
  (vimpulse-add-vi-bindings map
                            vimpulse-core-movement-cmds
                            (not augment)))

(defun vimpulse-inhibit-movement-cmds (map &optional replace)
  "Remap Viper movement commands to `viper-nil' in MAP.
The commands are taken from `vimpulse-viper-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (let (cmd)
    (dolist (cmd vimpulse-viper-movement-cmds)
      (eval `(vimpulse-augment-keymap
              map '(([remap ,cmd] . viper-nil))
              replace)))))

(defun vimpulse-inhibit-destructive-cmds (map &optional replace)
  "Remap destructive Viper commands to `viper-nil' in MAP.
This isn't complete since `viper-command-argument' is left out so
that yanking may work, but as change and delete fail silently in
read-only buffers anyway, it does the job."
  (let (cmd)
    (dolist (cmd '(viper-Append
                   viper-Insert
                   viper-append
                   viper-change-to-eol
                   viper-insert
                   viper-kill-line
                   viper-substitute
                   viper-substitute-line
                   vimpulse-visual-append
                   vimpulse-visual-change
                   vimpulse-visual-insert))
      (eval `(vimpulse-augment-keymap
              map '(([remap ,cmd] . viper-nil))
              replace)))))

;; Buffer-menu
(defcustom vimpulse-want-vi-keys-in-buffmenu t
  "Whether to use vi keys in Buffer menu, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (vimpulse-add-core-movement-cmds Buffer-menu-mode-map)
     (vimpulse-add-movement-cmds Buffer-menu-mode-map)
     (vimpulse-inhibit-destructive-cmds Buffer-menu-mode-map)))

;; Dired
(defcustom vimpulse-want-vi-keys-in-dired t
  "Whether to use vi keys in Dired mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'dired
  '(when vimpulse-want-vi-keys-in-dired
     (vimpulse-add-core-movement-cmds dired-mode-map)
     (vimpulse-add-movement-cmds dired-mode-map)
     (vimpulse-inhibit-destructive-cmds dired-mode-map)))

;; Info
(defcustom vimpulse-want-vi-keys-in-Info t
  "Whether to use vi keys in Info mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (vimpulse-add-core-movement-cmds Info-mode-map)
     (vimpulse-add-movement-cmds Info-mode-map)
     (define-key Info-mode-map "\C-t" 'Info-history-back) ; l
     (define-key Info-mode-map "\M-h" 'Info-help)         ; h
     (define-key Info-mode-map " " 'Info-scroll-up)
     (define-key Info-mode-map [backspace] 'Info-scroll-down)
     (vimpulse-inhibit-destructive-cmds Info-mode-map)))

;; Help
(defcustom vimpulse-want-vi-keys-in-help t
  "Whether to use vi keys in Help mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (vimpulse-add-core-movement-cmds help-mode-map)
     (vimpulse-add-movement-cmds help-mode-map)
     (vimpulse-inhibit-destructive-cmds help-mode-map)))

;; Eldoc compatibility
(eval-after-load 'eldoc
  '(let (cmd)
     (dolist (cmd (append vimpulse-viper-movement-cmds
                          vimpulse-core-movement-cmds))
       (eldoc-add-command cmd))))

;;;;
;;;; Almost all of this code is taken from extended-viper
;;;; coded by Brad Beveridge (bradbev@gmail.com)
;;;; - I changed the prefix of the custom functions to vimpulse
;;;;   to avoid multiple prefixes
;;;;
(defvar vimpulse-fold-level 0)
(defun vimpulse-hs-Open ()
  (interactive)
  (hs-show-block)
  (hs-hide-level -1))
(when (boundp 'hs-minor-mode)
  (add-hook 'hs-minor-mode-hook
            (lambda ()
              (call-interactively 'hs-hide-all)
              (define-key viper-vi-basic-map "za"
                (lambda () (hs-toggle-hiding) (hs-hide-level h)))
              (define-key viper-vi-basic-map "zA" 'hs-toggle-hiding)
              (define-key viper-vi-basic-map "zM" 'hs-hide-all)
              (define-key viper-vi-basic-map "zR" 'hs-show-all)
              (define-key viper-vi-basic-map "zO" 'vimpulse-hs-Open)
              (define-key viper-vi-basic-map "zo" 'hs-show-block)
              (define-key viper-vi-basic-map "zc" 'hs-hide-block))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; VISUAL MODE BINDINGS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key viper-vi-basic-map "v" 'vimpulse-visual-mode)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-mode-linewise)

(define-key viper-vi-basic-map "K" 'woman)
(define-key viper-vi-basic-map "g" nil) ; delete `viper-nil' binding
(define-key viper-vi-basic-map "gd" 'vimpulse-goto-definition)
(define-key viper-vi-basic-map "gf" 'find-file-at-point)
(define-key viper-vi-basic-map "gg" 'vimpulse-goto-first-line)
(define-key viper-vi-basic-map "zb" 'viper-line-to-bottom)
(define-key viper-vi-basic-map "zh" 'scroll-right)
(define-key viper-vi-basic-map "zl" 'scroll-left)
(define-key viper-vi-basic-map "zt" 'viper-line-to-top)
(define-key viper-vi-basic-map "zz" 'viper-line-to-middle)
(define-key viper-vi-basic-map "*" 'vimpulse-search-forward-for-symbol-at-point)
(define-key viper-vi-basic-map "#" 'vimpulse-search-backward-for-symbol-at-point)
(define-key viper-vi-basic-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-basic-map "\C-t" 'pop-tag-mark)
;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-basic-map "u" 'undo)
(define-key viper-vi-basic-map "\C-r" 'redo)

;; Window manipulation
(define-key viper-vi-basic-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-basic-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-ww" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-basic-map "\C-wc" 'delete-window)
(define-key viper-vi-basic-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-basic-map "\C-wv" 'split-window-horizontally)
(add-to-list 'ex-token-alist '("on" (delete-other-windows)))
(add-to-list 'ex-token-alist '("only" (delete-other-windows)))
(add-to-list 'ex-token-alist '("clo" (delete-window)))
(add-to-list 'ex-token-alist '("close" (delete-window)))

(when (fboundp 'windmove-left)
  (define-key viper-vi-basic-map "\C-wh" 'windmove-left)
  (define-key viper-vi-basic-map "\C-wj" 'windmove-down)
  (define-key viper-vi-basic-map "\C-wk" 'windmove-up)
  (define-key viper-vi-basic-map "\C-wl" 'windmove-right))

;;; Insert mode keys
;; Vim-like completion keys
(define-key viper-insert-basic-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-basic-map "\C-n" 'vimpulse-abbrev-expand-after)
;; (define-key viper-insert-basic-map [backspace] 'backward-delete-char-untabify) ; vim doesn't do this!
(define-key viper-insert-basic-map [delete] 'delete-char) ;; delete key
                                        ; make ^[ work
(define-key viper-insert-basic-map (kbd "ESC") 'viper-exit-insert-state)

;;; My code (Alessandro)
(defun vimpulse-indent-lines (count)
  (save-excursion
    (dotimes (i count)
      (indent-according-to-mode)
      (forward-line))))

;;; His code (Brad)
(defun vimpulse-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1))

(defun vimpulse-cycle-windows ()
  "Cycle point to another window."
  (interactive)
  (select-window (next-window)))

(defun vimpulse-search-for-symbol-at-point (forward-p &optional pos force)
  "Search forwards or backwards for the symbol under point.
FORWARD-P specifies the direction, POS the position from where
to start the search."
  (let ((str (thing-at-point 'symbol)))
    ;; If there's no symbol under point, go forwards
    ;; (or backwards) to find one
    (save-excursion
      (while (and (not str) (or (and forward-p (not (eobp)))
                                (and (not forward-p) (not (bobp)))))
        (if forward-p (forward-char) (backward-char))
        (setq str (thing-at-point 'symbol))))
    (when pos (goto-char pos))
    (cond
     ((stringp str)
      (setq str (regexp-quote str))
      (setq str (concat "\\_<" str "\\_>"))
      ;; If searching several times in a row,
      ;; use the same search string each time
      (when (or force
                (string= "" viper-s-string)
                (not (looking-at-p viper-s-string)))
        (setq viper-s-string str))
      (setq viper-s-forward forward-p)
      (viper-search viper-s-string forward-p 1))
     (t
      (error "No string under cursor")))))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t nil t))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point nil nil t))

(defun vimpulse-goto-definition ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t (point-min)))

(defun vimpulse-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

;; C-o/C-i
(viper-deflocalvar
 vimpulse-mark-list nil
 "List of mark positions to jump to with `vimpulse-jump-forward'.
They are stored as markers, the current position first:

    (car vimpulse-mark-list)  = current position (last popped)
    (cdr vimpulse-mark-list)  = future positions (previously popped)
    (cadr vimpulse-mark-list) = next position (to jump to)

In other words, a sort of \"reverse mark ring\": marks which are
popped off the mark ring, are collected here.")

(defadvice set-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defadvice push-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defun vimpulse-jump-backward (arg)
  "Go to older position in jump list.
To go the other way, press \\[vimpulse-jump-forward]."
  (interactive "p")
  (let ((current-pos (make-marker)))
    (unless vimpulse-mark-list
      (move-marker current-pos (point))
      (add-to-list 'vimpulse-mark-list current-pos))
    (dotimes (arg arg)
      (setq current-pos (make-marker))
      ;; Protect `vimpulse-mark-list'
      (let (vimpulse-mark-list)
        (set-mark-command 0))
      (move-marker current-pos (point))
      ;; Already there?
      (unless (= current-pos (car vimpulse-mark-list))
        (setq vimpulse-mark-list
              (cons current-pos vimpulse-mark-list))))))

(defun vimpulse-jump-forward (arg)
  "Go to newer position in jump list.
To go the other way, press \\[vimpulse-jump-backward]."
  (interactive "p")
  (let (current-pos next-pos)
    (dotimes (arg arg)
      (setq current-pos (car vimpulse-mark-list)
            next-pos (cadr vimpulse-mark-list))
      (when next-pos
        ;; Protect `vimpulse-mark-list'
        (let (vimpulse-mark-list)
          (push-mark current-pos t nil))
        (goto-char next-pos)
        (setq vimpulse-mark-list (cdr vimpulse-mark-list))))))

(define-key viper-vi-basic-map "\C-o" 'vimpulse-jump-backward)
(define-key viper-vi-basic-map "\C-i" 'vimpulse-jump-forward)
(global-set-key "\M-o" 'open-line) ; some may miss this command

;; N%
(defadvice viper-paren-match (around vimpulse activate)
  "Go to percentage in the file when ARG >= 10."
  (let ((val (viper-p-val arg)))
    (cond
     ((<= 10 val)
      (goto-char (+ (point-min)
                    (floor (* (- (point-max) (point-min)) 0.01
                              (max 0 (min 100 val))))))
      (beginning-of-line))
     (t
      ad-do-it))))

;; Replace backspace
(defcustom vimpulse-backspace-restore t
  "Whether Backspace restores the original text in Replace mode.
On by default."
  :group 'vimpulse
  :type  'boolean)

(viper-deflocalvar
 vimpulse-replace-alist nil
 "Alist of characters overwritten in Replace mode.
Used by `vimpulse-replace-backspace' to restore text.
The format is (POS . CHAR).")

(defun vimpulse-replace-pre-command ()
  "Remember the character under point."
  (cond
   (viper-replace-minor-mode
    (unless (assq (point) vimpulse-replace-alist)
      (add-to-list 'vimpulse-replace-alist
                   (cons (point) (char-after)))))
   ;; If not in Replace mode, remove itself
   (t
    (remove-hook 'pre-command-hook 'vimpulse-replace-pre-command))))

(add-hook 'viper-replace-state-hook
          (lambda ()
            (setq vimpulse-replace-alist nil)
            (vimpulse-replace-pre-command)
            (add-hook 'pre-command-hook
                      'vimpulse-replace-pre-command)))

(defun vimpulse-replace-backspace ()
  "Restore character under cursor.
If `vimpulse-backspace-restore' is nil,
call `viper-del-backward-char-in-replace' instead."
  (interactive)
  (cond
   (vimpulse-backspace-restore
    (backward-char)
    (let ((oldchar (cdr (assq (point) vimpulse-replace-alist))))
      (when oldchar
        (save-excursion
          (delete-char 1)
          (insert oldchar)))))
   (t
    (viper-del-backward-char-in-replace))))

(defadvice viper-adjust-keys-for (after vimpulse activate)
  "Map <backspace> to `vimpulse-replace-backspace' in Replace mode."
  (define-key viper-replace-map [backspace] 'vimpulse-replace-backspace))

;;; cppjavaperl's code
(defun vimpulse-abbrev-expand-after ()
  (interactive)
  (dabbrev-expand -1))

(provide 'vimpulse-misc-keybindings)

;;; All this code is taken from Brad Beveridge's extended viper.
(defvar vimpulse-extra-ex-commands
  '(
    ("b" "buffer")
    ("bdelete" (vimpulse-kill-current-buffer))
    ("bnext" "next")
    ("syntax" (global-font-lock-mode))
    ("split" (split-window))
    ;; Emacs and Vim use inverted naming conventions for splits.
    ("vsplit" (split-window-horizontally))
    ))


(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))


;;; Manipulation of Vipers functions by using the advice feature
;;; Many of the functions here rely as heavily on Viper's internals as Viper itself
;;; Additional Ex mode features.
;;; ex-token-alist is defined as a constant, but it appears I can safely push values to it!
(defadvice viper-ex (around vimpulse-extended-ex-commands (arg &optional string) activate)
  ad-do-it)

(setq ex-token-alist (append vimpulse-extra-ex-commands ex-token-alist))

(provide 'vimpulse-ex)

;;;;
;;;; This file contains an alternate paren matching function used when
;;;; viper is in vi mode, so that the paren under the cursor is matched,
;;;; instead of the paren before the cursor. This visually makes checking
;;;; parens at the end of the line possible.
;;;;

;;;
;;; paren matching LOADED BY DEFAULT
;;; set vimpulse-enhanced-paren-matching nil in your .emacs before requiring vimpulse to avoid loading it
;;;

;;; Begin Paren Matching Code {{{

;; Do we really need this option?
(defcustom vimpulse-enhanced-paren-matching t
  "Enhanced matching of parentheses, on by default."
  :group 'vimpulse
  :type  'boolean)

(when vimpulse-enhanced-paren-matching
  (require 'paren)
  (show-paren-mode 't) ;; enable the normal paren match highlight

  (defvar vimpulse-paren-overlay-open nil) ;; overlay used to highlight the opening paren
  (defvar vimpulse-paren-overlay-close nil) ;; overlay used to highlight the closing paren
  (make-variable-buffer-local 'vimpulse-paren-overlay-open) ;; overlays are buffer local
  (make-variable-buffer-local 'vimpulse-paren-overlay-close)

  (defun vimpulse-pm-parenp (pos)
    (let ((class (syntax-class (syntax-after pos))))
      (or (= class 4) (= class 5))))
  (defun vimpulse-pm-open-parenp (pos)
    "Returns t if the character at position `pos' is an opening paren."
    (let ((class (syntax-class (syntax-after pos))))
      (= 4 class)))
  (defun vimpulse-pm-close-parenp (pos)
    "Returns t if the character at position `pos' is an closing paren."
    (let ((class (syntax-class (syntax-after pos))))
      (= 5 class)))

  (defun vimpulse-pm-get-candidate-pos (pos)
    "Returns the position of the possible matching paren of the character at position `pos'
if it's a paren, 'not-a-paren if it's not a paren, nil if no match is found."
    (let ((result nil))
      (condition-case ()
          (cond
           ((vimpulse-pm-open-parenp pos)
            (setq result (1- (scan-sexps pos 1))))
           ((vimpulse-pm-close-parenp pos)
            (setq result (scan-sexps (1+ pos) -1)))
           (t
            (setq result 'not-a-paren)))
        (error (setq result nil)))
      result))

  (defun vimpulse-pm-is-real-match (pos1 pos2)
    "Return t if POS1 and POS2 are matching characters.
Checks the characters at position POS1 and POS2 and returns t
if they are matching characters (in a paren match meaning),
nil otherwise."
    (let ((class1 (car (syntax-after pos1)))
          (match1 (cdr (syntax-after pos1)))
          (class2 (car (syntax-after pos2)))
          (match2 (cdr (syntax-after pos2))))
      (or (eq match1 (char-after pos2))
          (eq match2 (char-after pos1))
          (eq match1 match2))))

  (defun vimpulse-pm-highlight-pos (pos face)
    "Highlights the paren at pos `pos' using `face'."
    (let ((ovl (if (vimpulse-pm-open-parenp pos)
                   vimpulse-paren-overlay-open
                 vimpulse-paren-overlay-close)))
      (overlay-put ovl 'face face)
      (move-overlay ovl pos (1+ pos))))

  (defun vimpulse-pm-show-paren ()
    "Paren matching routine. Highlights the paren at (point) and the eventual
matching paren, or mismatched paren." ;; FIXME: this description sucks.
    (let ((candidate-pos (vimpulse-pm-get-candidate-pos (point))))
      (cond
       ((not candidate-pos)
        (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch))
       ((eq candidate-pos 'not-a-paren)
        (delete-overlay vimpulse-paren-overlay-open)
        (delete-overlay vimpulse-paren-overlay-close))
       (t
        (let ((pos-1 (vimpulse-pm-get-candidate-pos candidate-pos)))
          (cond
           ((/= (point) pos-1)
            (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch))
           ((vimpulse-pm-is-real-match candidate-pos pos-1)
            (vimpulse-pm-highlight-pos (point) 'show-paren-match)
            (vimpulse-pm-highlight-pos candidate-pos 'show-paren-match))
           (t
            (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch)
            (vimpulse-pm-highlight-pos candidate-pos 'show-paren-mismatch))))))))

;;;
;;; We advice show-paren-function and use it for insert mode
;;;
;;; TODO: check if using this paren matching function in replace mode is a problem
;;;
  (defadvice show-paren-function (around vimpulse-parenmatching activate)
    (unless vimpulse-paren-overlay-open ;; define overlays if they don't exist
      (setq vimpulse-paren-overlay-open (make-overlay (point) (point)))
      (setq vimpulse-paren-overlay-close (make-overlay (point) (point)))
      (delete-overlay vimpulse-paren-overlay-open)
      (delete-overlay vimpulse-paren-overlay-close))
    (cond
     ((and show-paren-mode viper-mode (not (eq viper-current-state 'insert-state))) ;; viper not in insert mode
      (when (boundp 'show-paren-overlay) ;; we delete the overlays used by show-paren-function
        (delete-overlay show-paren-overlay) ;; and call the custom paren-matching function
        (delete-overlay show-paren-overlay-1))
      (vimpulse-pm-show-paren))
     (t ;; viper in insert mode
      (delete-overlay vimpulse-paren-overlay-open) ;; delete the overlays used by the custom function
      (delete-overlay vimpulse-paren-overlay-close)
      ad-do-it))) ;; call the adviced function

  )
;;; }}} End Paren Matching code

(provide 'vimpulse-paren-matching)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EXPERIMENTAL
;;;; Text Object Support:
;;;; This code implements the support for text objects,
;;;; and implements commands like diw daw ciw caw.
;;;; It's still experimental, and not all that's supported in vim
;;;; is (still) supported here. However, the most common text objects
;;;; are supported:
;;;;    - paren blocks: { [ ( < > ) ] }
;;;;    - sentences
;;;;    - paragraphs
;;;;    - quoted expressions " and '
;;;;    - words and Words
;;;; Using text objects as motions in visual mode is (still) not supported.
;;;; Please note that Vimpulse's text objects are very close to Vim's, but
;;;; the behavior on certain occasions (e.g. daw issued with the cursor
;;;; lying on whitespace) may be a little different. My aim was not acheiving
;;;; the exact same behavior in all limit cases, but rather to give a close
;;;; and consistent behavior to the commands.
;;;; Alessandro Piras
;;; Begin Text Objects code{{{

(defvar vimpulse-last-object-selection nil
  "Last object selection, in list format: (COUNT CHAR MOTION).")

(defun vimpulse-text-object-bounds
  (backward-func forward-func &optional arg pos)
  "Returns the boundaries of one or more text objects.
BACKWARD-FUNC moves point to the object's beginning,
FORWARD-FUNC moves to its end. Schematically,

\(vimpulse-text-object-bounds <beg-of-object> <end-of-object>)

Boundaries are returned as (START END). If specified,
ARG controls the number of objects and POS the starting point
\(`point' by default)."
  (let (beg end)
    (setq arg (or arg 1))
    ;; If ARG is negative, swap BACKWARD-FUNC and FORWARD-FUNC
    (cond
     ((> 0 arg)
      (setq beg backward-func)
      (setq backward-func forward-func)
      (setq forward-func beg))
     ((= 0 arg)
      (setq arg 1)))
    ;; To avoid errors when hitting upon buffer boundaries,
    ;; we make extensive use of `condition-case' ...
    (save-excursion
      (when pos
        (goto-char pos))
      ;; We might already be at the ending character --
      ;; go one character back so we don't run past it.
      (condition-case nil
          (if (> 0 arg) (forward-char)
            (backward-char))
        (error nil))
      (condition-case nil
          (funcall forward-func 1)
        (error nil))
      (condition-case nil
          (funcall backward-func 1)
        (error nil))
      (setq beg (point))
      (condition-case nil
          (funcall forward-func (abs arg))
        (error nil))
      (setq end (point)))
    (sort (list beg end) '<)))

(defun vimpulse-get-syntaxes-bounds (pos syntaxes)
  "Returns the bounds of contiguous character that match SYNTAXES,
where syntaxes is an Emacs' syntax specification."
  (let ((result))
    (save-excursion
      (goto-char pos)
      (skip-syntax-forward syntaxes)
      (add-to-list 'result (1- (point)))
      (skip-syntax-backward syntaxes)
      (cons (point) result))))

(defvar vimpulse-paren-matching-table
  (make-hash-table)
  "Table used for paren matching:
table[key] = (match . opening-paren)"
  )
(puthash ?\(
         '( ?\) . ?\( )
         vimpulse-paren-matching-table)
(puthash ?\)
         '( ?\( . ?\( )
         vimpulse-paren-matching-table)
(puthash ?{
         '( ?} . ?\{ )
         vimpulse-paren-matching-table)
(puthash ?}
         '( ?{ . ?\{ )
         vimpulse-paren-matching-table)
(puthash ?\[
         '( ?\] . ?\[)
         vimpulse-paren-matching-table)
(puthash ?\]
         '( ?\[ . ?\[ )
         vimpulse-paren-matching-table)
(puthash ?\<
         '( ?\> . ?\< )
         vimpulse-paren-matching-table)
(puthash ?\>
         '( ?\< . ?\< )
         vimpulse-paren-matching-table)

(defun vimpulse-skip-until-delimiters (pos paren match limb lime dir)
  "Skips all the character different from PAREN and MATCH starting
from POS following the direction DIR, with POS in [LIMB, LIME]."
  (let ((pos-1 pos))
    (while (and (/= (char-after pos-1) paren)
                (/= (char-after pos-1) match)
                (or (and (= dir -1) (/= pos-1 limb)) ;; reached limits
                    (and (= dir 1) (/= pos-1 lime))))
      (setq pos-1 (+ dir pos-1)))
    pos-1))

(defun vimpulse-find-first-unbalanced-1 (pos paren match limb lime dir)
  "Finds the first unbalanced PAREN following the direction DIR, starting
from position POS. MATCH is the paren that matches with PAREN, LIMB is the
lower bound of the position, LIME is the upper bound to the position."
  (cond
   ((or (eq pos 'not-found))
    'not-found)
   ((= (char-after pos) paren)
    pos)
   ((or (and (= dir -1) (= pos limb)) ;; reached limits
        (and (= dir 1) (= pos lime)))
    'not-found)
   ((= (char-after pos) match) ;;
    (let ((pos-1 (vimpulse-find-first-unbalanced-1 (+ dir pos) paren match limb lime dir)))
      (vimpulse-find-first-unbalanced-1 (+ dir pos-1) paren match limb lime dir)))
   (t
    (let ((pos-1 (vimpulse-skip-until-delimiters pos paren match limb lime dir)))
      (vimpulse-find-first-unbalanced-1 pos-1 paren match limb lime dir)))))

(defvar vimpulse-balanced-bounds-char-list
  (list
   ?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>)
  "Parens supported by the text-object system.")

(defun vimpulse-get-balanced-bounds (pos paren)
  "Returns the boundaries of a balanced expression."
  (let* ((limb (point-min))
         (lime (1- (point-max)))
         (paren-o (cdr (gethash paren vimpulse-paren-matching-table)))
         (paren-c (car (gethash paren-o vimpulse-paren-matching-table)))
         (pos-o (vimpulse-find-first-unbalanced-1 pos paren-o paren-c limb lime -1))
         (pos-c (vimpulse-find-first-unbalanced-1 (if (integerp pos-o) (1+ pos-o) pos-o) paren-c paren-o limb lime 1)))
    (cond
     ((eq pos-c 'not-found)
      nil)
     (t
      (list pos-o pos-c)))))

(defun vimpulse-get-vword-bounds (pos)
  "Returns the boundaries of a word."
  (let (syntax)
    (unless (eobp)
      (setq syntax (char-syntax (char-after pos))))
    (cond
     ((eq syntax ?\))
      (vimpulse-get-syntaxes-bounds pos (string syntax)))
     ((eq syntax ?\()
      (vimpulse-get-syntaxes-bounds pos (string syntax)))
     (t
      (save-excursion
        (goto-char pos)
        (vimpulse-text-object-bounds
         (lambda (arg)
           (forward-char)
           (viper-backward-word arg))
         (lambda (arg)
           (unless (viper-end-of-word-p)
             (viper-end-of-word arg)))))))))

(defun vimpulse-get-vWord-bounds (pos)
  "Returns the boundaries of a Word."
  (save-excursion
    (goto-char pos)
    (vimpulse-text-object-bounds
     (lambda (arg)
       (forward-char)
       (viper-backward-Word arg))
     (lambda (arg)
       (unless (looking-at "[[:space:]]"))
       (viper-end-of-Word arg)))))

(defun vimpulse-get-sentence-bounds (pos)
  "Returns the boundaries of a sentence."
  (save-excursion
    (goto-char pos)
    (vimpulse-text-object-bounds
     (lambda (arg)
       (viper-backward-sentence arg)
       (when (looking-at "[[:space:]]*$")
         (forward-char)))
     (lambda (arg)
       (viper-forward-sentence arg)
       (backward-char)))))

(defun vimpulse-get-paragraph-bounds (pos)
  "Returns the boundaries of a paragraph."
  (save-excursion
    (goto-char pos)
    (vimpulse-text-object-bounds
     (lambda (arg)
       (viper-backward-paragraph arg)
       (unless (bobp) (forward-char)))
     (lambda (arg)
       (viper-forward-paragraph arg)
       (backward-char)))))

(defun vimpulse-get-paired-bounds (pos char)
  "Returns the boundaries of a CHAR-quoted expression."
  (save-excursion
    (goto-char pos)
    (if (= (char-before (point)) ?\\) (backward-char))
    (let ((result))
      (when (re-search-forward (concat "[^\\\\]" (string char)) (point-max) t)
        (add-to-list 'result (1- (point)))
        (condition-case ()
            (add-to-list 'result (scan-sexps (point) -1))
          (error (setq result nil))))
      result)))

(defvar vimpulse-paired-expression-delimiters (list ?\" ?\')
  "Quotes supported by the text-object system.")

(defun vimpulse-get-text-object-bounds-i (pos motion)
  "Returns the inner boundaries of a text object at point POS.
MOTION identifies the text object:
  - w -> word
  - W -> Word
  - s -> sentence
  - p -> paragraph
  - <paren> -> paren block (see variable `vimpulse-paren-matching-table'
               to see the supported parens).
  - <quote> -> quoted expression (see variable `paired-expression-delimiter'
               to see the type of quotes supported)."
  (cond
   ((= motion ?w) (vimpulse-get-vword-bounds pos))
   ((= motion ?W) (vimpulse-get-vWord-bounds pos))
   ((= motion ?s) (vimpulse-get-sentence-bounds pos))
   ((= motion ?p) (vimpulse-get-paragraph-bounds pos))
   ((memq motion vimpulse-paired-expression-delimiters)
    (let ((bounds (vimpulse-get-paired-bounds pos motion)))
      (when bounds
        (let ((s (car bounds)) (e (cadr bounds)))
          (list (1+ s) (1- e))))))
   ((or (= motion ?b) (= motion ?B)
        (memq motion vimpulse-balanced-bounds-char-list))
    (when (= motion ?b) (setq motion ?\())
    (when (= motion ?B) (setq motion ?\{))
    (let ((bounds (vimpulse-get-balanced-bounds pos motion)))
      (when bounds
        (let ((s (car bounds)) (e (cadr bounds)))
          (list (1+ s) (1- e))))))))

(defun vimpulse-get-bounds-with-whitespace (func pos &optional trailing-newlines)
  "Given a function that returns inner boundaries, returns a boundary that includes
the whitespace needed to get the \"a\" behavior. The logic
followed is the same:
  - include all whitespace and newlines before the text object
  - include the text object
  - include trailing whitespace
  - if trailing-newlines is t, include also the trailing newlines"
  (save-excursion
    (goto-char pos)
    (let ((start (point))
          (end nil))
      (skip-chars-forward "[:blank:]\n\r")
      (let ((bounds (apply func  (list (point)))))
        (cond
         (bounds
          (goto-char (1+ (cadr bounds)))
          (skip-chars-forward (if trailing-newlines "[:blank:]\n\r" "[:blank:]"))
          (list (min start (car bounds)) (1- (point))))
         (t nil))))))

(defun vimpulse-get-text-object-bounds-a (pos motion)
  "Returns the boundaries of \"a\" text object, including whitespace."
  (cond
   ((= motion ?w)
    (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vword-bounds pos))
   ((= motion ?W) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vWord-bounds pos))
   ((= motion ?s) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-sentence-bounds pos t))
   ((= motion ?p) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-paragraph-bounds pos t))
   ((= motion ?b)
    (setq motion ?\()
    (vimpulse-get-balanced-bounds pos motion))
   ((= motion ?B)
    (setq motion ?\{)
    (vimpulse-get-balanced-bounds pos motion))
   ((memq motion vimpulse-paired-expression-delimiters)
    (vimpulse-get-paired-bounds pos motion))
   ((memq motion vimpulse-balanced-bounds-char-list)
    (vimpulse-get-balanced-bounds pos motion))))

(defun vimpulse-get-text-object-bounds (pos char motion)
  "Returns the boundaries of a text object. 'pos' indicates the start position,
char indicates 'inner' (?i) or 'a' (?a) behavior, 'motion' indicates the text-object."
  (cond
   ((= char ?a) (vimpulse-get-text-object-bounds-a pos motion))
   ((= char ?i) (vimpulse-get-text-object-bounds-i pos motion))
   ((= char ?r) (list pos (+ pos (- (cadr motion) (car motion) 1))))
   ((= char ?l) (vimpulse-get-line-margins pos))
   (t (error "called with wrong arguments"))))

(defun vimpulse-message-all-args (&rest args)
  "Helper function that prints all its arguments, plus some other values."
  (message "ARGS: %s, reg: %s" args (string viper-use-register)))

(defun vimpulse-test-function (value)
  "This function is only defined for developing purposes."
  (viper-set-destructive-command (list 'vimpulse-message-all-args 'first-argument ?d viper-use-register "cane" nil)))

;;;;;;;;;;;;;;;;;;;;
;;;   Commands   ;;;
;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-unify-multiple-bounds (pos char count motion)
  "Returns the boundaries of a multiple text object motion.
POS is the starting position,
CHAR indicates 'inner' or 'a' behavior,
COUNT indicates how many text objects to include,
MOTION indicates the kind of text object."
  (let* ((bounds-1 (vimpulse-get-text-object-bounds pos char motion))
         (start (when bounds-1 (car bounds-1)))
         (end (when bounds-1 (cadr bounds-1))))
    (dotimes (i (1- count))
      (setq end (cadr (vimpulse-get-text-object-bounds (1+ end) char motion))))
    (if end (list start end) nil)))

(defun vimpulse-delete-text-objects-function (arg)
  "Deletes COUNT text objects of MOTION kind starting from `point',
following the behavior indicated by CHAR: ?i stands for \"inner\",
?a stands for \"a\". ARG has the form ((COUNT CHAR MOTION) . ?d)."
  (let* ((count  (nth 0 (car arg)))
         (char   (nth 1 (car arg)))
         (motion (nth 2 (car arg)))
         (bounds (vimpulse-unify-multiple-bounds
                  (point) char count motion)))
    (when bounds
      (when viper-use-register          ; copy stuff to registers
        ;; This code is taken from `viper-exec-delete'
        (cond
         ((viper-valid-register viper-use-register '(letter digit))
          (copy-to-register
           viper-use-register (car bounds) (1+ (cadr bounds)) nil))
         ((viper-valid-register viper-use-register '(Letter))
          (viper-append-to-register
           (downcase viper-use-register) (car bounds) (1+ (cadr bounds))))
         (t (setq viper-use-register nil)
            (error viper-InvalidRegister viper-use-register)))
        (setq viper-use-register nil))
      ;; End of `viper-exec-delete' code
      (goto-char (car bounds))
      (set-mark (1+ (cadr bounds)))
      (kill-region (car bounds) (1+ (cadr bounds))))))

(defun vimpulse-delete-text-objects-command (count char)
  "Deletes COUNT text objects following the behavior CHAR ('inner' or 'a').
The user is queried for the type of object with `read-char'."
  (interactive)
  (let ((motion (read-char)))
    (viper-set-destructive-command (list 'vimpulse-delete-text-objects-function
                                         (list count char motion) ?d viper-use-register nil nil))
    (vimpulse-delete-text-objects-function (cons (list count char motion) ?d))))

(defun vimpulse-change-text-objects-function (arg)
  "Executes `vimpulse-delete-text-objects-function' passing ARG to it and yanks the last insertion."
  (vimpulse-delete-text-objects-function arg)
  (viper-yank-last-insertion))

(defun vimpulse-change-text-objects-command (count char)
  "Changes COUNT text objects following the behavior CHAR (\"inner\" or \"a\").
The kind of text object is asked interactively to the user using `read-char'."
  (interactive)
  (let ((motion (read-char)))
    (viper-set-destructive-command (list 'vimpulse-change-text-objects-function (list count char motion)
                                         ?c viper-use-register nil nil))
    (vimpulse-delete-text-objects-function (cons (list count char motion) ?c))
    (viper-change-state-to-insert)))

(defun vimpulse-yank-text-objects-function (arg)
  "Yanks COUNT text objects of MOTION kind starting from `point',
following the behavior indicated by CHAR: ?i stands for \"inner\",
?a stands for \"a\". ARG has the form ((COUNT CHAR MOTION) . ?d)."
  (let* ((count  (nth 0 (car arg)))
         (char   (nth 1 (car arg)))
         (motion (nth 2 (car arg)))
         (bounds (vimpulse-unify-multiple-bounds
                  (point) char count motion)))
    (when bounds
      (when viper-use-register        ; copy stuff to registers
        ;; This code is taken from `viper-exec-delete'
        (cond
         ((viper-valid-register viper-use-register '(letter digit))
          (copy-to-register
           viper-use-register (car bounds) (1+ (cadr bounds)) nil))
         ((viper-valid-register viper-use-register '(Letter))
          (viper-append-to-register
           (downcase viper-use-register) (car bounds) (1+ (cadr bounds))))
         (t (setq viper-use-register nil)
            (error viper-InvalidRegister viper-use-register)))
        (setq viper-use-register nil))
      ;; End of `viper-exec-delete' code
      (copy-region-as-kill (car bounds) (1+ (cadr bounds)))
      (goto-char (car bounds)))))

(defun vimpulse-yank-text-objects-command (count char)
  "Yanks COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
  (interactive)
  (let ((motion (read-char)))
    (vimpulse-yank-text-objects-function (cons (list count char motion) ?y))))
;; This is for silencing viper when he checks if the insertion must be repeated, never true for
;; this kind of commands.
(defvar vimpulse-text-objects-command (list 'vimpulse-delete-text-objects-function
                                            'vimpulse-change-text-objects-function
                                            'vimpulse-yank-text-objects-function))
(defadvice viper-repeat-insert-command (around vimpulse-text-objects-repeat-insert-command-fix activate)
  (when (not (memq (car viper-d-com) vimpulse-text-objects-command))
    ad-do-it))

;;; }}} End Text Objects code

(provide 'vimpulse-text-object-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This file contains all the code relative to Visual mode.  ;;;;;
;;;; Visual mode is implemented as a minor mode.               ;;;;;
;;;; Currently, Visual selection highlighting is done through  ;;;;;
;;;; the use of overlays for linewise and characterwise modes, ;;;;;
;;;; while for blockwise mode, rect-mark.el is needed (see     ;;;;;
;;;; installation instructions).                               ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Minor Mode code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup vimpulse-visual nil
  "visual-mode for viper"
  :prefix "vimpulse-visual-"
  :group  'emulations)

(defcustom vimpulse-visual-basic-map (make-sparse-keymap)
  "Visual mode keymap.
This keymap is active when in visual mode."
  :type  'keymap
  :group 'vimpulse-visual)

(define-minor-mode vimpulse-visual-mode
  "Toggles visual mode in viper"
  :initial-value nil
  :keymap vimpulse-visual-basic-map
  :global nil
  :group 'vimpulse-visual
  (cond
   (vimpulse-visual-mode
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-activate 'normal)))
   (t
    ;; This is executed when we do (vimpulse-visual-mode -1).
    ;; It must work even if Visual mode is not active.
    (vimpulse-visual-highlight -1)
    ;; Deactivate mark
    (viper-deactivate-mark)
    ;; Clean up local variables
    (mapcar (lambda (var)
              (when (assq var vimpulse-visual-vars-alist)
                (set var (cdr (assq var vimpulse-visual-vars-alist))))
              (when (memq var vimpulse-visual-global-vars)
                (kill-local-variable var)))
            vimpulse-visual-local-vars)
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    ;; Remove hooks
    (remove-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (remove-hook 'post-command-hook 'vimpulse-visual-post-command)
    (remove-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook)
    ;; If Viper state is not already changed,
    ;; change it to vi state
    (when (eq viper-current-state 'visual-state)
      (cond
       ((eq 'emacs-state vimpulse-visual-previous-state)
        (viper-change-state-to-emacs))
       (t
        (viper-change-state-to-vi))))
    (kill-local-variable 'vimpulse-visual-previous-state))))

;; These become minor modes when `vimpulse-add-visual-maps-macro'
;; is called below
(viper-deflocalvar
 vimpulse-visual-state-modifier-minor-mode nil
 "For making major mode-specific modifications to Visual state.")

(viper-deflocalvar
 vimpulse-visual-global-user-minor-mode nil
 "For user-defined global bindings in Visual state.")

(defvar vimpulse-visual-global-user-map (make-sparse-keymap)
  "Auxiliary map for user-defined bindings in Visual state.")

(defvar vimpulse-visual-state-modifier-alist nil)

(defvar vimpulse-visual-state-id "<VIS> "
  "Mode line tag for identifying Visual state.")

(defun vimpulse-region-face ()
  "Return face of region."
  (if (featurep 'xemacs) 'highlight 'region))

(defvar vimpulse-visual-mode nil
  "Current visual mode: may be nil, `normal', `line' or `block'.")

(defcustom vimpulse-visual-load-hook nil
  "Hooks to run after loading vimpulse-visual-mode."
  :type 'hook
  :group 'vimpulse-visual)

(defcustom vimpulse-visual-mode-hook nil
  "This hook is run whenever vimpulse-visual-mode is toggled."
  :type 'hook
  :group 'vimpulse-visual)

(viper-deflocalvar
 vimpulse-visual-global-vars nil
 "List of variables which were global.")

(viper-deflocalvar
 vimpulse-visual-local-vars
 '(cua-mode
   transient-mark-mode
   vimpulse-visual-region-changed)
 "System variables which are reset for each Visual session.")

(viper-deflocalvar
 vimpulse-visual-vars-alist nil
 "Alist of old variable values.")

(viper-deflocalvar
 vimpulse-visual-last nil
 "Last active Visual mode: may be nil, `normal', `line' or `block'.")

(viper-deflocalvar
 vimpulse-visual-previous-state 'viper-state
 "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar
 vimpulse-visual-region-changed nil
 "Whether region is expanded to Visual selection.")

(viper-deflocalvar
 vimpulse-visual-point nil
 "Last value of `point' in Visual mode.")

(viper-deflocalvar
 vimpulse-visual-mark nil
 "Last value of `mark' in Visual mode.")

(defun vimpulse-modifier-map (state &optional mode)
  "Return the current major mode modifier map for STATE.
If none, return an empty keymap (`viper-empty-keymap')."
  (setq mode (or mode major-mode))
  (setq state
        (cond
         ((eq state 'vi-state)
          viper-vi-state-modifier-alist)
         ((eq state 'insert-state)
          viper-insert-state-modifier-alist)
         ((eq state 'emacs-state)
          viper-emacs-state-modifier-alist)
         ((eq state 'visual-state)
          vimpulse-visual-state-modifier-alist)))
  (if (keymapp (cdr (assoc mode state)))
      (cdr (assoc mode state))
    viper-empty-keymap))

;; Adding Visual state maps. The advice for this gets somewhat
;; elaborate because Viper insists on making `minor-mode-map-alist'
;; buffer-local in XEmacs, so we need to set both the default value
;; and the local value.
(defmacro vimpulse-add-visual-maps-macro (keymaps)
  `(defadvice viper-normalize-minor-mode-map-alist
     (after ,keymaps activate)
     ,(format "Modifies `%s' to include visual keymaps." keymaps)
     (let (mode temp)
       (dolist (mode (list
                      (cons 'vimpulse-visual-mode
                            vimpulse-visual-basic-map)
                      (cons 'vimpulse-visual-state-modifier-minor-mode
                            (vimpulse-modifier-map 'visual-state))
                      (cons 'vimpulse-visual-global-user-minor-mode
                            vimpulse-visual-global-user-map)))
         (setq temp (default-value ',keymaps))
         (setq temp (assq-delete-all (car mode) temp)) ; already there?
         (add-to-list 'temp mode)
         (setq-default ,keymaps temp)
         (setq temp ,keymaps)
         (setq temp (assq-delete-all (car mode) temp))
         (add-to-list 'temp mode)
         (setq ,keymaps temp)))))

(cond
 ((featurep 'xemacs)
  (vimpulse-add-visual-maps-macro viper--key-maps)
  (vimpulse-add-visual-maps-macro minor-mode-map-alist))
 ((>= emacs-major-version 22)
  (vimpulse-add-visual-maps-macro viper--key-maps))
 (t
  (vimpulse-add-visual-maps-macro minor-mode-map-alist)))

(viper-normalize-minor-mode-map-alist)

(defadvice viper-refresh-mode-line (after vimpulse-states activate)
  "Add mode line tag for Visual state."
  (when (eq viper-current-state 'visual-state)
    (set (make-local-variable 'viper-mode-string)
         vimpulse-visual-state-id)
    (force-mode-line-update)))

(defadvice viper-change-state (around vimpulse-states activate)
  "Toggle Visual mode."
  (and (eq 'visual-state viper-current-state)
       (eq 'insert-state new-state)
       (viper-move-marker-locally 'viper-insert-point (point)))
  ad-do-it
  (cond
   ((eq 'visual-state new-state)
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-mode 1)))
   (t
    (vimpulse-visual-mode -1)))
  (viper-normalize-minor-mode-map-alist))

(defadvice viper-set-mode-vars-for (after vimpulse-states activate)
  "Activate minor modes for Visual state."
  (cond
   ((eq state 'visual-state)
    (setq vimpulse-visual-mode (or vimpulse-visual-mode t)
          vimpulse-visual-global-user-minor-mode t
          vimpulse-visual-state-modifier-minor-mode t
          ;; The rest is vi (command) maps
          viper-vi-intercept-minor-mode t
          viper-vi-minibuffer-minor-mode
          (viper-is-in-minibuffer)
          viper-vi-local-user-minor-mode t
          viper-vi-kbd-minor-mode
          (not (viper-is-in-minibuffer))
          viper-vi-global-user-minor-mode t
          viper-vi-state-modifier-minor-mode t
          viper-vi-diehard-minor-mode
          (not
           (or viper-want-emacs-keys-in-vi
               (viper-is-in-minibuffer)))
          viper-vi-basic-minor-mode t
          viper-emacs-intercept-minor-mode nil
          viper-emacs-local-user-minor-mode nil
          viper-emacs-kbd-minor-mode nil
          viper-emacs-global-user-minor-mode nil
          viper-emacs-state-modifier-minor-mode nil))
   (t
    (setq vimpulse-visual-mode nil
          vimpulse-visual-global-user-minor-mode nil
          vimpulse-visual-state-modifier-minor-mode nil))))

(defadvice viper-modify-major-mode (after vimpulse-visual activate)
  "Modify Visual state."
  (when (eq state 'visual-state)
    (let ((alist 'vimpulse-visual-state-modifier-alist) elt)
      (when (setq elt (assoc mode (eval alist)))
        (set alist (delq elt (eval alist))))
      (set alist (cons (cons mode keymap) (eval alist)))
      (viper-normalize-minor-mode-map-alist)
      (viper-set-mode-vars-for viper-current-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions related to visual selection activation, ;;;
;;; mode of operation change (character-wise,         ;;;
;;; line-wise, block-wise)                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-activate (&optional mode)
  "Activate Visual mode. MODE is `normal', `line' or `block'.
May also be used to change the Visual mode."
  (unless (memq vimpulse-visual-mode '(normal line block))
    ;; We are activating Visual mode for the first time
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    (setq vimpulse-visual-previous-state viper-current-state)
    ;; Make global variables buffer-local
    (setq vimpulse-visual-vars-alist nil)
    (mapcar (lambda (var)
              (add-to-list 'vimpulse-visual-vars-alist
                           (cons var (eval var)))
              (unless (assoc var (buffer-local-variables))
                (make-local-variable var)
                (add-to-list 'vimpulse-visual-global-vars var)))
            vimpulse-visual-local-vars)
    ;; Add hooks
    (add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook)
    ;; Activate mark at point
    (cond
     ((eq 'block mode)
      (set-mark (point))
      (viper-deactivate-mark)          ; `set-mark' activates the mark
      (vimpulse-transient-mark -1))
     (t
      (vimpulse-transient-mark 1)
      ;; Convert active Emacs region to Visual selection, if any.
      ;; To avoid confusion, do not move point, even if this means the
      ;; selection increases by one character when mark is before
      ;; point.
      (if mark-active
          (when (< (point) (mark t))
            (vimpulse-visual-contract-region))
        (vimpulse-activate-mark (point))))))
  ;; Set the Visual mode
  (setq mode (or mode 'normal))
  (setq vimpulse-visual-mode mode
        vimpulse-visual-last mode)
  (viper-change-state 'visual-state)
  (viper-restore-cursor-type)           ; use vi cursor
  ;; Reactivate mark
  (cond
   ((eq 'block mode)
    (viper-deactivate-mark)
    (vimpulse-transient-mark -1))
   (t
    (vimpulse-transient-mark 1)
    (vimpulse-activate-mark))))

(defalias 'vimpulse-change-state-to-visual 'vimpulse-visual-activate)

(defun vimpulse-visual-toggle (mode)
  "Activates visual-mode MODE if this is not the current visual mode.
Otherwise deactivates visual mode."
  (if (eq vimpulse-visual-mode mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-activate-normal ()
  "Activates normal visual mode."
  (interactive)
  (vimpulse-visual-activate 'normal)
  (message "-- VISUAL --"))

(defun vimpulse-visual-activate-line ()
  "Activates linewise visual mode."
  (interactive)
  (vimpulse-visual-activate 'line)
  (message "-- VISUAL LINE --"))

(defun vimpulse-visual-activate-block ()
  "Activates block visual mode."
  (interactive)
  (vimpulse-visual-activate 'block)
  (message "-- VISUAL BLOCK --"))

(defun vimpulse-visual-toggle-normal ()
  "Activates normal visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'normal)
  (when vimpulse-visual-mode
    (message "-- VISUAL --")))

(defun vimpulse-visual-toggle-line ()
  "Activates linewise visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'line)
  (when vimpulse-visual-mode
    (message "-- VISUAL LINE --")))

(defun vimpulse-visual-toggle-block ()
  "Activates block visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'block)
  (when vimpulse-visual-mode
    (message "-- VISUAL BLOCK --")))

;;;;;;;;;;;;;
;;; Lists ;;;
;;;;;;;;;;;;;

(defvar vimpulse-movement-cmds
  '(backward-char backward-list backward-paragraph backward-sentence
    backward-sexp backward-up-list backward-word beginning-of-buffer
    beginning-of-defun beginning-of-line beginning-of-visual-line
    down-list end-of-buffer end-of-defun end-of-line
    end-of-visual-line exchange-point-and-mark
    forward-char forward-list forward-paragraph
    forward-sentence forward-sexp forward-word move-beginning-of-line
    move-end-of-line next-line previous-line up-list
    vimpulse-goto-first-line viper-backward-Word viper-backward-char
    viper-backward-paragraph viper-backward-sentence
    viper-backward-word viper-beginning-of-line viper-end-of-Word
    viper-end-of-word viper-find-char-backward viper-find-char-forward
    viper-forward-Word viper-forward-char viper-forward-paragraph
    viper-forward-sentence viper-forward-word viper-goto-char-backward
    viper-goto-eol viper-goto-char-forward viper-goto-line
    viper-line-to-bottom viper-line-to-middle viper-line-to-top
    viper-next-line viper-previous-line viper-search-backward
    viper-search-forward viper-search-Next viper-search-next
    viper-window-bottom viper-window-middle viper-window-top
    vimpulse-visual-exchange-corners vimpulse-visual-select-text-object)
  "List of commands that move point.
If a command is listed here, or in `vimpulse-boundaries-cmds', or in
`vimpulse-misc-cmds', the region is not expanded to the visual selection
before executing it.")

(defvar vimpulse-boundaries-cmds
  '(mark-defun mark-end-of-sentence
    mark-paragraph mark-sexp mark-word)
  "List of commands that change boundaries of region.
If a command is listed here, or in `vimpulse-movement-cmds', or in
`vimpulse-misc-cmds', the region is not expanded to the visual selection
before executing it. It may, however, be adjusted afterwards.")

(defvar vimpulse-misc-cmds
  '(cua-cancel keyboard-quit scroll-down scroll-up undo
    viper-exec-mapped-kbd-macro viper-insert
    viper-intercept-ESC-key
    vimpulse-visual-toggle-normal
    vimpulse-visual-toggle-line
    vimpulse-visual-toggle-block
    vimpulse-visual-restore)
  "List of miscellaneous commands not acting on region.
If a command is listed here, or in `vimpulse-movement-cmds', or in
`vimpulse-boundaries-cmds', the region is not expanded to the visual selection
before executing it.")

(defun vimpulse-movement-cmd-p (command)
  "Whether COMMAND is a \"movement\" command.
That is, whether it is listed in `vimpulse-movement-cmds'."
  ;; We use `member' rather than `memq' to allow lambdas
  (member command vimpulse-movement-cmds))

(defun vimpulse-boundaries-cmd-p (command)
  "Whether COMMAND is a \"boundaries\" command.
 That is, whether it is listed in `vimpulse-boundaries-cmds'."
  (member command vimpulse-boundaries-cmds))

(defun vimpulse-misc-cmd-p (command)
  "Whether COMMAND is a \"misc\" command.
 That is, whether it is listed in `vimpulse-misc-cmds'."
  (member command vimpulse-misc-cmds))

(defun vimpulse-region-cmd-p (command)
  "Whether COMMAND may be acting on the contents of region."
  (and (not (vimpulse-movement-cmd-p command))
       (not (vimpulse-boundaries-cmd-p command))
       (not (vimpulse-misc-cmd-p command))))

;;;;;;;;;;;;;;;;;;;;
;;; Key bindings ;;;
;;;;;;;;;;;;;;;;;;;;

(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "\C-p" 'yank-rectangle)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-restore)

(define-key vimpulse-visual-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key vimpulse-visual-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key vimpulse-visual-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key vimpulse-visual-basic-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "x" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "D" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "y" 'vimpulse-visual-yank)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-basic-map "R" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-basic-map "c" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "C" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "s" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "S" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "\"" 'vimpulse-visual-set-current-register)
(define-key vimpulse-visual-basic-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-basic-map "O" 'vimpulse-visual-exchange-corners)
(define-key vimpulse-visual-basic-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-basic-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-basic-map "U" 'vimpulse-visual-make-upcase)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-visual-make-downcase)
(define-key vimpulse-visual-basic-map "~" 'vimpulse-visual-toggle-case)
(define-key vimpulse-visual-basic-map "J" 'vimpulse-visual-join)
(define-key vimpulse-visual-basic-map "<" 'vimpulse-visual-shift-left)
(define-key vimpulse-visual-basic-map ">" 'vimpulse-visual-shift-right)
(define-key vimpulse-visual-basic-map "=" 'indent-region)
(define-key vimpulse-visual-basic-map "a" 'vimpulse-visual-select-text-object)
(define-key vimpulse-visual-basic-map "i" 'vimpulse-visual-select-text-object)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-basic-map "." 'undefined)

;; Advice viper-intercept-ESC-key to exit visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit Visual mode with ESC."
  (if (and vimpulse-visual-mode
           (not (input-pending-p)))
      (vimpulse-visual-mode -1)
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual selection visualization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(viper-deflocalvar
 vimpulse-visual-overlay nil
 "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar
 vimpulse-visual-block-overlays nil
 "Overlays for Visual Block selection.")

;; Set functions for handling overlays (not yet provided by Viper)
(cond
 ((featurep 'xemacs)                    ; XEmacs
  (fset 'vimpulse-delete-overlay 'delete-extent)
  (fset 'vimpulse-mark-active 'region-exists-p))
 (t                                     ; GNU Emacs
  (fset 'vimpulse-delete-overlay 'delete-overlay)
  (fset 'vimpulse-mark-active (lambda () mark-active))))

;; Complement to `viper-deactivate-mark'
(defun vimpulse-activate-mark (&optional pos)
  "Activate mark if there is one. Otherwise set mark at point.
If POS if specified, set mark at POS instead."
  (setq pos (or pos (mark t) (point)))
  (cond
   ((and (boundp 'cua-mode) cua-mode)
    (let ((opoint (point))
          cua-toggle-set-mark)
      (goto-char (or pos (mark t) (point)))
      (cua-set-mark)
      (goto-char opoint)))
   (t
    (let (this-command)
      (push-mark pos t t)))))

(defun vimpulse-transient-mark (&optional arg)
  "Enable Transient Mark mode (and Cua mode).
Disable with negative ARG."
  (let (deactivate-mark)
    (cond
     ;; Disable Transient Mark/Cua
     ((and (integerp arg) (> 1 arg))
      (and (boundp 'cua-mode)
           cua-mode
           (cua-mode -1))
      (and (boundp 'transient-mark-mode)
           transient-mark-mode
           (transient-mark-mode -1)))
     ;; Enable Transient Mark/Cua
     ((and (boundp 'cua-mode)
           (not cua-mode))
      (cua-mode 1))
     ((and (boundp 'transient-mark-mode)
           (not transient-mark-mode))
      (transient-mark-mode 1)))))

(defun vimpulse-visual-beginning (&optional mode)
  "Return beginning of Visual selection,
based on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE,
which must be one of `normal', `line' and `block'.

In Normal mode, return beginning of region.
In Line mode, return beginning of first line.
In Block mode, return upper left corner of rectangle.

See also `vimpulse-visual-end'."
  (setq mode (or mode vimpulse-visual-mode))
  (cond
   ;; Upper left corner of block selection
   ((eq 'block mode)
    (let* ((start (min (point) (or (mark t) 1)))
           (end   (max (point) (or (mark t) 1)))
           (start-col (save-excursion
                        (goto-char start)
                        (current-column)))
           (end-col   (save-excursion
                        (goto-char end)
                        (current-column))))
      (if (<= start-col end-col)
          start
        (save-excursion
          (goto-char start)
          (condition-case nil
              (move-to-column end-col)
            (error nil))
          (point)))))
   ;; Beginning of first line
   ((eq 'line mode)
    (cond
     ((not (mark t))
      (line-beginning-position))
     (t
      (save-excursion
        (goto-char (min (point) (mark t)))
        (line-beginning-position)))))
   ;; Beginning of region
   (t
    (min (point) (or (mark t) 1)))))

(defun vimpulse-visual-end (&optional mode)
  "Return end of Visual selection,
based on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE,
which must be one of `normal', `line' and `block'.

In Normal mode, return end of region plus one character.
In Line mode, return end of last line, including newline.
In Block mode, return lower right corner of rectangle.

See also `vimpulse-visual-beginning'."
  (setq mode (or mode vimpulse-visual-mode))
  (cond
   ((eq 'block mode)
    ;; Lower right corner of block selection
    (let* ((start (min (point) (or (mark t) 1)))
           (end   (max (point) (or (mark t) 1)))
           (start-col (save-excursion
                        (goto-char start)
                        (current-column)))
           (end-col   (save-excursion
                        (goto-char end)
                        (current-column))))
      (if (<= start-col end-col)
          (1+ end)
        (save-excursion
          (goto-char end)
          (condition-case nil
              (move-to-column start-col)
            (error nil))
          (1+ (point))))))
   ;; End of last line (including newline)
   ((eq 'line mode)
    (cond
     ((not (mark t))
      (line-beginning-position 2))
     (t
      (save-excursion
        (goto-char (max (point) (mark t)))
        (line-beginning-position 2)))))
   ;; End of region plus one character
   (t
    (1+ (max (point) (or (mark t) 1))))))

(defun vimpulse-visual-select (beg end &optional widen)
  "Visually select text from BEG to END.
Return nil if selection is unchanged. If WIDEN is non-nil, only
modify selection if it does not already encompass BEG and END.

Under the hood, this function changes Emacs' `point' and `mark'.
The boundaries of the Visual selection are deduced from these and
the current Visual mode."
  (cond
   (widen
    (vimpulse-visual-select
     (min beg end (vimpulse-visual-beginning))
     (max beg end (vimpulse-visual-end))))
   (t
    (let ((opoint (point)) (omark (mark t)) mark-active)
      (cond
       ((< (point) (mark t))
        (goto-char (min beg end))
        ;; `vimpulse-visual-end' is always 1 larger than region's end
        ;; to ensure at least one character is selected. Therefore,
        ;; subtract 1 from region's end.
        (set-mark  (1- (max beg end))))
       (t
        (set-mark  (min beg end))
        (goto-char (1- (max beg end)))))
      ;; Was selection changed?
      (not (and (eq opoint (point))
                (eq omark  (mark t))))))))

(defun vimpulse-visual-expand-region ()
  "Expand Emacs region to Visual selection."
  (let ((newpoint (vimpulse-visual-beginning))
        (newmark  (vimpulse-visual-end))
        mark-active)
    (when (< (or (mark t) 1) (point))
      (setq newpoint (prog1 newmark
                       (setq newmark newpoint))))
    (set-mark  newmark)
    (goto-char newpoint)))

(defun vimpulse-visual-contract-region ()
  "Opposite of `vimpulse-visual-expand-region'.
I.e., the resulting Visual selection is equivalent to the former
Emacs region."
  (vimpulse-visual-select (region-beginning)
                          (region-end)))

(defun vimpulse-visual-restore ()
  "Restore previous selection."
  (interactive)
  (cond
   ;; If no previous selection, try a quick C-x C-x
   ((or (not vimpulse-visual-point)
        (not vimpulse-visual-mark))
    (vimpulse-activate-mark nil)
    (vimpulse-visual-mode 1))
   (t
    (unless vimpulse-visual-mode
      (cond
       ((eq 'line vimpulse-visual-last)
        (vimpulse-visual-activate-line))
       ((eq 'block vimpulse-visual-last)
        (vimpulse-visual-activate-block))
       (t                               ; normal
        (vimpulse-visual-activate-normal))))
    (set-mark vimpulse-visual-mark)
    (goto-char vimpulse-visual-point)
    (vimpulse-visual-highlight))))

(defun vimpulse-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on region and Visual mode.
With negative ARG, removes highlighting."
  (cond
   ((and (numberp arg) (> 1 arg))
    (when vimpulse-visual-overlay
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil))
   ((eq 'block vimpulse-visual-mode)
    ;; Remove any normal/line highlighting
    (when vimpulse-visual-overlay
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    ;; Block highlighting isn't perfect
    (condition-case nil
        (vimpulse-visual-highlight-block
         (vimpulse-visual-beginning)
         (vimpulse-visual-end))
      (error nil)))
   (vimpulse-visual-mode                ; normal or line
    ;; Remove any block highlighting
    (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil)
    ;; Reuse overlay if possible
    (if vimpulse-visual-overlay
        (viper-move-overlay vimpulse-visual-overlay
                            (vimpulse-visual-beginning)
                            (vimpulse-visual-end))
      (setq vimpulse-visual-overlay
            (viper-make-overlay (vimpulse-visual-beginning)
                                (vimpulse-visual-end)))
      (viper-overlay-put vimpulse-visual-overlay
                         'face (vimpulse-region-face))
      (viper-overlay-put vimpulse-visual-overlay
                         'priority 99)))))

(defun vimpulse-visual-highlight-block (beg end)
  "Highlight rectangular region from BEG to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.

Adapted from: `rm-highlight-rectangle' in rect-mark.el."
  (save-excursion
    ;; Calculate the rectangular region represented by point and mark,
    ;; putting BEG in the north-west corner and END in the
    ;; south-east corner
    (let ((beg-col (save-excursion
                     (goto-char beg)
                     (current-column)))
          (end-col (save-excursion
                     (goto-char end)
                     (current-column))))
      (if (>= beg-col end-col)
          (setq beg-col (prog1
                            (1- end-col)
                          (setq end-col (1+ beg-col)))
                beg (save-excursion
                      (goto-char beg)
                      (move-to-column beg-col nil)
                      (point))
                end (save-excursion
                      (goto-char end)
                      (move-to-column end-col nil)
                      (point))))
      ;; Force a redisplay so we can do reliable window BEG/END
      ;; calculations
      (sit-for 0)
      (let* ((old vimpulse-visual-block-overlays)
             (new nil)
             overlay
             (window-beg (max (window-start) beg))
             (window-end (min (window-end) end))
             (nlines (count-lines window-beg
                                  (min window-end
                                       (point-max)))))
        ;; Iterate over those lines of the rectangle which are visible
        ;; in the currently selected window
        (goto-char window-beg)
        (dotimes (i nlines)
          (let ((row-beg (progn
                           (move-to-column beg-col nil)
                           (point)))
                (row-end (progn
                           (move-to-column end-col nil)
                           (min (point)
                                (line-end-position)))))
            ;; Trim old leading overlays
            (while (and old
                        (setq overlay (car old))
                        (< (viper-overlay-start overlay) row-beg)
                        (/= (viper-overlay-end overlay) row-end))
              (vimpulse-delete-overlay overlay)
              (setq old (cdr old)))
            ;; Reuse an overlay if possible, otherwise create one
            (if (and old
                     (setq overlay (car old))
                     (or (= (viper-overlay-start overlay) row-beg)
                         (= (viper-overlay-end overlay) row-end)))
                (progn
                  (viper-move-overlay overlay row-beg row-end)
                  (setq new (cons overlay new)
                        old (cdr old)))
              (setq overlay (viper-make-overlay row-beg row-end))
              (viper-overlay-put overlay 'face (vimpulse-region-face))
              (viper-overlay-put overlay 'priority 99)
              (setq new (cons overlay new))))
          (forward-line 1))
        ;; Trim old trailing overlays
        (mapcar 'vimpulse-delete-overlay old)
        (setq vimpulse-visual-block-overlays (nreverse new))))))

(defun vimpulse-visual-pre-command ()
  "Run before each command in Visual mode."
  (when vimpulse-visual-mode
    (viper-move-marker-locally 'vimpulse-visual-point (point))
    (viper-move-marker-locally 'vimpulse-visual-mark  (mark t))
    (set-register (viper-int-to-char (1+ (- ?y ?a)))
                  (vimpulse-visual-beginning))
    (set-register (viper-int-to-char (1+ (- ?z ?a)))
                  (vimpulse-visual-end))
    (cond
     ((eq 'insert-state viper-current-state)
      nil)
     (vimpulse-visual-region-changed
      (vimpulse-visual-expand-region))
     ((vimpulse-region-cmd-p this-command)
      (vimpulse-visual-expand-region)
      (setq vimpulse-visual-region-changed t)))))

(defun vimpulse-visual-post-command ()
  "Run after each command in Visual mode."
  (when vimpulse-visual-mode
    (cond
     (quit-flag                         ; C-g
      (vimpulse-visual-mode -1))
     ((eq 'keyboard-quit this-command)
      (vimpulse-visual-mode -1))
     ((and (not (vimpulse-mark-active))
           (not (eq 'block vimpulse-visual-mode)))
      (vimpulse-visual-mode -1))
     (t
      (cond
       ((eq 'block vimpulse-visual-mode)
        (when vimpulse-visual-region-changed
          (vimpulse-visual-restore)
          (setq vimpulse-visual-region-changed nil)))
       ((vimpulse-boundaries-cmd-p this-command)
        (vimpulse-visual-contract-region)
        (setq vimpulse-visual-region-changed t))
       (vimpulse-visual-region-changed
        (vimpulse-visual-restore)
        (setq vimpulse-visual-region-changed nil)))
      (vimpulse-visual-highlight)))))

(defun vimpulse-visual-deactivate-hook ()
  "Hook run when mark is deactivated in visual mode."
  (when vimpulse-visual-mode
    (and (not (vimpulse-mark-active))
         (vimpulse-region-cmd-p this-command)
         ;; (not (eq 'block vimpulse-visual-mode))
         (vimpulse-visual-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-delete (beg end &optional dont-save)
  "Kills the Visual selection to the kill-ring.
If DONT-SAVE is non-nil, just delete it."
  (interactive "r")
  (let ((length (- end beg)))
    (cond
     (dont-save
      (cond
       ((eq 'block vimpulse-visual-mode)
        (delete-rectangle beg end)
        (goto-char (min vimpulse-visual-point vimpulse-visual-mark)))
       (t
        (delete-region beg end)
        (goto-char beg)))
      (vimpulse-visual-mode -1))
     ((eq 'normal vimpulse-visual-mode)
      (viper-prefix-arg-com ?r 1 ?d)
      (viper-set-destructive-command
       (list 'viper-forward-char
             length ?d viper-use-register nil nil)))
     ((eq 'line vimpulse-visual-mode)
      (setq length (count-lines beg end))
      (goto-char (min vimpulse-visual-point vimpulse-visual-mark))
      (viper-line (cons length ?D)))
     ((eq 'block vimpulse-visual-mode)
      (kill-rectangle beg end)
      (goto-char (min vimpulse-visual-point vimpulse-visual-mark))
      (vimpulse-visual-mode -1)))))

(defun vimpulse-visual-change (beg end &optional dont-save)
  "Change the Visual selection to the kill-ring.
If DONT-SAVE is non-nil, just delete it."
  (interactive "r")
  (let ((length (- end beg)))
    (vimpulse-visual-delete beg end dont-save)
    (cond
     ((eq 'block vimpulse-visual-mode)
      (viper-insert nil))
     (t
      (viper-insert nil)
      (viper-set-destructive-command
       (list 'viper-forward-char
             length ?c viper-use-register nil nil))))))

(defun vimpulse-visual-replace-region (beg end &optional arg)
  "Replace all selected characters with ARG."
  (interactive "r")
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (goto-char beg)
    (viper-replace-char arg)
    (let ((c (char-after (point))))
      (dotimes
          (i (- end beg))
        (cond
         ((member (char-after (point)) '(?\r ?\n))
          (forward-char))
         (t (delete-char 1)
            (insert c))))))
   ((eq 'block vimpulse-visual-mode)
    (goto-char beg)
    (viper-replace-char arg)
    (let* ((c (char-after (point)))
           (begin-col (current-column))
           (len (- (save-excursion
                     (goto-char end)
                     (current-column))
                   begin-col)))
      (while (< (point) end)
        (move-to-column begin-col nil)
        (let ((n 0))
          (while (and (< n len)
                      (not (member (char-after (point))
                                   '(?\r ?\n))))
            (delete-char 1)
            (insert c)
            (setq n (1+ n))))
        (forward-line))))
   (t
    (error "Viper not in Visual mode.")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

(defun vimpulse-visual-make-upcase (beg end)
  "Converts all selected characters to upper case."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'upcase-region))

(defun vimpulse-visual-make-downcase (beg end)
  "Converts all selected characters to lower case."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'downcase-region))

(defun vimpulse-visual-toggle-case (beg end)
  "Toggles the case of all selected characters."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'vimpulse-visual-toggle-case-region))

(defun vimpulse-visual-change-case (beg end &optional case-func)
  (setq case-func (or case-func 'vimpulse-visual-toggle-case-region))
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (funcall case-func beg end))
   ((eq 'block vimpulse-visual-mode)
    (let ((begin-col (save-excursion
                       (goto-char beg)
                       (current-column)))
          (len  (- (save-excursion
                     (goto-char end)
                     (current-column))
                   (save-excursion
                     (goto-char beg)
                     (current-column)))))
      (goto-char beg)
      (while (< (point) end)
        (let ((from (save-excursion
                      (move-to-column begin-col nil)
                      (point)))
              (to (save-excursion
                    (move-to-column (+ begin-col len) nil)
                    (point))))
          (funcall case-func from to)
          (forward-line)))))
   (t
    (error "Viper not in Visual mode.")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

(defun vimpulse-visual-toggle-case-region (beg end)
  "Toggles the case of all characters from BEG to END (exclusive)."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< beg end)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
          (insert-char (downcase c) 1)
        (insert-char (upcase c) 1))
      (setq beg (1+ beg)))))

(defun vimpulse-visual-join (beg end)
  "Joins the selected lines."
  (interactive "r")
  (when vimpulse-visual-mode
    (vimpulse-visual-mode -1)
    (goto-char beg)
    (viper-join-lines (count-lines beg end))))

;; Currently, I don't know how to take the argument ARG
;; into the Repeat-command
(defun vimpulse-visual-shift-left (beg end &optional arg)
  "Shift all selected lines to the left."
  (interactive "r\nP")
  (setq arg (viper-p-val arg))
  (vimpulse-visual-mode -1)
  (vimpulse-push-buffer-undo-list-mark)
  (let ((nlines (1- (count-lines beg end))))
    (dotimes (i arg)
      (goto-char beg)
      (viper-next-line (cons nlines ?<)))
    (vimpulse-connect-undos)))

(defun vimpulse-visual-shift-right (beg end &optional arg)
  "Shift all selected lines to the right."
  (interactive "r\nP")
  (setq arg (viper-p-val arg))
  (vimpulse-visual-mode -1)
  (vimpulse-push-buffer-undo-list-mark)
  (let ((nlines (1- (count-lines beg end))))
    (dotimes (i (or arg 1))
      (goto-char beg)
      (viper-next-line (cons nlines ?>)))
    (vimpulse-connect-undos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intermediate commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-set-current-register ()
  (interactive)
  (setq viper-use-register (read-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-yank (beg end)
  "Save the Visual selection in the kill-ring."
  (interactive "r")
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (viper-prefix-arg-com ?r 1 ?y))
   ((eq 'block vimpulse-visual-mode)
    (kill-rectangle beg end)
    (goto-char beg)
    (yank-rectangle)
    ;; Associate the rectangle with the last entry in the kill-ring
    (put 'killed-rectangle 'previous-kill (current-kill 0)))
   (t
    (error "Viper not in Visual mode.")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

(defun vimpulse-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.
For example, if mark is in the upper left corner and point in
the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (exchange-point-and-mark))
   ((eq 'block vimpulse-visual-mode)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (current-column)))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (move-to-column point-col t)
                  (point)))
      (move-to-column mark-col t)))
   (t
    (error "Viper not in Visual mode."))))

(defun vimpulse-visual-select-text-object
  (count &optional char motion)
  "Visually select a text object, read from keyboard."
  (interactive "p")
  (let* ((char    (or char last-command-event))
         (motion  (or motion (read-char)))
         (bounds  (vimpulse-unify-multiple-bounds
                   (point) char count motion))
         (beg     (car bounds))
         (end     (cadr bounds)))
    (when (and beg end)
      (setq end (1+ end))
      (unless (vimpulse-visual-select beg end t)
        ;; We're stuck; move and try again
        (if (< (point) (mark t))
            (backward-char) (forward-char))
        (setq bounds (vimpulse-unify-multiple-bounds
                      (point) char count motion)
              beg   (car bounds)
              end   (cadr bounds))
        (when (and beg end)
          (vimpulse-visual-select beg end t)))
      (setq vimpulse-last-object-selection
            (list count char motion)))))

(defun vimpulse-widen-selection (beg end)
  "Widen visual selection to BEG and END.
When called interactively, derives BEG and END from
previous text object selection."
  (interactive
   (let ((count  (nth 0 vimpulse-last-object-selection))
         (char   (nth 1 vimpulse-last-object-selection))
         (motion (nth 2 vimpulse-last-object-selection)))
     (when vimpulse-last-object-selection
       (vimpulse-visual-select-text-object count char motion))
     '(nil nil)))                       ; that's it, we're done
  (cond
   ((or (not (numberp beg)) (not (numberp end)))
    nil)
   (t
    (vimpulse-visual-select beg end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual Block Mode Support ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(viper-deflocalvar
 vimpulse-undo-needs-adjust nil
 "If true, several commands in the undo-list should be connected.")

(defconst vimpulse-buffer-undo-list-mark 'vimpulse
  "Everything up to this mark is united in the undo-list.")

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region
(defvar vimpulse-visual-insert-coords nil
  "A list with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

(defun vimpulse-visual-create-coords
  (mode i-com upper-left lower-right)
  "Update the list of block insert coordinates with current rectangle.
I-COM should be ?c, ?i, ?a, ?I or ?A; the column for the
insertion will be chosen according to this command.
Returns the insertion point."
  (setq vimpulse-visual-insert-coords nil)
  (let ((nlines (count-lines upper-left lower-right))
        (col 0)) ; for ?I and ?A, trivial -- column is 0
    (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
      ;; For ?i and ?a, choose the left (the right) rectangle column
      (let ((beg-col (save-excursion
                       (goto-char upper-left)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; Decide if we use the left or right column
        (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
                             beg-col
                           (1- end-col))))))
    ;; Save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (move-to-column col nil)
      (point))))

(defun vimpulse-filter-undos (undo-list)
  "Filters all `nil' marks from `undo-list' until the first
occurrence of `vimpulse-buffer-undo-list-mark'."
  (cond
   ((null undo-list)
    nil)
   ((eq (car undo-list) 'vimpulse)
    (cdr undo-list))
   ((null (car undo-list))
    (vimpulse-filter-undos (cdr undo-list)))
   (t
    (cons (car undo-list)
          (vimpulse-filter-undos (cdr undo-list))))))

(defun vimpulse-connect-undos ()
  "Connects all undo-steps from `buffer-undo-list' up to the
first occurrence of `vimpulse-buffer-undo-list-mark'."
  (when (and vimpulse-undo-needs-adjust
             (listp buffer-undo-list))
    (setq buffer-undo-list
          (vimpulse-filter-undos buffer-undo-list)))
  (setq vimpulse-undo-needs-adjust nil))

(defun vimpulse-push-buffer-undo-list-mark ()
  (setq vimpulse-undo-needs-adjust t)
  (push vimpulse-buffer-undo-list-mark buffer-undo-list))

;; Redefinitions of Viper functions to handle Visual block selection,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to visual mode.
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the visual region
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (move-to-column col)))
            ;; If we are in block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((< col cur-col)    ; we are inside a tab
                       (move-to-column (1+ col) 'fill) ; convert to spaces
                       (move-to-column col 'fill) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; Update undo-list
  (vimpulse-connect-undos))

(defadvice viper-Put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (let (inserted-text replaced-text mode)
    (setq yank-window-start (window-start))
    (cond
     (vimpulse-visual-mode
      (setq mode vimpulse-visual-mode)
      (unless (eq 'block mode)
        ;; Add replaced text to the kill-ring before the current kill
        (setq inserted-text (current-kill 0))
        (setq replaced-text
              (buffer-substring (region-beginning) (region-end)))
        (kill-new replaced-text t)
        (kill-new inserted-text))
      (vimpulse-visual-delete (region-beginning) (region-end) t)
      (and (eq 'normal mode)
           (viper-end-with-a-newline-p inserted-text)
           (newline)))
     (mark-active
      (delete-region (region-beginning) (region-end))))
    (if (and killed-rectangle
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (yank-rectangle)
      ad-do-it)))

(defadvice viper-put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (setq yank-window-start (window-start))
  (cond
   (vimpulse-visual-mode
    (viper-Put-back arg))
   (mark-active
    (viper-Put-back arg))
   (t
    (if (and killed-rectangle
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (yank-rectangle)
      ad-do-it))))

;; These two functions implement insertion at the beginning/end
;; of a visual block or linewise selection
(defun vimpulse-visual-insert (beg end &optional arg)
  "Enter Insert state at beginning of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'normal vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (viper-insert arg)
      (push-mark end t t)
      (goto-char beg))
     ((eq 'line vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (push-mark (save-excursion
                   (goto-char end)
                   ;; Don't want trailing newline
                   (when (bolp) (backward-char))
                   (point))
                 t t)
      (goto-char beg)
      (viper-insert arg))
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?i beg end))
      (viper-insert arg))
     (t
      (error "Viper not in Visual mode.")))))

(defun vimpulse-visual-append (beg end &optional arg)
  "Enter Insert state at end of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'normal vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (viper-insert arg)
      (push-mark beg t t)
      (goto-char end))
     ((eq 'line vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (push-mark beg t t)
      (goto-char end)
      ;; Don't want trailing newline
      (when (bolp) (backward-char))
      (viper-insert arg))
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?a beg end))
      (viper-append arg))
     (t
      (error "Viper not in Visual mode.")))))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the whole region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with the commands below (i.e., in a hook),
               ;; do (let (this-command) (push-mark)).
               (memq this-command
                     '(vimpulse-goto-first-line
                       viper-backward-paragraph
                       viper-backward-sentence
                       viper-forward-paragraph
                       viper-forward-sentence
                       viper-goto-line
                       viper-window-bottom
                       viper-window-middle
                       viper-window-top)))
    ad-do-it))

;; CHECKME: is this still needed?
(defadvice viper-deactivate-mark
  (around vimpulse-deactivate-mark-wrap activate)
  "Don't deactivate mark in Visual mode."
  (let (deactivate-mark-hook)
    (cond
     ((and vimpulse-visual-mode
           (not (eq 'block vimpulse-visual-mode)))
      nil)
     ((and (boundp 'cua-mode) cua-mode)
      (cua--deactivate))
     ((and (boundp 'transient-mark-mode)
           (not transient-mark-mode))
      (setq mark-active nil))
     ((and (boundp 'zmacs-region-active-p)
           (not zmacs-region-active-p))
      (let ((zmacs-region-active-p t))
        ad-do-it))
     (t
      ad-do-it))))

(provide 'vimpulse-visual-mode)

;; Load vimpulse components
(require 'vimpulse-dependencies)
(require 'vimpulse-viper-function-redefinitions)
(require 'vimpulse-utils)
(require 'vimpulse-modal)
(require 'vimpulse-misc-keybindings)
(require 'vimpulse-ex)
(require 'vimpulse-paren-matching)
(require 'vimpulse-text-object-system)
(require 'vimpulse-visual-mode)

(provide 'vimpulse)
