(add-hook 'org-mime-html-hook 'my-org-mime-insert-css)

(defun my-org-mime-insert-css ()
 (insert "<style type=\"text/css\">/

i {
 color: #1A1A1A;
}

h1, ul#tabs, h2, h3, h4, h5 {
 font-family: \"Trebuchet MS\",Verdana,sans-serif;
}

h1 {
 background-color: #0A3F69;
 color: #F8F8F8;
 font-size: 24px;
 margin: 0;
 padding: 9px 0px 0px 10px;
}

h2 {
 border-bottom: 4px solid #67B3E3;
 color: #13679D;
 font-size: 20px;
}

h3, h4, h5, h6 {
 color: #1572AE;
}

h3 {
 border-bottom: 1px solid #B5DAF1;
 font-size: 16px;
 margin-left: 25px;
}

h4 {
 border-bottom: 1px dotted #C9E3F5;
 font-size: 14px;
 margin-left: 60px;
}

h5 {
 font-size: 1em;
 margin-left: 87px;
}

h6 {
 font-size: 1em;
 margin-left: 100px;
}

.DONEheader {
 color: #ADADAD;
 text-decoration: line-through;
}

 h3.DONEheader {
 border-bottom: 1px solid #DDDDDD;
 }

 h4.DONEheader {
 border-bottom: 1px dotted #DDDDDD;
 }

.outline-text-2, .outline-text-3, .outline-text-4, .outline-text-5,
.outline-3 > ul, /* for HTML export of Beamer slides */
.outline-4 > ol, #text-footnotes {
 margin-left: 100px;
}

li > .outline-text-5 {
 margin-left: 20px;
}

ul, ol {
 padding-left: 1.5em;
}

dt {
 color: #1572AE;
 font-weight: bold;
}

dd {
 margin-bottom: 6px;
}

pre {
 /* Use #EAEAEA for background-color of border with src code block's name */
 background-color: #F8F8FF;
 border: 1px solid #DEDEDE;
 color: #444444;
 font-family: monospace;
 line-height: 1.14em;
 overflow: auto;
 /* overflow-x: auto; */
 padding: 10px;
}

code {
 background-color: #F8F8FF;
 border: 1px solid #DEDEDE;
 color: #444444;
 font-family: monospace;
 /* font-size: 0.93em; */
 margin: 0px 1px;
 padding: 0px 2px;
}

li > p, li > ul, li > .inlinetask, li > dl {
 margin-left: 0px;
}

dd > p, dd > ul, dd > .inlinetask, dd > dl {
 margin-left: 0px;
}

li.checked {
 list-style-image: url('../images/checked.png');
}

li.halfchecked {
 list-style-image: url('../images/halfchecked.png');
}

li.unchecked {
 list-style-image: url('../images/unchecked.png');
}

a, a:link, a:visited {
 color: #2061A2;
 text-decoration: none;
}

a:hover {
 text-decoration: underline;
}

a:focus {
 outline: none;
}


table, th, td
{
 border: 1px solid #B5DAF1;
 border-left: 2px solid white;
 border-right: 2px solid white;
}

th
{
 border-width: 1px 2px;
 border-color: white;
 background-color: #2061A2;
 color: white;
}

caption {
 color: #8D8D84;
}

img {
 display: block;
 margin-left: auto;
 margin-right: auto;
 text-align: center;
}

.figure {
 color: #8D8D84;
 text-align: center;
}


.left {
 text-align: left;
}

.right {
 text-align: right;
}

.center {
 text-align: center;
}

.justify {
 text-align: justify;
}

.inlinetask {
 background-color: #F7F7F7;
 border-collapse: separate;
 border-color: #EEEEEE #EEEEEE #EEEEEE #1E90FF;
 border-style: solid;
 border-width: 1px 1px 1px 6px;
 padding: 8px 8px 0px 8px;
 margin: 10px 0px;
}

 .inlinetask td {
 padding: 2px 5px 0px 2px;
 border: 0px;
 }

.info {
 border: 1px solid;
 background: url('../images/info.png') no-repeat 10px 10px #BDE5F8;
 color: #00529B;
 padding: 4px 10px 4px 52px;
 border-top-left-radius: 5px;
 border-top-right-radius: 5px;
 border-bottom-right-radius: 5px;
 border-bottom-left-radius: 5px;
 margin: 10px 0px;
}

.tip {
 border: 1px solid;
 background: url('../images/tip.png') no-repeat 10px 10px #DFF2BF;
 color: #4F8A10;
 padding: 4px 10px 4px 52px;
 border-top-left-radius: 5px;
 border-top-right-radius: 5px;
 border-bottom-right-radius: 5px;
 border-bottom-left-radius: 5px;
 margin: 10px 0px;
}

.note {
 border: 1px solid;
 background: url('../images/note.png') no-repeat 10px 10px #FFFCCB;
 color: #9F6000;
 padding: 4px 10px 4px 52px;
 border-top-left-radius: 5px;
 border-top-right-radius: 5px;
 border-bottom-right-radius: 5px;
 border-bottom-left-radius: 5px;
 margin: 10px 0px;
}

.warning {
 border: 1px solid;
 background: url('../images/warning.png') no-repeat 10px 10px #FFBABA;
 color: #D8000C;
 padding: 4px 10px 4px 52px;
 border-top-left-radius: 5px;
 border-top-right-radius: 5px;
 border-bottom-right-radius: 5px;
 border-bottom-left-radius: 5px;
 margin: 10px 0px;
}

.todo, .done {
 margin: 10px 0;
 padding: 0px 2px;
}

.NEW {
 background-color: #FDFCD8;
 border: 1px solid #EEE9C3;
 color: #302B13;
 font-weight: normal;
}

.TODO {
 background-color: #FED5D7;
 border: 1px solid #FC5158;
 color: #FC5158;
}

.STRT, .STARTED {
 background-color: #FEF2D4;
 border: 1px solid #FDBF3D;
 color: #FDBF3D;
}

.WAIT, .WAITING, .DLGT, .DELEGATED {
 background-color: #DFFFDF;
 border: 1px solid #55BA80;
 color: #55BA80;
}

.SDAY, .SOMEDAY, .DFRD, .DEFERRED {
 background-color: #D3EEFF;
 border: 1px solid #42B5FF;
 color: #42B5FF;
}

.DONE, .CANX, .CANCELED {
 background-color: #F2F2EE;
 border: 1px solid #969696;
 color: #969696;
}


span.todo {
 cursor: pointer;
 /* display: block; */
 /* float: left; */
 margin: -1px 3px 0px 0px;
}

 span.todo:hover {
 background: #BABDB6;
 color: #888888;
 }

span.todo .selected {
 background-color: #FFEBC1;
 border-color: #FDBF3B;
 color: #A6750C;
}


.timestamp {
 color: #777777;
 font-size: 80%;
}


.example {
 background-color: #DDFADE;
 border: 1px solid #9EC49F;
 color: #333333;
}





h4, h5 {
 font-weight:normal;
 font-family:Arial, Helvetica, sans-serif
}
h3 {font-size:100%;
font-family:Arial, Helvetica, sans-serif}

h1,h2 {
font-family:Arial, Helvetica, sans-serif}


span.todo.REJECTED {
 background-color: #FED5D7;
 border: 1px solid #FC5158;
 color: #FC5158;
}


span.done.DONE {
 color: #FFF;
 font-weight:bold;
background-color: #00C310;

}


span.done.REJECTED, span.done.MISSED {

 background-color: #FED5D7;
 border: 1px solid #FC5158;
 color: #FC5158;

}


.DONEheader {
text-decoration: none;

}

span.done.DONE:before {
 content: \"✅\";
  margin-left:-2.0em;
 padding-right:1em;
}




span.done.REJECTED:before, span.done.MISSED:before {
 content: \"❌\";
  margin-left:-2.0em;
 padding-right:1em;
}


span.todo.TODO:before {
 content: \"🔲\";
  margin-left:-2.0em;
 padding-right:1em;
}

span.todo.QUESTION {
 color:#D8000C;
}

span.todo.QUESTION:before {
 content: \"❓\";
  margin-left:-2.0em;
 padding-right:1em;
}


span.todo.MAYBE {
 color:#D8000C;
}

span.todo.MAYBE:before {
 content: \"🤔\";
  margin-left:-2.0em;
 padding-right:1em;
}




span.ellipsis {
 color:red;
}

span.ellipsis:before {
 content: \"More...\";
}

div div p {
 color:#000;
}



li.off {
 list-style-type: none
}

li.on {
 list-style-type: none
}



span.todo.MISSED, span.todo.MISSED__ {
 background-color: #add8e6;
 border: 1px solid #add8e6;
 color: #FFF;
}
 span.todo.MISSED:after, span.todo.MISSED__:after {
content: \"\00a0\00a0 ❌\"
}


span.todo.MISSED__ {
 background-color: #add8e6;
 border: 1px solid #add8e6;
 color: #FFF;

}

span.done.DONE__ {
 color: #FFF;
 font-weight:bold;
background-color: #00C310;

}

span.todo.STARTED__ {
 background-color: pink;
 border: 1px solid pink;
 color: #FFF;

}


/</style>"))

(defun claire-send-email ()
 "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields. Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
 (interactive)
; store location.
 (setq *email-heading-point* (set-marker (make-marker) (point)))
 (save-excursion
  (let ((content (progn
           (unless (org-on-heading-p) (outline-previous-heading))
           (let ((headline (org-element-at-point)))
            (buffer-substring
            (org-element-property :contents-begin headline)
            (org-element-property :contents-end headline)))))
     (TO "\"Claire Fridkin\" <Jay@jaydixit.com>")
          (SUBJECT (nth 4 (org-heading-components)))
     (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
     (continue nil)
     (switch-function nil)
     (yank-action nil)
     (send-actions '((email-send-action . nil)))
     (return-action '(email-heading-return)))

   (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
   (message-goto-body)
   (insert content)
   (if TO
     (message-goto-body)
    (message-goto-to))
)
))

(defun old-claire-send-email-styled ()
  "Send the current org-mode heading as the body of an email, with headline as the subject."
  (interactive)
  ;; Store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let* ((content (progn
                      (unless (org-on-heading-p) (outline-previous-heading))
                      (let ((headline (org-element-at-point)))
                        (buffer-substring
                         (org-element-property :contents-begin headline)
                         (org-element-property :contents-end headline)))))
           (TO "Claire Fridkin <jay@jaydixit.com>")
           (SUBJECT (nth 4 (org-heading-components)))
           (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
           (continue nil)
           (switch-function nil)
           (yank-action nil)
           (send-actions '((email-send-action . nil)))
           (return-action '(email-heading-return))
           html-content)
      (with-temp-buffer
        (insert content)
        (setq html-content (org-mime-org-buffer-htmlize)))
      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
      (message-goto-body)
      ;; Insert the HTML content
      (mml-insert-multipart "alternative")
      (mml-insert-part "text/html")
      (insert html-content)
      (message-goto-to)
      (message-send-and-exit))))

(defun claire-send-email-styled ()
 "Send the current org-mode heading as the body of an email, with headline as the subject."
 (interactive)
 (if (and (eq major-mode 'org-mode) (org-on-heading-p))
   (save-excursion
   (org-narrow-to-subtree)
   (let* ((TO "Claire Fridkin <jay@jaydixit.com>")
       (SUBJECT (nth 4 (org-heading-components)))
       (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
       (continue nil)
       (switch-function nil)
       (yank-action nil)
       (send-actions '((email-send-action . nil)))
       (return-action '(email-heading-return))
       (org-html-head-extra (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///Users/jay/Dropbox/github/org-html-themes/src/bigblow_theme/css/bigblow.css\" />"))
       (html-content (org-export-as 'html nil nil nil nil)))
    (widen)
    (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
    (message-goto-body)
    ;; Insert the HTML content
    (mml-insert-multipart "alternative")
    (mml-insert-part "text/html")
    (insert html-content)
    (message-goto-to)
    (message-send-and-exit)))
  (message "You must be in an Org mode buffer on a heading to use this function.")))

(defun claire-send-email-with-css ()
  "Send the current org-mode heading as the body of an email, with headline as the subject."
  (interactive)
  (if (and (eq major-mode 'org-mode) (org-on-heading-p))
      (progn
        (save-excursion
          (org-narrow-to-subtree)
          (let* ((TO "Claire Fridkin <jay@jaydixit.com>")
                 (SUBJECT (nth 4 (org-heading-components)))
                 (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
                 (continue nil)
                 (switch-function nil)
                 (yank-action nil)
                 (send-actions '((email-send-action . nil)))
                 (return-action '(email-heading-return))
                 (hardcoded-css "<style type=\"text/css\"> /* Your CSS Here */
i {
  color: #1A1A1A;
}

h1, ul#tabs, h2, h3, h4, h5 {
  font-family: \"Trebuchet MS\",Verdana,sans-serif;
}

h1 {
  background-color: #0A3F69;
  color: #F8F8F8;
  font-size: 24px;
  margin: 0;
  padding: 9px 0px 0px 10px;
}

h2 {
  border-bottom: 4px solid #67B3E3;
  color: #13679D;
  font-size: 20px;
}

h3, h4, h5, h6 {
  color: #1572AE;
}

h3 {
  border-bottom: 1px solid #B5DAF1;
  font-size: 16px;
  margin-left: 25px;
}

h4 {
  border-bottom: 1px dotted #C9E3F5;
  font-size: 14px;
  margin-left: 60px;
}

h5 {
  font-size: 1em;
  margin-left: 87px;
}

h6 {
  font-size: 1em;
  margin-left: 100px;
}

.DONEheader {
  color: #ADADAD;
  text-decoration: line-through;
}

  h3.DONEheader {
    border-bottom: 1px solid #DDDDDD;
  }

  h4.DONEheader {
    border-bottom: 1px dotted #DDDDDD;
  }

.outline-text-2, .outline-text-3, .outline-text-4, .outline-text-5,
.outline-3 > ul, /* for HTML export of Beamer slides */
.outline-4 > ol, #text-footnotes {
  margin-left: 100px;
}

li > .outline-text-5 {
  margin-left: 20px;
}

ul, ol {
  padding-left: 1.5em;
}

dt {
  color: #1572AE;
  font-weight: bold;
}

dd {
  margin-bottom: 6px;
}

pre {
  /* Use #EAEAEA for background-color of border with src code block's name */
  background-color: #F8F8FF;
  border: 1px solid #DEDEDE;
  color: #444444;
  font-family: monospace;
  line-height: 1.14em;
  overflow: auto;
  /* overflow-x: auto; */
  padding: 10px;
}

code {
  background-color: #F8F8FF;
  border: 1px solid #DEDEDE;
  color: #444444;
  font-family: monospace;
  /* font-size: 0.93em; */
  margin: 0px 1px;
  padding: 0px 2px;
}

li > p, li > ul, li > .inlinetask, li > dl {
  margin-left: 0px;
}

dd > p, dd > ul, dd > .inlinetask, dd > dl {
  margin-left: 0px;
}

li.checked {
  list-style-image: url('../images/checked.png');
}

li.halfchecked {
  list-style-image: url('../images/halfchecked.png');
}

li.unchecked {
  list-style-image: url('../images/unchecked.png');
}

a, a:link, a:visited {
  color: #2061A2;
  text-decoration: none;
}

a:hover {
  text-decoration: underline;
}

a:focus {
  outline: none;
}


table, th, td
{
  border: 1px solid #B5DAF1;
  border-left: 2px solid white;
  border-right: 2px solid white;
}

th
{
  border-width: 1px 2px;
  border-color: white;
  background-color: #2061A2;
  color: white;
}

caption {
  color: #8D8D84;
}

img {
  display: block;
  margin-left: auto;
  margin-right: auto;
  text-align: center;
}

.figure {
  color: #8D8D84;
  text-align: center;
}


.left {
  text-align: left;
}

.right {
  text-align: right;
}

.center {
  text-align: center;
}

.justify {
  text-align: justify;
}

.inlinetask {
  background-color: #F7F7F7;
  border-collapse: separate;
  border-color: #EEEEEE #EEEEEE #EEEEEE #1E90FF;
  border-style: solid;
  border-width: 1px 1px 1px 6px;
  padding: 8px 8px 0px 8px;
  margin: 10px 0px;
}

  .inlinetask td {
    padding: 2px 5px 0px 2px;
    border: 0px;
  }

.info {
  border: 1px solid;
  background: url('../images/info.png') no-repeat 10px 10px #BDE5F8;
  color: #00529B;
  padding: 4px 10px 4px 52px;
  border-top-left-radius: 5px;
  border-top-right-radius: 5px;
  border-bottom-right-radius: 5px;
  border-bottom-left-radius: 5px;
  margin: 10px 0px;
}

.tip {
  border: 1px solid;
  background: url('../images/tip.png') no-repeat 10px 10px #DFF2BF;
  color: #4F8A10;
  padding: 4px 10px 4px 52px;
  border-top-left-radius: 5px;
  border-top-right-radius: 5px;
  border-bottom-right-radius: 5px;
  border-bottom-left-radius: 5px;
  margin: 10px 0px;
}

.note {
  border: 1px solid;
  background: url('../images/note.png') no-repeat 10px 10px #FFFCCB;
  color: #9F6000;
  padding: 4px 10px 4px 52px;
  border-top-left-radius: 5px;
  border-top-right-radius: 5px;
  border-bottom-right-radius: 5px;
  border-bottom-left-radius: 5px;
  margin: 10px 0px;
}

.warning {
  border: 1px solid;
  background: url('../images/warning.png') no-repeat 10px 10px #FFBABA;
  color: #D8000C;
  padding: 4px 10px 4px 52px;
  border-top-left-radius: 5px;
  border-top-right-radius: 5px;
  border-bottom-right-radius: 5px;
  border-bottom-left-radius: 5px;
  margin: 10px 0px;
}

.todo, .done {
  margin: 10px 0;
  padding: 0px 2px;
}

.NEW {
  background-color: #FDFCD8;
  border: 1px solid #EEE9C3;
  color: #302B13;
  font-weight: normal;
}

.TODO {
  background-color: #FED5D7;
  border: 1px solid #FC5158;
  color: #FC5158;
}

.STRT, .STARTED {
  background-color: #FEF2D4;
  border: 1px solid #FDBF3D;
  color: #FDBF3D;
}

.WAIT, .WAITING, .DLGT, .DELEGATED {
  background-color: #DFFFDF;
  border: 1px solid #55BA80;
  color: #55BA80;
}

.SDAY, .SOMEDAY, .DFRD, .DEFERRED {
  background-color: #D3EEFF;
  border: 1px solid #42B5FF;
  color: #42B5FF;
}

.DONE, .CANX, .CANCELED {
  background-color: #F2F2EE;
  border: 1px solid #969696;
  color: #969696;
}


span.todo {
  cursor: pointer;
  /* display: block; */
  /* float: left; */
  margin: -1px 3px 0px 0px;
}

  span.todo:hover {
    background: #BABDB6;
    color: #888888;
  }

span.todo .selected {
  background-color: #FFEBC1;
  border-color: #FDBF3B;
  color: #A6750C;
}


.timestamp {
  color: #777777;
  font-size: 80%;
}


.example {
  background-color: #DDFADE;
  border: 1px solid #9EC49F;
  color: #333333;
}





h4, h5 {
 font-weight:normal;
 font-family:Arial, Helvetica, sans-serif
}
h3 {font-size:100%;
font-family:Arial, Helvetica, sans-serif}

h1,h2 {
font-family:Arial, Helvetica, sans-serif}


span.todo.REJECTED {
   background-color: #FED5D7;
  border: 1px solid #FC5158;
  color: #FC5158;
}


span.done.DONE {
   color: #FFF;
   font-weight:bold;
background-color: #00C310;

}


span.done.REJECTED, span.done.MISSED {

 background-color: #FED5D7;
 border: 1px solid #FC5158;
 color: #FC5158;

}


.DONEheader {
text-decoration: none;

}

span.done.DONE:before {
    content: \"✅\";
     margin-left:-2.0em;
  padding-right:1em;
}




span.done.REJECTED:before, span.done.MISSED:before {
    content: \"❌\";
     margin-left:-2.0em;
  padding-right:1em;
}


span.todo.TODO:before {
  content: \"🔲\";
     margin-left:-2.0em;
  padding-right:1em;
}

span.todo.QUESTION {
 color:#D8000C;
}

span.todo.QUESTION:before {
    content: \"❓\";
     margin-left:-2.0em;
  padding-right:1em;
}


span.todo.MAYBE {
 color:#D8000C;
}

span.todo.MAYBE:before {
    content: \"🤔\";
     margin-left:-2.0em;
  padding-right:1em;
}




span.ellipsis {
 color:red;
}

span.ellipsis:before {
 content: \"More...\";
}

div div p {
 color:#000;
}



li.off {
 list-style-type: none
}

li.on {
 list-style-type: none
}



span.todo.MISSED, span.todo.MISSED__ {
  background-color: #add8e6;
  border: 1px solid #add8e6;
  color: #FFF;
}
 span.todo.MISSED:after, span.todo.MISSED__:after {
content: \"\00a0\00a0 ❌\"
}


span.todo.MISSED__ {
   background-color: #add8e6;
  border: 1px solid #add8e6;
  color: #FFF;

}

span.done.DONE__ {
    color: #FFF;
   font-weight:bold;
background-color: #00C310;

}

span.todo.STARTED__ {
   background-color: pink;
  border: 1px solid pink;
  color: #FFF;

}

</style>")
                 (html-content (org-export-as 'html nil nil nil nil)))
            (if html-content
                (progn
                  ;; Insert CSS into the <head> section
                  (setq html-content (replace-regexp-in-string "<head>\\(.*\\)</head>"
                                                              (concat "<head>\\1" hardcoded-css "</head>")
                                                              html-content))
                  (widen)
                  (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
                  (message-goto-body)
                  (mml-insert-multipart "alternative")
                  (mml-insert-part "text/html")
                  (insert html-content)
                  (message-goto-to)
                  (message-send-and-exit))
              (message "Failed to generate HTML content."))))
        (message "Email sent."))
    (message "You must be in an Org mode buffer on a heading to use this function.")))
