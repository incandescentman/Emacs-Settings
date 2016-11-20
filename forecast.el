;;; forecast.el --- Display a forecast.io weather report in a buffer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015-2016 Göktuğ Kayaalp
;;
;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: weather, forecast
;; Version: 0.4.1
;; URL: http://gkayaalp.com/emacs.html#forecast.el
;; Package-Requires: ((emacs "24.4"))
;;
;; Permission  is  hereby  granted,  free of  charge,  to  any  person
;; obtaining  a copy  of  this software  and associated  documentation
;; files   (the  "Software"),   to  deal   in  the   Software  without
;; restriction, including without limitation  the rights to use, copy,
;; modify, merge, publish, distribute,  sublicense, and/or sell copies
;; of the  Software, and  to permit  persons to  whom the  Software is
;; furnished to do so, subject to the following conditions:
;;
;; The  above copyright  notice and  this permission  notice shall  be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE  IS PROVIDED  "AS IS", WITHOUT  WARRANTY OF  ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT  NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY,   FITNESS    FOR   A   PARTICULAR    PURPOSE   AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT,  TORT OR OTHERWISE, ARISING FROM, OUT  OF OR IN
;; CONNECTION WITH  THE SOFTWARE OR THE  USE OR OTHER DEALINGS  IN THE
;; SOFTWARE.
;;

;;; Commentary:
;;
;; forecast.el generates  a _weather forecast report_  and displays it
;; in a  buffer.  It uses data  from Forecast.io (http://forecast.io),
;; and thus one needs to acquire an  api key from them in order to use
;; this package.  They  allow 1000 requests a day in  their free plan,
;; which should be enough for any user.
;;
;; See Installation section for installation and setup instructions.
;;
;; Report bugs to the Issues page in the Github repo:
;;
;; https://github.com/cadadr/forecast.el/issues
;;

;;; Installation:
;; 
;; See also «Example configuration».
;;
;; forecast.el is available on Melpa with the package name `forecast'.
;; 
;; Otherwise, put  the forecast.el file  somewhere in your  path, then
;; `require'   it.   Then   set   these  variables   either  in   your
;; configuration, or via the customisation group `forecast':
;; 
;; `forecast-latitude'  Latitude of your location,       float
;; `forecast-longitude' Longitude of your configuration  float
;; `forecast-city'      The name of your city            string
;; `forecast-country'   The name of your country         string
;; `forecast-api-key'   The API key from Forecast.io     string
;; `forecast-language'  Language to use                  symbol
;; `forecast-units'     Unit standard to use             symbol
;; 
;; Only the first  five variables are mandatory.  The  first four have
;; *non-sane* defaults,  and if `forecast-api-key' is  absent, program
;; will not run.
;;
;; Use  [ M-x  customize-group  RET forecast  ] in  order  to see  all
;; possible customisations.
;; 
;; The variables  `forecast-city' and `forecast-country' are  used for
;; display  purposes only.   At the  moment forecast.el  cannot deduce
;; these names  from the latitude  and longitude values, but  maybe in
;; future it will be able to.
;;
;; The first two  variables default to 0.0. The  following two default
;; to "nil".
;; 
;; The API key  can be obtained via registering  oneself through their
;; developer website:
;;
;; https://developer.forecast.io/
;; 
;; For the rest of variables, see their docstrings (C-h v RET var-name
;; RET) and the  customize buffer for forecast,  via the customisation
;; group `forecast'.
;;
;; See also  the docstring  for the face  `forecast-moon-phase', which
;; governs the face for the moon phase visualisation.  Most fonts will
;; not have  defined the  necessary characters, thus  one may  need to
;; install a special font, e.g. Quivira (http://quivira-font.com/).
;;
;; Then on,  you may run  the command  `forecast' to get  the forecast
;; buffer.  The forecast  buffer uses `org-level-*' faces,  so it will
;; look like  your org  files.  It is  called «*Weather  Forecast*».

;;; Example configuration:
;;
;; (require 'forecast)
;; (setq forecast-latitude 41.168602
;;       forecast-longitude 29.047024
;;       forecast-city "İstanbul"
;;       forecast-country "Türkiye"
;;       forecast-api-key "<deduced>")
;;
;; Or, for the privacy of the API key:
;;
;; (require 'forecast)
;; (setq forecast-latitude 41.168602
;;       forecast-longitude 29.047024
;;       forecast-city "İstanbul"
;;       forecast-country "Türkiye")
;;
;; (load (locate-user-emacs-file "forecast-api-key.el"))
;;
;; And in the file ~/.emacs.d/forecast-api-key.el:
;;
;; (setq forecast-api-key "<deduced>")
;;

;;; Usage:
;;
;; There are 3 keybindings:
;; 
;; g      Refresh the forecast.  Will re-download data.
;; q      Bury the buffer.
;; C-u q  Kill the buffer.
;; 
;; Two  contemporaneous  instances  of  the  program  *will  not*  run
;; correctly, as global state is used to store data.
;; 
;; The buffer is made up of these elements:
;;
;; - At the top, the title, details of location, last update time
;;
;; - Temperature and summary
;;
;; - Apparent temperature (Feels like ...) and detailed summary
;;
;; - Pressure in  ATMs, humidity percentage, wind  speed and direction
;;   from which wind comes. If available, visibility distance.
;;
;; - Graphic showing  hourly temperature  changes for the  upcoming 24
;;   hours.
;;
;; - Summary information for upcoming seven days.
;;
;; - Link to the http://forecast.io.
;;

;;; Contributing:
;;
;; Feel free to send me a pull request.
;;
;; Git repo:
;;
;; https://github.com/cadadr/forecast.el
;;

;;; Changes:
;;
;; v0.4.1, 13 May 2016
;;   - Fix use of `loop' instead of `cl-loop'.  Thanks to
;;     github.com/Topslick
;; v0.4.0, 19 April 2016
;;   - Implement an hourly temperature graphic.
;;   - Use the maximum instead of averate for upcoming forecasts.
;;   - Add procedure `forecast--temperature-unit-string', exported from
;;     `forecast--temperature-string'.
;;   - Add variable `forecast-graph-marker' for allowing users to customise
;;     the graph.
;; v0.3.0, 09 February 2016
;;   - Adapt for customisations via `customize'.
;;   - Allow to customise time representation, via `forecast-time-format'.
;;     (Thanks to Sharon Kimble for the recommendation)
;; v0.2.0, 05 February 2016
;;   - Fix bugs in `forecast--sun-position-graphic' (Thanks to Zoli Kertesz)
;; v0.1.9, 06 November 2015
;;   - Fix multibyte string problem.
;;   - Some doc fixes.
;; v0.1.8, 21 October 2015
;;   - Use special mode as parent mode.
;; v0.1.7, 20 October 2015
;;   - Doc fixes.
;; v0.1.6, 20 October 2015
;;   - Prepare for Melpa.
;; v0.1.5, 28 August 2015
;;   - New release because I can't really do releases properly.
;; v0.1.4, 28 August 2015
;;   - Fix callback called immaturely in `forecast--load-data'
;;   - Add faces `forecast-upcoming-summary' and `forecast-upcoming-temperature'
;; v0.1.3, 28 August 2015
;;   - Fix non-prefixed functions.
;;   - Fix some errors regarding rendering.
;; v0.1.2, 28 August 2015
;;   - Add missing docs.
;; v0.1.1, 28 August 2015
;;   - Incredibly important changes.
;; v0.1.0, 28 August 2015
;;   - First release.
;;

;;; TODO:
;; 
;; - Automatically find city and country names.
;; - Get location from computer?
;; - I18N?
;; 

;;; Code:
(require 'json)
(require 'cl-lib)
(require 'url)
(require 'subr-x)
(require 'org) ;; Org faces are used.
(require 'button)

(defgroup forecast
  nil
  "Customisations for the forecast.el, the Emacs weather forecasts program."
  :group 'emacs
  :prefix "forecast-")

;;; Variables:
(defcustom forecast-city "Nil"
  "The city for which the forecast is given for.
Only for display purposes, variables `forecast-latitude' and
`forecast-longitude' still have to be set correctly."
  :type 'string
  :group 'forecast)

(defcustom forecast-country "Nil"
  "The country for which the forecast is given for.
Only for display purposes, variables `forecast-latitude' and
`forecast-longitude' still have to be set correctly."
  :type 'string
  :group 'forecast)

(defcustom forecast-latitude 0.0
  "The latitude of the location for which the forecast shall be generated"
  :type 'float
  :group 'forecast)

(defcustom forecast-longitude 0.0
  "The longitude of the location for which the forecast shall be generated"
  :type 'float
  :group 'forecast)

(defcustom forecast-api-key ""
  "The API Key from Forecast.io."
  :type 'string
  :group 'forecast)

(defcustom forecast-api-url "https://api.forecast.io"
  "Base url of the Forecast.io API.
Without the trailing slash."
  :type 'string
  :group 'forecast)

(defcustom forecast-time-format "%H:%M:%S, %F"
  "Format string for displaying timestamps.
See `format-time-string'."
  :type 'string
  :group 'forecast)

(defcustom forecast-units 'si
  "Sets the unit standard.
`si'  Standard units.
`us'  US Imperial units.
`ca'  Identical to si, but wind speed in km/h
`uk'  Identical to si, but wind speed is in miles/h, visibility in miles

Any other symbol means that the unit standard is automatically
selected based on the location."
  :type 'symbol
  :group 'forecast)

(defcustom forecast-language 'en
  "Language of the forecast (click the more link if in customisation buffer).
One of: ar (Arabic), bs (Bosnian), de (German), en (English,
which is the default), es (Spanish), fr (French), it (Italian),
nl (Dutch), pl (Polish), pt (Portuguese), ru (Russian),
sk (Slovak), sv (Swedish), tet (Tetum), tr (Turkish),
uk (Ukrainian), x-pig-latin (Igpay Atinlay), or zh (Chinese).

If not one of these, then `en' is selected."
  :type 'symbol
  :group 'forecast)

(defcustom forecast-graph-marker "█"
  "A single-character string for the graph marks."
  :type 'string
  :group 'forecast)

(defvar forecast--debug nil
  "Whether to surpress error messages.")

(defconst forecast--supported-languages
  '(ar bs de en  es fr it nl pl pt ru sk sv tet tr uk x-pig-latin or zh)
  "List of supported languages.")

(defvar forecast--data nil
  "Forecast data container.")

(defvar forecast--update-time 0
  "The time of last update to the buffer.
As per returned from `current-time'.")

(defvar forecast--buffer nil
  "The Forecast buffer object or name.")

;;; Faces:
(defface forecast-moon-phase
  nil
  "Face for visualisation of moon-phase.

Ideally, set the font family attribute to some font that supports
the characters 01F311-01F318, e.g. Quivira, which can be found at
<http://www.quivira-font.com/>:

\(set-face-attribute 'forecast-moon-phase nil
                     :font \"Quivira\")

On Linux, one can download the Quivira font and put that under
the $HOME/.fonts directory for using the font.  There are not
many fonts that support this character.  There are also the
BabelStone fonts.")

(defface forecast-upcoming-temperature
  nil
  "Face for the temperature part of the upcoming forecasts.")

(defface forecast-upcoming-summary
  nil
  "Face for the summary part of the upcoming forecasts.")

;;; Functions:
(defun forecast--assoca (keyseq list)
  "Arbitrary depth multi-level alist query.

KEYSEQ is the list of keys to look up in the LIST.  The first key
from KEYSEQ is looked up in the LIST, then the next key from
KEYSEQ is looked up in the CDR of the return value of that
operation, and so on until all the KEYSEQ is exhausted.  The
resultant value is returned, or nil, in case one or more keys are
not found in the LIST.

Examples:
\(forecast--assoca '(a b c)
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => e

\(forecast--assoca '(a t)
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => nil

\(forecast--assoca '(a o t)
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => nil

\(forecast--assoca nil
 '((a . ((b . ((c . e)
               (k . g)))
         (z . q)))
   (r . s)))
 => ((a (b (c . e) (k . g)) (z . q)) (r . s))."
  (let ((ks keyseq)
        (ret list))
    (dolist (k ks ret)
      (setq ret (cdr (assoc k ret))))))

(defun forecast--insert (str)
  "Insert STR to the buffer, at point.

Assume STR to be a unibyte string, convert it to multibyte, then
insert it."
  ;; XXX I do not really understand  why this works.  In my *scratch*,
  ;; I was able to  apply `string-as-multibyte' directly.  However, it
  ;; works.
  (insert (string-as-multibyte (string-as-unibyte str))))

(defun forecast--insert-with-props (text &rest props)
  "Insert the given string TEXT and set PROPS lock on it."
  (let ((p1) (p2))
    (setf p1 (point))
    (forecast--insert text)
    (setf p2 (point))
    (add-text-properties p1 p2 props)))

(defun forecast--insert-format (str &rest fa)
  "Apply format, then insert into the buffer.

STR is the format string.  FA are the arguments to format.  See
`format' for details."
  (forecast--insert (apply 'format str fa)))

(defun forecast--get-forecast (callback)
  "Get the forecasts from the Forecast.io API.

CALLBACK is a function of a single argument, WEATHER, the Elisp
representation of the returned JSON from the Forecast.io API."
  (let ((la forecast-latitude)
        (lo forecast-longitude)
        (request-url))
    ;; Make sure LO and LA end up being numbers.
    (when (not (cl-every #'numberp (list la lo)))
      (user-error "Forecast: Latitude and longitude have to be numbers"))
    ;; Check whether we're set for making an API call.
    (when (or (not forecast-api-key)
              (string-empty-p forecast-api-key))
      (user-error "Forecast: `forecast-api-key' not set"))
    (setf request-url
          (format "%s/forecast/%s/%d,%d?%s"
                  forecast-api-url
                  forecast-api-key
                  la lo
                  (forecast--api-opts)))
    (let ((cb (lambda (status &optional args)
                (ignore args)
                (let ((err (plist-get status :error)))
                  (when err
                    (apply 'signal err)))
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^{")
                  (beginning-of-line)
                  (funcall callback
                           (json-read-from-string
                            (buffer-substring (point)
                                              (point-max))))))))
      (url-retrieve request-url cb nil
                    forecast--debug
                    t ; Inhibit cookies, they are not necessary.
                    ))))

(defun forecast--api-opts ()
  "Generate API options string."
  (let ((f "%s=%s")
        (opts))
    (push (format f "units"
                  (cl-case forecast-units
                    (si "si")
                    (us "us")
                    (ca "ca")
                    (uk "uk2")
                    (otherwise  "auto")))
          opts)
    (push (format f "lang"
                  (symbol-name
                   (if (memq forecast-language forecast--supported-languages)
                       forecast-language
                     'en)))
          opts)
    (mapconcat 'identity opts "&")))

(defun forecast--load-data (callback)
  "Load the forecast data into `forecast--data'.

After the data is loaded, the CALLBACK function is called,
passing into it as the argument CBARG.

Arguments LAT, LONG and TIME are identical to those of
`forecast--get-forecast'.

Returns NIL, as it is asynchronous."
  (forecast--get-forecast (lambda (w)
                            (setq forecast--data w)
                            (when forecast--debug
                              (message "Forecast: loaded forecast data."))
                            (setf forecast--update-time (current-time))
                            (funcall callback))))

(defun forecast--summary ()
  "Return an human-readable summary of the current forecast."
  (forecast--assoca '(currently summary) forecast--data))

(defun forecast--temperature ()
  "Return the temperature from the current forecast.

If not available, i.e. not using 'currently, then return the
maximum."
  (or (forecast--assoca '(currently temperature) forecast--data)
      (forecast--assoca '(currently temperatureMax) forecast--data)))

(defun forecast--temperature-unit ()
  "Return the temperature unit.

Returns 'F for Fahrenheit, 'C for Centigrade."
  (cl-case forecast-units
    (us 'F)
    (otherwise  'C)))

(defun forecast--timezone ()
  "The time zone of the forecast."
  (forecast--assoca '(timezone) forecast--data))

(defun forecast--offset ()
  "The offset of the timezone of the forecast from GMT."
  (forecast--assoca '(offset) forecast--data))

(defun forecast--temperature-unit-string ()
  "Return the proper string for temperature unit."
  (cl-case (forecast--temperature-unit)
    (C "°C")
    (F "°F")))

(defun forecast--temperature-string ()
  "Return a string representing the current temperature.

The temperature, plus the degree sign, plus the unit in capital
letter."
  (format "%.0f%s"
          (forecast--temperature)
          (forecast--temperature-unit-string)))

(defun forecast--pressure (unit)
  "Return pressure in UNIT."
  (let ((p (forecast--assoca '(currently pressure) forecast--data)))
    (cl-case unit
      (bar p)
      (atm (forecast--bars-to-atm p))
      (otherwise (error "Forecast: unknown pressure unit: %s" unit)))))

(defun forecast--bars-to-atm (bars)
  "Convert pressure from BARS to ATM."
  (/ bars 1013.25))

(defun forecast--wind-speed ()
  "Return the value for the wind speed."
  (forecast--assoca '(currently windSpeed) forecast--data))

(defun forecast--wind-unit ()
  "Find the correct unit for the wind value."
  (cl-case forecast-units
    ((us uk) "mph")
    ( ca     "km/h")
    ( si     "m/s")))

(defun forecast--apparent-temperature ()
  "Feels-like temperature, truncated."
  (truncate (or (forecast--assoca '(currently apparentTemperature) forecast--data)
                (/ (+ (forecast--assoca '(currently apparentTemperatureMin) forecast--data)
                      (forecast--assoca '(currently apparentTemperatureMax) forecast--data))
                   2))))

(defun forecast--format-current-time (formats)
  "Format forecast's time with a format string.
FORMATS is the format string to use.  See `format-time-string'."
  (format-time-string
   formats
   (seconds-to-time (forecast--assoca '(currently time) forecast--data))))

(defun forecast--wind-direction ()
  "Calculate and return the direction of current wind."
  (if (zerop (forecast--wind-speed)) ""
    (let ((dir (forecast--assoca '(currently windBearing) forecast--data)))
      (upcase (symbol-name (forecast--cardinal-from-degrees dir))))))

(defun forecast--cardinal-from-degrees (d)
  "Turn degrees to one of 4 equivalent cardinal directions or a composed one.

D is a number value, degrees."
  (cl-case (truncate (/ d 22.5))
    (0  'n)
    (1  'n-ne)
    (2  'ne)
    (3  'e-ne)
    (4  'e)
    (5  'e-se)
    (6  'se)
    (7  's-se)
    (8  's)
    (9  's-sw)
    (10 'sw)
    (11 'w-sw)
    (12 'w)
    (13 'w-nw)
    (14 'nw)
    (15 'n-nw)
    (16 'n)
    ;; Wrap around..
    (otherwise (forecast--cardinal-from-degrees (- d 360)))))


(defun forecast--sun-position-graphic ()
  "Visualise the time since the rise of the sun and the time to the set thereof.

E.g.:

Quasi-midday:
>————————☉———————————<
Sunrise:
☉————————————————————<
Sunset:
>————————————————————☉"
  (let* ((today   (aref (forecast--assoca '(daily data) forecast--data) 0))
         (sunrise (forecast--assoca '(sunriseTime) today))
         (sunset  (forecast--assoca '(sunsetTime) today))
         (now     (float-time))
         (daylen  (- sunset sunrise))
         (sunsec  (- now sunrise))
         (wwidth  (window-body-width))
         (graph   (concat ">" (make-string (- wwidth 5) ?—) "<"))
         (sun     ?☉)
         (pos    (cond
                  ((< sunrise sunset now) (- wwidth 5))
                  ((> sunrise now) 0)
                  (t (truncate
                      (let ((x (/ sunsec (/ daylen wwidth))))
                        (- x 3)))))))
    (aset graph pos sun)
    graph))

(defun forecast--detailed-summary ()
  "The more detailed summary of the forecast."
  (forecast--assoca '(summary) (aref (forecast--assoca '(daily data) forecast--data) 0)))

(defun forecast--visualised-moon-phase ()
  "Visualise the moon phase w/ unicode characters.

See the face `forecast-moon-phase'"
  (let ((mp (forecast--assoca '(moonPhase)
                              (aref (forecast--assoca '(daily data) forecast--data) 0))))
    (cond ((zerop mp)  "🌑") ; New moon
          ((<  mp .25) "🌒") ; Waxing crescent moon
          ((=  mp .25) "🌓") ; First quarter moon
          ((<  mp .5)  "🌔") ; Waxing gibbous moon
          ((-  mp .5)  "🌕") ; Full moon
          ((<  mp .75) "🌖") ; Waning gibbous moon
          ((=  mp .75) "🌗") ; Last quarter moon
          ((<= mp  1)  "🌘") ; Waning crescent moon
          )))

(defun forecast--humidity ()
  "Humidity percentage."
  (* 100 (forecast--assoca '(currently humidity) forecast--data)))

(defun forecast--visibility ()
  "Visibility percentage."
  (let ((v (forecast--assoca '(currently visibility) forecast--data)))
    (when v
      (* 100 v))))

(defun forecast--insert-atmosphere-details ()
  "Insert details like pressure, humidity, visibility and wind."
  (forecast--insert-format
   "Pressure %1.3f atm; Humidity %.1f%%"
   (forecast--pressure 'atm)
   (forecast--humidity))
  (newline)
  (let ((v (forecast--visibility)))
    (when v
      (forecast--insert-format "Visibility %.1f%%; " v)))
  (forecast--insert-format
   "Wind %s %s, from %s"
   (forecast--wind-speed)
   (forecast--wind-unit)
   (forecast--wind-direction)))

(defun forecast--insert-upcoming ()
  "Forecasts about upcoming 7 days."
  (forecast--insert-with-props
   "Upcoming"
   'font-lock-face 'org-level-2)
  (newline)
  (let ((b forecast--data))
    (cl-loop for i from 1 to 7
             do
             (setcdr (assoc 'currently b)
                     (aref (forecast--assoca '(daily data) b) i))
             (let ((forecast--data b))
               (forecast--insert-with-props
                (forecast--format-current-time "%A")
                'font-lock-face 'org-level-3)
               (newline)
               (forecast--insert-with-props
                (forecast--temperature-string)
                'font-lock-face 'forecast-upcoming-temperature)
               (insert ", ")
               (forecast--insert-with-props
                (forecast--summary)
                'font-lock-face 'forecast-upcoming-summary)
               (newline)
               (forecast--insert-atmosphere-details)
               (newline 2)))))

(defun forecast--insert-io-link ()
  "Insert link to Forecast.io."
  (newline)
  (insert "Powered by")
  (insert " ")
  (insert-text-button
   "forecast.io"
   'follow-link t 'action
   (lambda (b)
     (ignore b)
     (browse-url "http://forecast.io"))))

(defun forecast--insert-location ()
  "Insert location details."
  (forecast--insert-with-props
   (format "Forecasts for %s, %s, %s"
           forecast-city
           forecast-country
           (forecast--format-current-time "%F"))
   'font-lock-face 'org-level-5)
  (newline)
  (forecast--insert-format "Lat: %f, Long: %f"
                           forecast-latitude
                           forecast-longitude))

(defun forecast--insert-update-time ()
  "Insert the last update time."
  (insert (format-time-string
           (concat "Last updated "
                   forecast-time-format)
           forecast--update-time))
  (forecast--insert-format "; %s, GMT+%d"
                           (forecast--timezone)
                           (forecast--offset)))

(defun forecast--insert-summary ()
  "Insert the summary of today's forecast."
  (forecast--insert-with-props
   (format "%s - %s"
           (forecast--temperature-string)
           (forecast--summary))
   'font-lock-face 'org-level-1)
  (newline)
  (forecast--insert-with-props
   (format "Feels like %d, %s"
           (forecast--apparent-temperature)
           (forecast--detailed-summary))
   'font-lock-face 'org-level-4))

(defun forecast--insert-sun-moon-graphic ()
  "Insert the combined sun phase and moon phase visualisations."
  (forecast--insert-with-props
   (forecast--sun-position-graphic)
   'intangible t)
  (forecast--insert-with-props
   (forecast--visualised-moon-phase)
   'font-lock-face 'forecast-moon-phase))

(defun forecast--insert-hourly-forecast ()
  "Insert a listing of hourly forecasts for today."
  ;; Display a graphic of hourly temperature.
  (let* ((data (forecast--assoca '(hourly data) forecast--data))
         (temps (mapcar (lambda (x)
                          (truncate
                           (forecast--assoca '(temperature) x)))
                        data))
         (min-today (apply 'min temps))
         (max-today (apply 'max temps))
         (x (1- (length temps))))
    (forecast--insert-with-props
     "Temperature graphic for the next 24 hours\n"
     'font-lock-face 'org-level-2)
    (forecast--insert-format "%4s \n" (forecast--temperature-unit-string))
    (dolist (i (number-sequence max-today min-today -1))
      (forecast--insert-format "%4d  " i)
      (cl-loop for j downfrom x to 0 do
            (insert
             (cond ((= i (nth j temps)) forecast-graph-marker)
                   ((= 0 (mod j 3))     "|")
                   ((cl-oddp i)         "-")
                   (t                   " "))))
      (newline))
    (insert "Hour: ")
    (cl-loop for j from 0 to (1- (length data)) by 3 do
          (let* ((time (seconds-to-time
                        (forecast--assoca '(time) (aref data j))))
                 (ts (format-time-string "%H" time)))
            (forecast--insert-format "%-3s" ts)))
    (newline)))

(defun forecast--make-buffer (buffername)
  "(Re)prepare the forecast buffer.

BUFFERNAME is the name of the forecast buffer to use.  Created if
absent."
  (with-current-buffer (get-buffer-create buffername)
    (setq forecast--buffer (current-buffer))
    (when (not buffer-read-only)
      (read-only-mode))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Begin inserting data to the buffer.
      (forecast--insert-location)
      (newline)
      (forecast--insert-update-time)
      (newline)
      (newline)
      (forecast--insert-summary)
      (newline)
      (forecast--insert-sun-moon-graphic)
      (newline)
      (forecast--insert-atmosphere-details)
      (newline 2)
      (forecast--insert-hourly-forecast)
      (newline)
      (forecast--insert-upcoming)
      (newline)
      (forecast--insert-io-link)

      ;; Finished preparing buffer.
      (goto-char (point-min))
      (forecast-mode))
    ;; Return the prepared buffer.
    (current-buffer)))

;;;###autoload
(defun forecast ()
  "Bring up the forecast buffer.
Keybindings for `forecast-mode':
\\{forecast-mode-map}"
  (interactive)
  (forecast--load-data
   (lambda ()
     (let ((buf (forecast--make-buffer "*Weather Forecast*")))
       (switch-to-buffer buf)))))

(defalias 'forecast-today 'forecast)

(defvar forecast-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (prog1 map
      (define-key map "g" 'forecast-refresh)
      (define-key map "q" 'forecast-quit))))

;;; Major mode and keybindings:
(define-derived-mode forecast-mode special-mode
  "Weather Forecast Mode"
  "Major mode for weather forecast buffers.
Keybindings for `forecast-mode':
\\{forecast-mode-map}"
  (kill-all-local-variables)
  (use-local-map forecast-mode-map)
  (buffer-disable-undo))

(defun forecast-refresh ()
  "Refresh the current forecast buffer."
  (interactive)
  (when (not forecast--buffer)
    (user-error "Run `forecast' instead"))
  (forecast--load-data
   (lambda ()
     (forecast--make-buffer (buffer-name forecast--buffer)))))

(defun forecast-quit (&optional quit)
  "Put away the Forecast buffer.

If QUIT is non-nil or the universal argument is non-nil, kill the
buffer."
  (interactive "^P")
  (quit-window quit))

(provide 'forecast)
;;; forecast.el ends here
