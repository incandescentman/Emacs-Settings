;;; writegood-mode.el --- Polish up poor writing on the fly
;;
;; Author: Benjamin Beckwith
;; Created: 2010-8-12
;; Version: 1.3
;; Last-Updated: 2014-2-13
;; URL: http://github.com/bnbeckwith/writegood-mode
;; Keywords: writing weasel-words grammar
;; Compatability:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This minor mode tries to find and highlight problems with your
;;  writing (in english).
;;
;;  Behavior inspired by the weaselwords scripts to aid in good
;;  writing.
;;  http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 1.3 Several pull requests added, comments checked, passive voice regexp fixed
;; 1.2 Fixed weasel-words regexp to have word boundaries
;; 1.1 Fixed regexps to be multiline.
;; 1.0 Initial version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Test Text:
;;
;; This mode will improve various aspects of your writing in many ways.
;; With this mode, text within comments will be searched for the
;; the duplicate problem.
;; The text is searched and aspects (even within comments) are 
;; highlighted. 
;; Another benefit is the the finding of duplicates.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'regexp-opt)
(require 'faces)

(defgroup writegood nil
  "Minor mode for highlighting bad english writing."
  :prefix "writegood-"
  :group 'help
  :link '(url-link "http://github.com/bnbeckwith/writegood-mode"))

(defconst writegood-version "1.2"
  "WriteGood mode version")

;; Weaselwords
(defface writegood-weasels-face
  '((((class color) (background light))
     (:inherit font-lock-warning-face :background "moccasin"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DarkOrange")))
  "Writegood face for weasel words"
  :group 'writegood)

(defcustom writegood-weasel-words
  '("very" "fairly" "several" "extremely" 
    "exceedingly" "quite" "great" 
      "are a number" "is a number" 
      "relatively" "many" "awesome"
    "not rocket science" "outside the box"
"20/20 hindsight"
"800 pound gorilla"
"a few cans short of a six-pack"
"a leading provider of  "
"a plus for all stakeholders"
"ace in the hole"
"ace up your sleeve"
"airing dirty laundry"
"all booster no payload"
"all foam no beer"
"all foam no beer"
"all hammer no nail"
"all hat no cattle"
"all icing no cake"
"all in a day's work"
"all lime and salt no tequila"
"all missile no warhead"
"all shot no powder"
"all sizzle no steak"
"all talk no action"
"all that and a bag of chips"
"all thumbs"
"all wax and no wick"
"all wet"
"all's fair in love and war"
"almighty dollar"
"always a bridesmaid"
"ambulance chaser"
"another day another dollar"
"ants in your pants"
"apple-pie order"
"arm and a leg"
"armchair quarterback"
"army brat"
"art imitates life"
"artsy-craftsy"
"artsy-fartsy"
"as luck would have it"
"as old as time"
"at loggerheads"
"at the end of the day   "
"babe in the woods"
"back against the wall"
"back in the saddle"
"back to square one"
"back to the drawing board"
"bad to the bone"
"badge of honor"
"badonkadonk"
"ballpark figure"
"balls to the wall"
"baptism of fire"
"bare bones"
"bark is worse than the bite"
"bark up the wrong tree"
"bat out of hell"
"bats in the belfry"
"battle royal"
"beat around the bush"
"beat the bushes"
"beats me"
"behind the eight ball"
"benchmark"
"bent out of shape"
"best foot forward"
"best of breed"
"best practices"
"bet your bottom dollar"
"better half"
"better late than never"
"better mousetrap"
"better safe than sorry"
"better than ever"
"better the devil you know"
"between a rock and a hard place"
"beyond the pale"
"bib and tucker"
"big as life"
"big fish in a small pond"
"big man on campus"
"bigger they are"
"bird in the hand"
"birds and the bees"
"birds of the feather"
"bite the dust"
"bite your tongue"
"bitter disappointment"
"black as coal"
"blast from the past"
"bleeding heart"
"blind as a bat"
"blood is thicker than water"
"blood money"
"blood on your hands"
"blood sweat and tears"
"blow this pop stand / joint"
"blushing bride"
"body by fisher brains by mattel"
"boil it down to"
"bone of contention"
"boots on the ground"
"booze and broads"
"bored to tears"
"born and raised"
"born with a silver spoon in your mouth"
"born yesterday"
"bottom line"
"brain drain"
"brain dump"
"brass tacks"
"bring home the bacon"
"bring our 'a' game"
"brings a lot of value to the table"
"broken record"
"brother's keeper"
"bull by the horns"
"bull in a china shop"
"bump in the night"
"busy as a bee"
"but seriously"
"by and large"
"calm before the storm"
"can't cut the mustard"
"candle at both ends"
"case of mistaken identity"
"cast a wider net"
"cat got your tongue"
"cat out of the bag"
"caught red-handed"
"change agent"
"chapter and verse"
"checkered career"
"chickens come home to"
"chomping at the bit"
"circle back"
"cleanliness is next to"
"clear as a bell"
"clear as mud"
"client-centered"
"cold shoulder"
"communist conspiracy"
"conniption fit"
"core competency"
"could care less"
"couldn't care less"
"couldn't get to first base"
"count your blessings"
"countless hours"
"creature comfort"
"crime in the street"
"curiosity killed the cat"
"curry favor"
"cut a fine figure"
"cut and dried"
"cut to the chase"
"cut to the quick"
"cute as a button"
"darkest before the dawn"
"dead as a doornail"
"death and destruction"
"death and taxes"
"death's doorstep"
"devil is in the details"
"dim view"
"do no evil"
"dog days"
"dog in the manger"
"don't count your chickens before they're hatched"
"don't do the crime if you can't do the time"
"don't throw out the baby with the bathwater"
"doubting thomas"
"down and dirty"
"down in the dumps"
"down pat"
"down the drain/toilet"
"down the hatch"
"down to earth"
"downsizing"
"drinking kool-aid"
"drive you up a wall"
"drop the ball"
"dutch uncle"
"dyed in the wool"
"e-ticket"
"ear to the ground"
"early bird catches the worm"
"easier said than done"
"easy as 1-2-3"
"easy as pie"
"eat crow"
"eat humble pie"
"eco-anything"
"enough already"
"even money"
"every dog has its day"
"every fiber of my being"
"everything but the kitchen sink"
"evil twin"
"existential angst"
"experts agree "
"eye for an eye"
"facts of life"
"fair weather friend"
"fair-haired one"
"fall off of a turnip truck"
"faster cheaper better"
"fat slob"
"favor us with a song"
"fear and loathing"
"feather your nest"
"fellow traveler"
"few and far between"
"field this one"
"fifteen minutes of fame"
"fish nor fowl"
"fly by night"
"fly the coop"
"for the birds"
"fox in the henhouse"
"freudian slip"
"from a professional standpoint"
"fun and games"
"fun in the sun"
"game changer"
"garbage in garbage out"
"get the sack"
"get your groove back"
"gets my goat"
"gift horse in the mouth"
"gilding the lily"
"give a damn"
"give me a break"
"gives me the creeps"
"giving 110%"
"go him one better"
"godliness"
"goes without saying"
"going forward"
"going from good to great"
"good deed for the day"
"good time was had by all"
"greek to me"
"green thumb"
"green-eyed monster"
"grist for the mill"
"guiding light"
"hair of the dog"
"hard to believe"
"has legs and can go really far  "
"have a nice day"
"he couldn't pour water out of a boot with instructions printed on the heel"
"he couldn't think his way out of a paper bag"
"he donated his brain to science  science sent it back "
"he fell out of the stupid tree and hit every branch on his way down"
"he's a walking advertisement/poster boy for birth control/planned parenthood"
"he's as useful as tits on a wart/boar hog"
"he's dumber than a bag of hammers"
"he's dumber than a box of rocks"
"he's not the brightest crayon in the box "
"he's not the sharpest knife in the drawer "
"head honcho"
"heart's content"
"hell-bent for leather"
"hidden agenda"
"high and the mighty"
"high on the hog"
"his bread ain't done"
"his elevator doesn't go to the top floor"
"his pilot light isn't lit"
"his yeast went bad"
"hold a candle to"
"hold your horses"
"hold your tongue"
"hook or by crook"
"horse of a different color"
"hot knife through butter"
"how goes the battle?"
"i beg to differ"
"i don't have the bandwidth"
"i'm not throwing him under the bus but"
"i'm okay you're okay"
"if brains were dynamite he couldn't blow his nose"
"if the shoe fits"
"if you gave him a penny for his thoughts you'd get change "
"impactful"
"improve roi"
"in a nutshell"
"in a pinch"
"in a wink"
"in harm's way"
"in the tank"
"in today's highly competitive marketplace"
"in your dreams"
"in your face"
"industry leader"
"industry standard"
"inexorably drawn"
"influence peddling"
"info dump"
"integrated approach"
"intents and purposes"
"it is what it is"
"it takes him an hour to cook minute rice"
"it was a dark and stormy night"
"it won't fly"
"jack of all trades"
"jockey for position"
"johnny-come-lately"
"joined at the hip"
"jump down your throat"
"jump her/his bones"
"jump in with both feet"
"jump on the bandwagon"
"jump the gun"
"junk in the trunk"
"jury is still out"
"justice is blind"
"keep an eye on you"
"keep it down"
"keep it simple stupid"
"keep up with the joneses"
"keep your cards close to vest "
"keep your chin up"
"keep your fingers crossed"
"keep your powder dry"
"kick ass"
"kick the bucket"
"kick up your heels"
"kick you to the curb"
"kick your feet up"
"kickbutt"
"kid in a candy store"
"kill two birds with one stone"
"king's english"
"king's ransom"
"kiss and tell"
"kiss ass"
"kiss of death"
"kit and kaboodle"
"knee-high to a grasshopper"
"knock it out of the park"
"knock on wood"
"knock your socks off"
"knocked up"
"know him from adam"
"know the ropes"
"know the score"
"knuckle down"
"knuckle sandwich"
"knuckle under"
"labor of love"
"lap of luxury"
"last but not least"
"last hurrah"
"last-ditch effort"
"law of the jungle"
"law of the land"
"lay down the law"
"leaps and bounds"
"let sleeping dogs lie"
"let the cat out of the bag"
"let's be proactive here people"
"let's get granular"
"let's hit the ground running"
"let's split"
"let's take this off-line"
"liberal media"
"lie like a rug"
"life and limb"
"life imitates art"
"life's a bitch"
"lighten up"
"lights out"
"like a sore thumb"
"like butter"
"like the plague"
"like there's no tomorrow"
"lion's share"
"litmus test"
"little black book"
"live and learn"
"long and short of it"
"long lost love"
"look before you leap"
"lounge lizard"
"loved and lost"
"low man on the totem pole"
"low-hanging fruit"
"luck of the draw"
"luck of the irish"
"make my day"
"male chauvinist"
"man's best friend"
"manage expectations"
"many moons"
"many-splendored thing"
"mark my words"
"marketing-driven"
"maximize customer satisfaction"
"maximize leverage"
"meaningful relationship"
"mellow out"
"moment of glory"
"moment's respite"
"monday morning quarterback"
"monkey see monkey do"
"monkey suit"
"motherhood and apple pie"
"movers and shakers"
"moving experience"
"moving up the value chain"
"multi task"
"my two cents"
"neat as a pin"
"needless to say"
"net-net"
"next generation"
"nip it in the bud"
"no guts no glory"
"no love lost"
"no pain no gain"
"no stone unturned"
"no time like the present"
"nose to the grindstone"
"not in my back yard"
"not on your tintype"
"now more than ever"
"number one fan"
"numerous to mention"
"off the wagon"
"offline"
"old college try"
"old meets new"
"older and wiser"
"older than dirt"
"older than methuselah"
"on steroids"
"on the bandwagon"
"on the nose"
"on the wagon"
"on thin ice"
"one born every minute"
"one brick shy of a load"
"one foot in the grave"
"one fry short of a happy meal"
"one in a million"
"one pickle short of a barrel"
"one sandwich short of a picnic"
"one step short of a flight"
"one taco short of a combination plate"
"only game in town"
"only to be met"
"out of pocket"
"out of the frying pan"
"out on a limb"
"out-of-pocket"
"p's and q's"
"pain and suffering"
"panic button"
"paradigm shift"
"party pooper"
"pass the sniff test"
"patter of little feet"
"pay through the nose"
"peas in a pod"
"perfect storm"
"pig in a poke"
"pillar of society"
"pinko"
"plenty of fish in the sea"
"poison pen"
"poor as a churchmouse"
"poor excuse for"
"pot calling the kettle black"
"proud possessor"
"pull through in the end"
"push the envelope"
"put a stake in the ground"
"put my/your foot down"
"putting lipstick on a pig"
"quick and the dead"
"quick as a bunny"
"radical chic"
"rags to riches"
"raining buckets"
"raining cats and dogs"
"raise the bar"
"rank and file"
"read my lips"
"red herring"
"redheaded stepchild"
"reign supreme"
"remember the alamo"
"resonate"
"rightsizing"
"road to hell is paved with good intentions"
"rob peter to pay paul"
"robust"
"rock and a hard place"
"rocket science/scientist"
"roost"
"rope a dope"
"run it up the flagpole"
"run it up the flagpole and see who salutes"
"running dog lackey"
"safe than sorry"
"salt of the earth"
"save face"
"scalable"
"scared stiff"
"scared to death"
"school's out"
"screaming meemies"
"seamless integration"
"senses reel"
"set the record straight"
"shake a stick should of"
"shapes and forms"
"shoulder to the wheel"
"shouldered his way"
"shut your trap"
"sigh of relief"
"significant other"
"silence is golden"
"six sigma"
"slept like a log"
"small world"
"snake in the grass"
"snow job"
"snug as a bug"
"some of my best friends"
"something the cat dragged in"
"somewhere a villiage is missing its idiot"
"spade a spade"
"spare the rod"
"spending more time with my family"
"spitting image"
"spring to life"
"squeaky wheel gets the grease/oil"
"start from scratch"
"step up to the plate"
"stick in the mud"
"stick in your craw"
"still waters run deep"
"stop and smell the roses"
"store bought"
"stranger than fiction"
"strategic"
"straw that broke the camel's back"
"stubborn as a mule"
"stuff that dreams are made of"
"stuffed shirt"
"synergy"
"take it to the next level"
"take one for the team"
"take the bull by the horns"
"take the plunge"
"takes one to know one"
"talk turkey"
"team player"
"teamwork"
"ten foot pole"
"the 80%-20% rule"
"the bottom line"
"the cheese slid off his cracker"
"the customer is always right"
"the earth moved"
"the elephant in the room"
"the final analysis"
"the gates are down the lights are flashing but there is no train "
"the light is on but nobody is home"
"the real mccoy"
"the same old story"
"the scenery only changes for the lead dog"
"the starting gate is open but he's still asking directions"
"the wheel is turning but the hamster is dead "
"there's nothing in the attic but cobwebs "
"these things happen"
"thick as thieves"
"think outside of the box"
"thinking outside of the box"
"third time's the charm"
"this day and age"
"this point in time"
"thought leader"
"three strikes and you're out"
"through the grapevine"
"throw in the towel"
"tiger by the tail"
"till the fat lady sings"
"time and time again"
"time is of the essense"
"tip of the iceberg"
"to err is human"
"to the best of my knowledge"
"today more than ever  "
"tongue-in-cheek"
"too hot to handle"
"too many chiefs and not enough indians"
"touch base"
"touch of blarney"
"tough as nails"
"tough luck"
"tough row to hoe"
"traditional family values"
"transparency"
"trials and tribulations"
"tried and true"
"trip down memory lane"
"true blue"
"turn your smile/frown upside-down"
"twist of fate"
"twists and turns"
"two to tango"
"under the gun"
"under the same roof"
"understated elegance"
"unexpected twist"
"until the cows come home"
"up his sleeve"
"up the creek"
"up the wrong tree"
"user-experience"
"user-focused"
"value-added proposition"
"very real concern"
"view with alarm"
"viral"
"wakeup call"
"was my face red"
"watch your tongue"
"web 2 0"
"web of intrigue"
"week of sundays"
"what a bummer"
"what comes around goes around"
"what the cat dragged in"
"what the dickens"
"what the heck/hell"
"what you see is what you get"
"what's not to like"
"wheeler-dealer"
"when in doubt punt"
"when push comes to shove"
"when rubber meets the road"
"when the cat's away"
"when the going gets tough the tough get going"
"who has everything"
"whole ball of wax"
"whole hog"
"whole nine yards"
"whole other story"
"wild goose chase"
"will wonders never cease"
"win friends and influence"
"win one for the gipper"
"wisdom of the ages"
"without benefit of clergy"
"wnning is everything"
"wolf at the door"
"words fail"
"work like a dog"
"worst nightmare"
"wrong side of the bed"
"years young"
"you can run but you can't hide"
"you know what they say"
"young and foolish"
"young and restless"


)
  "The weasel words to use"
  :group 'writegood
  :type 'list)
  
(defvar writegood-weasels-font-lock-keywords-regexp
  (concat "\\b" (regexp-opt writegood-weasel-words) "\\b")
  "Matches weasel-words")

(defvar writegood-weasels-font-lock-keywords
  (list (list writegood-weasels-font-lock-keywords-regexp
	      0 (quote 'writegood-weasels-face) 'prepend)))

;; Passive Voice
(defface writegood-passive-voice-face
  '((((class color))
     (:inherit font-lock-warning-face :background "LemonChiffon")))
  "Writegood face for passive-voice"
  :group 'writegood)

(defcustom writegood-passive-voice-irregulars
  '("awoken" "been" "born" "beat" "become" "begun" "bent" "beset" 
    "bet" "bid" "bidden" "bound" "bitten" "bled" "blown" "broken" 
    "bred" "brought" "broadcast" "built" "burnt" "burst" "bought" 
    "cast" "caught" "chosen" "clung" "come" "cost" "crept" "cut" 
    "dealt" "dug" "dived" "done" "drawn" "dreamt" "driven" "drunk" 
    "eaten" "fallen" "fed" "felt" "fought" "found" "fit" "fled" 
    "flung" "flown" "forbidden" "forgotten" "foregone" "forgiven" 
    "forsaken" "frozen" "gotten" "given" "gone" "ground" "grown" 
    "hung" "heard" "hidden" "hit" "held" "hurt" "kept" "knelt" "knit" 
    "known" "laid" "led" "leapt" "learnt" "left" "lent" "let" "lain" 
    "lighted" "lost" "made" "meant" "met" "misspelt" "mistaken" "mown" 
    "overcome" "overdone" "overtaken" "overthrown" "paid" "pled" "proven" 
    "put" "quit" "read" "rid" "ridden" "rung" "risen" "run" "sawn" 
    "said" "seen" "sought" "sold" "sent" "set" "sewn" "shaken" "shaven" 
    "shorn" "shed" "shone" "shod" "shot" "shown" "shrunk" "shut" 
    "sung" "sunk" "sat" "slept" "slain" "slid" "slung" "slit" 
    "smitten" "sown" "spoken" "sped" "spent" "spilt" "spun" "spit" 
    "split" "spread" "sprung" "stood" "stolen" "stuck" "stung" 
    "stunk" "stridden" "struck" "strung" "striven" "sworn" "swept" 
    "swollen" "swum" "swung" "taken" "taught" "torn" "told" "thought" 
    "thrived" "thrown" "thrust" "trodden" "understood" "upheld" "upset" 
    "woken" "worn" "woven" "wed" "wept" "wound" "won" "withheld" 
    "withstood" "wrung" "written")
  "List of passive voice irregular verbs"
  :group 'writegood
  :type 'list)

(defvar writegood-passive-voice-font-lock-keywords-regexp
  (concat "\\b\\(am\\|are\\|were\\|being\\|is\\|been\\|was\\|be\\)\\b\\([[:space:]]\\|\\s<\\|\\s>\\)+\\([[:word:]]+ed\\|"
	  (regexp-opt writegood-passive-voice-irregulars)
	  "\\)\\b")
  "Font-lock keywords regexp for passive-voice")

(defvar writegood-passive-voice-font-lock-keywords
  (list (list writegood-passive-voice-font-lock-keywords-regexp
	      0 (quote 'writegood-passive-voice-face) 'prepend)))

;; Duplicates
(defface writegood-duplicates-face
  '((((class color) (background light))
     (:inherit font-lock-warning-face :background "MistyRose"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DeepPink")))
  "Writegood face for duplicate words"
  :group 'writegood)

(defvar writegood-duplicates-font-lock-keywords-regexp
  "\\b\\([[:word:]]+\\)\\([[:space:]]\\|\\s<\\|\\s>\\)+\\1\\b"
  "Font-lock keywords for duplicates")

(defvar writegood-duplicates-font-lock-keywords
  (list (list writegood-duplicates-font-lock-keywords-regexp
	      0 (quote 'writegood-duplicates-face) 'prepend)))

;;;;;;;;;;;;;;;;;;;; Functions:

(defun writegood-version ()
  "Tell the version you are using"
  (interactive)
  (message writegood-version))

(defun writegood-weasels-turn-on ()
  "Turn on syntax highlighting for weasels"
  (font-lock-add-keywords nil writegood-weasels-font-lock-keywords))

(defun writegood-passive-voice-turn-on ()
  "Turn on warnings for passive voice"
  (font-lock-add-keywords nil writegood-passive-voice-font-lock-keywords))

(defun writegood-duplicates-turn-on ()
  "Turn on warnings for duplicate words"
  (font-lock-add-keywords nil writegood-duplicates-font-lock-keywords))

(defun writegood-weasels-turn-off ()
  "Turn on syntax highlighting for weasels"
  (font-lock-remove-keywords nil writegood-weasels-font-lock-keywords))

(defun writegood-passive-voice-turn-off ()
  "Turn on warnings for passive voice"
  (font-lock-remove-keywords nil writegood-passive-voice-font-lock-keywords))

(defun writegood-duplicates-turn-off ()
  "Turn on warnings for duplicate words"
  (font-lock-remove-keywords nil writegood-duplicates-font-lock-keywords))

(defun writegood-turn-on ()
  "Turn on writegood-mode."
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (writegood-weasels-turn-on)
  (writegood-passive-voice-turn-on)
  (writegood-duplicates-turn-on))

(defun writegood-turn-off ()
  "Turn off writegood-mode."
  (writegood-weasels-turn-off)
  (writegood-passive-voice-turn-off)
  (writegood-duplicates-turn-off))

;;;###autoload
(define-minor-mode writegood-mode
  "Colorize issues with the writing in the buffer."
  :lighter " Wg"
  (progn
    (if writegood-mode
	(writegood-turn-on)
      (writegood-turn-off))
    (font-lock-mode 1)))

(provide 'writegood-mode)

;;; writegood-mode.el ends here
