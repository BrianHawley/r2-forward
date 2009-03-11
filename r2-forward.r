REBOL [
	Title:  "REBOL 3 Forward Compatibility Functions"
	File: %r2-forward.r
	Type: 'module
	Purpose: "Make REBOL 2 more compatible with REBOL 3."
	Author: "Brian Hawley" ; BrianH
	License: {
		Copyright (c) 2008-2009 Brian Hawley
		
		Permission is hereby granted, free of charge, to any person obtaining a copy
		of this software and associated documentation files (the "Software"), to deal
		in the Software without restriction, including without limitation the rights
		to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
		copies of the Software, and to permit persons to whom the Software is
		furnished to do so, subject to the following conditions:
		
		The above copyright notice and this permission notice shall be included in
		all copies or substantial portions of the Software.
		
		THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
		IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
		FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
		AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
		LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
		OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
		THE SOFTWARE.
	} ; MIT
	Exports: [
		; Function creators
		funco
		func
		function
		funct
		functor
		does
		has
		;closure
		; Error management
		cause-error
		; Datatype spoofing
		map! map? to-map
		get-path! get-path? to-get-path
		;closure! closure? to-closure
		typeset! typeset? to-typeset
		any-path! any-path?
		scalar! scalar?
		; Control functions
		!
		++
		--
		also
		quote
		true?
		default
		apply
		eval
		;delect
		; Object functions
		object
		extend
		resolve
		; Series functions
		ajoin
		first+
		append
		swap
		take
		move
		array
		extract
		replace
		alter
		map
		collect
		; Character/string encoding functions
		ascii?
		latin1?
		utf?
		deline
		enline
		; File functions
		what-dir
		info?
		dir?
		exists?
		undirize
		list-dir
		ls
		pwd
		rm
		mkdir
		cd
		more
		in-dir
		to-relative-file
		; Reflection functions
		reflect
		spec-of
		body-of
		words-of
		values-of
		types-of
		title-of
		; Profiling functions
		dt delta-time
		;dp delta-profile
	] ; No Globals to limit any potential damage.
]

; Note: The functions in this file are backports from R3, and will stay as
; compatible with R3 as possible. Don't add functions that existed in 2.7.5
; unless they are not compatible with their behavior in 2.7.5.
; 
; R3's current behavior will be tracked as best as it is feasible, and any
; missing functions or changed behavior will be added eventually. In the short
; term some proposed features/changes will be added if they seem likely to be
; accepted (the acceptance process can be a little slow sometimes).
; 
; Intentionally not supported (in some cases impossible in R2):
; - Unicode codepoints over 255 in string! and char!.
; - Conversion from UTF-8 binary! to string! of Unicode codepoints over 127.
; - Conversion from string! to UTF-8 binary! of Unicode codepoints over 127.
; - Functions related to the new port model.
; - Functions related to the new graphics model.
; - Functions related to other new types I can't spoof (task!, utype!, ...).
; - Functions or types for guru or internal use (evoke, stack, native, ...).
; - Functions that call chat or the new docs.
; - Changes or fixes to datatypes already in R2 (string!, port!, error!, ...).
; - Changed or fixed R2 natives (there will be another file for those).
; - Compatibility patches to R2 GUI or port code (there will be other files
;   for those, maybe, as possible or needed).
; - Pre-2.6.2 changes (there will be another file for those).
; - Syntax changes (percent!, get-word meaning get/any, ...).
; 
; Unless otherwise marked, all functions are based on the behavior of their
; R3 counterparts (written by Carl if native, by BrianH if not), and are
; all new code written by BrianH. Some were suggested or proposed by others,
; and will be marked as such. The doc strings are copied from R3.

; History:
; 27-Jan-2009: Initial version, plus initial changes.
; - Moved the post-2.7.5 new R2 functions I wrote in 2008-2009 here.
; - Added MAP/into, SUBSTITUTE and DEFAULT to the future additions.
; 3-Feb-2009: Tweaking future additions based on community input.
; - Renamed SUBSTITUTE to REWORD on Carl's request.
; - Added FILEIZE, FILE-EXISTS? and DIR-EXISTS? to future additions.
; 6-Feb-2009: Tweaking future additions based on community input.
; - Renamed FILEIZE to UNDIRIZE.
; - Replaced FILE-EXISTS? and DIR-EXISTS? with new EXISTS? function.
; 12-Feb-2009: Community requests and backports.
; - QUOTE, ACCUMULATE, COLLECT, and GATHER added to future additions.
; - FUNCT fixed for 'a and :a parameters, and a: directives.
; - Backported CAUSE-ERROR and the whole R3 reflection model.
; 13-Feb-2009: Friday the 13th, prep for release.
; - Made the header compatible with Gabriele's R2 modules.
; - Clarified licensing, authorship and attribution.
; - Tweaked the comments and ported the history from DevBase.
; - Created a detab-and-copy script so DevBase and Qtask sync.
; 14-Feb-2009: Backported APPLY. Compatibility comment for QUOTE.
; 15-Feb-2009: Implemented MAP with full ideal R3 behavior. Exported APPLY.
; 16-Feb-2009: The rest of the practical, for now.
; - Spoofing of MAP!, GET-PATH!, TYPESET!, ANY-PATH! and SCALAR!.
; - Backported the remaining function creators, !, ++, --, TRUE?,
;   OBJECT, EXTEND, ASCII?, LATIN1? (partial), UTF?, DELINE, ENLINE,
;   WHAT-DIR, LIST-DIR, LS, PWD, RM, MKDIR, CD, MORE, DT & DELTA-TIME.
; - Enhanced ARRAY, EXTRACT, REPLACE, ALTER (some long ago).
; - Formatting and documentation tweaks.
; 17-Feb-2009: Fix to the platform numbers in ENLINE (silly bug), comments.
; 20-Feb-2009: Tweaks.
; - Fixed ASCII? and LATIN1? (bug-for-bug compatible with R3 now).
; - Backported EVAL (don't know what it's used for, but it's in the docs).
; - Changed 'script 'invalid-arg error to 'invalid (R2 compatible, need docs).
; - Removed error! from default parameter types in TYPES-OF (R2 compatible).
; - Added R3 lit-word! parameter compatiblity paren! trick to FIRST+.
; 21-Feb-2009: More coverage of the newly semi-documented R3 function set.
; - Backported SWAP, INFO?, DIR? (partial), ALTER/case, and promoted COLLECT.
; - Replaced the PRINTF call in LIST-DIR since R2 doesn't have PRINTF yet.
; - Minor rearrangement in the order of functions for better grouping.
; - Filled in the rest of my todo list (commented exports above).
; - Added a section to the notes about what changes won't be done here.
; 22-Feb-2009: Added proposed /with option to ENLINE, modeled after WRITE/with.
; 25-Feb-2009: Fixes...
; - Changed 'script 'invalid error back to 'invalid-arg (found docs).
; - Nicer and more specific errors for TO-TYPESET, ++, --, APPLY, TAKE, ARRAY,
;   EXTRACT, MAP, LIST-DIR, CD, MORE and REFLECT.
; - Backported new APPEND. Put RESOLVE on todo list.
; 25-Feb-2009: New R3 acceptances.
; - Promoted QUOTE, DEFAULT, EXISTS? and UNDIRIZE. EXISTS? now uses new INFO?.
; - Reworked the error handling of the *-OF reflectors to better match R3.
; 26-Feb-2009: Backported RESOLVE.
; 28-Feb-2009: Tweaks.
; - R3's WORDS-OF binds to an object, so changed WORDS-OF to match.
; - Tweaked APPLY and RESOLVE for new WORDS-OF behavior.
; 10-Mar-2009: Removed catch from APPLY function spec to aid debugging.
; 11-Mar-2009: EXTRACT tweaked to work around a bug in PARSE.


; Function creation functions

funco: make function! [
	"Defines a function, but does not copy spec or body."
	spec [block!] "Help string (opt) followed by arg words (and opt type and string)"
	body [block!] "The body block of the function"
][ ; For functions known to have no syntax errors or recursive issues.
	make function! spec body
]
; Carl wrote the R3 version, partly copied here.

func: funco [
	"Defines a user function with given spec and body."
	[catch]
	spec [block!] "Help string (opt) followed by arg words (and opt type and string)"
	body [block!] "The body block of the function"
][
	throw-on-error [make function! copy/deep spec copy/deep body]
]
; Carl wrote the R3 version, partly copied here.

function: funco [
	"Defines a user function with local words."
	[catch]
	spec [block!] "Optional help info followed by arg words (and optional type and string)"
	vars [block!] "List of words that are local to the function"
	body [block!] "The body block of the function"
][
	throw-on-error [make function! copy/deep compose [(spec) /local (vars)] copy/deep body]
]
; Carl wrote the R3 version, partly copied here.

funct: funco [
	"Defines a user function assuming all set-words are locals."
	[catch]
	spec [block!] "Help string (opt) followed by arg words (and opt type and string)"
	body [block!] "The body block of the function"
	/local r ws wb a
][
	spec: copy/deep spec
	body: copy/deep body
	; Get the words in the spec (ws) as word! so we can screen them out later
	ws: make block! length? spec
	parse spec [any [
		set-word! | set a any-word! (insert tail ws to-word a) | skip
	]]
	; Get any set-words in the code block as words (wb)
	wb: copy ws  ; Start with ws so we can remove ws with difference
	parse body r: [any [
		set a set-word! (
			unless find wb a: to-word a [insert tail wb a]
		) |
		hash! | into r | skip
	]]
	; Remove the ws words from wb and add the rest as locals
	unless empty? wb: difference ws wb [
		remove find wb 'local
		unless find spec /local [insert tail spec /local]
		insert tail spec wb
	]
	throw-on-error [make function! spec body]
]
; Note: The set-word! collection and spec word screening is native in R3.
; All new code based on an initial R3 version from Carl.

functor: funco [
	"Defines a user function with all set-words collected into a persistent object (self)."
	[catch]
	spec [block!] "Help string (opt) followed by arg words (and opt type and string)"
	body [block!] "The body block of the function"
	/local r wb a
][
	wb: copy []  ; Get any set-words in the code block (wb)
	parse body r: [any [
		set a set-word! (unless find wb a [insert tail wb a]) |
		hash! | into r | skip
	]]
	remove find wb [self:]  ; Remove self: for object construction safety
	throw-on-error [make function! copy/deep spec  bind/copy body construct wb]
]
; Note: The set-word! collection is native in R3, the whole function one line.
; Function parameter words override the bindings of persistent object words,
; but the words still exist in the object and can be accessed through self.
; R3 version based on a suggestion from Carl, written by BrianH.

does: funco [
	"A shortcut to define a function that has no arguments or locals."
	[catch]
	body [block!] "The body block of the function"
][
	throw-on-error [make function! copy [] copy/deep body]
]
; Carl wrote the R3 version, partly copied here.

has: funco [
	"A shortcut to define a function that has local variables but no arguments."
	[catch]
	vars [block!] "List of words that are local to the function"
	body [block!] "The body block of the function"
][
	throw-on-error [make function! head insert copy/deep vars /local copy/deep body]
]
; Carl wrote the R3 version, partly copied here.


; Error management

cause-error: funco [
	"Causes an immediate error throw with the provided information."
	err-type [word!]
	err-id [word!]
	args
	/local err
][
	err: insert insert make block! 5 err-type err-id
	case [
		block? :args [
			insert/part err args 3
			remove-each x err [any-function? get/any 'x]
		]
		any-function? :args []
		:args [insert/only err :args]
	]
	throw make error! head err
]
; Note: Some of the errors have changed names between R2 and R3.
; All new code based on an initial R3 version from Carl.


; Datatype spoofing (be careful)

; Fake map! with hash!
map!: :hash!  ; Doesn't work in function specs, TYPE?
map?: :hash?
to-map: :to-hash
; Note: Not exactly the same thing, so use /skip and be careful.

; Fake get-path! with path!
get-path!: :path!  ; Doesn't work in function specs, TYPE?
get-path?: funco ["Returns TRUE if it is this type." value [any-type!]] [
	found? all [path? get/any 'value get-word? pick value 1]
] ; Note: PATH? will also succeed in R2
to-get-path: funco [value] [
	value: to-path :value
	if word? pick value 1 [poke value 1 to-get-word pick value 1]
	value ; get-path? fails if first element not get-word!
]

; Fake typeset! with block! of datatype!
typeset!: :block!
typeset?: funco ["Returns TRUE if it is this type." value [any-type!]] [
	found? all [block? get/any 'value parse value [any datatype!]]
] ; Note: BLOCK? will also succeed in R2
to-typeset: funct [
	"Make a (fake) typeset." [catch] value
] [
	anytype: (reduce [
		none! logic! integer! decimal! money! char! pair! tuple! time! date!
		string! binary! file! email! url! tag! issue! bitset! image! block!
		paren! path! set-path! lit-path! datatype! word! set-word! get-word!
		lit-word! refinement! native! action! routine! op! function! object!
		error! port! event! struct! library! hash! list! symbol! unset!
	])
	anyblock: (reduce [
		block! paren! path! set-path! lit-path! hash! list!
	])
	anyfunction: (reduce [
		native! action! routine! op! function! ;rebcode!
	])
	anystring: (reduce [
		string! binary! file! email! url! tag! issue!
	])
	anyword: (reduce [
		word! set-word! get-word! lit-word! refinement!
	])
	series: (reduce [
		string! binary! file! email! url! tag! issue! image!
		block! paren! path! set-path! lit-path! hash! list!
	])
	number: (reduce [integer! decimal!])
	switch/default :value [
		any-type! #[datatype! any-type!] [copy anytype]
		any-block! #[datatype! any-block!] [copy anyblock]
		any-function! #[datatype! any-function!] [copy anyfunction]
		any-string! #[datatype! any-string!] [copy anystring]
		any-word! #[datatype! any-word!] [copy anyword]
		series! #[datatype! series!] [copy series]
		number! #[datatype! number!] [copy number]
	] [
		switch/default type?/word :value [
			datatype! [reduce [value]]
			block! [
				parse value: copy value [any [
					datatype! | value: word! (
						change value throw-on-error [to-datatype first value]
					) | value: block! (
						value: change/part value to-typeset first value 1
					) :value |
					value: skip (
						cause-error 'script 'invalid-arg reduce [first value]
					)
				]]
				head value
			]
		] [cause-error 'script 'invalid-arg reduce [:value]] ; 'bad-make-arg in R3
	]
] ; MAKE or TO typeset! anything-else doesn't work - use TO-TYPESET.
; These blocks of datatypes can be used with FIND like R3 typesets.
; Note: You need to use FOUND? with FIND typeset in R2 to get the R3 result.
; R3 new types not included: percent! vector! get-path! map! typeset! rebcode!
;   closure! frame! module! task! gob! handle! utype!
; R2 pseudotypes special-cased:
;   any-block! any-function! any-string! any-type! any-word! series! number!
; R2 obsolete types included: hash! list! symbol!

; Fake any-path! typeset
any-path!: reduce [path! lit-path! set-path!]
any-path?: funco [
	"Return TRUE if value is any type of path."
	value [any-type!]
][
	found? find any-path! type? get/any 'value
]

; Fake scalar! typeset
scalar!: reduce
	[integer! decimal! money! char! pair! tuple! date! time! logic! none!]
scalar?: funco [
	"Return TRUE if value is any type of scalar."
	value [any-type!]
][
	found? find scalar! type? get/any 'value
]

; The module! and vector! types are not done yet in R3, no others practical...


; Control functions

!: :not

++: funco [
	"Increment an integer or series index. Return its prior value."
	'word [word! paren!] "Integer or series variable."
][
	if all [paren? word not word? set/any 'word do word] [
		cause-error 'script 'expect-arg reduce ['++ 'word type? get/any 'word]
	] ; Workaround for R3 change in lit-word! parameters with paren! arguments
	case [
		number? get/any word [also get word set word add get word 1]
		series? get/any word [also get word set word next get word]
		'else [cause-error 'script 'expect-arg reduce ['++ 'word type? get/any 'word]]
	]
]
; Note: Native in R3.

--: funco [
	"Decrement an integer or series index. Return its prior value."
	'word [word! paren!] "Integer or series variable."
][
	if all [paren? word not word? set/any 'word do word] [
		cause-error 'script 'expect-arg reduce ['-- 'word type? get/any 'word]
	] ; Workaround for R3 change in lit-word! parameters with paren! arguments
	case [
		number? get/any word [also get word set word subtract get word 1]
		series? get/any word [also get word set word back get word]
		'else [cause-error 'script 'expect-arg reduce ['-- 'word type? get/any 'word]]
	]
]
; Note: Native in R3.

also: funco [
	"Returns the first value, but also evaluates the second."
	value1 [any-type!]
	value2 [any-type!]
][
	get/any 'value1
]
; Note: Native in R3.

quote: funco [
	"Returns the value passed to it without evaluation."
	:value [any-type!]
][ ; Broken for word! arguments - use lit-word! instead.
	get/any 'value
]
; Based on a proposal by Peta.

true?: funco [
	"Returns true if an expression can be used as true."
	val
][not not :val]
; Code written by Carl, spec by BrianH.

default: funco [ ; Needs consensus
	"Set a word to a default value if it hasn't been set yet."
	'word [word! set-word! lit-word!] "The word (use :var for word! values)"
	value "The value" ; unset! not allowed on purpose
][
	unless all [value? word not none? get word] [set word :value] :value
]
; Suggested by a discussion in R3 chat in Jan-2009.

apply: funco [
	"Apply a function to a reduced block of arguments."
	[throw]
	func [any-function!] "Function to apply"
	block [block!] "Block of args, reduced first (unless /only)"
	/only "Use arg values as-is, do not reduce the block"
	/local words path todo noref value vars var
		v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
][
	unless only [block: reduce block]
	words: words-of :func
	vars: [ ; Used to special-case 'a and :a parameters (this should be enough)
		v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20
	]
	path: to-path [func]  ; Even a one-element path works.
	todo: head insert/only make block! 1 + length? block path  ; Should be OK
	noref: false  ; True if the refinement isn't used
	while [not tail? words] [
		set/any 'value pick block 1  ; Safe for unset! and error!
		switch type?/word first words [
			word! [ ; Regular param, all values need evaluation blocked.
				unless noref [
					insert/only insert tail todo 'first reduce [get/any 'value]
				] ; Could use QUOTE here, waiting on approval.
			]
			lit-word! [ ; Lit-word param, need to special-case get-words.
				unless noref [
					either get-word? get/any 'value [
						; Needs to be a get-word! assigned to a get-word! to get in
						unless var: pick vars 1 [ ; Only a limited number in R2
							cause-error 'script 'past-end none
						] ; Needs a better error
						vars: next vars
						set var value
						insert tail todo to-get-word var
					] [
						insert/only tail todo get/any 'value
					]
				]
			]
			get-word! [ ; Get-word param, need to special-case words.
				unless noref [
					either word? get/any 'value [
						; Needs to be a word! assigned to a word! to get in
						unless var: pick vars 1 [ ; Only a limited number in R2
							cause-error 'script 'past-end none
						] ; Needs a better error
						vars: next vars
						set var value
						insert tail todo var
					] [
						insert/only tail todo get/any 'value
					]
				]
			]
			refinement! [ ; Refinement, skip its associated params if not TRUE?
				unless noref: not get/any 'value [
					insert tail path to-word first words
				]
			]
		]
		words: next words
		block: next block
	]
	also do todo ( ; DO the built code, then cleanup memory references
		set [func words block path todo value vars var] set head vars none
	)
]
; Note: APPLY evaluates the arguments block (or not with /only), regardless of
; how parameters are specified for the function itself. This means that the
; normal evalation needs to be shortcircuited, so to speak - the REBOL code
; above does this. However, the R3 version is a native that is about as quick
; as a regular function call, while the R2 version is a bit slower.
; Even so, there are good reasons to call a function with APPLY:
; - APPLY does positional evaluation of refinements, so it is easier to write
;   wrapper functions that pass along refinements.
; - If you are calling function values that are passed as arguments to your
;   function, any lit-word! or get-word! parameters can be used to inject
;   code into your function, which can be a security hole. APPLY can make
;   functional-style REBOL code safer to write.
; R3-compatibility notes about lit-word! and get-word! parameters:
; - In R2, lit-word! parameters evaluate get-word! arguments. In R3, param!
;   arguments are also evaluated. This can be emulated in R2: see CD below.
; - In R2, get-word! parameters retrieve the value from word! arguments. In R3
;   they don't evaluate or retrieve anything. There is no way to do this in R2,
;   which is why QUOTE is impossible in R2 for word! arguments (use lit-words).
; Also, op!s in R2 operate in prefix mode when assigned to another word, while
; they still operate in infix mode in R3 - we take advantage of that here.

eval: funco [
	"Evaluates a block, file, URL, function, word, or any other value."
	value "Normally a file name, URL, or block"
][
	either block? :value [do value] [:value]
]
; Note: Don't know what this native! is for in R3, but there's a doc page...


; Object functions

object: funco [
	"Defines a unique object."
	blk [block!] "Object words and values."
][ ; Build an object! safely even if there is trailing set-words
	make object! head insert tail blk none
]
; Note: CONSTRUCT already does this, but doesn't evaluate expressions.
; Carl wrote the R3 version, mostly copied here.

extend: funco [
	"Extend a block type with word and value pair." ; Not objects in R2 :(
	obj [block! paren! hash! list!] ; No point in including paths
	word [any-word!]
	val ; No unset!
][
	if :val [insert tail obj reduce [to-set-word word :val]]
	:val
]
; Note: This also works on object! and map! in R3.
; Carl wrote the R3 code, spec by BrianH, mostly copied here.

resolve: funco [
	"Set known values in target from those given by the source."
	target [object! port!]
	source [object! port!]
][
	set/any bind words-of source target get/any source
	also target set [source target] none
]
; Note: This is native in R3 and supports module! too.


; Series functions

ajoin: funco [
	"Reduces and joins a block of values into a new string."
	[throw]
	block [block!]
][
	make string! reduce block
]
; Note: Native in R3 without the reduce overhead.

first+: funco [
	{Return FIRST of series, and increment the series index.}
	[catch]
	'word [word! paren!] "Word must be a series."  ; paren! added for R2
][
	; Workaround for R3 change in lit-word! parameters with paren! arguments
	if paren? :word [set/any 'word do :word] 
	throw-on-error [also pick get word 1 set word next get word]
]
; Note: Native in R3.

append: funco [
	"Inserts a value at tail of series and returns the series at head. (Modifies)"
	series [series! port! bitset!] "Series at point to insert"
	value [any-type!] "The value to insert"
	/part "Limits to a given length or position"
	length [number! series! port! pair!]
	/only "Inserts a series as a series"
	/dup "Duplicates the insert a specified number of times"
	count [number! pair!]
][ ; Nasty, but the best you can do without native APPLY
	head do pick pick pick [[[
		[insert tail series get/any 'value]
		[insert/part tail series get/any 'value length]
	] [
		[insert/only tail series get/any 'value]
		[insert/part/only tail series get/any 'value length]
	]] [[
		[insert/dup tail series get/any 'value count]
		[insert/part/dup tail series get/any 'value length count]
	] [
		[insert/dup/only tail series get/any 'value count]
		[insert/part/dup/only tail series get/any 'value length count]
	]]] not dup not only not part
]
; Note: Native in R3.

swap: funco [
	"Swaps elements of a series. (Modifies)"
	series1 [series!]
	series2 [series!]
][
	unless any [empty? series1 empty? series2] [
		poke series1 1 also pick series2 1 poke series2 1 pick series1 1
	]
	series1
]
; Note: Native (action!) in R3.

alter: func [
	"If a value is not found in a series, append it; otherwise, remove it. Returns true if added. (Modifies)"
	series [series! port!]
	value
	/case "Case-sensitive comparison"
][
	found? unless remove (
		either case [find/case series :value] [find series :value]
	) [append series :value]
]
; Note: Useful change to previously useless return value.

take: funco [
	"Copies and removes from series. (Modifies)"
	[catch]
	value [series! port! none!]
	/part "Limits to a given length or position"
	length [number! series! port! pair!]
	/last "Take it from the tail end"
][
	if value [throw-on-error [
		either part [
			case [
				pair? length [
					unless image? value [
						cause-error 'script 'invalid-part length
					]
					last: none
				]
				any [series? length port? length] [
					either same? head value head length [
						length: subtract index? length index? value
					][
						cause-error 'script 'invalid-part reduce [length]
					]
				]
			]
			if last [
				length: negate length
				value: tail value
			]
			also copy/part value length remove/part value length
		][
			also pick either last [
				value: back tail value
			] [value] 1 remove value
		]
	]]
]
; Note: Native (action!) in R3.

move: funco [
	"Move a value or span of values in a series."
	source [series!] "Source series"
	offset [integer!] "Offset to move by, or index to move to"
	/part "Move part of a series"
	length [integer!] "The length of the part to move"
	/skip "Treat the series as records of fixed size" ;; SKIP redefined
	size [integer!] "Size of each record"
	/to "Move to an index relative to the head of the series" ;; TO redefined
][
	unless length [length: 1]
	if skip [
		offset: offset * size: max 1 size
		length: length * size
	]
	part: copy/part source length
	remove/part source length
	insert either to [at head source offset] [
		system/words/skip source offset
	] part
]
; Note: This is the best you can do without overlap and aliasing issues.
; The R3 version is included to prevent it from being reinvented, badly.
; Suggested by a discussion in AltME in Nov-2007.

array: func [
	"Makes and initializes a series of a given size."
	size [integer! block!] "Size or block of sizes for each dimension"
	/initial "Specify an initial value for all elements"
	value "Initial value (will be called each time if a function)"
	/local block rest
][
	if block? size [
		if tail? rest: next size [rest: none]
		unless integer? set/any 'size pick size 1 [
			cause-error 'script 'expect-arg reduce ['array 'size type? get/any 'size]
		]
	]
	block: make block! size
	case [
		block? rest [
			loop size [block: insert/only block array/initial rest :value]
		]
		series? :value [
			loop size [block: insert/only block copy/deep value]
		]
		any-function? :value [ ; So value can be a thunk :)
			loop size [block: insert/only block value] ; Called every time
		]
		insert/dup block value size
	]
	head block
]
; Note: Function values are evaluated at end of block to ensure safety.

replace: func [
	"Replaces the search value with the replace value within the target series."
	target  [series!] "Series that is being modified"
	search  "Value to be replaced"
	replace "Value to replace with (will be called each time if a function)"
	/all "Replace all occurrences"  ;!!! Note ALL is redefined in here!
	/case "Case-sensitive replacement"  ;!!! Note CASE is redefined in here!
	/tail "Return target after the last replacement position"  ;!!! Note TAIL is redefined in here!
	/local save-target len value pos do-break
][
	save-target: target
	; If target is a string but search is not, make search a string (except for bitset).
	; If target is a bitset, or a block and search is not a block, len = 1
	len: system/words/case [
		bitset? :search  1
		any-string? target [
			if any [not any-string? :search tag? :search] [search: form :search]
			length? :search
		]
		any-block? :search [length? :search]
		true  1		
	]
	; /all and /case checked before the while, /tail after
	do-break: unless all [:break] ; Will be none if not /all, a noop
	while pick [
		[pos: find target :search]
		[pos: find/case target :search]
	] not case [
		(value: replace pos) ; The replace argument can be a function
		target: change/part pos :value len
		do-break
	]
	either tail [target] [save-target]
]
; Note: Code injection vulnerability with get-word! parameters. Needs APPLY.
; An /any refinement is waiting for FIND/any to work in R3.

extract: func [
	"Extracts a value from a series at regular intervals."
	[catch]
	series [series!]
	width [integer!] "Size of each entry (the skip)"
	/index "Extract from an offset position"
	pos "The position" [number! logic! block!]
	/default "Use a default value instead of none"
	value "The value to use (will be called each time if a function)"
	/into "Insert into a buffer instead (returns position after insert)"
	output [series!] "The buffer series (modified)"
	/local len
][
	if zero? width [return any [output make series 0]]  ; To avoid an infinite loop
	len: either positive? width [  ; Length to preallocate
		divide length? series width  ; Forward loop, use length
	][
		divide index? series negate width  ; Backward loop, use position
	]
	unless index [pos: 1]
	either block? pos [
		if empty? pos [return any [output make series 0]] ; Shortcut return
		parse pos [some [number! | logic! | set pos skip (
			cause-error 'script 'expect-set reduce [[number! logic!] type? get/any 'pos]
		)]]
		unless into [output: make series len * length? pos]
		if all [not default any-string? output] [value: copy ""]
		; R2 PARSE doesn't work well for binary!, so spoof a string!.
		if binary? series [series: as-string series]
		forskip series width [forall pos [output: insert/only output any [pick series pos/1 value]]]
	][
		unless into [output: make series len]
		if all [not default any-string? output] [value: copy ""]
		; R2 PARSE doesn't work well for binary!, so spoof a string!.
		if binary? series [series: as-string series]
		forskip series width [output: insert/only output any [pick series pos value]]
	]
	either into [output] [head output]
]
; Note: Function values are evaluated at end of block to ensure safety.

; MAP with set-words, best datatype! support and /into (ideal full version)
map: funco [
	"Evaluates a block for each value(s) in a series and returns them as a block."
	[throw catch]
	'word [word! block!] "Word or block of words to set each time (local)"
	data [any-block! any-string! none!] "The series to traverse" ; Not image!
	body [block!] "Block to evaluate each time"
	/into "Collect into a given series, rather than a new block"
	output [any-block! any-string!] "The series to output to" ; Not image!
	/local init len x
][
	; Shortcut return for empty data
	either data [
		if empty? data [return any [output make data 0]]
	] [return output] ; Will be none if not specified
	; BIND/copy word and body
	word: either block? word [
		if empty? word [cause-error 'script 'invalid-arg [[]]]
		copy/deep word  ; /deep because word is rebound before errors checked
	] [reduce [word]]
	word: use word reduce [word]
	body: bind/copy body first word
	; Build init code
	init: make block! 4 * length? word
	parse word [any [word! | x: set-word! (
		; Add [x: at data index] to init, and remove from word
		insert insert insert tail init first x [at data] index? x
		remove x
	) :x | x: skip (
		cause-error 'script 'expect-set reduce [[word! set-word!] type? first x]
	)]]
	insert tail init [set word data]
	len: length? word ; Can be zero now (for advanced code tricks)
	; Create the output series if not specified
	unless into [output: make block! divide length? data max 1 len]
	; Process the data (which is not empty at this point)
	until [ ; Note: output: insert/only output needed for list! output
		do init
		unless unset? set/any 'x do body [output: insert/only output :x]
		tail? data: skip data len
	]
	; Return the output and clean up memory references
	also either into [output] [head output] (
		set [word data body output init x] none
	)
]
; Note: This is pretty fast by R2 mezzanine loop standards, native in R3.

#do [comment [ ; So this section is not loaded by prerebol.
; MAP, minimal fast version
map: funco [
	"Evaluates a block for each value(s) in a series and returns them as a block."
	[throw]
	'word [word! block!] "Word or block of words to set each time (local)"
	data [block!] "The series to traverse"
	body [block!] "Block to evaluate each time"
] compose/deep [ ; To reduce function creation overhead to just once
	foreach :word data reduce [
		first [(func [output val [any-type!]] [
			if value? 'val [insert/only tail output :val]
			output
		])]
		make block! either word? word [length? data] [divide length? data length? word]
		:do body
	]
]
]]

collect: funco [ ; Needs consensus
	"Evaluates a block, storing values via KEEP function, and returns block of collected values."
	body [block!] "Block to evaluate"
	/into "Insert into a buffer instead (returns position after insert)"
	output [series!] "The buffer series (modified)"
][ ; Note: Needs new FUNC (defined above)
	unless output [output: make block! 16]
	do func [keep] body func [value /only] [
		output: either only [insert/only output :value] [insert output :value]
		:value
	]
	either into [output] [head output]
]
; R3 version based on a discussion with Gregg and Gabriele in AltME.


; Character/string encoding functions

;use [
;	ascii utf8+1 utf8+2 utf8+3 utf8+4 utf8+5 utf8rest
;	utf8+1-latin1 utf8rest-latin1
;] [
;ascii: charset [#"^(00)" - #"^(7F)"]
;utf8+1: charset [#"^(C0)" - #"^(DF)"]
;utf8+2: charset [#"^(E0)" - #"^(EF)"]
;utf8+3: charset [#"^(F0)" - #"^(F7)"]
;utf8+4: charset [#"^(F8)" - #"^(FB)"]
;utf8+5: charset [#"^(FC)" - #"^(FD)"]
;utf8rest: charset [#"^(80)" - #"^(BF)"]
;utf8+1-latin1: charset [#"^(C0)" - #"^(C3)"]
;utf8rest-latin1: charset [#"^(80)" - #"^(83)"] ; For bad UTF-8

ascii?: funct [
	"Returns TRUE if value or string is in ASCII character range (below 128)."
	value [any-string! char! integer!]
] compose [
	ascii: (charset [#"^(00)" - #"^(7F)"])
	either any-string? value [parse/all/case value [any ascii]] [value < 128]
]
; Note: Native in R3.

latin1?: funct [
	"Returns TRUE if value or string is in Latin-1 character range (below 256)."
	value [any-string! char! integer!]
] compose [ ; Decodes UTF-8 if binary!
	ascii: (charset [#"^(00)" - #"^(7F)"])
	utf8+1-latin1: (charset [#"^(C0)" - #"^(C3)"])
	utf8rest: (charset [#"^(80)" - #"^(BF)"])
	utf8rest-latin1: (charset [#"^(80)" - #"^(83)"]) ; For bad UTF-8
	switch/default type?/word value [
		integer! [value < 256]
		binary! [parse/all/case value [any [
			ascii | utf8+1-latin1 utf8rest | ; Minimized Latin-1
			#{E0} utf8rest-latin1 utf8rest | ; Bad 3-byte Latin-1
			#{F080} utf8rest-latin1 utf8rest | ; Bad 4-byte Latin-1
			#{F88080} utf8rest-latin1 utf8rest | ; Bad 5-byte Latin-1
			#{FC808080} utf8rest-latin1 utf8rest ; Bad 6-byte Latin-1
		]]]
	] [true] ; R2 has Latin-1 chars and strings
]
; Note: Native in R3. Bug of accepting non-minimized UTF-8 in R3 too, for now.

utf?: funco [
	"Returns the UTF encoding from the BOM (byte order marker): + for BE; - for LE."
	data [binary!]
][
	parse/all/case data [
		#{EF BB BF} (return 8) |
		#{00 00 FE FF} (return 32) |
		#{FF FE 00 00} (return -32) |
		#{FE FF} (return 16) |
		#{FF FE} (return -16) |
		(return 0) ; No BOM or an unsupported BOM
	] ; Note: R3 only supports these BOMs.
]
; Note: Native in R3.

;]

deline: funct [
	"Converts string terminators to standard format, e.g. CRLF to LF. (Modifies)"
	string [any-string!]
	/lines "Convert to block of lines (does not modify)"
] compose [
	linechar: (complement charset crlf)
	also case [
		not lines [ ; Convert the string in place
			; Change the newlines from any platform-specific line ending
			; R2 PARSE doesn't work well for binary!, so spoof a string!
			parse/all/case either binary? string [as-string string] [string] [
				any [to cr a: cr opt lf b: (
					b: change/part a lf b
				) :b] to end
			]
			string
		] ; Otherwise generate a block of lines
		empty? string [copy []] ; Shortcut return for empty string
		'else [
			output: make block! divide length? string 50 ; Seems OK
			; R2 PARSE doesn't work well for binary!, so spoof a string!
			if binary? string [string: as-string string]
			parse/all/case string [any [
				; Lines with stuff in them
				copy a some linechar (output: insert output a) [crlf | cr | lf | end]
				|
				; Empty lines
				[crlf | cr | lf] (output: insert output copy "")
			]]
			head output
		]
	] set [string output a b] none ; Cleanup after return
] ; /lines needs testing with final line ending once it works in R3.
; Note: Native in R3.

enline: funct [
	"Converts standard string terminators to current OS format, e.g. LF to CRLF. (Modifies)"
	series [any-string! block!]
	/with "Specifies alternate line termination."
	end-of-line [char! string!]
] compose [
	; The platform-specific line ending
	platform-line: (switch/default system/version/4 [
		3 15 [crlf] 2 [to-string either system/version/5 < 4 [cr] [lf]]
	] [to-string lf]) ; Don't know about Amiga (1) and Tao Elate (27)
	switch type?/word end-of-line [
		none! [end-of-line: platform-line]
		char! [end-of-line: to-string end-of-line]
	]
	either block? series [
		; First pass: Precalculate the length of the output string
		len: 0 len-eol: length? end-of-line
		foreach s series [len: len + len-eol + length? s]
		; Second pass: Build and fill the output string
		output: make string! len
		foreach s series [insert insert tail output s end-of-line]
		; Return and clean up
		also output set [series output] none
	] [
		unless end-of-line = "^/" [
			; Change the newlines to the platform-specific line ending
			; R2 PARSE doesn't work well for binary!, so spoof a string!
			parse/all/case either binary? series [as-string series] [series] [
				any [to lf a: lf (a: change/part a end-of-line 1) :a] to end
			]
		]
		; Return and clean up
		also series set [series a] none
	]
] ; block! needs testing with final line ending once it works in R3.
; Note: Native in R3.


; File functions

what-dir: funco [
	"Prints the active directory path"
][
	copy system/script/path
]
; R2 version with copy - safer. Native in R3.

info?: func [
	"Returns information about a file or url."
	[catch]
	target [file! url!]
	/local port
][
	throw-on-error [
		port: make port! target
		query port
	]
	also unless none? port/status [
		make object! [
			name: target size: port/size date: port/date
			type: either 'directory = port/status ['dir] [port/status]
		]
	] port: none
]
; R3 version is just a call to the new QUERY.

dir?: func [
	"Returns TRUE if a file or URL is a directory."
	[catch]
	file [file! url! string! issue! none!]
	;/any "Allow * or ? wildcards for directory" ; Note: Option, not function
	/local info
][
	switch/default type?/word file [
		string! issue! ["/" = back tail file]
		file! url! [
			info: throw-on-error [info? file]
			either none? info ["/" = back tail file] [info/type = 'dir]
		]
	] [false]
]
; A work in process, compatible with a subset of R3's DIR? native! with one fix.

exists?: funco [
	{Returns the type of a file or URL if it exists, otherwise none.}
	target [file! url!]
][ ; Returns 'file or 'dir, or none
	all [target: attempt [info? target] target/type]
]
; Suggested by a Kaj's compatibility complaints.

undirize: funco [
	{Returns a copy of the path with any trailing "/" removed.}
	path [file! string! url!]
][
	path: copy path
	if #"/" = last path [clear back tail path]
	path
]
; Suggested by a Kaj's compatibility complaints.

list-dir: funco [
	"Print contents of a directory (ls)."
	'path [file! word! path! string! unset! paren!] "Accepts %file, :variables, and just words (as dirs)"
	/l "Line of info format"
	/f "Files only"
	/d "Dirs only"
;	/t "Time order"
	/r "Recursive"
	/i indent
	/local files save-dir info
][
	; Workaround for R3 change in lit-word! parameters with paren! arguments
	if paren? get/any 'path [set/any 'path do path] 
	save-dir: what-dir
	switch/default type?/word get/any 'path [
		unset! [] ; Stay here
		file! [change-dir path]
		string! [change-dir to-rebol-file path]
		word! path! [change-dir to-file path]
	] [cause-error 'script 'expect-arg reduce ['list-dir 'path type? get/any 'path]]
	if r [l: true]
	unless l [l: make string! 62] ; approx width
	unless indent [indent: ""]
	files: attempt [read %./]
	if not files [print ["Not found:" :path] change-dir save-dir exit]
	foreach file files [
		case [
			all [f dir? file] [] ;continue
			all [d not dir? file] [] ;continue
			string? l [
				insert tail l
				insert/dup tail l file #" " 15 - remainder length? l 15
				if greater? length? l 60 [print l clear l]
			]
			'else [ ; This section doesn't work yet, no printf
				;printf [indent 16 -8 #" " 26 #" " 6] get info? file
				info: info? file
				info/name: form info/name
				info/size: form info/size
				info/date: form info/date
				info/type: form info/type
				print ajoin [
					indent  copy/part info/name 16
					head insert/dup copy/part info/size 8 #" " 8 - length? info/size
					#" " info/date #" " copy/part info/type 6
				]
				if all [r dir? file] [
					list-dir/l/r/i :file join indent "    "
				]
			]
		]
	]
	if all [string? l not empty? l] [print l]
	change-dir save-dir
	exit
]
; Carl wrote the R3 version, mostly copied here.

; Aliases copied from R3 mezz-file
ls:		:list-dir
pwd:	:what-dir
rm:		:delete
mkdir:	:make-dir

cd: func [
	"Change directory (shell shortcut function)."
	[catch]
	'path [file! word! path! unset! string! paren!] "Accepts %file, :variables and just words (as dirs)"
][
	; Workaround for R3 change in lit-word! parameters with paren! arguments
	if paren? get/any 'path [set/any 'path do path] 
	switch/default type?/word get/any 'path [
		unset! [print what-dir]
		file! [change-dir path]
		string! [change-dir to-rebol-file path]
		word! path! [change-dir to-file path]
	] [cause-error 'script 'expect-arg reduce ['cd 'path type? get/any 'path]]
]

more: func [
	"Print file (shell shortcut function)."
	[catch]
	'file [file! word! path! string! paren!] "Accepts %file, :variables and just words (as file names)"
][
	; Workaround for R3 change in lit-word! parameters with paren! arguments
	if paren? :file [set/any 'file do :file] 
	print read switch/default type?/word get/any 'file [
		file! [file]
		string! [to-rebol-file file]
		word! path! [to-file file]
	] [cause-error 'script 'expect-arg reduce ['more 'file type? get/any 'file]]
]

in-dir: funco [
	"Evaluate a block while in a directory."
	[throw]
	dir [file!] "Directory to change to (changed back after)"
	block [block!] "Block to evaluate"
	/local old-dir
][
	old-dir: what-dir
	change-dir dir
	also do block change-dir old-dir
]
; Initial version made by BrianH for DevBase 2.

to-relative-file: funco [
	"Returns the relative portion of a file if in a subdirectory, or the original if not."
	file [file! string!] "File to check (local if string!)"
	/no-copy "Don't copy, just reference"
	/as-rebol "Convert to REBOL-style filename if not"
	/as-local "Convert to local-style filename if not"
	/local tmp
][
	either string? file [ ; Local file (Note: to-local-file drops trailing /)
		if tmp: find/match file to-local-file what-dir [file: next tmp]
		if as-rebol [file: to-rebol-file file  no-copy: true]
	] [
		file: any [find/match file what-dir  file]
		if as-local [file: to-local-file file  no-copy: true]
	]
	unless no-copy [file: copy file]
	file
]
; Initial version made by BrianH for DevBase 2.


; Reflection functions (reverse implementation compared to R3)

reflect: funco [
	"Returns definition-related details about a value."
	[catch]
	value [any-type!]
	field [word!] "Such as: spec body words values title etc."
][ ; In R3 the *-OF functions redirect to REFLECT - vice-versa in R2.
	switch/default field [
		spec [spec-of :value]
		body [body-of :value]
		words [words-of :value]
		values [values-of :value]
		types [types-of :value]
		title [title-of :value]
	] [
		cause-error 'script 'invalid-arg field
	]
]
; Note: Native (action!) in R3, and the *-OF functions below are wrappers.

spec-of: funco [
	"Returns a copy of the spec of a function."
	value
][
	case [
		object? :value [none]
		any-function? :value [copy/deep third :value]
		'else [cause-error 'script 'cannot-use reduce ['reflect type? :value]]
	]
]

body-of: funco [
	"Returns a copy of the body of a function or object."
	value
][
	case [
		object? :value [third :value]
		function? :value [copy/deep second :value] ; Note: Still bound!
		any-function? :value [none] ; none if native
		'else [cause-error 'script 'cannot-use reduce ['reflect type? :value]]
	]
]

words-of: funco [
	"Returns a copy of the words of a function or object."
	value
][
	case [
		object? :value [bind remove first :value :value] ; removes 'self
		any-function? :value [first :value]
		'else [cause-error 'script 'cannot-use reduce ['reflect type? :value]]
	]
]

values-of: funco [
	"Returns a copy of the values of an object."
	value
][
	case [
		object? :value [remove second :value] ; removes :self
		any-function? :value [none]
		'else [cause-error 'script 'cannot-use reduce ['reflect type? :value]]
	]
]

types-of: funct [
	"Returns a copy of the types of a function."
	value
] compose/only [ ; Returns types as blocks of datatype! values
	anytype: (to-typeset any-type!)
	anyblock: (to-typeset any-block!)
	anyfunction: (to-typeset any-function!)
	anystring: (to-typeset any-string!)
	anyword: (to-typeset any-word!)
	series: (to-typeset series!)
	number: (to-typeset number!)
	valtype: (
		head remove find head remove find to-typeset any-type! error! unset!
	) ; R2 excludes error! here where R3 doesn't
	reftype: (reduce [none! logic!])
	case [
		object? :value [cause-error 'script 'invalid-arg 'types]
		any-function? :value [
			result: copy []
			types: none
			parse third :value [any [to any-word! [
				[word! | lit-word! | get-word!] [
					set types block! (
						types: copy types
						while [not tail? types] [
							switch/default first types [
								#[datatype! any-type!] [types: change/part types anytype 1]
								#[datatype! any-block!] [types: change/part types anyblock 1]
								#[datatype! any-function!] [types: change/part types anyfunction 1]
								#[datatype! any-string!] [types: change/part types anystring 1]
								#[datatype! any-word!] [types: change/part types anyword 1]
								#[datatype! series!] [types: change/part types series 1]
								#[datatype! number!] [types: change/part types number 1]
							] [
								types: next types
							]
						]
						insert/only tail result head types
					) | (
						insert/only tail result copy valtype
					)
				] |
				refinement! (
					insert/only tail result copy reftype
				) |
				skip ; I have no idea how set-words in the spec are to be treated
			]]]
			result
		]
		'else [cause-error 'script 'cannot-use reduce ['reflect type? :value]]
	]
]
; Note: R3 doesn't exclude error! from the default parameter types, but R2 does.
; See the typeset! spoofing for details about the return values.

title-of: funco [
	"Returns a copy of the title of a function."
	value
][
	case [
		object? :value [cause-error 'script 'invalid-arg 'title]
		any-function? :value [if string? value: pick third :value 1 [copy value]]
		'else [cause-error 'script 'cannot-use reduce ['reflect type? :value]]
	]
]


; Profiling functions

dt: delta-time: func [
	"Delta-time - return the time it takes to evaluate a block."
	block [block!]
	/local start
][
	start: now/precise
	do block
	difference now/precise start
]
; Carl wrote the R3 version, copied here.


; Future additions, maybe - haven't been accepted in R3 yet.

#do [comment [ ; So this section is not loaded by prerebol.

; FORMAT, PRINTF and SPLIT waiting on consensus, no point in backporting them yet.

reword: funco [ ; Needs a lot of work
	"Create a new string from a template and rewording rules."
	[catch]
	source [any-string!] "Template string with escape sequences"
	values [block! object!] "Pairs of values and replacements (will be called if functions)"
	/escape "Choose your own escape char"
	char [char! any-string!] "Use this escape char (default $)"
	/into "Insert into a buffer instead (returns position after insert)"
	output [any-string!] "The buffer string (modified)"
	/local words a b
][
	output: any [output make source length? source]
	char: to string! any [char "$"]
	if block? values [values: make object! values]
	words: next first values ; R2 equivalent of words-of, but not bound
	escape: head insert make block! 1 + (4 * length? words) char
	foreach word words [
		word: in values word
		insert tail escape compose [| (to string! word) a: (
			either all [value? word  not none? get word] [
				head insert insert/only insert make paren! 5 [output: insert output] get word [:b]
			] [[]] ; [] composes to nothing
		)]
	]
	parse/all source [
		a:
		any [
			to char b: (output: insert/part output a b)
			char a: escape
		]
		to end (output: insert output a)
	]
	also
		either into [output] [head output]  ; Returned
		set [source values escape output words a b] none  ; GC cleanup
]
; Based loosely on a suggestion from Carl in Nov-2008.

gather: funco [ ; Needs work
	"Get the values of a given field from all objects in a block that have it."
	block [block!] "A block which may contain objects"
	word [word!] "The field to look for"
	/into "Insert into a buffer instead (returns position after insert)"
	output [series!] "The buffer series (modified)"
][
	unless output [output: make block length? block]
	foreach item block [all [
		object? get/any 'item
		in item word
		output: insert/only output get/any in item word
	]]
	either into [output] [head output]
]
; Based on a proposal by Henrik.

accumulate: funco [ ; Needs consensus
	"Combines the results of a function applied to each value in a series."
	series [series!] "The series"
	fn [any-function!] "Function taking two arguments: result so far and value"
	/with "Use a different starting value than the first in the series"
	value "The value to start with"
][
	unless with [value: pick series 1 series: next series]
	foreach item series [value: fn :value :item]
]
; Has a code-injection vulnerability with get-word! parameters (R3 uses APPLY).
; R3 version based on a discussion about FOLD in AltME.

]]