(defvar vmodule-mode-hook nil)

(defvar vmodule-mode-map
    (let ((map (make-keymap)))
        (define-key map "\C-j" 'newline-and-indent)
        map)
    "Keymap for vmodule major mode")

(add-to-list 'auto-mode-alist '("\\.v\\'" . vmodule-mode))
(add-to-list 'auto-mode-alist '("\\.sv\\'" . vmodule-mode))

; Syntax highlight
; Verilog reserved keywords
;(regexp-opt '(
;"always"
;"end"
;"ifnone"
;"or"
;"rpmos"
;"tranif1"
;"and"
;"endcase"
;"initial"
;"output"
;"rtran"
;"tri"
;"assign"
;"endmodule"
;"inout"
;"parameter"
;"rtranif0"
;"tri0"
;"begin"
;"endfunction"
;"input"
;"pmos"
;"rtranif1"
;"tri1"
;"buf"
;"endprimitive"
;"integer"
;"posedge"
;"scalared"
;"triand"
;"bufif0"
;"endspecify"
;"join"
;"primitive"
;"small"
;"trior"
;"bufif1"
;"endtable"
;"large"
;"pull0"
;"specify"
;"trireg"
;"case"
;"endtask"
;"macromodule"
;"pull1"
;"specparam"
;"vectored"
;"casex"
;"event"
;"medium"
;"pullup"
;"strong0"
;"wait"
;"casez"
;"for"
;"module"
;"pulldown"
;"strong1"
;"wand"
;"cmos"
;"force"
;"nand"
;"rcmos"
;"supply0"
;"weak0"
;"deassign"
;"forever"
;"negedge"
;"real"
;"supply1"
;"weak1"
;"default"
;"for"
;"nmos"
;"realtime"
;"table"
;"while"
;"defparam"
;"function"
;"nor"
;"reg"
;"task"
;"wire"
;"disable"
;"highz0"
;"not"
;"release"
;"time"
;"wor"
;"edge"
;"highz1"
;"notif0"
;"repeat"
;"tran"
;"xnor"
;"else"
;"if"
;"notif1"
;"rnmos"
;"tranif0"
;"xor") t)
; Systemverilog reserved keywords
;(regexp-opt '(
;"accept_on"
;"export"
;"ref"
;"alias"
;"extends"
;"restrict"
;"always_comb"
;"extern"
;"return"
;"always_ff"
;"final"
;"s_always"
;"always_latch"
;"first_match"
;"s_eventually"
;"assert"
;"foreach"
;"s_nexttime"
;"assume"
;"forkjoin"
;"s_until"
;"before"
;"global"
;"s_until_with"
;"bind"
;"iff"
;"sequence"
;"bins"
;"ignore_bins"
;"shortint"
;"binsof"
;"illegal_bins"
;"shortreal"
;"bit"
;"implies"
;"solve"
;"break"
;"import"
;"static"
;"byte"
;"inside"
;"string"
;"chandle"
;"int"
;"strong"
;"checker"
;"interface"
;"struct"
;"class"
;"intersect"
;"super"
;"clocking"
;"join_any"
;"sync_accept_on"
;"const"
;"join_none"
;"sync_reject_on"
;"constraint"
;"let"
;"tagged"
;"context"
;"local"
;"this"
;"continue"
;"logic"
;"throughout"
;"cover"
;"longint"
;"timeprecision"
;"covergroup"
;"matches"
;"timeunit"
;"coverpoint"
;"modport"
;"type"
;"cross"
;"new"
;"typedef"
;"dist"
;"nexttime"
;"union"
;"do"
;"null"
;"unique"
;"endchecker"
;"package"
;"unique0"
;"endclass"
;"packed"
;"until"
;"endclocking"
;"priority"
;"until_with"
;"endgroup"
;"program"
;"untypted"
;"endinterface"
;"property"
;"var"
;"endpackage"
;"protected"
;"virtual"
;"endprogram"
;"pure"
;"void"
;"endproperty"
;"rand"
;"wait_order"
;"endsequence"
;"randc"
;"weak"
;"enum"
;"randcase"
;"wildcard"
;"eventually"
;"randsequence"
;"with"
;"expect"
;"reject_on"
;"within") t)

(defface vmodule-macro-face
    '((t :foreground "#FF007F"))
    "Face for punc.")
(defvar vmodule-macro-face 'vmodule-macro-face)

(defface vmodule-punc-face
    '((t :foreground "orange"))
    "Face for punc.")
(defvar vmodule-punc-face 'vmodule-punc-face)

(defconst vmodule-font-lock-keywords-1
    (list
    ;'("\\/\\/.*". font-lock-comment-face)
    ;'("\\/\\*[[:word:][:space:]\\\\]*\\*\\/". font-lock-comment-face)
    '("`[[:word:]]*" . vmodule-macro-face)
    '("$[[:word:]]*" . vmodule-macro-face)
    '("\\(#\\|\(\\|\)\\|;\\|=\\|<\\|>\\|*\\|&\\|\^\\|%\\|@\\|-\\|+\\|\/\\|~\\|?\\|:\\|\\[\\|\\]\\)" . vmodule-punc-face)
    ;'("[\^[:digit:]]*\\.[\^[:digit:]]*" . vmodule-punc-face)
    '("[[:upper:]]+[_[:upper:]]*" . font-lock-string-face)
    '("[0-9]*\'[bBhHdDoO][[:digit:]aAbBcCdDeEfFxXzZ\\?_\\.]*" . font-lock-constant-face)
    '("[^_a-zA-Z]\\([0-9]+\\.?[0-9]*\\)" . font-lock-constant-face)
    ; Verilog reserved keywords
    '("\\<\\(a\\(?:lways\\|nd\\|ssign\\)\\|b\\(?:egin\\|uf\\(?:if[01]\\)?\\)\\|c\\(?:ase[xz]?\\|mos\\)\\|d\\(?:e\\(?:assign\\|f\\(?:ault\\|param\\)\\)\\|isable\\)\\|e\\(?:dge\\|lse\\|nd\\(?:case\\|function\\|module\\|primitive\\|specify\\|ta\\(?:ble\\|sk\\)\\)?\\|vent\\)\\|f\\(?:or\\(?:ce\\|ever\\)?\\|unction\\)\\|highz[01]\\|i\\(?:f\\(?:none\\)?\\|n\\(?:itial\\|out\\|put\\|teger\\)\\)\\|join\\|large\\|m\\(?:acromodule\\|edium\\|odule\\)\\|n\\(?:and\\|egedge\\|mos\\|o\\(?:tif[01]\\|[rt]\\)\\)\\|o\\(?:r\\|utput\\)\\|p\\(?:arameter\\|mos\\|osedge\\|rimitive\\|ull\\(?:down\\|up\\|[01]\\)\\)\\|r\\(?:cmos\\|e\\(?:al\\(?:time\\)?\\|g\\|lease\\|peat\\)\\|nmos\\|pmos\\|tran\\(?:if[01]\\)?\\)\\|s\\(?:calared\\|mall\\|pec\\(?:ify\\|param\\)\\|trong[01]\\|upply[01]\\)\\|t\\(?:a\\(?:ble\\|sk\\)\\|ime\\|r\\(?:an\\(?:if[01]\\)?\\|i\\(?:and\\|or\\|reg\\|[01]\\)?\\)\\)\\|vectored\\|w\\(?:a\\(?:it\\|nd\\)\\|eak[01]\\|hile\\|ire\\|or\\)\\|x\\(?:n?or\\)\\)\\>" . font-lock-keyword-face)
    '("\\<\\(a\\(?:ccept_on\\|l\\(?:ias\\|ways_\\(?:comb\\|ff\\|latch\\)\\)\\|ss\\(?:ert\\|ume\\)\\)\\|b\\(?:efore\\|i\\(?:n\\(?:sof\\|[ds]\\)\\|t\\)\\|reak\\|yte\\)\\|c\\(?:h\\(?:andle\\|ecker\\)\\|l\\(?:ass\\|ocking\\)\\|o\\(?:n\\(?:st\\(?:raint\\)?\\|t\\(?:ext\\|inue\\)\\)\\|ver\\(?:group\\|point\\)?\\)\\|ross\\)\\|d\\(?:ist\\|o\\)\\|e\\(?:n\\(?:d\\(?:c\\(?:hecker\\|l\\(?:ass\\|ocking\\)\\)\\|group\\|interface\\|p\\(?:ackage\\|ro\\(?:gram\\|perty\\)\\)\\|sequence\\)\\|um\\)\\|ventually\\|x\\(?:p\\(?:\\(?:ec\\|or\\)t\\)\\|te\\(?:nds\\|rn\\)\\)\\)\\|f\\(?:i\\(?:nal\\|rst_match\\)\\|or\\(?:each\\|kjoin\\)\\)\\|global\\|i\\(?:ff\\|gnore_bins\\|llegal_bins\\|mp\\(?:lies\\|ort\\)\\|n\\(?:side\\|t\\(?:er\\(?:face\\|sect\\)\\)?\\)\\)\\|join_\\(?:any\\|none\\)\\|l\\(?:et\\|o\\(?:cal\\|gic\\|ngint\\)\\)\\|m\\(?:atches\\|odport\\)\\|n\\(?:e\\(?:w\\|xttime\\)\\|ull\\)\\|p\\(?:ack\\(?:age\\|ed\\)\\|r\\(?:iority\\|o\\(?:gram\\|perty\\|tected\\)\\)\\|ure\\)\\|r\\(?:and\\(?:c\\(?:ase\\)?\\|sequence\\)?\\|e\\(?:f\\|ject_on\\|strict\\|turn\\)\\)\\|s\\(?:_\\(?:always\\|eventually\\|nexttime\\|until\\(?:_with\\)?\\)\\|equence\\|hort\\(?:int\\|real\\)\\|olve\\|t\\(?:atic\\|r\\(?:ing\\|ong\\|uct\\)\\)\\|uper\\|ync_\\(?:\\(?:accep\\|rejec\\)t_on\\)\\)\\|t\\(?:agged\\|h\\(?:is\\|roughout\\)\\|ime\\(?:precision\\|unit\\)\\|ype\\(?:def\\)?\\)\\|un\\(?:i\\(?:on\\|que0?\\)\\|t\\(?:il\\(?:_with\\)?\\|ypted\\)\\)\\|v\\(?:ar\\|irtual\\|oid\\)\\|w\\(?:ait_order\\|eak\\|i\\(?:ldcard\\|th\\(?:in\\)?\\)\\)\\)\\>" . font-lock-keyword-face))
    ; Systemverilog reserved keywords
    "Minimal highlighting expressions for vmodule mode")

(defvar vmodule-font-lock-keywords vmodule-font-lock-keywords-1
    "Default highlighting expressions for vmodule mode")

; Syntax table
(defvar vmodule-mode-syntax-table
    (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    ;(modify-syntax-entry ?\' "\"" st)
     st)
    "Syntax table for vmodule-mode")

; Define vmodule
(defun vmodule-mode ()
    "A mode for IC design front-end flow."
    (interactive)
    (kill-all-local-variables)
    (set-syntax-table vmodule-mode-syntax-table)
    (use-local-map vmodule-mode-map)
    (set (make-local-variable 'font-lock-defaults) '(vmodule-font-lock-keywords))
    (setq major-mode 'vmodule-mode)
    (setq mode-name "vmodule")
    (run-hooks 'vmodule-mode-hook))

(provide 'vmodule)

