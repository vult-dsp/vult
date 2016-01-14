define("ace/mode/vult_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var VultHighlightRules = function() {

    this.$rules = {
        start: [{
            token: [
                "keyword.control.source.Vult",
                "meta.function.source.Vult",
                "entity.name.function.source.Vult"
            ],
            regex: /(fun|and|external)(\s+)([a-zA-Z_]\w*)/
        }, {
            token: "keyword.source.Vult",
            regex: /\b(?:fun|val|mem|return|if|else|while|then|type|table|external)\b/
        }, {
            token: "constant.numeric.source.Vult",
            regex: /\b(?:[0-9]+\.?[0-9]*|\.[0-9]+)(?:(?:e|E)(?:\+|-)?[0-9]+)?\b/
        }, {
            token: "storage.type.source.Vult",
            regex: /\b(?:int|real|num|bool)\b/
        }, {
            token: [
                "storage.type.source.Vult",
                "constant.character.source.Vult"
            ],
            regex: /\b([a-zA-Z_]\w*)\b(\()/
        }, {
            token: "comment.line.double-slash.source.Vult",
            regex: /\/\/.*$/
        }, {
            token: "punctuation.definition.string.begin.source.Vult",
            regex: /"/,
            push: [{
                token: "punctuation.definition.string.end.source.Vult",
                regex: /"/,
                next: "pop"
            }, {
                token: "constant.character.escape.source.Vult",
                regex: /\\./
            }, {
                defaultToken: "string.quoted.double.source.Vult"
            }]
        }, {
            token: "comment.block.begin.source.Vult",
            regex: /\/\*/,
            push: [{
                token: "comment.block.end.source.Vult",
                regex: /\*\//,
                next: "pop"
            }, {
                token: "constant.character.escape.source.Vult",
                regex: /\\./
            }, {
                defaultToken: "comment.block.source.Vult"
            }]
        }, {
            token: "constant.character.source.Vult",
            regex: /=|:|;|<|>|,|!=|\+|\*|\/|-|\||&|\(|\)|{|}/
        }, {
            token: "constant.language.source.Vult",
            regex: /true|false/
        }]
    }
    
    this.normalizeRules();
};

VultHighlightRules.metaData = {
    fileTypes: ["vult"],
    name: "Vult",
    scopeName: "source.Vult"
}


oop.inherits(VultHighlightRules, TextHighlightRules);

exports.VultHighlightRules = VultHighlightRules;
});

define("ace/mode/folding/cstyle",["require","exports","module","ace/lib/oop","ace/range","ace/mode/folding/fold_mode"], function(require, exports, module) {
"use strict";

var oop = require("../../lib/oop");
var Range = require("../../range").Range;
var BaseFoldMode = require("./fold_mode").FoldMode;

var FoldMode = exports.FoldMode = function(commentRegex) {
    if (commentRegex) {
        this.foldingStartMarker = new RegExp(
            this.foldingStartMarker.source.replace(/\|[^|]*?$/, "|" + commentRegex.start)
        );
        this.foldingStopMarker = new RegExp(
            this.foldingStopMarker.source.replace(/\|[^|]*?$/, "|" + commentRegex.end)
        );
    }
};
oop.inherits(FoldMode, BaseFoldMode);

(function() {
    
    this.foldingStartMarker = /(\{|\[)[^\}\]]*$|^\s*(\/\*)/;
    this.foldingStopMarker = /^[^\[\{]*(\}|\])|^[\s\*]*(\*\/)/;
    this.singleLineBlockCommentRe= /^\s*(\/\*).*\*\/\s*$/;
    this.tripleStarBlockCommentRe = /^\s*(\/\*\*\*).*\*\/\s*$/;
    this.startRegionRe = /^\s*(\/\*|\/\/)#?region\b/;
    this._getFoldWidgetBase = this.getFoldWidget;
    this.getFoldWidget = function(session, foldStyle, row) {
        var line = session.getLine(row);
    
        if (this.singleLineBlockCommentRe.test(line)) {
            if (!this.startRegionRe.test(line) && !this.tripleStarBlockCommentRe.test(line))
                return "";
        }
    
        var fw = this._getFoldWidgetBase(session, foldStyle, row);
    
        if (!fw && this.startRegionRe.test(line))
            return "start"; // lineCommentRegionStart
    
        return fw;
    };

    this.getFoldWidgetRange = function(session, foldStyle, row, forceMultiline) {
        var line = session.getLine(row);
        
        if (this.startRegionRe.test(line))
            return this.getCommentRegionBlock(session, line, row);
        
        var match = line.match(this.foldingStartMarker);
        if (match) {
            var i = match.index;

            if (match[1])
                return this.openingBracketBlock(session, match[1], row, i);
                
            var range = session.getCommentFoldRange(row, i + match[0].length, 1);
            
            if (range && !range.isMultiLine()) {
                if (forceMultiline) {
                    range = this.getSectionRange(session, row);
                } else if (foldStyle != "all")
                    range = null;
            }
            
            return range;
        }

        if (foldStyle === "markbegin")
            return;

        var match = line.match(this.foldingStopMarker);
        if (match) {
            var i = match.index + match[0].length;

            if (match[1])
                return this.closingBracketBlock(session, match[1], row, i);

            return session.getCommentFoldRange(row, i, -1);
        }
    };
    
    this.getSectionRange = function(session, row) {
        var line = session.getLine(row);
        var startIndent = line.search(/\S/);
        var startRow = row;
        var startColumn = line.length;
        row = row + 1;
        var endRow = row;
        var maxRow = session.getLength();
        while (++row < maxRow) {
            line = session.getLine(row);
            var indent = line.search(/\S/);
            if (indent === -1)
                continue;
            if  (startIndent > indent)
                break;
            var subRange = this.getFoldWidgetRange(session, "all", row);
            
            if (subRange) {
                if (subRange.start.row <= startRow) {
                    break;
                } else if (subRange.isMultiLine()) {
                    row = subRange.end.row;
                } else if (startIndent == indent) {
                    break;
                }
            }
            endRow = row;
        }
        
        return new Range(startRow, startColumn, endRow, session.getLine(endRow).length);
    };
    this.getCommentRegionBlock = function(session, line, row) {
        var startColumn = line.search(/\s*$/);
        var maxRow = session.getLength();
        var startRow = row;
        
        var re = /^\s*(?:\/\*|\/\/|--)#?(end)?region\b/;
        var depth = 1;
        while (++row < maxRow) {
            line = session.getLine(row);
            var m = re.exec(line);
            if (!m) continue;
            if (m[1]) depth--;
            else depth++;

            if (!depth) break;
        }

        var endRow = row;
        if (endRow > startRow) {
            return new Range(startRow, startColumn, endRow, line.length);
        }
    };

}).call(FoldMode.prototype);

});

define("ace/mode/vult",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/vult_highlight_rules","ace/worker/worker_client","ace/mode/folding/cstyle"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var VultHighlightRules = require("./vult_highlight_rules").VultHighlightRules;
var WorkerClient = require("../worker/worker_client").WorkerClient;
var FoldMode = require("./folding/cstyle").FoldMode;

var Mode = function() {
    this.HighlightRules = VultHighlightRules;
    this.foldingRules = new FoldMode();
};
oop.inherits(Mode, TextMode);

(function() {
    this.$id = "ace/mode/vult"
    this.createWorker = function(session) {
        var worker = new WorkerClient(["ace"], "ace/mode/vult_worker", "VultWorker");
        worker.attachToDocument(session.getDocument());

        worker.on("annotate", function(results) {
            session.setAnnotations(results.data);
        });

        worker.on("terminate", function() {
            session.clearAnnotations();
        });

        return worker;
    };
}).call(Mode.prototype);

exports.Mode = Mode;
});
