
/*
 * GET home page.
 */

exports.index = function(req, res){
  //fs = require('fs')
  //fs.readFile('/home/leonardo/Development/simple/Main.mo', 'utf8', function (err,data) {
  //  if (err) {
  //    return console.log(err);
  //  }
    var myJSONString = JSON.stringify("");
	var myEscapedJSONString = myJSONString;
    res.render('index', { pagedata : {title: 'Vult interpreter', text: myEscapedJSONString}});
  //});

};

exports.live = function(req, res){
  //fs = require('fs')
  //fs.readFile('/home/leonardo/Development/simple/Main.mo', 'utf8', function (err,data) {
  //  if (err) {
  //    return console.log(err);
  //  }
    var myJSONString = JSON.stringify("fun default(){\n}\nfun process(i){\n  mem pitch;\n  mem param1;\n  mem param2;\n  mem param3;\n  mem param4;\n  mem param5;\n  mem param6;\n  mem param7;\n  mem param8;\n\n  mem param9;\n  mem param10;\n  mem param11;\n  mem param12;\n\n  mem param13;\n  mem param14;\n  mem param15;\n  mem param16;\n\n  return i;\n}\n\nfun process_noteOn(n){\n   mem pitch;\n   pitch=n;\n}\n\nfun process_noteOff(n){\n   mem pitch;\n   pitch=n;\n}\n");
    var myEscapedJSONString = myJSONString;
    res.render('live', { pagedata : {title: 'Vult interpreter', text: myJSONString}});
  //});

};