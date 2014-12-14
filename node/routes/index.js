
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