/* Support JavaScript code for the Subrequest1 example */

var subrequest;
var base_url;

function compute_response() {
  var login_form = document.forms[0];
  var url;

  url = base_url + "?number1=" + login_form.number1.value + "&number2=" + login_form.number2.value;

  if (window.XMLHttpRequest) {
     subrequest = new XMLHttpRequest();
     subrequest.onreadystatechange = process_response;
     subrequest.open("GET", url, true);
     subrequest.send(null);
  } else if (window.ActiveXObject) {
     subrequest = new ActiveXObject("Microsoft.XMLHTTP");
     if (req) {
        subrequest.onreadystatechange = process_response;
        subrequest.open("GET", url, true);
        subrequest.send();
     }
  }
}

function process_response() {
  var login_form = document.forms[0];

  if (subrequest.readyState == 4) {
    if (subrequest.status == 200) {
	login_form.sum.value = subrequest.responseText;
    } else {
      alert("There was a problem retrieving the XML data:\n" + subrequest.statusText);
    }
  }
}
