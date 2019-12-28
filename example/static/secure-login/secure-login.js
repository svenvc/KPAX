/* Support JavaScript code for the Secure Login example */

var response_computed = false;

function compute_response() {
  var login_form = document.forms[0];

  if (!response_computed) {
    login_form.response.value = hex_hmac_sha1(login_form.password.value, login_form.challenge.value);
    login_form.password.value = "";
  } 
}

function login_button_onclick() {
  var login_form = document.forms[0];

  compute_response();
  login_form.submit();
}

function login_form_onsubmit() {
  compute_response();
}
