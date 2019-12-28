/** JS Code to support the KPAX ChatRoom Example - depends on the Prototype JS lib */

var base_url = "http://localhost/base_url/";

var last_message_id = -1;

function update_messages(messages) {
    var ul = $("messages");
    $("progress").innerHTML = "Updating...";
    if (messages != null) {
	for (var i = 0; i < messages.length; i++) {
	    var message = messages[i];
	    var li = document.createElement("li");
	    li.innerHTML = "[" + message.user + " " + message.time + "]> " + message.text;
	    ul.appendChild(li);
	}
	last_message_id = messages[messages.length - 1].id;
    }
    $("progress").innerHTML = "";
}

function send_message() {
    $("progress").innerHTML = "Sending...";
    var message = $F("message");
    new Ajax.Request(base_url + "chatroom-new-message-subrequest",
		     {method: "post",
		      parameters: "message=" + message,
		      onSuccess: function (t) {
			$("message").value = ""; 
			$("progress").innerHTML = "";},
		      onFailure: function (t) {
		        alert("send_message failure:" + t.statusText);
			$("progress").innerHTML = "";}});
}

var last_call = null;

function get_updates() {
    last_call = new Date().getTime();
    new Ajax.Request(base_url + "chatroom-get-updates-subrequest",
		     {method: "get",
		      parameters: "since="+last_message_id,
		      onSuccess: function (t) {  
		        var messages;
			if (t.responseText && t.responseText.length > 0)
			    eval("messages = " + t.responseText);
			update_messages(messages);
			last_call = null;},
		      onFailure: function (t) { 
			last_call = null;
			alert("get_updates failure:" + t.statusText);}});
}

var auto_updater = function() {
    var now = new Date().getTime();
    if (last_call == null || 60 < ((now - last_call) / 1000)) {
	get_updates();
    }
}

function install_auto_updater() {
    new PeriodicalExecuter(auto_updater, 5);
}
