function parseClient(){
	var client = {};
	if( isMturk() ){
		if((getParameterByName('workerId') == null) ||
		   (getParameterByName('assignmentId') == "ASSIGNMENT_ID_NOT_AVAILABLE")){
			// just browsing?
			client.type = 'preview';
			client.sid = 'preview-' + Math.random().toString(36).substr(2, 5);
		} else {
			client.type = 'mturk';
			client.assignmentId = getParameterByName('assignmentId');
			client.hitId = getParameterByName('hitId');
			client.workerId = getParameterByName('workerId');
			client.turkSubmitTo = getParameterByName('turkSubmitTo') + '/mturk/externalSubmit';
			client.sid = client.workerId;
		}
	} else if(isSona()){
		// is sona
		client.type = 'sona';
		client.experiment_id = expt.sona.experiment_id;
		client.credit_token = expt.sona.credit_token;
		client.survey_code = getParameterByName("survey_code");
		client.sid = 'sona-' + client.survey_code;
	} else {
		// just a random visitor?
		client.type = 'visitor';
		client.sid = 'visitor-' + Math.random().toString(36).substr(2, 5);
	}
	client.window = {width: $(window).width(), height: $(window).height()};
	client.screen = {width: screen.width, height: screen.height};
	client.userAgent = navigator.userAgent;
	client.score = 0;
	client.bonus = 0;
	return(client);
}

function isSona(){
	var code = getParameterByName("survey_code");
	var sona = getParameterByName("sona");
	if(!(code == null) && !(sona == null)){
		return true;
	} else {
		return false;
	}
}


function isMturk() {
  try {
    return ((window.location.host.indexOf('mturk')!=-1) || 
    		document.forms["mturk_form"] ||
      		(top != self && window.parent.location.host.indexOf("mturk") != -1));
  } catch(err) {
    // insecure content trying to access https://turk probably, so say yes:
    return true;
  }
}

function addHidden(id, value){
	var input = document.createElement("input");
	input.setAttribute("type", "hidden");
	input.setAttribute("name", id);
	input.setAttribute("value", value);
	return(input);
}


function postMturk(client){
	var form = document.createElement('form')
	form.method = 'POST';
	form.action = client.turkSubmitTo;
	// should be:
	// live:  		http://www.mturk.com/mturk/externalSubmit
	// sandbox: 	http://workersandbox.mturk.com/mturk/externalSubmit

    form.appendChild(addHidden('assignmentId', client.assignmentId));  
    form.appendChild(addHidden('bonus', client.bonus));

    document.body.appendChild(form);
    form.submit();
}

function postSona(client){
	var form = document.createElement('form')
	form.method = 'GET';
	form.action = 'https://ucsd.sona-systems.com/webstudy_credit.aspx';
	// systems.com/services/SonaAPI.svc/WebstudyCredit?experiment_id=123&credit_token=9185d436e5f94b1581b0918162f6d7e8&survey_code=XXXX
 
    form.appendChild(addHidden('experiment_id', client.experiment_id));  
    form.appendChild(addHidden('credit_token', client.credit_token));
    form.appendChild(addHidden('survey_code', client.survey_code));  

    document.body.appendChild(form);
    form.submit();
}

function submitExternal(client){
	switch(client.type){
		case 'mturk':
			postMturk(client);
			break;
		case 'sona':
			postSona(client);
			break;
		case 'visitor':
			window.location = "http://www.evullab.org";
			break;
		default:
			window.location = "http://www.evullab.org";
	}
}


function RandomOrder(num) {
  var order = new Array();
  for (var i=0; i<num; i++) { 
    order.push(i); 
  }
  return Shuffle(order);
}

//For todays date;
Date.prototype.today = function(){ 
    return ((this.getDate() < 10)?"0":"") + this.getDate() +"/"+(((this.getMonth()+1) < 10)?"0":"") + (this.getMonth()+1) +"/"+ this.getFullYear() 
};
//For the time now
Date.prototype.timeNow = function(){
     return ((this.getHours() < 10)?"0":"") + this.getHours() +":"+ ((this.getMinutes() < 10)?"0":"") + this.getMinutes() +":"+ ((this.getSeconds() < 10)?"0":"") + this.getSeconds();
};

/**
 * URL decode a parameter
 */
function decode(strToDecode)
{
  return unescape(strToDecode.replace(/\+/g,  " "));
}

function getParameterByName(name, url) {
    if (!url) url = window.location.href;
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
}

