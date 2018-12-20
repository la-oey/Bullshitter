// https://ucsd.sona-systems.com/webstudy_credit.aspx?experiment_id=1465&credit_token=c6393dd431374ab48035c7fafafced2e&survey_code=XXXX
// experiment settings
var expt = {
    saveURL: 'submit.simple.php',
    trials: 40, 
    //goalScore: 100,
    marblesSampled: 10, //total number of marbles drawn per trial
    roleFirst: 'bullshitter', //roles: {'bullshitter','bullshitDetector'}
    trialProbs: [0.2,0.5,0.8],
    playerTotalScore: 0,
    oppTotalScore: 0,
    sona: {
        experiment_id: 1505,
        credit_token: 'b20092f9d3b34a378ee654bcc50710ea'
    }
};
var trial = {
    roleCurrent: 'bullshitter',
    trialNumber: 0,
    startTime: 0,
    trialTime: 0,
    waitTime: 0,
    responseStartTime: 0,
    responseTime: 0,
    timer: 0,
    probabilityRed: 0.5,
    probabilityBlue: 0.5,
    numRed: 0,
    numBlue: 0,
    drawnRed: 0,
    drawnBlue: 0,
    reportedDrawn: 0,
    compLie: 0,
    compDetect: 0,
    callBS: false,
    playerTrialScore: 0,
    oppTrialScore: 0
};
var turn = {
    numDrawn: 0
}
var client = parseClient();
var trialData = []; // store of all trials



// TODO, Potentially: pick randomly between human/threePoints instructions.
function pageLoad() {
    document.getElementById('consent').style.display = 'block';
}

function clickConsent() {
    document.getElementById('consent').style.display = 'none';
    document.getElementById('instructions').style.display = 'block';
    //$('#instructGoalScore').html(expt.goalScore);
    $('#instructRounds').html(expt.trials);
    $('#instructMarblesSampled').html(expt.marblesSampled);
}

function clickInstructions() {
    document.getElementById('instructions').style.display = 'none';

    if(expt.roleFirst == 'bullshitter'){
        bullshitter();
    } else{
        bullshitDetector();
    }
}

function fillUrn(totalMarbles, probability) {
    for(var i=0; i<totalMarbles; i++){
        var color = "blue";
        if(Math.random() < probability){
            color = "red";
            trial.numRed += 1;
        } else{
            trial.numBlue += 1;
        }
        marble("#urnsvg", color, 17.5, randomDouble(.05*$('#urn').width(), .95*$('#urn').width()), randomDouble(.05*$('#urn').height(), .95*$('#urn').height()));

        // hide urn if in the bullshitDetector role
        // if(trial.roleCurrent == 'bullshitter'){
        //     marble("#urnsvg", color, 20, randomDouble(.05*$('#urn').width(), .95*$('#urn').width()), randomDouble(.05*$('#urn').height(), .95*$('#urn').height()));
        // }
    }
}

function marble(container, color, size, locX, locY){
    d3.select(container).append("circle").attr("cx",locX).attr("cy",locY).attr("r",size).attr("stroke-width",2).attr("stroke","black").style("fill",color);
}

function draw(){
    var color = "blue";
    if(Math.random() < trial.probabilityRed){
        color = "red";
        trial.drawnRed += 1;
    } else{
        trial.drawnBlue += 1;
    }
    marble("#tubesvg", color, 17.5, .5*$('#tube').width(), ($('#tube').height()*.95)-(turn.numDrawn/expt.marblesSampled)*$('#tube').height())

    turn.numDrawn += 1;

    if(turn.numDrawn == expt.marblesSampled){
        $('#draw-button').prop('disabled',true);
        $('#subjResponse').css('opacity','1');
        $('#reportMarbles').prop('disabled',false);
        $('#trialInstruct').html("Type into the textbox a number <b>between 0 and 10</b>. Then, click 'Report!'<br><br>");
        trial.responseStartTime = Date.now();
    } 
}

function report(){
    trial.responseTime = Date.now() - trial.responseStartTime;
    $('#report-button').prop('disabled', true);

    function bullshitterWait() {
        flickerWait();
        
        trial.waitTime = 1000 + 5000*exponential(0.9);
        setTimeout(function(){
            clearInterval(trial.timer);
            $('#subjResponse').html("<p><br>Your opponent made a decision. Click 'Next!' to continue.<br><br></p>")
            $('#subjResponse').css('opacity','1');
            $('#next').prop('disabled',false);
        }, trial.waitTime);
    }
    bullshitterWait();
}

function computerDraw(){
    //groundTruth
    for(var i=0; i<expt.marblesSampled; i++){
        if(Math.random() < trial.probabilityRed){
            trial.drawnRed += 1;
        } else{
            trial.drawnBlue += 1;
        }
    }

    var rand = Math.random();
    var lie = getK(expt.marblesSampled, trial.probabilityRed, rand);
    trial.compLie = lie;
    trial.compDetect = -1;
    console.log("CompLie: " + trial.compLie)
    if(lie <= trial.drawnRed){
        trial.reportedDrawn = trial.drawnRed;
    } else{
        trial.reportedDrawn = lie;
    }
}

function callout(call){
    trial.responseTime = Date.now() - trial.responseStartTime;
    $('.callout-button').prop('disabled', true);
    if(call == 'accept'){
        $('#accept-button').css('opacity','1');
        $('#reject-button').css('opacity','0.5');
        trial.callBS = false;
    } else{
        $('#reject-button').css('opacity','1');
        $('#accept-button').css('opacity','0.5');
        trial.callBS = true;
    }
    $('#next').prop('disabled',false);
}

function computerBSDetector(){
    trial.callBS = false;
    trial.compDetect = cbinom(expt.marblesSampled, trial.probabilityRed, trial.reportedDrawn) - (cbinom(expt.marblesSampled, trial.probabilityRed, (expt.marblesSampled/2)) - 0.5) //lowers prob of celling out by centering cbinom at 0.5 for 5 marbles drawn
    trial.compLie = -1;
    console.log("CompDetect: " + trial.compDetect)
    if(Math.random() < trial.compDetect){
        trial.callBS = true;
    }
}

function restartTrial(){
    document.getElementById('trial').style.display = 'block';
    if(trial.roleCurrent == "bullshitter"){
        var roletxt = "marble-drawer"
    } else{
        var roletxt = "responder"
    }
    $('.trialNum').html("Round " + (trial.trialNumber+1) + ": You are the <i>" + roletxt + "</i>");
    $('#urnsvg').empty();
    $('#tubesvg').empty();

    trial.probabilityRed = sample(expt.trialProbs); //can set this to a number that changes across trials
    console.log(trial.probabilityRed);
    trial.probabilityBlue = 1-trial.probabilityRed;
    trial.numRed = 0;
    trial.numBlue = 0;
    trial.drawnRed = 0;
    trial.drawnBlue = 0;
    fillUrn(1000,trial.probabilityRed);
    
    $('#subjResponse').css('opacity','0');
    $('.callout-button').css('opacity','0.8');
    $('.callout-button').prop('disabled', false);
    turn.numDrawn = 0;
    $('input[type=text]').val("");
    $('#reportMarbles').prop('disabled',true);
    $('#next').prop('disabled',true);

    trial.startTime = Date.now();
}

function bullshitter() {
    restartTrial();

    $('#trialInstruct').html("Click the 'Draw Marble' button to sample marbles from the box. Draw <b>10</b> marbles.<br>Here's how points work: each red marble is 1 point for you; each blue marble is 1 point for your opponent.")
    $('#subjResponse').html('<label><br><br><br>Say how many <b style="color:red">red</b> marbles you want your opponent to think you drew:</label><input type="text" id="reportMarbles" value="" size="2" maxlength="2"/> <button class="active-button" id="report-button" type="button" onclick="report();">Report!</button><br>');
    $('#urnsvg').css('background-color','white');
    $('#tubesvg').css('background-color','white');
    $('#draw-button').prop('disabled',false);
    $('#buttonResponse').css('opacity','0');
    $('#report-button').prop('disabled',true);
    $('input[type=text]').on('input',
        function(){
            trial.reportedDrawn = parseInt($(this).val());
            if(trial.reportedDrawn >= 0 && trial.reportedDrawn <= 10 ){
                $('#report-button').prop('disabled',false);
            } else{
                $('#report-button').prop('disabled',true);
            }
    });
    
}

function flickerWait(){
    var op = 0.1;
        var increment = 0.1;
        $('#subjResponse').html('<p><br>Waiting for your opponent...<br><br></p>');
        $('#subjResponse').css('opacity','0');
    trial.timer = setInterval(go, 50)
    function go(){
        op += increment;
        $('#subjResponse').css('opacity', op);
        if(op >= 1){
            increment = -increment;
        }
        if(op <= 0){
            increment = -increment;
        }
    }
}

function bullshitDetector() {
    restartTrial();

    $('#trialInstruct').html("It's your opponent's turn to sample marbles.<br><br>")
    //$('#urnsvg').css('background-color','purple');
    $('#tubesvg').css('background-color','purple');
    $('#draw-button').prop('disabled',true);

    function bullshitDetectWait() {
        flickerWait();
        
        trial.waitTime = 5000 + 10000*exponential(0.9);
        setTimeout(function(){
            clearInterval(trial.timer);
            $('#trialInstruct').html("Click <b style='color:green'>'Accept'</b> if you think your opponent is <b style='color:green'>telling the truth</b> or <b style='color:red'>'Reject'</b> if you think your opponent is <b style='color:red'>lying</b>. Then click 'Next!'<br>Here's how points work: each red marble is 1 point for you; each blue marble is 1 point for your opponent.");
            $('#subjResponse').html('<p>Your opponent said they drew <b id="reportMarbles"/> red marbles.<br><br>Your opponent will win <b id="oppPoints"></b> points and you will win <b id="yourPoints"/> points this round.</p>');
            computerDraw();
            $('#reportMarbles').html(trial.reportedDrawn);
            $('#oppPoints').html(trial.reportedDrawn);
            $('#yourPoints').html(expt.marblesSampled - trial.reportedDrawn);
            $('#subjResponse').css('opacity','1');
            $('#buttonResponse').css('opacity','1');
            trial.responseStartTime = Date.now();
        }, trial.waitTime);
    }
    bullshitDetectWait();
}

function toScoreboard(){
    document.getElementById('trial').style.display = 'none';
    document.getElementById('scoreboard').style.display = 'block';

    if(trial.roleCurrent == 'bullshitter'){
        computerBSDetector();
    }

    if(!trial.callBS){
        if(trial.roleCurrent == 'bullshitter'){
            $('#calledBS').html("Your opponent <b>accepted</b> your reported answer.");
            trial.playerTrialScore = trial.reportedDrawn;
            trial.oppTrialScore = expt.marblesSampled - trial.reportedDrawn;
        } else{
            $('#calledBS').html("You <b>accepted</b> your opponent's reported answer.");
            trial.oppTrialScore = trial.reportedDrawn;
            trial.playerTrialScore = expt.marblesSampled - trial.reportedDrawn;
        }
    } else{
        if(trial.roleCurrent == 'bullshitter'){
            $('#calledBS').html("Your opponent <b>rejected</b> your reported answer. ");
            //if player is telling the truth
            if(trial.reportedDrawn == trial.drawnRed){
                $('#calledBS').append("You were telling the truth.");
                trial.playerTrialScore = trial.reportedDrawn; //player gets points as reported
                trial.oppTrialScore = -trial.reportedDrawn; //opponent gets negative of points reported
            } else{
                $('#calledBS').append("You were lying.");
                trial.oppTrialScore = trial.reportedDrawn; //opponent gets points as reported
                trial.playerTrialScore = -trial.reportedDrawn; //player gets negative of points reported
            }
        } else{
            $('#calledBS').html("You <b>rejected</b> your opponent's reported answer. ");
            //if player catches a liar
            if(trial.reportedDrawn != trial.drawnRed){
                $('#calledBS').append("Your opponent was lying.");
                trial.playerTrialScore = trial.reportedDrawn;
                trial.oppTrialScore = -trial.reportedDrawn;
            } else{
                $('#calledBS').append("Your opponent was telling the truth.");
                trial.oppTrialScore = trial.reportedDrawn;
                trial.playerTrialScore = -trial.reportedDrawn;
            }
        }
    }
    $('#playerPts').html(trial.playerTrialScore);
    $('#oppPts').html(trial.oppTrialScore);

    expt.playerTotalScore += trial.playerTrialScore;
    expt.oppTotalScore += trial.oppTrialScore;

    $('#playerScore').html(expt.playerTotalScore);
    $('#oppScore').html(expt.oppTotalScore);
}

function trialDone() {
    // hide trial.
    document.getElementById('scoreboard').style.display = 'none';
    trial.trialTime = Date.now() - trial.startTime;
    trial.trialNumber += 1;
    recordData();

    //if(expt.playerTotalScore >= expt.goalScore || expt.oppTotalScore >= expt.goalScore){
    if(trial.trialNumber >= expt.trials){
        if(expt.playerTotalScore == expt.oppTotalScore){
            $('#whowon').html("You and your opponent tied!");
        } else if(expt.playerTotalScore > expt.oppTotalScore){
            $('#whowon').html("You won!");
        } else{
            $('#whowon').html("Your opponent won!");
        }
        // expt done
        data = {client: client, expt: expt, trials: trialData};
        writeServer(data);

        document.getElementById('completed').style.display = 'block';
    } else {
        if(trial.roleCurrent == 'bullshitter'){
            trial.roleCurrent = 'bullshitDetector';
            bullshitDetector();
        } else{
            trial.roleCurrent = 'bullshitter';
            bullshitter();
        }
    }
}


function experimentDone() {
    submitExternal(client);
}

// helper functions
function sample(set) {
    return (set[Math.floor(Math.random() * set.length)]);
}

function randomDouble(min, max){
    return Math.random() * (max - min) + min;
}

function recordData(){
    trialData.push({
        trialNumber: trial.trialNumber,
        roleCurrent: trial.roleCurrent,
        marblesSampled: expt.marblesSampled,
        probabilityRed: trial.probabilityRed,
        drawnRed: trial.drawnRed,
        reportedDrawn: trial.reportedDrawn,
        compLie: trial.compLie,
        compDetect: trial.compDetect,
        callBS: trial.callBS,
        playerTrialScore: trial.playerTrialScore,
        oppTrialScore: trial.oppTrialScore,
        playerTotalScore: expt.playerTotalScore,
        oppTotalScore: expt.oppTotalScore,
        waitTime: trial.waitTime,
        responseTime: trial.responseTime,
        trialTime: trial.trialTime
    })
}

function debugLog(message) {
    if(expt.debug){
        console.log(message);
    }
}

function binom(n, p, k){
    return (factorial(n)/(factorial(k)*factorial(n-k))) * p ** k * (1-p) ** (n-k);
}

function factorial(x){
    if(x == 0){
        return 1;
    } else{
        return x*factorial(x-1);
    }
}

function cbinom(n, p, k){
    if(k == 0){
        return binom(n, p, 0);
    } else{
        return binom(n, p, k) + cbinom(n, p, k-1);
    }
}

function getK(n, p, r){
    var i = 0;
    while(r > cbinom(n, p, i)){
        i += 1;
    }
    return i;
}

function exponential(lambda){
    return lambda * Math.E ** (-lambda*Math.random())
}







