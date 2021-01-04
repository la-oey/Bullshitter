// https://ucsd.sona-systems.com/webstudy_credit.aspx?experiment_id=1465&credit_token=c6393dd431374ab48035c7fafafced2e&survey_code=XXXX
// experiment settings
var expt = {
    saveURL: 'submit.simple.php',
    trials: 100, //switch to 100
    practiceTrials: 4, //how many practice trials //switch to 4
    //goalScore: 100,
    marblesSampled: 10, //total number of marbles drawn per trial
    roles: ['bullshitter', 'bullshitDetector'],
    roleFirst: 'bullshitter', //roles: {'bullshitter','bullshitDetector'}
    allTrialProbs: [0.2,0.5,0.8],
    trialProbs: 0,
    catchTrials: [],
    pseudo: null,
    stat: {
        playerTotalScore: 0,
        oppTotalScore: 0,
        truth: 0,
        truth_noBS: 0,
        truth_BS: 0,
        lie: 0,
        lie_noBS: 0,
        lie_BS: 0,
        noBS: 0,
        noBS_truth: 0,
        noBS_lie: 0,
        BS: 0,
        BS_truth: 0,
        BS_lie: 0
    },
    sona: {
        experiment_id: 1505,
        credit_token: 'b20092f9d3b34a378ee654bcc50710ea'
    }
};
var trial = {
    exptPart: 'practice', //parts: {'practice','trial'}
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
    marblesDrawn: [],
    numRed: 0,
    numBlue: 0,
    drawnRed: 0,
    drawnBlue: 0,
    reportedDrawn: 0,
    compLie: 0,
    compUnifLie: false,
    compDetect: 0,
    callBS: false,
    callBStxt: '',
    catch: {
        question: '',
        response: 0,
        key: 0,
        responseStartTime: 0,
        responseTime: 0
    },
    pseudoRound: false,
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
    $('#instructPractice').html(expt.practiceTrials);
    $('#instructRounds').html(expt.trials);
    $('#instructMarblesSampled').html(expt.marblesSampled);
    expt.trialProbs = sample(expt.allTrialProbs);
}

function clickInstructions() {
    document.getElementById('instructions').style.display = 'none';
    document.getElementById('prePractice').style.display = 'block';
}

function clickPrePractice(){
    document.getElementById('prePractice').style.display = 'none';
    expt.catchTrials = distributeChecks(expt.practiceTrials, 0.50); // 0.5 of practice trials have an attention check
    if(expt.roleFirst == 'bullshitter'){
        bullshitter();
    } else{
        bullshitDetector();
    }
}

function clickPostPractice(){
    document.getElementById('postPractice').style.display = 'none';

    expt.catchTrials = distributeChecks(expt.trials, 0.10); // 0.1 of expt trials have an attention check
    expt.pseudo = distributePseudo(expt.trials, 0, 10);
    //console.log(expt.pseudo);
    expt.roleFirst = sample(expt.roles);
    trial.roleCurrent = expt.roleFirst;
    if(expt.roleFirst == 'bullshitter'){
        bullshitter();
    } else{
        bullshitDetector();
    }
}

function fillUrn(totalMarbles, probability) {
    var exactRed = Math.round(totalMarbles * probability);
    var exactBlue = totalMarbles - exactRed;

    for(var i=0; i<totalMarbles; i++){
        var color = "blue";

        // balls in urn correspond to exact probability distribution, i.e. no random sampling
        if(Math.random() < (exactRed/(exactRed + exactBlue))){
            color = "red"
            trial.numRed += 1;
            exactRed -= 1;
        } else{
            trial.numBlue += 1;
            exactBlue -= 1;
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
    //predetermine ordering of marbles drawn
    if(turn.numDrawn == 0){
        if(trial.pseudoRound){
            var tempOrder = [];
            for(var i=0; i<expt.marblesSampled; i++){
                if(i < expt.pseudo[trial.trialNumber]){
                    tempOrder.push("red");
                    trial.drawnRed += 1;
                } else{
                    tempOrder.push("blue");
                    trial.drawnBlue += 1;
                }
            }
            trial.marblesDrawn = shuffle(tempOrder);
        } else{
            for(var i=0; i<expt.marblesSampled; i++){
                var color = "blue";
                if(Math.random() < trial.probabilityRed){
                    color = "red";
                    trial.drawnRed += 1;
                } else{
                    trial.drawnBlue += 1;
                }
                trial.marblesDrawn.push(color);
            }
        }
    }
    
    marble("#tubesvg", trial.marblesDrawn[turn.numDrawn], 17.5, .5*$('#tube').width(), ($('#tube').height()*.95)-(turn.numDrawn/expt.marblesSampled)*$('#tube').height())

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
        
        trial.waitTime = 1000 + 3000*exponential(0.75);
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

    if(trial.pseudoRound){
        trial.reportedDrawn = expt.pseudo[trial.trialNumber];
    } else{
        if(Math.random() < 0.2){
            trial.compUnifLie = true;
            trial.reportedDrawn = Math.floor(randomDouble(0,11));
        } else{
            var rand = Math.random();
            var lie = getK(expt.marblesSampled, trial.probabilityRed, rand);
            trial.compLie = lie;
            trial.compDetect = -1;
            //console.log("CompLie: " + trial.compLie)
            if(lie <= trial.drawnRed){
                trial.reportedDrawn = lie;
            } else{
                trial.reportedDrawn = trial.drawnRed;
            }
        }
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
    trial.compDetect = 1- cbinom(expt.marblesSampled, trial.probabilityRed, trial.reportedDrawn) - (cbinom(expt.marblesSampled, trial.probabilityRed, (expt.marblesSampled*trial.probabilityRed)) - 0.5) //lowers prob of celling out by centering cbinom at expected mean
    trial.compLie = -1;
    //console.log("CompDetect: " + trial.compDetect)
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

    trial.probabilityRed = expt.trialProbs; //can set this to a number that changes across trials
    trial.probabilityBlue = 1-trial.probabilityRed;
    trial.numRed = 0;
    trial.numBlue = 0;
    trial.drawnRed = 0;
    trial.drawnBlue = 0;
    fillUrn(400,trial.probabilityRed);
    trial.compUnifLie = false;
    
    $('#subjResponse').css('opacity','0');
    $('.callout-button').css('opacity','0.8');
    $('.callout-button').prop('disabled', false);
    $('#buttonResponse').css('opacity','0');
    turn.numDrawn = 0;
    trial.marblesDrawn = [];
    $('input[type=text]').val("");
    $('#reportMarbles').prop('disabled',true);
    $('#next').prop('disabled',true);

    if(trial.exptPart != 'practice'){
        trial.pseudoRound = trial.trialNumber in expt.pseudo;
    }

    trial.catch.key = -1;
    trial.catch.response = -1;
    trial.catch.responseTime = -1;
    $('#catchQ').hide();

    trial.startTime = Date.now();
}

function bullshitter() {
    restartTrial();

    $('#trialInstruct').html("Click the 'Draw Marble' button to sample marbles from the box. Draw <b>10</b> marbles.<br>Here's how points work: each <b style='color:red'>red</b> marble is 1 point for your opponent; each <b style='color:blue'>blue</b> marble is 1 point for you.")
    $('#subjResponse').html('<label><br><br><br>Say how many <b style="color:red">red</b> marbles you want your opponent to think you drew:</label><input type="text" id="reportMarbles" value="" size="2" maxlength="2" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false"/> <button class="active-button" id="report-button" type="button" onclick="report();">Report!</button><br>');
    $('#urnsvg').css('background-color','white');
    $('#tubesvg').css('background-color','white');
    $('#draw-button').prop('disabled',false);
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
        
        trial.waitTime = 3000 + 6000*exponential(0.75);
        setTimeout(function(){
            clearInterval(trial.timer);
            $('#trialInstruct').html("Click <b style='color:green'>'Accept'</b> if you think your opponent is <b style='color:green'>telling the truth</b> or <b style='color:red'>'Reject'</b> if you think your opponent is <b style='color:red'>lying</b>. Then click 'Next!'<br>Here's how points work: each <b style='color:red'>red</b> marble is 1 point for you; each <b style='color:blue'>blue</b> marble is 1 point for your opponent.");
            $('#subjResponse').html('<p>Your opponent said they drew <b id="reportMarbles"/> red marbles.<br><br>Your opponent will win <b id="oppPoints"></b> points and you will win <b id="yourPoints"/> points this round.</p>');
            computerDraw();
            $('#reportMarbles').html(trial.reportedDrawn);
            $('#oppPoints').html(expt.marblesSampled - trial.reportedDrawn);
            $('#yourPoints').html(trial.reportedDrawn);
            $('#subjResponse').css('opacity','1');
            $('#buttonResponse').css('opacity','1');
            trial.responseStartTime = Date.now();
        }, trial.waitTime);
    }
    bullshitDetectWait();
}

function submitCatch(){
    trial.catch.responseTime = Date.now() - trial.catch.responseStartTime;
    $('input[type=text]').prop('disabled',true);
    $('input[type=text]').css('opacity','0.7');
    $('#catch-button').prop('disabled', true);
    var timeoutTime = 0;
    if(trial.catch.key == trial.catch.response){
        $('#catchQ').append('<img src="img/yup.png" height=18 vertical-align="middle" hspace="20">');
    } else{
        $('#catchQ').append('<img src="img/nah.png" height=18 vertical-align="middle" hspace="20">');
        timeoutTime = 3000;
    }
    setTimeout(function(){
        if(trial.exptPart == 'practice' | (trial.trialNumber + 1) % 5 == 0){
            $('.scoreboardDiv').css('opacity','1');
        } 
        $('.scoreReport').css('opacity','1');
        $('#nextScoreboard').css('opacity','1');
    }, timeoutTime);
}

function catchTrial(role, exptPart){
    if(role == 'bullshitter'){
        trial.catch.question = 'How many red marbles did you actually draw?'
        trial.catch.key = trial.drawnRed;
    } else{
        trial.catch.question = 'How many red marbles did your opponent report drawing?'
        trial.catch.key = trial.reportedDrawn;
    }
    $('#catchQ').html('<label>'+trial.catch.question+'</label>');
    $('#catchQ').append('<input type="text" id="reportCatch" value="" size="2" maxlength="2" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false"/> <button class="active-button" id="catch-button" type="button" onclick="submitCatch();">Submit</button> ');

    $('#catch-button').prop('disabled',true);
    $('input[type=text]').on('input',
        function(){
            trial.catch.response = parseInt($(this).val());
            if(trial.catch.response >= 0 && trial.catch.response <= 10 ){
                $('#catch-button').prop('disabled',false);
            } else{
                $('#catch-button').prop('disabled',true);
            }
    });

    $('.scoreReport').css('opacity','0');
    $('.scoreboardDiv').css('opacity','0');
    $('#nextScoreboard').css('opacity','0');
}

function toScoreboard(){
    document.getElementById('trial').style.display = 'none';
    document.getElementById('scoreboard').style.display = 'block';

    trial.catch.responseStartTime = Date.now();
    if(expt.catchTrials.includes(trial.trialNumber)){
        $('#catchQ').show();
        catchTrial(trial.roleCurrent, trial.exptPart);
    } else if(trial.exptPart == 'trial' & (trial.trialNumber + 1) % 5 != 0){
        $('#totalScoreboardDiv').css('opacity','0');
    } else{
        $('#totalScoreboardDiv').css('opacity','1');
    }

    if(trial.roleCurrent == 'bullshitter'){
        computerBSDetector();
    }

    if(!trial.callBS){
        if(trial.roleCurrent == 'bullshitter'){
            trial.callBStxt = "Your opponent <b style='color:green'>accepted</b> your reported answer.<br><br>";
            trial.playerTrialScore = expt.marblesSampled - trial.reportedDrawn; 
            trial.oppTrialScore = trial.reportedDrawn;
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "You were <b>telling the truth</b>.<br><br>";
                expt.stat.truth += 1;
                expt.stat.truth_noBS += 1;
            } else{
                trial.callBStxt = trial.callBStxt + "You were <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.lie += 1;
                expt.stat.lie_noBS += 1;
            }
        } else{
            trial.callBStxt = "You <b style='color:green'>accepted</b> your opponent's reported answer.<br><br>";
            expt.stat.noBS += 1;
            trial.oppTrialScore = expt.marblesSampled - trial.reportedDrawn;
            trial.playerTrialScore = trial.reportedDrawn;
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>telling the truth</b>.<br><br>";
                expt.stat.noBS_truth += 1;
            } else{
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.noBS_lie += 1;
            }
        }
    } else{
        if(trial.roleCurrent == 'bullshitter'){
            trial.callBStxt = "Your opponent <b style='color:red'>rejected</b> your reported answer.<br><br>";
            //if player is telling the truth
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "You were <b>telling the truth</b>.<br><br>";
                expt.stat.truth += 1;
                expt.stat.truth_BS += 1;
                trial.playerTrialScore = expt.marblesSampled - trial.reportedDrawn; //opponent gets 10 - points as reported w/ -5 penalty
                trial.oppTrialScore = trial.reportedDrawn - 5; //player gets points as reported

            } else{
                trial.callBStxt = trial.callBStxt + "You were <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.lie += 1;
                expt.stat.lie_BS += 1;
                trial.playerTrialScore = -5; //player gets -5 points
                trial.oppTrialScore = 5; //opponent gets +5 points
            }
        } else{
            trial.callBStxt = "You <b style='color:red'>rejected</b> your opponent's reported answer.<br><br>";
            expt.stat.BS += 1;
            //if player catches a liar
            if(trial.reportedDrawn == trial.drawnRed){
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>telling the truth</b>.<br><br>";
                expt.stat.BS_truth += 1;
                trial.playerTrialScore = trial.reportedDrawn - 5; //player gets 10 - points as reported w/ -5 penalty
                trial.oppTrialScore = expt.marblesSampled - trial.reportedDrawn; //opponent gets points as reported
            } else{
                trial.callBStxt = trial.callBStxt + "Your opponent was <b>lying</b>.<br>The true answer was <b>" + trial.drawnRed + "</b>.";
                expt.stat.BS_lie += 1;
                trial.playerTrialScore = 5; //player gets +5 points
                trial.oppTrialScore = -5; //opponent gets -5 points
            }
        }
    }

    expt.stat.playerTotalScore += trial.playerTrialScore;
    expt.stat.oppTotalScore += trial.oppTrialScore;
    $('.playerScore').html(expt.stat.playerTotalScore);
    $('.oppScore').html(expt.stat.oppTotalScore);

    if(trial.exptPart == "practice"){
        $('#calledBS').html(trial.callBStxt);
        $('#playerPts').html(scorePrefix(trial.playerTrialScore));
        $('#oppPts').html(scorePrefix(trial.oppTrialScore));
        //$('.playerScore').html((expt.stat.playerTotalScore - trial.playerTrialScore) + " + " + trial.playerTrialScore + " = " + expt.stat.playerTotalScore);
        //$('.oppScore').html((expt.stat.oppTotalScore - trial.oppTrialScore) + " + " + trial.oppTrialScore + " = " + expt.stat.oppTotalScore);
    } else{
        $('.scoreReport').html("Click to move on to the next round.");
        $('#trialScoreboardDiv').hide();
    }
}

function trialDone() {
    // hide trial.
    document.getElementById('scoreboard').style.display = 'none';
    trial.trialTime = Date.now() - trial.startTime;
    trial.trialNumber += 1;
    recordData();

    if(trial.exptPart == "practice" & trial.trialNumber >= expt.practiceTrials){
        trial.trialNumber = 0;
        trial.exptPart = 'trial';
        expt.stat.playerTotalScore = 0;
        expt.stat.oppTotalScore = 0;
        expt.stat.truth = 0;
        expt.stat.truth_noBS = 0;
        expt.stat.truth_BS = 0;
        expt.stat.lie = 0;
        expt.stat.lie_noBS = 0;
        expt.stat.lie_BS = 0;
        expt.stat.noBS = 0;
        expt.stat.noBS_truth = 0;
        expt.stat.noBS_lie = 0;
        expt.stat.BS = 0;
        expt.stat.BS_truth = 0;
        expt.stat.BS_lie = 0;
        document.getElementById('trial').style.display = 'none';
        document.getElementById('postPractice').style.display = 'block';
    } else if(trial.trialNumber >= expt.trials){
        if(expt.stat.playerTotalScore == expt.stat.oppTotalScore){
            $('#whowon').html("You and your opponent tied!");
        } else if(expt.stat.playerTotalScore > expt.stat.oppTotalScore){
            $('#whowon').html("You won!");
        } else{
            $('#whowon').html("Your opponent won!");
        }

        $('.scoreboardDiv').show();

        $('.playerScore').html(expt.stat.playerTotalScore);
        $('.oppScore').html(expt.stat.oppTotalScore);

        calculateStats('#stat_truth', expt.stat.truth, expt.stat.truth + expt.stat.lie);
        calculateStats('#stat_truth_noBS', expt.stat.truth_noBS, expt.stat.truth);
        calculateStats('#stat_truth_BS', expt.stat.truth_BS, expt.stat.truth);
        calculateStats('#stat_lie', expt.stat.lie, expt.stat.truth + expt.stat.lie);
        calculateStats('#stat_lie_noBS', expt.stat.lie_noBS, expt.stat.lie);
        calculateStats('#stat_lie_BS', expt.stat.lie_BS, expt.stat.lie);
        calculateStats('#stat_noBS', expt.stat.noBS, expt.stat.noBS + expt.stat.BS);
        calculateStats('#stat_noBS_truth', expt.stat.noBS_truth, expt.stat.noBS);
        calculateStats('#stat_noBS_lie', expt.stat.noBS_lie, expt.stat.noBS);
        calculateStats('#stat_BS', expt.stat.BS, expt.stat.noBS + expt.stat.BS);
        calculateStats('#stat_BS_truth', expt.stat.BS_truth, expt.stat.BS);
        calculateStats('#stat_BS_lie', expt.stat.BS_lie, expt.stat.BS);

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

function shuffle(set){
    var j, x, i;
    for (i = set.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = set[i];
        set[i] = set[j];
        set[j] = x;
    }
    return set;
}

function recordData(){
    trialData.push({
        exptPart: trial.exptPart,
        trialNumber: trial.trialNumber,
        roleCurrent: trial.roleCurrent,
        marblesSampled: expt.marblesSampled,
        probabilityRed: trial.probabilityRed,
        drawnRed: trial.drawnRed,
        reportedDrawn: trial.reportedDrawn,
        compLie: trial.compLie,
        compUnifLie: trial.compUnifLie,
        compDetect: trial.compDetect,
        callBS: trial.callBS,
        playerTrialScore: trial.playerTrialScore,
        oppTrialScore: trial.oppTrialScore,
        playerTotalScore: expt.stat.playerTotalScore,
        oppTotalScore: expt.stat.oppTotalScore,
        waitTime: trial.waitTime,
        responseTime: trial.responseTime,
        catchQuestion: trial.catch.question,
        catchKey: trial.catch.key,
        catchResponse: trial.catch.response,
        catchResponseTime: trial.catch.responseTime,
        pseudoRound: trial.pseudoRound,
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

function calculateStats(string, numer, denom){
    if(denom == 0){
        $(string).html("N/A");
    } else{
        $(string).html(Math.round(numer * 100 / denom)+"%");
    }
}

function scorePrefix(score){
    if(score <= 0){
        return(score);
    } else{
        return("+" + score);
    }
}

function distributeChecks(totalTrials, freq){
    var round = Math.floor(totalTrials * freq);
    var checkRounds = [];
    for(var i=0; i<totalTrials/round; i++){
        checkRounds.push(round*i + Math.floor(randomDouble(0,round)));
    }
    return(checkRounds);
}

function distributePseudo(totalTrials, minArrPseudo, maxArrPseudo){
    var pseudoDict = {};
    var arrPseudo = [];
    var bucketOdd = [];

    for(var a=minArrPseudo; a <= maxArrPseudo; a++){
        arrPseudo.push(a);
    }
    for(var i=0; i<=totalTrials/2; i++){
        bucketOdd.push(i);
    }
    var bucketEven = bucketOdd.slice(0);

    for(var o=0; o<arrPseudo.length; o++){
        index = Math.floor(randomDouble(0, bucketOdd.length));
        pseudoDict[(2*bucketOdd.splice(index, 1)[0]+1)] = arrPseudo[o];
    }
    for(var e=0; e<arrPseudo.length; e++){
        index = Math.floor(randomDouble(0, bucketEven.length));
        pseudoDict[(2*bucketEven.splice(index, 1)[0])] = arrPseudo[e];
    }
    return(pseudoDict);
}







