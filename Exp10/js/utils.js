
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
        $('#trueDraw').html(trial.drawnRed);
        $('#draw-button').prop('disabled',true);
        $('#subjResponse').css('opacity',1);
        $('#reportMarbles').prop('disabled',false);
        $('#trialInstruct').css('opacity',0);
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
            $('#subjResponse').html("<p><br><br><br>Your opponent made a decision. Click 'Next!' to continue.<br><br><br></p>")
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
                trial.reportedDrawn = trial.drawnRed;
            } else{
                trial.reportedDrawn = lie;
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
    trial.compDetect = cbinom(expt.marblesSampled, trial.probabilityRed, trial.reportedDrawn) - (cbinom(expt.marblesSampled, trial.probabilityRed, (expt.marblesSampled*trial.probabilityRed)) - 0.5) //lowers prob of celling out by centering cbinom at expected mean
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
    $('#qInstruct').hide();

    trial.startTime = Date.now();
}


function flickerWait(){
    var op = 0.1;
    var increment = 0.1;
    $('#subjResponse').html('<p><br><br><br>Waiting for your opponent...<br><br><br></p>');
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
    if(trial.exptPart == "practice" & trial.trialNumber == 0){
        $('#catchQ').before("<p id='qInstruct'>Throughout the experiment, you will randomly be asked questions about the task.<br>If you get the question wrong, you have to wait 3 seconds before being able to move on.<br><br></p>")
    }

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







