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
