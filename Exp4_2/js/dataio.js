

function readServer(request){  
  debugLog('initiate server read');
  $.ajax({
      dataType: 'json',
      type: 'POST',
      url: expt.readURL,
      data: { 
        request: JSON.stringify(request)
      },
      beforeSend: function(xhr){
          debugLog('request: ' + JSON.stringify(request));
        },
      success: function(data){
          debugLog('success');
          debugLog(data);

          trial.expt = data['Data']['expt'];
          trial.seed = data['Data']['seed'];
          trial.chain = data['Data']['chain'];
          trial.iter = data['Data']['iter'];
          trial.stimuli = data['Data']['stimuli'];
          startTrial();
        },
      error:function(xhr, status, error){
          debugLog('failure loading data');
          debugLog(xhr.responseText);
          debugLog(status);
          debugLog(error);

          genTrial(expt.default.n);
          startTrial();
        }
      });
}            

function writeServer(data){
	debugLog('initiate server write');
  $.ajax({
      dataType: 'json',
      type: 'POST',
      url: expt.saveURL,
      data: { data: JSON.stringify(data)},
        success: function(data){
          debugLog('success saving data!');
        },
        error:function(xhr, status, error){
          debugLog('failure saving data');
          debugLog(xhr.responseText);
          debugLog(status);
          debugLog(error);
        }
      });
}

