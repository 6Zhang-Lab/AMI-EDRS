(function(){
 function clearVisible(){['risk_cards','result_details','result_context','download_controls'].forEach(function(id){var n=document.getElementById(id);if(n)n.innerHTML='';});}
 document.addEventListener('input',function(e){if(e.target&&e.target.id&&e.target.id.indexOf('manual_')===0)clearVisible();});
 document.addEventListener('change',function(e){var id=e.target&&e.target.id;if(id&&(['csv','row_index','confirm','manual_eligible','manual_confirm','manual_sex','manual_icu','manual_hospital'].indexOf(id)>=0||e.target.name==='endpoint')){clearVisible();if(id==='csv'&&window.Shiny)Shiny.setInputValue('invalidate_nonce',Date.now(),{priority:'event'});}});
})();
