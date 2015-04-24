Shiny.addCustomMessageHandler('jsCode', function(message) { eval(message.code); });

$(document).ready(function() {
    Shiny.addCustomMessageHandler('show_error', function(message) { alert(message); });
});