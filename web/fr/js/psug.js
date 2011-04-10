(function( $ ){
    $.fn.serializeJSON=function() {
        var json = {};
        jQuery.map($(this).serializeArray(), function(n, i){
            json[n['name']] = n['value'];
        });
    return json;
    };
})( jQuery );

var usi_log=function(msg){
    var $dbg=$("#usi_debug");
    $dbg.append(msg+"<br />");
};
