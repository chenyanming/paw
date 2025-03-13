javascript:(function() {
    function loadScript(url, callback) {
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;
        script.onload = callback;
        document.head.appendChild(script);
    }
    function disable_highlight() {
        console.log('disable auto highlight mode');
        $(`.xqdd_highlight_new_word`).removeClass("xqdd_highlight_new_word").addClass("xqdd_highlight_disable");
    }
    loadScript('https://code.jquery.com/jquery-3.6.0.min.js', disable_highlight);
})();
