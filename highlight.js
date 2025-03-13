javascript:(function() {
    function loadScript(url, callback) {
        var script = document.createElement('script');
        script.type = 'text/javascript';
        script.src = url;
        script.onload = callback;
        document.head.appendChild(script);
    }
    function fetchAndHighlightWords() {
        // Define the CSS styles
        var style = document.createElement('style');
        style.innerHTML = `.xqdd_highlight_new_word {background-color: #ffe895;}`;
        document.head.appendChild(style);
        // Fetch words from server and highlight
        fetch('http://localhost:5001/words')
            .then(response => response.json())
            .then(data => {
                newWords = data;
                highlight(textNodesUnder(document.body, mygoodfilter));
                console.log("Highlight done");
            })
            .catch(error => {
                console.error(error);
            });
    }
    function textNodesUnder(el, filter) {
        var n, a = [], walk = document.createTreeWalker(el, NodeFilter.SHOW_TEXT, filter, false);
        while(n = walk.nextNode()) a.push(n);
        return a;
    }
    function mygoodfilter(node) {
        var good_tags_list = [
            "PRE", "A", "P", "H1", "H2", "H3", "H4", "H5", "H6", "B", "SMALL", "STRONG",
            "Q", "DIV", "SPAN", "LI", "TD", "OPTION", "I", "BUTTON", "UL", "CODE", "EM", "TH", "CITE", "RUBY"
        ];
        if (node.parentElement && good_tags_list.indexOf(node.parentElement.tagName) !== -1) {
            return NodeFilter.FILTER_ACCEPT;
        }
        return NodeFilter.FILTER_SKIP;
    }
    function highlight(nodes) {
        for (var i = 0; i < nodes.length; i++) {
            var node = nodes[i];
            var text = node.textContent;
            if (text.trim() === "") { continue; }
            var newNodeChildrens = highlightNode(text);
            var parent_node = node.parentNode;
            if (newNodeChildrens === undefined || newNodeChildrens.length === 0) { continue; }
            for (var j = 0; j < newNodeChildrens.length; j++) {
                parent_node.insertBefore(newNodeChildrens[j], node);
            }
            parent_node.removeChild(node);
        }
    }
    function highlightNode(texts) {
        var words = [];
        var tempTexts = texts.match(/[^\s]+/g) || [];
        for (var i = 0; i < tempTexts.length; i++) {
            if (tempTexts[i].trim() !== "") {
                words.push(tempTexts[i]);
            }
        }
        if (words.length >= 1) {
            var newNodeChildrens = [];
            var remainTexts = texts;
            var checkedText = "";
            for (var i = 0; i < words.length; i++) {
                var word = words[i];
                var currPos = remainTexts.indexOf(word);
                if (newWords && newWords.wordInfos && (newWords.wordInfos.hasOwnProperty(word.toLowerCase()) || newWords.wordInfos.hasOwnProperty(word))) {
                    if (checkedText !== "") {
                        newNodeChildrens.push(document.createTextNode(checkedText));
                        checkedText = "";
                    }
                    if (currPos == 0) {
                        newNodeChildrens.push(hightlightText(word));
                    } else {
                        newNodeChildrens.push(document.createTextNode(remainTexts.slice(0, currPos)));
                        newNodeChildrens.push(hightlightText(word));
                    }
                } else {
                    checkedText += remainTexts.slice(0, currPos + word.length);
                }
                remainTexts = remainTexts.slice(currPos + word.length);
            }
            if (newNodeChildrens.length !== 0) {
                if (checkedText !== "") {
                    newNodeChildrens.push(document.createTextNode(checkedText));
                }
                newNodeChildrens.push(document.createTextNode(remainTexts));
            }
            return newNodeChildrens;
        }
    }
    function hightlightText(text) {
        return $("<span>")
            .attr("word", text.toLowerCase())
            .attr("class", "xqdd_highlight_new_word")
            .text(text)[0];
    }
    // Load jQuery and then execute the highlight function
    loadScript('https://code.jquery.com/jquery-3.6.0.min.js', fetchAndHighlightWords);
})();
