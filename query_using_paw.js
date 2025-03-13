javascript:(function(){
    var selection = window.getSelection().toString();
    if (selection.length > 0) {
        var url = encodeURIComponent(window.location.href);
        var title = encodeURIComponent(document.title || "[untitled page]");
        var body = encodeURIComponent(selection);
        var parent = window.getSelection().getRangeAt(0).commonAncestorContainer.parentNode;
        while (parent.nodeType !== Node.ELEMENT_NODE) {
            parent = parent.parentNode;
        }
        var p_tag_parent = parent;
        while (p_tag_parent.tagName !== undefined && p_tag_parent.tagName !== 'P') {
            p_tag_parent = p_tag_parent.parentNode;
        }
        if (p_tag_parent !== document) {
            parent = p_tag_parent;
        }
        var note = encodeURIComponent(parent.textContent || "");
        location.href = 'org-protocol://paw?template=w&url=' + url + '&title=' + title + '&note=' + note + '&body=' + body;
    }
}());
