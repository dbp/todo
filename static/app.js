function desc_click_listener(event) {
    var d = event.target;
    var oldText = d.innerText;
    d.innerHTML = "<form action='/todos/" + d.getAttribute("data-id") + "/update?acnt=" + d.getAttribute("data-acnt") + "' method=POST><textarea name='txt'>" + oldText + "</textarea><input type='submit'/></form>";
    var textArea = d.querySelector("textarea");
    textArea.addEventListener('keydown', function(event) {
        if (event.keyCode === 27) {
            d.innerHTML = oldText;
            d.addEventListener('click', desc_click_listener, false);
        }
    }, false);
    textArea.focus();
    textArea.selectionStart = textArea.selectionEnd = textArea.value.length
    d.removeEventListener('click', desc_click_listener);
}
document.querySelectorAll(".description").forEach(function(d) {
    d.addEventListener('click', desc_click_listener, false);
});
