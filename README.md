## Requirements
* https://github.com/ahyatt/emacs-websocket

## How to install

### Clone repository
```el
git clone https://github.com/ahyatt/emacs-websocket
git clone https://github.com/mechairoi/crxel
```

### Load chrome extension
1. Visit chrome://extensions in your browser.
2. Ensure that the Developer Mode checkbox is checked.
3. Click Load unpacked extension… and select `/path/to/crxel/crxel.crx`

### Configure Emacs
```el
;; ~/.emacs.d/
(add-to-list 'load-path "/path/to/emacs-websocket")
(add-to-list 'load-path "/path/to/crxel")
(require 'crxel)
(crxel/start 9649)
```

## Samples

```el
;; Evaluate "1+1" in javascript and `print` the outome "2"
(crxel/eval "1+1" :success 'print :fail 'error)
```

or

```el
;; Call asynchronous API `chrome.tabs.query`
;; and return the outcome by `window.crxel.callback`
(crxel/eval "var callback = window.crxel.callback;
             chrome.tabs.query({}, function(tabs) {
                 callback(JSON.stringify(tabs));
             });"
            :async t
            :success (lambda (data)
                       (print (json-read-from-string data)))
            :fail 'error)
```


## Sample applications
* [anything-chrome-tabs.el](https://gist.github.com/mechairoi/4730136)
