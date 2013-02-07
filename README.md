## Requirements
* https://github.com/ahyatt/emacs-websocket

## Install

### Clone repository
```el
git clone https://github.com/ahyatt/emacs-websocket
git clone https://github.com/mechairoi/crxel
```

### Install Chrome Extension
Install chrome extension to /path/to/crxel/crxel.crx

### Configure Emacs
~/.emacs.d/
```el
(add-to-list 'load-path "/path/to/emacs-websocket")
(add-to-list 'load-path "/path/to/crxel")
(require 'crxel)
(crxel/start 9649)
```

### Run
```el
(crxel/eval "1+1" :success 'print :fail 'error)
(crxel/eval "var callback = window.crxel.callback;
             chrome.tabs.query({}, function(tabs) {
                 callback(JSON.stringify(tabs));
             });"
            :async t
            :success (lambda (data)
                       (print (json-read-from-string data)))
            :fail 'error)
```

## Example
* https://gist.github.com/mechairoi/4730136
