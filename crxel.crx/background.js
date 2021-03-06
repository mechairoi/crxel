//
// Copyright (c) 2010 Ivan Shvedunov. All rights reserved.
// Copyright (c) 2012 Robert Krahn. All rights reserved.
// Copyright (c) 2013 Takaya Tsujikawa. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following
// disclaimer in the documentation and/or other materials
// provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// forked form https://github.com/swank-js/swank-js/blob/bf85069cfe8ad87337e9844e94e8a44b3f39cb82/client/swank-js.js

(function() {
     function JSON_stringify(s, emit_unicode)
     {
         var json = JSON.stringify(s);
         return emit_unicode ? json : json.replace(/[\u007f-\uffff]/g,
             function(c) {
                 return '\\u'+('0000'+c.charCodeAt(0).toString(16)).slice(-4);
             }
         );
     }

     window.crxel = {};
     function connect(){
         const url = "ws://localhost:9649/";
         var ws;
         ws = new WebSocket(url);
         if (!ws) return false;
         window.crxel.ws = ws;

         ws.onmessage = function(event){
             var m = JSON.parse(event.data);

             var callback = function(r){
                 ws.send(
                     JSON_stringify( {
                         "op": "result",
                         "id": m.id,
                         "values": [String(r)]
                     })
                 );
             };

             try {
                 window.crxel.callback = callback;
                 var r = window.eval(m.code);
             } catch (e) {
                 var message = String(e);
                 if (message == "[object Error]") {
                     try {
                         message = "ERROR: " + e.message;
                     } catch(e1) {}
                 }
                 ws.send(
                     JSON.stringify( {
                         "op": "result",
                         "id": m.id,
                         "error": message + "\n" + swank_printStackTrace({ e: e }).join("\n")
                     } )
                 );
                 return;
             }
             if (m.op === "eval")
                 callback(r);
         };

         ws.onclose = function(event){
             setTimeout(connect, 2000);
         };

         window.onunload = function(){
             ws.onclose = undefined;
             var code = 4500;
             ws.close(4500 , "client closed");
         };
         return true;
     }
     connect();
})();

