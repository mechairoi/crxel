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

(function() {
     const url = "ws://localhost:9649/";
     var ws;

     ws = new WebSocket(url);

     ws.onmessage = function(event){
         var m = JSON.parse(event.data);

         var send = function(r){
             ws.send(
                 JSON.stringify( {
                     "op": "result",
                     "id": m.id,
                     "values": [String(r)]
                 })
             );
         };

         try {
             window.crxel = send;
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
             send(r);
     };

     window.onunload = function(){
         var code = 4500;
         ws.close(4500 , "client closed");
     };
})();

