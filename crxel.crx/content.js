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

(function(){
     window.crxel = {};
     chrome.runtime.onMessage.addEventListener(
         function(message, sender, sendResponse){
             if(!(message && message.op === "eval"))
                 return;
             var callback = function(r){
                 sendResponse({
                     "op": "result",
                     "values": [String(r)]
                 });
             };
             try {
                 window.crxel.callback = callback;
                 var r = window.eval(m.code);
             } catch (e) {
                 var msg = String(e);
                 if (msg == "[object Error]") {
                     try {
                         msg = "ERROR: " + e.message;
                     } catch(e1) {}
                 }
                 sendResponse({
                     "op": "result",
                     "error": msg + "\n" + swank_printStackTrace({ e: e }).join("\n")
                 });
             }
         }
     );
})();

