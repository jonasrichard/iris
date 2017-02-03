
angular.module('chat', [])
  .controller('ChatController', function($scope) {
      $scope.chat = this;
      var chat = $scope.chat;

      chat.channels = [];
      chat.connected = false;
          
      chat.onopen = function() {
          chat.connected = true;
      };

      chat.onclose = function() {
          chat.connected = false;
      };

      chat.onmessage = function(msg) {
          var json = $.parseJSON(msg.data);
          console.log(json);
          switch (json.type) {
              case "session":
                  chat.getChannelList();
                  break;

              case "message":
                  if (chat.channelId == json.channel) {
                      chat.appendMessage(json.user, json.text, json.ts);
                  } else {
                      chat.update(chat.channels, json.channel, 'id', function(c) {
                          if (c.unread) {
                              c.unread++;
                          } else {
                              c.unread = 1;
                          }
                          return c;
                      });
                      console.log(chat.channels);
                      $scope.$apply();
                  }
                  break;

              case "channel":
                  chat.channels.push({id: json.id, name: json.name});
                  $scope.$apply();
                  break;

              case "channel.get":
                  chat.channels.push({id: json.channelId, name: json.channelName});
                  $scope.$apply();
                  break;

              case "channel.history":
                  for (i in json.messages) {
                      var msg = json.messages[i];
                      chat.appendMessage(msg.user, msg.text, msg.ts);
                  }
                  break;

          }
      };

      chat.open = function(url) {
          chat.ws = new WebSocket(url);
          chat.ws.onopen = chat.onopen;
          chat.ws.onclose = chat.onclose;
          chat.ws.onmessage = chat.onmessage;
      };

      chat.login = function() {
          $("#log-in").modal('hide');
          chat.loginMsg(chat.user, chat.pass);
      };

      chat.logout = function() {
          chat.ws.close();
          chat.connected = false;
      };

      chat.connect = function(host) {
          if (chat.ws && chat.ws.readyState) {
              chat.ws.close();
          }
          chat.open("ws://localhost:" + host + "/ws");
      };

      chat.showLoginDialog = function() {
          $('#log-in').modal();
      };

      chat.loginMsg = function(user, pass) {
          if (!pass) {
              pass = "";
          }
          chat.send({type: "auth", user: user, pass: pass});
      };

      chat.send = function(obj) {
          chat.ws.send(JSON.stringify(obj));
      };

      // Chat message functions

      chat.clearMessage = function() {
          $('#chat').empty();
      };

      chat.appendMessage = function(user, message, ts) {
          $('#chat').append(
              '<p class="text-success">' + user + ': ' + message + '</p>'
          );
      };

      chat.sendText = function() {
          chat.send({type: "message", user: chat.user,
                     text: chat.text, channel: chat.channelId});
          chat.appendMessage(chat.user, chat.text, undefined);
      };

      // Channel functions

      chat.getChannelList = function() {
          chat.send({type: "channel.list"});
      };

      chat.selectChannel = function(channelId) {
          chat.channelId = channelId.toString();
          chat.clearMessage();
          chat.send({type: "channel.history", "channel": chat.channelId});
      };

      chat.showChannelDialog = function() {
          $('#create-channel').modal();
      };

      chat.createChannel = function() {
          var users = chat.channelUsers.split(",")
                          .map(function(u) { return u.trim(); });
          var msg =
              {
                  type: "channel.create",
                  name: chat.channelName,
                  invitees: users
              };

          chat.send(msg);
          $("#create-channel").modal('hide');
      };

      chat.update = function(coll, idx, id, fun) {
          for (var i = 0; i < coll.length; i++) {
              if (coll[i][id] == idx) {
                  var result = fun.apply(null, [coll[i]]);
                  coll[i] = result;
                  return;
              }
          }
      };
  });

