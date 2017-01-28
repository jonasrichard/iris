
angular.module('chat', [])
  .controller('ChatController', function($scope) {
      $scope.chat = this;
      var chat = $scope.chat;

      chat.channels = [];

      chat.ws = new WebSocket("ws://localhost:8080/ws");

      chat.ws.onmessage = function(msg) {
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

      chat.login = function() {
          $("#log-in").modal('hide');
          chat.loginMsg(chat.user, chat.pass);
      };

      chat.logout = function() {
          chat.ws.close();
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

      chat.getChannelList = function() {
          chat.send({type: "channel.list"});
      };

      chat.selectChannel = function(channelId) {
          chat.channelId = channelId.toString();
          chat.send({type: "channel.history", "channel": chat.channelId});
      };

      chat.showLoginDialog = function() {
          $('#log-in').modal();
      };

      chat.loginMsg = function(user, pass) {
          chat.send({type: "auth", user: user, pass: pass});
      };

      chat.send = function(obj) {
          chat.ws.send(JSON.stringify(obj));
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

