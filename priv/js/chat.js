
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
                  // if channel is selected append message otherwise badge++
                  break;

              case "channel.get":
                  chat.channels.push({id: json.channelId, name: json.channelName});
                  $scope.$apply();
                  break;
          }
      };

      chat.login = function() {
          $("#log-in").modal('hide');
          console.log(chat.user);
          chat.loginMsg(chat.user, chat.pass);
      };

      chat.logout = function() {
          chat.ws.close();
      };

      chat.sendText = function() {
          console.log(chat.text);
          chat.send({type: "message", user: chat.user,
                     text: chat.text, channel: chat.channelId});
      };

      chat.getChannelList = function() {
          chat.send({type: "channel.list"});
      };

      chat.selectChannel = function(channelId) {
          console.log(channelId);
          chat.channelId = channelId;
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
  });

