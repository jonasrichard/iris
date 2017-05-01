
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
          $('#log').append(
              '<p class="text-success">' + msg.data + '</p>'
          );
          console.log(json);
          switch (json.type) {
              case "authenticated":
                  chat.getChannelList();
                  break;

              case "message":
                  switch (json.subtype) {
                      case "send":
                          // we send message
                          chat.handleMessage(json.channel, json.user, json.text, json.ts);
                          break;
                      case "incoming":
                          // we send message
                          chat.handleMessage(json.channel, json.from, json.text, json.ts);
                          break;
                      case "received":
                          // add some data to html element and search by channel-ts
                          // and put an ok to the message
                          break;
                      case "read":
                          // put a double ok to the message
                          break;
                  }
                  break;

              case "channel.list":
                  for (i in json.channels) {
                      var c = json.channels[i];
                      chat.channels.push({id: c.id,
                                          name: c.name,
                                          owner: c.owner});
                  }
                  $scope.$apply();
                  break;

              case "channel.created":
                  chat.channels.push({id: json.channelId,
                                      name: json.name,
                                      owner: json.owner});
                  $scope.$apply();
                  break;

              case "channel.history":
                  var maxTS = "";
                  var lastMsg;
                  for (i in json.messages) {
                      var msg = json.messages[i];
                      chat.appendMessage(msg.from, msg.text, msg.ts);
                      if (msg.ts > maxTS) {
                          maxTS = msg.ts;
                          lastMsg = msg;
                      }
                  }
                  if (maxTS != "") {
                      chat.send({
                          type: "message",
                          subtype: "read",
                          from: chat.user,
                          to: lastMsg.from,
                          channel: chat.channelId,
                          ts: maxTS});
                  }
                  break;

              case "channel.left":
                  var newChannels = chat.channels.filter(
                          function(ch) {
                              return ch.id != json.channel;
                          });
                  chat.channels = newChannels;
                  $scope.$apply();
                  break;

              case "channel.archived":
                  var newChannels = chat.channels.filter(
                          function(ch) {
                              return ch.id != json.channel;
                          });
                  chat.channels = newChannels;
                  $scope.$apply();
                  break;

              case "error":
                  chat.displayError(json.error.code, json.error.msg);
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
          json = JSON.stringify(obj);
          chat.ws.send(json);
          $('#log').append(
              '<p class="text-danger">' + json + '</p>'
          );
      };

      // Chat message functions

      chat.handleMessage = function(channel, user, text, ts) {
          if (chat.channelId == channel) {
              chat.appendMessage(user, text, ts);
          } else {
              chat.update(chat.channels, channel, 'id', function(c) {
                  if (c.unread) {
                      c.unread++;
                  } else {
                      c.unread = 1;
                  }
                  return c;
              });
          }
          chat.send({type: "message",
                     subtype: "received",
                     from: chat.user,
                     to: user,
                     channel: channel,
                     ts: ts});
      };

      chat.clearMessage = function() {
          $('#chat').empty();
      };

      chat.appendMessage = function(user, message, ts) {
          $('#chat').append(
              '<p class="text-success">' + user + ': ' + message + '</p>'
          );
      };

      chat.displayError = function(code, message) {
          $('#chat').append(
              '<p class="text-danger">' + code + ': ' + message + '</p>'
          );
      };

      chat.sendText = function() {
          chat.send({type: "message",
                     subtype: "send",
                     from: chat.user,
                     text: chat.text,
                     channel: chat.channelId});
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
          chat.send({type: "channel.status", "channel": chat.channelId});
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

      chat.leaveChannel = function(id) {
          chat.send({type: "channel.leave", channel: id, user: chat.user});
      };

      chat.archiveChannel = function(id) {
          chat.send({type: "channel.archive", channel: id, user: chat.user});
      };

      chat.reload = function() {
          chat.clearMessage();
          chat.channels = [];
          chat.send({type: "channel.list"});
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

