-module(pong).
-author({ "David J Goehrig", "dave@dloh.org" }).
-copyright(<<"© 2016 David J Goehrig"/utf8>>).
-export([ init/0, get/1 ]).

init() ->
	{ "/pong", ?MODULE }.

get(_Request) ->
	Session = binary:list_to_bin(uuid:to_string(uuid:v4())),
	<<"<script>var ws = new WebSocket('wss://' + document.location.host + '/0=", Session/binary,
	  "','json'); ws.msg = function(data) { ",
	  "var msg = [ '", Session/binary, "' ].concat(data);",
	  "this.send(JSON.stringify(msg));",
	  "};",
	  "ws.onmessage = function(msg) { console.log(JSON.parse(msg.data)) }",
	  "</script>"
	  "<script src=\"/pong/pong.js\"></script>">>.
