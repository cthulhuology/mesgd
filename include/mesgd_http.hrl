-compile({no_auto_import,[get/1]}).

-record(request, {
	socket, 
	stage = request_line, 
	data = <<>>,
	method, path, protocol,
	headers = [],
	claims = [],
	body = <<>>
}).

-record(response, {
	socket, 
	upgrade = false,
	status = 200,
	protocol = <<"HTTP/1.1">>,
	headers = [],
	claims = [],
	body = <<>>
}).

-record(websocket, { 
	socket,
	pid,
	path, 
	protocol,
	request,
	data, 
	state }).
