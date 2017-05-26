%% record for storing stats for a given time unit
-record(mesgd_stats, { 
	time, duration, domain, clients, data_in, data_out, msgs_in, msgs_out, auth_ack, auth_nak }).
