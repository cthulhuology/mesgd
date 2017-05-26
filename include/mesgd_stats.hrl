%% record for storing stats for a given time unit
-record(mesgd_stats, { time, samples, duration, domain, clients_in, clients_out, data_in, data_out, msgs_in, msgs_out, auth_ack, auth_nak }).
