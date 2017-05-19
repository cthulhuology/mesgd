%% mesgd_auth.hrl
-record(mesgd_auth, { token, domain, user, email, active, paths = [] }).
