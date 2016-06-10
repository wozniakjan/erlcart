export PATH=$OPENSHIFT_ERL_DIR/usr/bin/:$PATH
erl

l(inets).
inets:start().
httpc:set_options([{ip, {127,7,66,129}}]).
httpc:request("http://www.google.com").




##DIY
export PATH=$OPENSHIFT_DIY_DIR/test/:$PATH
erl

l(inets).
inets:start().
httpc:set_options([{ip, {127,7,66,129}}, {port, 9000}]).%{verbose, trace}]).
httpc:request("http://www.google.com").

ltrace -p <PID> -o output.txt -e bind,connect
