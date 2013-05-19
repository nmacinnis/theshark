ebin/:
	(mkdir -p ebin)

all: ebin/
	erlc -o ebin src/*.erl
	erlc -W0 -o ebin include/*.erl 

debug: ebin/
	erlc +debug_info -o ebin src/*.erl
	erlc +debug_info -W0 -o ebin include/*.erl 

clean:
	rm -rf ebin/*.beam erl_crash.dump

repl: clean all
	erl -pa ebin -config priv/settings

boot: clean all
	erl -pa ebin -config priv/settings -boot shark_app 

dial: clean debug
	dialyzer --src src -Wunmatched_returns
	#dialyzer --apps kernel inets ssl crypto sasl -I src -Wunmatched_returns -Wunderspecs
