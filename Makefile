ebin/:
	(mkdir -p ebin)

all: ebin/
	erlc -o ebin src/*.erl
	erlc -W0 -o ebin include/*.erl 

clean:
	rm -rf ebin/*.beam erl_crash.dump

repl: clean all
	erl -pa ebin -config priv/settings

boot: clean all
	erl -pa ebin -config priv/settings -boot shark_app 
