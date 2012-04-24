ebin/:
	(mkdir -p ebin)

all: ebin/
	erlc -o ebin src/* 
	erlc -o ebin include/* 

clean:
	rm -rf ebin/*.beam erl_crash.dump

run: clean all
	erl -pa ebin -config priv/settings -s sasl -s crypto -s inets -s theshark do

repl: clean all
	erl -pa ebin -config priv/settings
