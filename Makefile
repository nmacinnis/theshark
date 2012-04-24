ebin/:
	(mkdir -p ebin)

all: ebin/
	erlc -o ebin src/* 
	erlc -o ebin include/* 

clean:
	rm -rf ebin/*.beam

run: clean all
	(cd ebin;erl -pa ebin -config priv/settings -s theshark do)

repl: clean all
	erl -pa ebin -config priv/settings
