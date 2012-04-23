ebin/:
	(mkdir -p ebin)

all: ebin/
	erlc -o ebin src/* 
	erlc -o ebin include/* 

clean:
	rm -rf ebin/*

run: clean all
	(cd ebin;erl -config priv/settings -s theshark do)

repl: clean all
	erl -pa ebin -config priv/settings
