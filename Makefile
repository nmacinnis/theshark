ebin/:
	(mkdir -p ebin)

all: ebin/
	erlc -o ebin src/* 

clean:
	rm -rf ebin/*

run: clean all
	(cd ebin;erl -s theshark do)


