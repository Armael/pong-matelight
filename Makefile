all:
	dune build src/pong.exe
	dune build src/pong_client.exe

clean:
	dune clean
