.PHONY: all test

FSC=fsharpc --nologo

all: bin/checksum.exe

bin/checksum.exe: src/checksum.fs
	$(FSC) -o:bin/checksum.exe src/checksum.fs
