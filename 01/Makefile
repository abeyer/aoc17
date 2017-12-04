.PHONY: all test

FSC=fsharpc --nologo

all: bin/captcha.exe

test: bin/captcha.exe
	cd bin && ./captcha.exe --test

bin:
	mkdir -p bin

bin/Fuchu.dll: bin packages/Fuchu.1.0.3.0/lib/Fuchu.dll
	cp packages/Fuchu.1.0.3.0/lib/Fuchu.dll bin/

bin/captcha.exe: bin src/captcha.fs bin/Fuchu.dll
	$(FSC) -o:bin/captcha.exe -r:bin/Fuchu.dll src/captcha.fs
