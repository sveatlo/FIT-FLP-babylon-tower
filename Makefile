.PHONY: all
all:
	swipl -o flp20-log -q -g main -t true -c src/main.pl

.PHONY: zip
zip: clean
	zip -r9 flp-log-xhanze10.zip ./*

.PHONY: clean
clean:
	rm -rf ./flp20-log
