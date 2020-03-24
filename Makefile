.PHONY: watch

RETRO = MyRetro.md
TARGET = docs

all: $(RETRO)
	make pdf

html: $(RETRO)
	./myRetroGenerator $(RETRO) -o $(TARGET)

pdf: $(RETRO)
	make html ; \
	cd $(TARGET) ; \
	browser-sync start --server --no-open & \
	sleep 2 ; \
	google-chrome --headless --disable-gpu --print-to-pdf=myRetro.pdf http://localhost:3000

watch:
	cwd=$$(pwd) ; \
	make html ; \
	cd $(TARGET) ; \
	browser-sync start --server --files index.html & \
	sleep 2 ; \
	while [ -z $$QUIT ]; do \
	if [[ "$$OSTYPE" == "linux-gnu" ]]; then \
	inotifywait -e modify "$$cwd/$(RETRO)" ; \
	elif [[ "$$OSTYPE" == "darwin"* ]]; then \
	fswatch -1 --event Updated "$$cwd/$(RETRO)" ; \
	fi ; \
	cd "$$cwd" && make html && cd $(TARGET) ; \
	done
