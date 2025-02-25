# Project: Decision tree
# Author: Marián Tarageľ (xtarag01)
# Year: 2025

EXECUTABLE = flp-fun
LOGIN = xtarag01
SERVER = merlin.fit.vutbr.cz
SERVER_DIR = ~/FLP/project
ZIP_FILE = $(LOGIN).zip
SRC_FILES = main.hs

GHC = ghc
GHCFLAGS = -Wall

.PHONY = all run clean pack upload

all: $(EXECUTABLE)

$(EXECUTABLE):
	$(GHC) $(GHCFLAGS) $(SRC_FILES) -o $(EXECUTABLE)

run: $(EXECUTABLE)
	./$(EXECUTABLE) $(ARGS)

clean:
	rm -f $(EXECUTABLE) *.o *.hi $(ZIP_FILE)

pack: $(ZIP_FILE)

$(ZIP_FILE): *.hs Makefile
	zip $@ $^

upload: $(ZIP_FILE)
	scp $^ $(SERVER):$(SERVER_DIR)
	ssh $(LOGIN)@$(SERVER) \
		"cd $(SERVER_DIR) && unzip $^ && make"

gdb: $(EXECUTABLE)
	gdb -exec $(EXECUTABLE) --args $(ARGS)