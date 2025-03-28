# Project: Decision tree
# Author: Marián Tarageľ (xtarag01)
# Year: 2025

EXECUTABLE = flp-fun
LOGIN = xtarag01
SERVER = merlin.fit.vutbr.cz
SERVER_DIR = ~/FLP/project
ZIP_FILE = $(LOGIN).zip
SRC_FILE = Main.hs

GHC = ghc
GHCFLAGS = -Wall

.PHONY = all run clean pack upload

all: $(EXECUTABLE)

$(EXECUTABLE):
	$(GHC) $(GHCFLAGS) --make $(SRC_FILE) -o $(EXECUTABLE)
	rm -f *.o *.hi

run: $(EXECUTABLE)
	./$(EXECUTABLE) $(ARGS)

clean:
	rm -f $(EXECUTABLE) *.o *.hi $(ZIP_FILE)

pack: $(ZIP_FILE)

$(ZIP_FILE): *.hs Makefile
	zip $@ $^

upload: $(ZIP_FILE)
	scp $^ $(LOGIN)@$(SERVER):$(SERVER_DIR)
	ssh $(LOGIN)@$(SERVER) \
		"cd $(SERVER_DIR) && unzip $^ && make"

gdb: $(EXECUTABLE)
	gdb -exec $(EXECUTABLE) --args $(ARGS)