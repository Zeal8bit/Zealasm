SHELL := /bin/bash

SRCS = main.asm file.asm parser.asm strutils.asm data.asm
BIN = zealasm.bin

# Directory where source files are and where the binaries will be put
INPUT_DIR = src
OUTPUT_DIR = bin

# If z88dk has been install through snap, the binary may be prefixed with "z88dk"
# So choose any of z88dk-dis or z88dk.z88dk-dis, as loong as one exists
DISASSEMBLER=$(shell which z88dk-dis z88dk.z88dk-dis | head -1)


# Include directory containing Zeal 8-bit OS header file.
ifndef ZOS_PATH
$(error "Please define ZOS_PATH environment variable. It must point to Zeal 8-bit OS source code path.")
endif

ZOS_INCLUDE = $(ZOS_PATH)/kernel_headers/z88dk-z80asm/

# Assembler binary name
ASM = z88dk-z80asm
# Assembler flags
ASMFLAGS = -m -b -I$(ZOS_INCLUDE) -O$(OUTPUT_DIR)

# Mark version.txt as PHONY to force generating it every time
.PHONY: all version.txt

all: version.txt $(OUTPUT_DIR) $(BIN)

version.txt:
	git describe --always | tr "\n" " " > $@

$(BIN): $(addprefix $(INPUT_DIR)/, $(SRCS))
	$(ASM) $(ASMFLAGS) -o$@ $^
	$(DISASSEMBLER) -mz80 -o 0x4000 -x $(OUTPUT_DIR)/zealasm.map $(OUTPUT_DIR)/$@ > $(OUTPUT_DIR)/zealasm.dump

$(OUTPUT_DIR):
	mkdir -p $@

clean:
	rm -r bin/ version.txt
