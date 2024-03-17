rwildcard = $(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

ASM_PATH := tools/asm
ASM := $(ASM_PATH)/target/release/pasm
LD := $(ASM_PATH)/target/release/plink

SRCS := $(call rwildcard,src,*.asm)
OBJS := $(SRCS:.asm=.o)

ASM_FLAGS :=
LD_FLAGS := -c link.toml

all: toolchain game.nes

toolchain: $(ASM) $(LD)

$(ASM) $(LD):
	cd $(ASM_PATH) && cargo build --release

game.nes: $(OBJS)
	$(LD) $(LD_FLAGS) -o $@ $^

%.o: %.asm
	$(ASM) $(ASM_FLAGS) -o $@ $<

deepclean: clean
	cd tools/asm && cargo clean

clean:
	rm -f $(call rwildcard,src,*.o)
	rm -f game.asm

