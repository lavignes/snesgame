MAKEFLAGS += -j
rwildcard = $(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

ASM_PATH := tools/asm
ASM := $(ASM_PATH)/target/release/nyasm
LD := $(ASM_PATH)/target/release/nylink

SRCS := $(call rwildcard,src,*.asm)
OBJS := $(SRCS:.asm=.o)
DEPS := $(SRCS:.asm=.d)

LOG_LEVEL := WARN
ASM_FLAGS := -DDEBUG=1 -l $(LOG_LEVEL) -I include
LD_FLAGS := -c link.toml -l $(LOG_LEVEL) -g game.sym --tags game.tags

game.sfc: link.toml $(DEPS) $(OBJS) $(LD)
	$(LD) $(LD_FLAGS) -o $@ $(OBJS)

%.o: %.asm $(ASM)
	$(ASM) $(ASM_FLAGS) -o $@ $<

%.d: %.asm $(ASM)
	$(ASM) $(ASM_FLAGS) -M -o $@ $<

$(ASM):
	cd $(ASM_PATH) && cargo build --release --bin nyasm

$(LD):
	cd $(ASM_PATH) && cargo build --release --bin nylink

.PHONY: deepclean
deepclean: clean
	cd tools/asm && cargo clean

.PHONY: clean
clean:
	rm -f $(call rwildcard,src,*.o)
	rm -f $(call rwildcard,src,*.d)
	rm -f game.sfc
	rm -f game.sym
	rm -f game.tags

ifeq (,$(findstring clean,$(MAKECMDGOALS)))
include $(DEPS)
endif

