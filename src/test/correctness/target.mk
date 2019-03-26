TARGET := cbe-test-correctness
SRC_CC := main.cc
SRC_ADB := component.adb test.adb ringbuffer.adb
INC_DIR := $(PRG_DIR)
LIBS := base spark ada_interface libsparkcrypto permutation
