TARGET := cbe-block_server
SRC_CC := main.cc
SRC_ADB := component.adb
INC_DIR := $(PRG_DIR)
LIBS := base spark ada_interface
CC_CXX_OPT += -Wno-attributes
