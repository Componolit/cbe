TARGET := cbe-block_server
SRC_CC := main.cc
SRC_ADB := cai-block-server.adb
SRC_ADS := cai-component.ads
INC_DIR := $(PRG_DIR)
LIBS := base spark ada_interface ada_interface-block-server
CC_CXX_OPT += -Wno-attributes
