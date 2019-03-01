TARGET := cbe-block_server
SRC_CC := main.cc block_server.cc
SRC_ADB := cxx-block.adb block-server.adb
SRC_ADS := cxx.ads cxx-genode.ads block.ads component.ads
LIBS := base spark
INC_DIR := $(PRG_DIR)
CC_CXX_OPT += -Wno-attributes
