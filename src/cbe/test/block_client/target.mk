TARGET := cbe-block_client
SRC_CC := main.cc cxx_block_test.cc
SRC_ADB := ada_block_test.adb block_client.adb
LIBS := base spark ada_interface
INC_DIR += $(PRG_DIR) \
	   $(PRG_DIR)/interfaces
CC_CXX_WARN_STRICT =
