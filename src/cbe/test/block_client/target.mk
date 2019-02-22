TARGET := cbe-block_client
SRC_CC := main.cc block_class.cc cxx_block_test.cc
SRC_ADS := cbe.ads cbe-block.ads cbe-genode.ads block.ads
SRC_ADB := block-client.adb ada_block_test.adb
LIBS := base spark
INC_DIR += $(PRG_DIR) \
	   $(PRG_DIR)/interfaces
CC_CXX_WARN_STRICT =
