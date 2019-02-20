TARGET := cbe-block_client
SRC_CC := main.cc block.cc cxx_block_test.cc
LIBS := base
INC_DIR += $(PRG_DIR) \
	   $(PRG_DIR)/interfaces
CC_CXX_WARN_STRICT =
