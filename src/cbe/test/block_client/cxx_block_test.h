
#include <block.h>

#include <block_session/connection.h>

class Cxx_block_test
{
    private:
        Block::Client _block;
        char _buffer[4096];
    public:
        Cxx_block_test();
        void run();
};
