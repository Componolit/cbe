
#ifndef _BLOCK_SERVER_H_
#define _BLOCK_SERVER_H_

#include <base/fixed_stdint.h>
//#include <block.h>

#define Ada __attribute__((annotate("ada")))

namespace Block
{
    struct Request;

    class Server
    {
        private:

             void *_session;
             void *_state;

        public:

            Server();
            Ada void initialize(const char *label, Genode::uint64_t length, void *session);
            Ada void finalize();
            Ada Genode::uint64_t block_count();
            Ada Genode::uint64_t block_size();
            Ada bool writable();
            Ada Genode::uint64_t maximal_transfer_size();
            Ada void read(
                    Genode::uint8_t buffer[],
                    Genode::uint64_t size,
                    Request &req);
            Ada void sync(
                    Request &req);
            Ada void write(
                    Genode::uint8_t buffer[],
                    Genode::uint64_t size,
                    Request &req);
            void acknowledge(Request &req);
            void malloc_state(void **state, Genode::uint64_t size);
            void free_state(void *state, Genode::uint64_t size);
    };
}

#endif
