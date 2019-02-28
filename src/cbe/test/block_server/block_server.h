
#ifndef _BLOCK_SERVER_H_
#define _BLOCK_SERVER_H_

#include <base/fixed_stdint.h>
#include <block.h>

#define Ada __attribute__((annotate("ada")))

namespace Block
{

    class Server
    {
        private:

            Genode::uint64_t _session;
            Genode::uint64_t _block_count;
            Genode::uint64_t _block_size;

        public:

            Server(const char *label, Genode::uint64_t session);
            Ada void initialize(char *label, Genode::uint64_t session);
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
            void acknowledge(Request &req, bool success);
    };
}

#endif
