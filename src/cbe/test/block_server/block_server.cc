
#include <block/driver.h>

#include <genode_block_server.h>
#include <genode_packet.h>
namespace Cai {
#include <block_server.h>
}

Cai::Block::Server::Server() :
    _session(nullptr),
    _state(nullptr)
{ }

static Genode::Constructible<Block_session_component> *blk(void *session)
{
    return static_cast<Genode::Constructible<Block_session_component> *>(session);
}

void Cai::Block::Server::acknowledge(Cai::Block::Request &req)
{
    (*blk(_session))->try_acknowledge([&] (Block_session_component::Ack &ack){
            ack.submit(create_genode_block_request(req));
        });
}

void Cai::Block::Server::malloc_state(void **state, Genode::uint64_t size)
{
    if(!(*blk(_session))->_heap.alloc(
            *reinterpret_cast<Genode::size_t *>(&size),
            state)){
        Genode::error("Failed to allocate state of size ", size);
        throw Genode::Exception();
    }
}

void Cai::Block::Server::free_state(void *state, Genode::uint64_t size)
{
    (*blk(_session))->_heap.free(
            state,
            *reinterpret_cast<Genode::size_t *>(&size));
}
