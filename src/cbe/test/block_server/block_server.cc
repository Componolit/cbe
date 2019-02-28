
#include <block/driver.h>

#include <genode_block_server.h>
#include <genode_packet.h>
namespace Cai {
#include <block_server.h>
}

Cai::Block::Server::Server() :
    _session(0),
    _block_count(0),
    _block_size(0)
{ }

void Cai::Block::Server::acknowledge(Cai::Block::Request &req)
{
    Genode::Constructible<Block_session_component> *session =
        reinterpret_cast<Genode::Constructible<Block_session_component> *>(_session);
    (*session)->try_acknowledge([&] (Block_session_component::Ack &ack){
            ack.submit(create_genode_block_request(req));
        });
}
