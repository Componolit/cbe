
#include <block/driver.h>

#include <genode_block_server.h>
#include <genode_packet.h>
namespace Cai {
#include <block_server.h>
}

Cai::Block::Server::Server(const char *, Genode::uint64_t) :
    _session(0),
    _block_count(0),
    _block_size(0)
{
//    initialize(label, session);
}

void Cai::Block::Server::acknowledge(Cai::Block::Request &, bool)
{
    /*
    Block::Packet_descriptor packet = Genode_Packet::create(req);
    reinterpret_cast<Driver *>(_session)->ack_packet(packet, success);
    */
}
