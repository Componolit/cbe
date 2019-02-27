
#include <block/driver.h>
#include <block_server.h>
#include <genode_packet.h>
#include <genode_block_server.h>

Block::Server::Server(char *label, Genode::uint64_t session) :
    _session(0),
    _block_count(0),
    _block_size(0)
{
    initialize(label, session);
}

void Block::Server::acknowledge(Block::Request &req, bool success)
{
    Block::Packet_descriptor packet = Genode_Packet::create(req);
    reinterpret_cast<Driver *>(_session)->ack_packet(packet, success);
}
