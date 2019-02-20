
#include <block.h>

#include <block_session/connection.h>
#include <util/string.h>
#include <ada/exception.h>

inline Block::Connection *blk(void *device)
{
    return static_cast<Block::Connection *>(device);
}

struct Genode_Packet
{
    Genode::uint64_t offset;
    Genode::uint64_t size;

    Genode_Packet(Genode::off_t o, Genode::size_t s) :
        offset(o), size(s) { }

    void uid(Genode::uint8_t *u)
    {
        Genode::memcpy(u, this, 16);
    }
};

Block::Client::Client(void *device)
{
    if(device){
        _device = device;
    }else{
        throw Ada::Exception::Null_Access_Parameter();
    }
}

void Block::Client::submit_read(Block::Client::Request &req)
{
    Block::Packet_descriptor packet(
            blk(_device)->dma_alloc_packet(BLOCK_SIZE * req.length),
            Block::Packet_descriptor::READ,
            req.start, req.length);
    blk(_device)->tx()->submit_packet(packet);
    Genode_Packet(packet.offset(), packet.size()).uid(req.uid);
}

void Block::Client::submit_sync(Block::Client::Request &req)
{
    Block::Packet_descriptor packet(
            blk(_device)->dma_alloc_packet(BLOCK_SIZE * req.length),
            Block::Packet_descriptor::END,
            req.start, req.length);
    blk(_device)->tx()->submit_packet(packet);
    Genode_Packet(packet.offset(), packet.size()).uid(req.uid);
}

void Block::Client::submit_write(
        Block::Client::Request &req,
        Genode::uint8_t *data,
        Genode::uint64_t length)
{
    if(length > req.length * BLOCK_SIZE){
        throw Ada::Exception::Length_Check();
    }

    Block::Packet_descriptor packet(
            blk(_device)->dma_alloc_packet(length),
            Block::Packet_descriptor::WRITE,
            req.start, req.length);
    Genode::memcpy(blk(_device)->tx()->packet_content(packet), data, length);
    blk(_device)->tx()->submit_packet(packet);
    Genode_Packet(packet.offset(), packet.size()).uid(req.uid);
}

Block::Client::Request Block::Client::next()
{
    Block::Client::Request req = {NONE, {}, 0, 0, 0};
    Block::Client::Kind genode_request_id[3] = {READ, WRITE, SYNC};
    if(blk(_device)->tx()->ack_avail()){
        Block::Packet_descriptor packet = blk(_device)->tx()->get_acked_packet();
        req.kind = genode_request_id[packet.operation()];
        req.start = packet.block_number();
        req.length = packet.block_count();
        req.success = packet.succeeded();
        Genode_Packet(packet.offset(), packet.size()).uid(req.uid);
    }
    return req;
}

void Block::Client::acknowledge_read(
        Block::Client::Request &req,
        Genode::uint8_t *data,
        Genode::uint64_t length)
{
    Block::Packet_descriptor::Opcode opcode[4] = {
        Block::Packet_descriptor::READ,
        Block::Packet_descriptor::READ,
        Block::Packet_descriptor::WRITE,
        Block::Packet_descriptor::END,
    };
    Block::Packet_descriptor packet(
            Block::Packet_descriptor(
                ((Genode_Packet*)&req.uid)->offset,
                ((Genode_Packet*)&req.uid)->size),
            opcode[req.kind],
            req.start,
            req.length);
    if(length > packet.size()){
        throw Ada::Exception::Length_Check();
    }
    Genode::memcpy(data, blk(_device)->tx()->packet_content(packet), length);
    blk(_device)->tx()->release_packet(packet);
}

void Block::Client::acknowledge_sync(Block::Client::Request &)
{ }

void Block::Client::acknowledge_write(Block::Client::Request &)
{ }
