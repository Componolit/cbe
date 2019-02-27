
#ifndef _GENODE_PACKET_H_
#define _GENODE_PACKET_H_

#include <block.h>
#include <block_session/block_session.h>
#include <util/string.h>

struct Genode_Packet
{
    Genode::uint64_t offset;
    Genode::uint64_t size;

    Genode_Packet(Genode::uint64_t o, Genode::uint64_t s) :
        offset(o),
        size(s)
    { }

    static Block::Packet_descriptor create(const Block::Request &r)
    {
        Block::Packet_descriptor::Opcode opcode[4] = {
            Block::Packet_descriptor::END,
            Block::Packet_descriptor::READ,
            Block::Packet_descriptor::WRITE,
            Block::Packet_descriptor::END
        };
        return Block::Packet_descriptor(
                Block::Packet_descriptor(
                    ((Genode_Packet*)&r.uid)->offset,
                    ((Genode_Packet*)&r.uid)->size),
                opcode[r.kind],
                r.start,
                r.length);
    }

    static Block::Request create(const Block::Packet_descriptor &p)
    {
        Block::Kind request_kind[3] = {
            Block::READ,
            Block::WRITE,
            Block::NONE
        };
        Block::Request req = {
            request_kind[p.operation()],
            {},
            p.block_number(),
            p.block_count(),
            p.succeeded() ? Block::OK : Block::ERROR
        };
        ((Genode_Packet*)&req.uid)->offset = p.offset();
        ((Genode_Packet*)&req.uid)->size = p.size();
        return req;
    }

};

#endif
