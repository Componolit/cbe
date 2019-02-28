
#ifndef _GENODE_PACKET_H_
#define _GENODE_PACKET_H_

#include <block_session/block_session.h>
#include <block/request_stream.h>
#include <util/string.h>
namespace Cai {
#include <block.h>
}

struct Genode_Packet
{
    Genode::uint64_t offset;
    Genode::uint64_t size;

    Genode_Packet(Genode::uint64_t o, Genode::uint64_t s) :
        offset(o),
        size(s)
    { }

    static Block::Packet_descriptor create(const Cai::Block::Request &r)
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

    static Cai::Block::Request create(const Block::Packet_descriptor &p)
    {
        Cai::Block::Kind request_kind[3] = {
            Cai::Block::READ,
            Cai::Block::WRITE,
            Cai::Block::NONE
        };
        Cai::Block::Request req = {
            request_kind[p.operation()],
            {},
            p.block_number(),
            p.block_count(),
            p.succeeded() ? Cai::Block::OK : Cai::Block::ERROR
        };
        ((Genode_Packet*)&req.uid)->offset = p.offset();
        ((Genode_Packet*)&req.uid)->size = p.size();
        return req;
    }

};

#endif
