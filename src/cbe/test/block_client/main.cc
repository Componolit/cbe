
#include <base/component.h>
#include <base/heap.h>
#include <block_session/connection.h>
#include <util/string.h>

struct Main
{
    Genode::Env &_env;
    Genode::Heap _heap;
    Genode::Allocator_avl _alloc;
    Block::Connection _block;

    Genode::Io_signal_handler<Main> _ack;
    Genode::Io_signal_handler<Main> _submit;

    enum { SIZE = 512 };

    void ack()
    {
        Genode::log(__func__);
    }

    void submit()
    {
        Genode::log(__func__);
    }

    void read_packet(Block::sector_t nr)
    {
        Block::Packet_descriptor packet(
                _block.dma_alloc_packet(SIZE),
                Block::Packet_descriptor::READ,
                nr, 1);
        _block.tx()->submit_packet(packet);
        packet = _block.tx()->get_acked_packet();
        Genode::log("Packet_content:");
        Genode::log(Genode::String<SIZE>(static_cast<const char *>(_block.tx()->packet_content(packet))));
        _block.tx()->release_packet(packet);
    }

    void write_packet(Block::sector_t nr, char c)
    {
        Block::Packet_descriptor packet(
                _block.dma_alloc_packet(SIZE),
                Block::Packet_descriptor::WRITE,
                nr, 1);
        Genode::memset(_block.tx()->packet_content(packet), c, SIZE);
        _block.tx()->submit_packet(packet);
        _block.tx()->release_packet(packet);
    }

    Main(Genode::Env &env) :
        _env(env),
        _heap(env.ram(), env.rm()),
        _alloc(&_heap),
        _block(env, &_alloc),
        _ack(env.ep(), *this, &Main::ack),
        _submit(env.ep(), *this, &Main::submit)
    {
        Genode::log("Ada block");
        write_packet(1, 'a');
        write_packet(2, 'd');
        write_packet(3, 'a');
        read_packet(1);
        read_packet(2);
        read_packet(3);
    }
};

void Component::construct(Genode::Env &env)
{
    static Main main(env);
}
