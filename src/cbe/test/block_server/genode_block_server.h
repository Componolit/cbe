
#ifndef _GENODE_BLOCK_SERVER_H_
#define _GENODE_BLOCK_SERVER_H_

#include <base/heap.h>
#include <block/component.h>
#include <block/driver.h>
#include <util/string.h>

class Driver : public Block::Driver
{
    private:

        Driver(Driver const &);
        Driver &operator = (Driver const &);

    public:
        Driver(Genode::Env &env, const char *label) :
            Block::Driver(env.ram())
        {
            Genode::log(__func__, " ", Genode::Cstring(label));
        }

        Genode::size_t block_size() override
        {
            return 512;
        }

        Block::sector_t block_count() override
        {
            return 1024;
        }

        Block::Session::Operations ops() override
        {
            Block::Session::Operations ops;
            ops.set_operation(Block::Packet_descriptor::READ);
            ops.set_operation(Block::Packet_descriptor::WRITE);
            return ops;
        }

        void session_invalidated() override
        {
            Genode::log(__func__);
        }

        void read(
                Block::sector_t,
                Genode::size_t count,
                char *buffer,
                Block::Packet_descriptor &packet) override
        {
            Genode::log(__func__);
            Genode::memset(buffer, 'x', count * 512);
            ack_packet(packet);
        }

        void write(
                Block::sector_t,
                Genode::size_t,
                const char *,
                Block::Packet_descriptor &packet) override
        {
            Genode::log(__func__);
            ack_packet(packet);
        }
};

class Factory : public Block::Driver_factory
{
    private:

        Factory(Factory const &);
        Factory &operator = (Factory const &);

    public:

        Driver *_driver = nullptr;

        Factory(Genode::Env &env, Genode::Heap &heap)
        {
            _driver = new (&heap) Driver(env, "");
        }

        Block::Driver *create() override
        {
            return _driver;
        }

        void destroy(Block::Driver *) override
        { }
};

class Block_Server_Main
{
    private:

        Genode::Env &_env;
        Genode::Heap _heap;
        Factory _factory;
        Block::Root _root;

    public:

        Block_Server_Main(Genode::Env &env) :
            _env(env),
            _heap(env.ram(), env.rm()),
            _factory(env, _heap),
            _root(env.ep(), _heap, env.rm(), _factory, true)
        { }

        void announce()
        {
            _env.parent().announce(_env.ep().manage(_root));
        }
};

#endif
