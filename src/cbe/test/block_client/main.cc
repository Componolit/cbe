
#include <base/component.h>
#include <base/heap.h>
#include <block_session/connection.h>
#include <util/string.h>

#include <block.h>
#include <cxx_block_test.h>

struct Main
{
    Genode::Env &_env;
    Genode::Heap _heap;
    Genode::Allocator_avl _alloc;
    Block::Connection _block_connection;

    Cxx_block_test _cxx_block;

    Main(Genode::Env &env) :
        _env(env),
        _heap(env.ram(), env.rm()),
        _alloc(&_heap),
        _block_connection(env, &_alloc),
        _cxx_block(_block_connection)
    {
        _cxx_block.run();
    }
};

void Component::construct(Genode::Env &env)
{
    static Main main(env);
}
