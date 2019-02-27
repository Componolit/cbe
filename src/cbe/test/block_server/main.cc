
#include <base/component.h>

#include <genode_block_server.h>

struct Main
{

    Block_Server_Main _block;

    Main(Genode::Env &env) :
        _block(env)
    {
        Genode::log("Block server");
        _block.announce();
    }
};

void Component::construct(Genode::Env &env)
{
    static Main main(env);
}
