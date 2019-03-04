
#include <base/component.h>
#include <terminal_session/connection.h>
#include <util/reconstructible.h>

#include <genode_block_server.h>

Genode::Constructible<Terminal::Connection> _terminal;
Terminal::Connection *__genode_terminal;

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
    env.exec_static_constructors();
    _terminal.construct(env);
    __genode_terminal = &*_terminal;
    static Main main(env);
}
