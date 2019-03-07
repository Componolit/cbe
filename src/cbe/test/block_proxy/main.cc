
#include <base/component.h>
#include <terminal_session/connection.h>
#include <util/reconstructible.h>

#include <cai_block_server.h>

Genode::Env *component_env;
Genode::Constructible<Terminal::Connection> _terminal;
Terminal::Connection *__genode_terminal;

extern "C" void adainit();

extern "C" {

    void log_uint(Genode::uint64_t u)
    {
        Genode::log(__func__, " ", u);
    }

}

struct Main
{
    Block_Server_Main _block;

    Main(Genode::Env &env) :
        _block(env)
    {
        Genode::log("Block proxy");
        _block.announce();
    }
};

void Component::construct(Genode::Env &env)
{
    env.exec_static_constructors();
    component_env = &env;
    _terminal.construct(env);
    __genode_terminal = &*_terminal;
    adainit();
    static Main main(env);
}
