
#include <base/component.h>
#include <terminal_session/connection.h>
#include <util/reconstructible.h>

Genode::Env *component_env;
Genode::Constructible<Terminal::Connection> _terminal;
Terminal::Connection *__genode_terminal;

extern "C" void adainit();
extern "C" void ada_component_construct();

struct Main
{

    Main(Genode::Env &)
    {
        Genode::log("Block server");
        adainit();
        ada_component_construct();
    }
};

void Component::construct(Genode::Env &env)
{
    component_env = &env;
    env.exec_static_constructors();
    _terminal.construct(env);
    __genode_terminal = &*_terminal;
    static Main main(env);
}
