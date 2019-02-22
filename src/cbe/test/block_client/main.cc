
#include <base/component.h>
#include <terminal_session/connection.h>

#include <block.h>
#include <cxx_block_test.h>

Genode::Env *component_env;
Terminal::Connection *__genode_terminal;

extern "C" void ada_block_test_run(void);

struct Main
{
    Terminal::Connection _terminal;
    Cxx_block_test _cxx_block;

    Main(Genode::Env &env) :
        _terminal(env),
        _cxx_block()
    {
        __genode_terminal = &_terminal;
        Genode::log("Running C++ block test...");
        _cxx_block.run();
        Genode::log("C++ block test finished.");
        Genode::log("Running Ada block test...");
        ada_block_test_run();
        Genode::log("Ada block test finished.");
    }
};

void Component::construct(Genode::Env &env)
{
    component_env = &env;
    env.exec_static_constructors();
    static Main main(env);
}
