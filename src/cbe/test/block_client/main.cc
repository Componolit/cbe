
#include <base/component.h>

#include <block.h>
#include <cxx_block_test.h>

Genode::Env *component_env;

struct Main
{
    Cxx_block_test _cxx_block;

    Main(Genode::Env &env) :
        _cxx_block()
    {
        _cxx_block.run();
    }
};

void Component::construct(Genode::Env &env)
{
    component_env = &env;
    env.exec_static_constructors();
    static Main main(env);
}
