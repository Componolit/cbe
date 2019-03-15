
#include <spark/component.h>

#include <block.h>
#include <cxx_block_test.h>

extern "C" void ada_block_test_run(void);

static Cxx_block_test _cxx_block = Cxx_block_test();

Spark::Component::Result Spark::Component::construct()
{
    Genode::log("Running C++ block test...");
    _cxx_block.run();
    Genode::log("C++ block test finished.");
    Genode::log("Running Ada block test...");
    ada_block_test_run();
    Genode::log("Ada block test finished.");
    return Spark::Component::EXIT;
}
