
#include <spark/component.h>

extern "C" void ada_component_construct();

Spark::Component::Result Spark::Component::construct()
{
    ada_component_construct();
    return Spark::Component::Result::CONT;
}
