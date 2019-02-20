#include <cxx_block_test.h>

Cxx_block_test::Cxx_block_test() : _block()
{ }

void Cxx_block_test::run()
{
    Genode::log("Ada block");
    Block::Client::Request req {Block::Client::WRITE, {}, 1, 1};
    Genode::memset(_buffer, 'c', Block::Client::BLOCK_SIZE);
    req.start = 1;
    _block.submit_write(req, (Genode::uint8_t *)_buffer, Block::Client::BLOCK_SIZE);
    Genode::memset(_buffer, '+', Block::Client::BLOCK_SIZE);
    req.start = 2;
    _block.submit_write(req, (Genode::uint8_t *)_buffer, Block::Client::BLOCK_SIZE);
    Genode::memset(_buffer, '+', Block::Client::BLOCK_SIZE);
    req.start = 3;
    _block.submit_write(req, (Genode::uint8_t *)_buffer, Block::Client::BLOCK_SIZE);
    bool handled = false;
    bool started = false;
    while(!handled){
        req = _block.next();
        started = started || req.kind != Block::Client::NONE;
        handled = started && req.kind == Block::Client::NONE;
        if(handled){
            Genode::log("writing finished");
        }
        if(started && !handled){
            Genode::log("Write to block ", req.start, req.success ? " succeeded" : " failed");
        }
    }
    req.kind = Block::Client::READ;
    req.start = 1;
    req.length = 1;
    _block.submit_read(req);
    req.start = 2;
    _block.submit_read(req);
    req.start = 3;
    _block.submit_read(req);
    handled = false;
    started = false;
    while(!handled){
        req = _block.next();
        started = started || req.kind != Block::Client::NONE;
        handled = started && req.kind == Block::Client::NONE;
        if(handled){
            Genode::log("reading finished");
        }
        if(started && !handled){
            Genode::log("Reading from block ", req.start, req.success ? " succeeded" : " failed");
            _block.acknowledge_read(req, (Genode::uint8_t *)_buffer, Block::Client::BLOCK_SIZE);
            Genode::log(Genode::String<Block::Client::BLOCK_SIZE>(static_cast<const char*>(_buffer)));
        }
    }
}
