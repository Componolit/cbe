
#include <base/fixed_stdint.h>
#include <cxx_block_test.h>

Cxx_block_test::Cxx_block_test() : _block()
{ }

void Cxx_block_test::run()
{
    Genode::memset(_buffer, '\0', sizeof(_buffer));
    _block.initialize("c++ test client");
    Genode::log("Block device with ", _block.block_count(), " blocks of size ", _block.block_size());
    if(_block.block_size() > 4096)
    {
        Genode::error("Block size too big");
    }
    Genode::log("Writing...");
    Cai::Block::Request req {Cai::Block::WRITE, {}, 1, 1};
    Genode::memset(_buffer, 'c', _block.block_size());
    req.start = 1;
    _block.submit_write(req, (Genode::uint8_t *)_buffer, _block.block_size());
    Genode::memset(_buffer, '+', _block.block_size());
    req.start = 2;
    _block.submit_write(req, (Genode::uint8_t *)_buffer, _block.block_size());
    Genode::memset(_buffer, '+', _block.block_size());
    req.start = 3;
    _block.submit_write(req, (Genode::uint8_t *)_buffer, _block.block_size());
    int acked_block = 0;
    while(acked_block < 3){
        req = _block.next();
        if(req.kind == Cai::Block::WRITE){
            acked_block++;
            Genode::log("Write to block ", req.start, req.status == Cai::Block::OK ? " succeeded" : " failed");
            _block.acknowledge(req);
        }
    }
    Genode::log("Writing finished.");
    Genode::log("Reading...");
    req.kind = Cai::Block::READ;
    req.start = 1;
    req.length = 1;
    _block.submit_read(req);
    req.start = 2;
    _block.submit_read(req);
    req.start = 3;
    _block.submit_read(req);
    acked_block = 0;
    while(acked_block < 3){
        req = _block.next();
        if(req.kind == Cai::Block::READ){
            acked_block++;
            if(req.status == Cai::Block::OK){
                _block.read(req, (Genode::uint8_t *)_buffer, _block.block_size());
            }
            Genode::log("Reading from block ", req.start, req.status == Cai::Block::OK ? " succeeded" : " failed");
            if(req.status == Cai::Block::OK){
                Genode::log(Genode::String<sizeof(_buffer)>(static_cast<const char*>(_buffer)));
            }
            _block.acknowledge(req);
        }
    }
    Genode::log("Reading finished...");
    _block.finalize();
}
