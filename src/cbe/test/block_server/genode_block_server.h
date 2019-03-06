
#ifndef _GENODE_BLOCK_SERVER_H_
#define _GENODE_BLOCK_SERVER_H_

#include <session/session.h>
#include <base/attached_ram_dataspace.h>
#include <base/heap.h>
#include <block/request_stream.h>
#include <util/reconstructible.h>
#include <util/string.h>

#include <genode_packet.h>
namespace Cai {
#include <block_server.h>
}

struct Block_session_component : Genode::Rpc_object<Block::Session>, Block::Request_stream
{

    Genode::Entrypoint &_ep;
    Cai::Block::Server &_server;
    Genode::Sliced_heap &_heap;

    Block_session_component(
            Genode::Region_map &rm,
            Genode::Dataspace_capability ds,
            Genode::Entrypoint &ep,
            Genode::Signal_context_capability sigh,
            Cai::Block::Server &server,
            Genode::Sliced_heap &heap) :
        Request_stream(rm, ds, ep, sigh, server.block_size()),
        _ep(ep),
        _server(server),
        _heap(heap)
    {
        _ep.manage(*this);
    }

    ~Block_session_component()
    {
        _ep.dissolve(*this);
    }

    void info(Block::sector_t *count, Genode::size_t *size, Block::Session::Operations *ops) override
    {
        *count = _server.block_count();
        *size = _server.block_size();
        *ops = Block::Session::Operations();
        ops->set_operation(Block::Packet_descriptor::Opcode::READ);
        if(_server.writable()){
            ops->set_operation(Block::Packet_descriptor::Opcode::WRITE);
        }
    }

    void sync() override { }

    Genode::Capability<Tx> tx_cap() override
    {
        return Request_stream::tx_cap();
    }
};

struct Root : Genode::Rpc_object<Genode::Typed_root<Block::Session>>
{
    Genode::Env &_env;
    Genode::Sliced_heap _heap;
    Genode::Signal_handler<Root> _request_handler;
    Genode::Constructible<Genode::Attached_ram_dataspace> _ds;
    Genode::Constructible<Block_session_component> _session;
    Genode::Constructible<Cai::Block::Server> _server;

    void handle_request()
    {
        if(!_session.constructed()){
            return;
        }

        _session->with_requests([&] (Block::Request request){
                Cai::Block::Request cai_request = create_cai_block_request(request);
                _session->with_content(request, [&] (void *ptr, Genode::size_t size){
                        switch(request.operation){
                            case Block::Request::Operation::READ:
                                _server->read(static_cast<Genode::uint8_t *>(ptr), size, cai_request);
                                break;
                            case Block::Request::Operation::WRITE:
                                _server->write(static_cast<Genode::uint8_t *>(ptr), size, cai_request);
                                break;
                            case Block::Request::Operation::SYNC:
                                _server->sync(cai_request);
                                break;
                            default:
                                Genode::warning("Invalid packet");
                                break;
                        }
                    });
                if(cai_request.status == Cai::Block::Status::ERROR){
                    return Block_session_component::Response::RETRY;
                }else{
                    return Block_session_component::Response::ACCEPTED;
                }
            });
        _session->wakeup_client();
    }

    Genode::Capability<Genode::Session> session(Root::Session_args const &args, Genode::Affinity const &) override
    {
        Genode::size_t const ds_size = Genode::Arg_string::find_arg(args.string(), "tx_buf_size").ulong_value(0);
        Genode::Ram_quota const ram_quota = Genode::ram_quota_from_args(args.string());
        const char *label = Genode::session_label_from_args(args.string()).last_element().string();
        if (ds_size >= ram_quota.value) {
            Genode::warning("communication buffer size exceeds session quota");
            throw Genode::Insufficient_ram_quota();
        }

        _server.construct();
        _ds.construct(_env.ram(), _env.rm(), ds_size);
        _session.construct(_env.rm(), _ds->cap(), _env.ep(), _request_handler, *_server, _heap);
        _server->initialize(
                label,
                Genode::strlen(label),
                reinterpret_cast<Genode::uint64_t>(&_session));
        return _session->cap();
    }

    void upgrade(Genode::Capability<Genode::Session>, Root::Upgrade_args const &) override
    { }

    void close(Genode::Capability<Genode::Session>) override
    {
        _server->finalize();
        _session.destruct();
        _server.destruct();
        _ds.destruct();
    }

    Root(Genode::Env &env) :
        _env(env),
        _heap(env.ram(), env.rm()),
        _request_handler(env.ep(), *this, &Root::handle_request),
        _ds(),
        _session(),
        _server()
    { }
};

class Block_Server_Main
{
    private:

        Genode::Env &_env;
        Root _root;

    public:

        Block_Server_Main(Genode::Env &env) :
            _env(env),
            _root(env)
        { }

        void announce()
        {
            _env.parent().announce(_env.ep().manage(_root));
        }
};

#endif
