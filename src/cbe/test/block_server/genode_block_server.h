
#ifndef _GENODE_BLOCK_SERVER_H_
#define _GENODE_BLOCK_SERVER_H_

#include <session/session.h>
#include <base/attached_ram_dataspace.h>
#include <block/request_stream.h>
#include <util/reconstructible.h>

#include <genode_packet.h>
namespace Cai {
#include <block_server.h>
}

struct Block_session_component : Genode::Rpc_object<Block::Session>, Block::Request_stream
{

    Genode::Entrypoint &_ep;

    Block_session_component(
            Genode::Region_map &rm,
            Genode::Dataspace_capability ds,
            Genode::Entrypoint &ep,
            Genode::Signal_context_capability sigh) :
        Request_stream(rm, ds, ep, sigh, 512),
        _ep(ep)
    {
        _ep.manage(*this);
    }

    ~Block_session_component()
    {
        _ep.dissolve(*this);
    }

    void info(Block::sector_t *count, Genode::size_t *size, Block::Session::Operations *ops) override
    {
        *count = 1024;
        *size = 512;
        *ops = Block::Session::Operations();
        ops->set_operation(Block::Packet_descriptor::Opcode::READ);
        ops->set_operation(Block::Packet_descriptor::Opcode::WRITE);
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
    Genode::Signal_handler<Root> _request_handler;
    Genode::Constructible<Genode::Attached_ram_dataspace> _ds;
    Genode::Constructible<Block_session_component> _session;

    void temp_ack(
            Block::Request::Operation op,
            Block::Request::Success ,
            Genode::uint64_t bn,
            Genode::uint64_t os,
            Genode::uint32_t ct)
    {
        Block::Request req {op, Block::Request::Success::TRUE, bn, os, ct};
        _session->try_acknowledge([&] (Block_session_component::Ack &ack){
                ack.submit(req);
                });
    }

    void handle_request()
    {
        Genode::log(__func__);
        if(!_session.constructed()){
            return;
        }

        _session->with_requests([&] (Block::Request request){
                _session->with_content(request, [&] (void *ptr, Genode::size_t size){
                        Block::Request::Operation op = request.operation;
                        Genode::uint64_t bn = request.block_number;
                        Genode::uint32_t ct = request.count;
                        Genode::uint64_t os = request.offset;
                        Block::Request::Success sc = request.success;
                        Genode::log((Genode::uint32_t)op, " ", bn, " ", ct, " ", os);
                        Genode::log(ptr, " ", size);
                        temp_ack(op, sc, bn, os, ct);
                        });
                return Block_session_component::Response::ACCEPTED;
            });

        _session->wakeup_client();
    }

    Genode::Capability<Genode::Session> session(Root::Session_args const &args, Genode::Affinity const &) override
    {
        Genode::log(__func__, " ", args.string());

        Genode::size_t const ds_size = Genode::Arg_string::find_arg(args.string(), "tx_buf_size").ulong_value(0);
        Genode::Ram_quota const ram_quota = Genode::ram_quota_from_args(args.string());
        if (ds_size >= ram_quota.value) {
            Genode::warning("communication buffer size exceeds session quota");
            throw Genode::Insufficient_ram_quota();
        }

        _ds.construct(_env.ram(), _env.rm(), ds_size);
        _session.construct(_env.rm(), _ds->cap(), _env.ep(), _request_handler);
        return _session->cap();
    }

    void upgrade(Genode::Capability<Genode::Session>, Root::Upgrade_args const &) override
    { }

    void close(Genode::Capability<Genode::Session>) override
    {
        _session.destruct();
        _ds.destruct();
    }

    Root(Genode::Env &env) :
        _env(env),
        _request_handler(env.ep(), *this, &Root::handle_request),
        _ds(),
        _session()
    {
        Genode::log(__func__);
    }
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
