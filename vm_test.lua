VM = require 'vm'

luaunit = require 'luaunit'
local gen_server = require 'gen_server'

--TODO setup and teardown of the VM?
function setup_each()
  VM.init()
end

function test_spawn()
  local co = VM.spawn(function() coroutine.yield() end)
  luaunit.assert_equals(VM.status(co),"suspended")
  luaunit.assert_true(VM.coroutines[co])
end

function test_resume()
  local co = VM.spawn(function() coroutine.yield() end)
  VM.resume(co)
  luaunit.assertEquals(VM.status(co),"dead")
  --luaunit.assertErrorMsgContains("does not exist",VM.status,co)
end

function test_send()
  local msg,msg2
  local co = VM.spawn(function() msg,msg2 = VM.receive() end)
  VM.send(co,"a message","second msg")
  luaunit.assert_equals(msg,"a message")
  luaunit.assert_equals(msg2,"second msg")
  --send doesn't fail except for badarg
  luaunit.assertError(VM.send,{"badarg"},"msg")
  luaunit.assertErrorMsgContains("badarg",VM.send,"badname","msg")
end

function test_receive()
  local msg = "a msg"
  local msg2
  local co = VM.spawn(function() msg2 = VM.receive() end)
  VM.send(co,msg)
  luaunit.assert_equals(msg,msg2)
end

function test_terminate()
  local co = VM.spawn(function() VM.receive() end)
  VM.send(co,"terminate")
  luaunit.assertError(VM.status,"dead")
end

function test_register()
  local co = VM.spawn(function() VM.receive() end)
  VM.register("routine",co)
  luaunit.assertEquals(VM.registered(),{"ROOT","routine"})
  luaunit.assertEquals(VM.co2names,{[co]={"routine"}})
  luaunit.assertEquals(VM.coroutines["routine"],VM.coroutines[co])
  VM.resume(co)
  luaunit.assertEquals(VM.registered(),{"ROOT"})
  luaunit.assertErrorMsgContains("badarg",VM.register,"routine",co)
  local deadCo = VM.spawn(function() end)
  luaunit.assertErrorMsgContains("badarg",VM.register,"imdead",deadCo)
end

function test_registered_co()
  local msg
  local co = VM.spawn(function() msg = VM.receive() end)
  local name = "name"
  VM.register(name,co)
  VM.send(name,"msg")
  luaunit.assertEquals(msg,"msg")
end

function test_unlinked()
  local function sup()
    VM.spawn(function() error("error") end)
  end
  VM.spawn(sup)
end

function test_createLink()
  local co2
  local function sup()
    co2 = VM.spawn(function() VM.receive() end)
    VM.link(co2)
    VM.receive()
    VM.unlink(co2)
    VM.receive()
    VM.send(co2,"msg")
  end
  local co = VM.spawn(sup)
  luaunit.assertEquals(VM.links,{[co]={co2},[co2]={co}})
  VM.send(co,"msg")
  luaunit.assertEquals(VM.links,{})
  --luaunit.assertEquals(VM.links,{[co]={},[co2]={}})
end

function test_link()
  local _error, msg
  local unreachable = true
  local child = function() 
      VM.receive()
      unreachable = false
  end
  local function sup()
    local co = VM.spawn(child)
    VM.link(co)
    error("Some error")
  end
  VM.spawn(sup)
  --luaunit.assertEquals(_error,"error")
  --luaunit.assertStrIContains(msg,"Some error")
  luaunit.assertTrue(unreachable)
  luaunit.assertEquals(VM.coroutines,{ROOT="ROOT"})
end

function test_link2()
  local linker = function()
    local msg = VM.receive() 
    while msg ~= "die" do
      VM.link(msg)
      msg = VM.receive() 
    end
    error(msg)
  end
  local co2 = VM.spawn(linker)
  local co3 = VM.spawn(linker)
  local co4 = VM.spawn(linker)
  
  VM.send(co2,co3)
  VM.send(co3,co4)
  VM.send(co2,"die")
  luaunit.assertEquals(VM.coroutines,{ROOT="ROOT"})
end

--TODO test a flagged routine that throws an error
--should crash..?
function test_link3()
  local event
  local co = VM.spawn(function()
    event = VM.spawnlink(function()
      VM.receive() VM.send(co,"msg") end) end)
  local co2 = VM.spawn(function() VM.receive() end)
end

function test_exit()
  local co = function()
    VM.exit("custom reason")
    end
  luaunit.assertError(VM.spawnlink,co)
  co = VM.spawnlink(function()
    VM.exit("normal")
    end)
  luaunit.assertEquals(VM.coroutines,{ROOT="ROOT"})
end

function test_trap_exit()
  local child
  local sup = function()
    child = VM.spawn(function() 
      VM.process_flag("trap_exit",true)
      VM.receive()
      VM.receive() end)
    VM.link(child)
    VM.receive()
    error("An error!")
  end
  local co = VM.spawn(sup)
  VM.process_flag("trap_exit",true)
  VM.link(co)
  VM.send(co,"amsg")
  --luaunit.assertError(VM.send,co,"amsg")
  luaunit.assertTrue(VM.coroutines[child])
  luaunit.assertTrue(VM.coroutines[co] == nil)
end

function test_kill()
  local co = VM.spawn(function()
    VM.process_flag("trap_exit",true)
    while true do
      VM.receive()
    end
  end)
  VM.exit("normal",co)
  VM.flushAll()
  luaunit.assertTrue(VM.coroutines[co])
  VM.exit("a reason",co)
  VM.flushAll()
  luaunit.assertTrue(VM.coroutines[co])
  VM.exit("kill",co)
  VM.flushAll()
  luaunit.assertEquals(VM.coroutines,{ROOT="ROOT"})
  VM.spawn(function() while true do VM.receive() end end)
  luaunit.assertError(VM.exit,"kill",VM.self())
  luaunit.assertEquals(VM.coroutines,{ROOT="ROOT"})
end

function test_flush()
  VM.send(VM.running(),"Hello!")
  VM.send(VM.running(),"Hello again.")
  VM.send(VM.running(),"Goodbye?")
  local results = VM.flush()
  luaunit.assertEquals(results[1][1],"Hello!")
  luaunit.assertEquals(results[3][1],"Goodbye?")
end

function test_monitor()
  local co = VM.spawn(function() VM.receive() VM.exit("boom") end)
  local ref = VM.monitor("process",co)
  luaunit.assertEquals(VM.monitors[ref],{target=co,watching=VM.running()})
  luaunit.assertTrue(ref)
  VM.send(co,"exit")
  local result = VM.flush()
  luaunit.assertEquals(result[1],{'DOWN',ref,"process",co,"boom",n=5})
end

function test_spawn_mon()
  local co,ref = VM.spawn_monitor(function() VM.receive() end)
  luaunit.assertEquals(VM.monitors[ref],{target=co,watching=VM.running()})
end

function test_demonitor()
  local co,ref = VM.spawn_monitor(function() VM.receive() end)
  local bool = VM.demonitor(ref)
  luaunit.assertTrue(bool)
  VM.send(co,"die")
  local r = VM.flush()
  luaunit.assertEquals(r,{})
end

function test_handle_call()
  local server = {}
  function server.start_link() return gen_server.start_link(server,{},{}) end
  function server.init() return true, {} end
  function server.handle_call(Req,From,State)
    gen_server.reply(From,"ok")
    return State
  end
  local ok, co = server.start_link()
  luaunit.assertEquals(gen_server.call(co,"hello"),"ok")
end

function test_gen_server_fail_init()
  local msg = "failed init"
  local Server = {}
  function Server.start_link() return gen_server.start(Server,{},{}) end
  function Server.init() error(msg) end
  local ok, reason = Server.start_link()
  luaunit.assertFalse(ok)
  luaunit.assertStrIContains(reason,msg)
end

function test_gen_server_start()
  local name = "server"
  local Server = {}
  function Server.start_link() return gen_server.start_link(Server,{},{},name) end
  function Server.init() return true, {} end
  local _, co = Server.start_link()
  luaunit.assertEquals({Server.start_link()},{false,{"alreadyRegistered",co}})
end

function test_gen_server_shutdown()
  local executed = false
  local Server = {}
  function Server.init() return true, {} end
  function Server.start() local _,co = gen_server.start_link(Server,{},{}) return co end
  function Server.terminate(reason) executed = reason end
  function Server.handle_info(Req,State)
    if Req[1] == "stop" then
      return "stop","normal",State
    end
    return "noreply", State
  end
  local co = Server.start()
  VM.exit("normal",co)
  luaunit.assertEquals(executed,false)
  executed = false
  co = Server.start()
  VM.send(co,"hello")
  luaunit.assertFalse(executed)
  VM.send(co,"stop")
  luaunit.assertEquals(executed,"normal")
  executed = false
  co = Server.start()
  gen_server.stop(co)
  luaunit.assertEquals(executed,"normal")
  co = Server.start()
  gen_server.stop(co,"abnormal")
  luaunit.assertEquals(executed,"abnormal")
end

function test_gen_server_exit_normal()
  local executed = false
  local Server = {}
  function Server.init() return true, {} end
  function Server.start() local _,co = gen_server.start_link(Server,{},{}) return co end
  function Server.terminate(reason,State) executed = reason end
  function Server.handle_cast(Request,State)
    VM.exit("normal")
    return State
  end
  local co = Server.start()
  gen_server.cast(co,{"a req"})
  luaunit.assertEquals(executed,false)
end

function test_exec()
  local Mod = {}
  function Mod.fun(x) return x end
  luaunit.assertError(VM.exec,Mod,"bun",2)
  luaunit.assertEquals(VM.exec(Mod,"fun",2),2)
end

os.exit(luaunit.LuaUnit.run())