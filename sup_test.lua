VM = require 'vm'
local gen_server = require 'gen_server'
luaunit = require 'luaunit'
local supervisor = require 'supervisor'

---------------
--Test Module--
---------------
local Server = {worker = true}
function Server.start_link(Type)
  return gen_server.start_link(Server,{Type},{},{})
end

function Server.init(Type)
  if Type =="fail" then
    return true, {type = Type}
  else
    error("type is: "..Type)
    error("bad server type",2)
  end
end

function Server.handle_cast(Request,State)
  if State.type == "fail" then
    error(tostring(VM.running()).." failing!")
  end
  return State
end

function Server.handle_call(Request,From,State)
  gen_server.reply(From,"ok")
  return State
end

function Server.handle_info(Event,State)
  print("handle info received something")
  return State
end

-------------------
--Test Supervisor--
-------------------

local Sup = {supervisor = true}

function Sup.start_link()
  supervisor:start_link(Sup,{})
end

--------------
--Test Suite--
--------------


function setup_each()
  VM.init()
end

function test_Module()
  local ok, Co = Server.start_link("fail")
  luaunit.assertTrue(VM.coroutines[Co])
  luaunit.assertEquals(gen_server.call(Co,"ok?"),"ok")
  luaunit.assertError(gen_server.cast,Co,{"gonna die?"})
  --TODO this kills ROOT which reinitializes the VM...
  luaunit.assertFalse(VM.coroutines[Co])
end

function test_start()
  local ChildSpec = {"child",{Server,"start_link",{"fail"}},
        "permanent",500,"worker",{Server}}
  Sup.init = function()
    return true,{{"one_for_one"},{ChildSpec}} 
  end
  local ok, co = supervisor.start_link(Sup,{})
  luaunit.assertEquals(supervisor.count_children(co),1)
  local ok, response = unpack(supervisor.start_child(co,ChildSpec))
  luaunit.assertFalse(ok)
  ChildSpec[1]="child2"
  ok, response = supervisor.start_child(co,ChildSpec)
  luaunit.assertEquals(supervisor.count_children(co),2)
end

function test_restart()
  local ChildSpec = {"child",{Server,"start_link",{"fail"}},
    "permanent",500,"worker",{Server}}
  Sup.init = function()
    return true,{{"one_for_one"},{ChildSpec}}
  end
  local ok, co = supervisor.start_link(Sup,{})
  local child = supervisor.which_children(co)[1][2]
  gen_server.cast(child,"die")
  luaunit.assertEquals(supervisor.count_children(co),1)
end

function test_terminate_child()
  local childId = "child"
  local ChildSpec = {childId,{Server,"start_link",{"fail"}},
    "permanent",500,"worker",{Server}}
  Sup.init = function()
    return true, {{"one_for_one"},{ChildSpec}}
  end
  local ok, co = supervisor.start_link(Sup,{})
  luaunit.assertEquals({supervisor.terminate_child(co,"not a child")},{false,"not found"})
  luaunit.assertTrue(supervisor.terminate_child(co,childId))
  luaunit.assertEquals(supervisor.count_children(co),0)
end

function test_fail_child_init()
  local Server = {init=function() error("Failed init") end }
  function Server.start_link(Type)
    return gen_server.start_link(Server,{Type},{},{}) end
  
  local ChildSpec = {"child",{Server,"start_link",{"fail"}},
    "permanent",500,"worker",{Server}}
  Sup.init = function()
    return true, {{"one_for_one"},{ChildSpec}}
  end
  
  luaunit.assertEquals(supervisor.start_link(Sup,{}),{false,"Failed init"})
end

os.exit(luaunit.LuaUnit.run())