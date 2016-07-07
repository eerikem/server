VM = require 'vm'
local gen_server = require 'gen_server'
luaunit = require 'luaunit'
local supervisor = require 'supervisor'

---------------
--Test Module--
---------------
local Server = {}
function Server.start_link(Type)
  return gen_server.start_link(Server,{Type},{},{})
end

function Server.init(Type)
  if Type =="fail" then
    return {type = Type}
  else
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

-------------------
--Test Supervisor--
-------------------

local Sup = {}

function Sup.start_link()
  supervisor:start_link(Sup,{})
end

--------------
--Test Suite--
--------------

function test_Module()
  local Co = Server.start_link("fail")
  luaunit.assertTrue(VM.coroutines[Co])
  luaunit.assertEquals(gen_server.call(Co,"ok?"),"ok")
  luaunit.assertError(gen_server.cast,Co,{"gonna die?"})
  luaunit.assertFalse(VM.coroutines[Co])
end

function test_start()
  Sup.init = function()
    return {"ok",{{"one_for_one"},
      {{"child",{Server,"start_link",{"fail"}},
        "permanent",500,"worker",{Server}}}}} 
  end
  VM.init()
  local co = supervisor.start_link(Sup,{})
  luaunit.assertEquals(supervisor.count_children(co),1)
end

os.exit(luaunit.LuaUnit.run())