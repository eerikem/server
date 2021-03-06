--local VM = require 'vm'
local luaunit = require 'lib.luaunit'
--Warning uncommenting creates new VM instance in CC
--TODO fix Require to not overwrite this?!?

local errorSound = "/playsound frontierdevelopment:event.mondecline @p"

local function beep()
  exec(errorSound)
end

---
-- The generic server object
-- gen_server expects a child module to have all three handler functions defined
-- @type [parent=#gen_server] server

---
-- Initialize the server state
-- This function is required to imlpement a gen_server
-- @function [parent = #server] init
-- @return #boolean ok
-- @return State

---
-- Handles asynchronous requests
-- This function is required to implement a gen_server
-- @function [parent = #server] handle_cast
-- @param Request
-- @param State
-- @return State

---
-- Handles synchronous requests
-- This function is required to implement a gen_server
-- Use gen_server.reply(From,Response)
-- @function [parent = #server] handle_call
-- @param Request
-- @param From
-- @param State
-- @return State

---
-- Handles exit signals and messages sent directly using VM.send
-- This function is required to implement a gen_server
-- @function [parent = #server] handle_info
-- @param Request
-- @param State
-- @return State

local gen_server = {}

---
-- Send a synchronous request 
-- @param #thread Co The destination of the request
-- @param Request Can be anything
-- @param #number Timeout optional
function gen_server.call(Co, Request, Timeout)
  --if type(Request) ~= "table" then error("Request must be a list",2) end
  if Co == nil or Request == nil then error("Badarg",2) end
  if not Timeout then Timeout = 5 end--TODO implement receive timeout behaviour
  local Ref = VM.monitor("process",Co)
  VM.send(Co,"sync",Request,VM.running(),Ref)
  while true do
    local Response = {VM.receive(Timeout)}
        --Response = {exit,_Ref,type,_Co,Reason}
    if unpack(Response) == nil then error("Gen_server.call received nil response.")
    elseif #Response == 5 and Response[1] == "DOWN" and Response[2] == Ref and Response[3] == "process" and Response[4] == Co then
      error(Response[5],2)
    elseif Response == nil then
      error("Bad reply to: ".. luaunit.prettystr(Request,true) .."\ngen_server.call GOT: "..luaunit.prettystr(Response,true),2)
    else
      local _Ref = table.remove(Response,1)
      if Ref == _Ref then
        VM.demonitor(Ref)
        return unpack(Response)
      else
        VM.log("gen_server.call got incorrect response: "..luaunit.prettystr(Response,true))
--        error("here",2)
      end
    end
  end
end

function gen_server.cast(Co, Request)
  if Co == nil then error("Badarg 1",3)
  elseif Request == nil then error("Badarg 2",3) end
  --if type(Request) ~= "table" then error("Request must be a list",2) end
  return VM.send(Co,"async",Request)
end

function gen_server.stop(Co, Reason)
  return VM.send(Co,"gen_server_stop",Reason or "normal")
end

--redefined later
local function loop(Module,State) end

local function callTerminate(Module,Reason,State)
  if Module.terminate then
    return Module.terminate(Reason,State)
  end
end

local function handleResults(Module,...)
  if arg == nil then error("badarg returned after gen_server.handle_...") end
  if type(arg[1])=="table" then
    return loop(Module,arg[1])
  else
    local action = arg[1]
    if action == "reply" then
      gen_server.reply(unpack[arg[2]])
      return loop(Module,arg[3])
    elseif action == "noreply" then
      return loop(Module,arg[2])
    elseif action == "stop" then
      return callTerminate(Module,arg[2],arg[3])
    else
      error("bad action received after gen_server.handle_...")
    end
  end
end

local function callHandleInfo(Module, Response,State)
  if Module.handle_info then
    return handleResults(Module,Module.handle_info(Response,State))
  else
    if VM.co2names[VM.running()] then
      error("handle_info not defined in Module "..VM.co2names[VM.running()][1],2)
    else
      error("handle_info not defined in Module "..tostring(VM.running()),2)
    end
  end
end

loop = function(Module,State)
  if VM.dead[VM.running()] then error("here",2) end
  if State == nil then error("gen_server received nil state",2) end
  local Response = {VM.receive()}
  --TODO better pattern matching on messages!?!?!?!?!
  if Response[1] == "EXIT" then
    local _,co,Reason = unpack(Response)
    --TODO verify these conditions for callHandleInfo
    if Reason == "normal" or VM.co2flags[VM.running()].trap_exit then
      return callHandleInfo(Module,Response,State)
    else
      return callTerminate(Module,Reason,State)
    end
  elseif Response[1] == "gen_server_stop" then
    return callTerminate(Module,Response[2],State)
  elseif Response[1] == "shutdown" then --TODO correct way to handle shutdown?!?
    return callTerminate(Module,"shutdown",State)
  end
  local Type, Msg, Co, Ref = unpack(Response) 
  if Type == "async" then
    return loop(Module,Module.handle_cast(Msg,State))
  elseif Type == "sync" then
    return loop(Module,Module.handle_call(Msg,{Co,Ref},State))
  else
    return callHandleInfo(Module,Response,State)
  end
end

function gen_server.reply(From,...)
  if type(From) ~= "table" then error("Bad From",2) end
  local co,Ref = unpack(From)
  VM.send(co,Ref,...)
end

local function init(Module, ...)
  if not Module.init then error("Module.init required by gen_server",3) end
  local ok, State = Module.init(...)
  if ok ~= true and State then
    error(luaunit.prettystr(State,true),0)
  elseif State == nil then
    error("init return nil State.",0)
  end
  return loop(Module,State)
end

local function startServer(co,ServerName)
  if ServerName then
    if VM.registered(ServerName) then
      return false, {"alreadyRegistered",VM.coroutines[ServerName]}
    else
      VM.register(ServerName,co)
    end
  end
  local ok, reason = VM.resume(co)
  if ok then
    return true, co
  else
    return false, reason
  end
end

function gen_server.start(Module, Args, Options, ServerName)
  local co = VM.queue(function() return init(Module,unpack(Args)) end)
  return startServer(co,ServerName)
end


function gen_server.start_link(Module, Args, Options, ServerName)
  local co =  VM.queueLink(function() return init(Module,unpack(Args)) end)
  return startServer(co,ServerName)
end


return gen_server