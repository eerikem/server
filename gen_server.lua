--local VM = require 'vm'
local luaunit = require 'lib.luaunit'
--Warning uncommenting creates new VM instance in CC
--TODO fix Require to not overwrite this?!?

local errorSound = "/playsound frontierdevelopment:event.mondecline @p"

local function beep()
  exec(errorSound)
end

local gen_server = {}

function gen_server.call(Co, Request, Timeout)
  --if type(Request) ~= "table" then error("Request must be a list",2) end
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
        --TODO something other then silently fail here?!
      end
    end
  end
end

function gen_server.cast(Co, Request)
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
      error("handle_info not defined in Module "..VM.co2names[VM.running()][1])
    else
      error("handle_info not defined in Module "..tostring(VM.running()))
    end
  end
end

loop = function(Module,State)
  if VM.dead[VM.running()] then error("here",2) end
  local Response = {VM.receive()}
  --TODO better pattern matching on messages!?!?!?!?!
  if Response[1] == "EXIT" then
    local _,co,Reason = unpack(Response)
    --TODO if normal and trapping exits then callHandleInfo
    if Reason == "normal" then
      return callHandleInfo(Module,Response,State)
    else
      return callTerminate(Module,Reason,State)
    end
  elseif Response[1] == "gen_server_stop" then
    return callTerminate(Module,Response[2],State)
  end
  --TODO catch shutdown message?
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
  if ok ~= true then error("init returned bad state: "..luaunit.prettystr(State,true),2) end  
  VM.process_flag("trap_exit",true)
  return loop(Module,State)
end

local function startServer(co,ServerName)
  local ok, reason = VM.resume(co)
  if VM.status(co) ~= "dead" and ServerName then
    if VM.registered(ServerName) then
      return false, {"alreadyRegistered",VM.coroutines[ServerName]}
    else
      VM.register(ServerName,co)
    end
  end
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