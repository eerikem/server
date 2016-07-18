--local VM = require 'vm'
local luaunit = require 'luaunit'
--Warning uncommenting creates new VM instance in CC
--TODO fix Require to not overwrite this?!?

local gen_server = {}

function gen_server.call(Co, Request, Timeout)
  --if type(Request) ~= "table" then error("Request must be a list",2) end
  if not Timeout then Timeout = 5 end--TODO implement receive timeout behaviour
  local Ref = VM.monitor("process",Co)
  VM.send(Co,"sync",Request,VM.running(),Ref)
  while true do
    local Response = {VM.receive(Timeout)}
    --Response = {exit,_Ref,type,_Co,Reason}
    if #Response == 5 and Response[1] == "DOWN" and Response[2] == Ref and Response[3] == "process" and Response[4] == Co then
      error(Response[5],2)
    elseif Response == nil then
      error("Bad reply to: ".. luaunit.prettystr(Request,true) .."\ngen_server.call GOT: "..luaunit.prettystr(Response,true),2)
    else
      local _Ref = table.remove(Response,1)
      if Ref == _Ref then
        VM.demonitor(Ref)
        return unpack(Response)
      end
    end
  end
end

function gen_server.cast(Co, Request)
  --if type(Request) ~= "table" then error("Request must be a list",2) end
  return VM.send(Co,"async",Request)
end

local function loop(Module,State)
  local Response = {VM.receive()}
  --TODO better pattern matching on messages!?!?!?!?!
  --TODO catch shutdown message?
  if #Response > 1 or #Response < 5 then
    local Type, Msg, Co, Ref = unpack(Response) 
    if Type == "async" then
      return loop(Module,Module.handle_cast(Msg,State))
    elseif Type == "sync" then
      return loop(Module,Module.handle_call(Msg,{Co,Ref},State))
    end
  end
  return loop(Module,Module.handle_info(Response,State))
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