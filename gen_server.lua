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
  return loop(Module,Module.init(...))
end


function gen_server.start(Module, Args, Options, ServerName)
  local co = VM.spawn(function() return init(Module,unpack(Args)) end)
  VM.log(VM.status(co))
  if  VM.status(co) ~= "dead" then
    if ServerName then
      VM.registerName(ServerName,co) end
    return co
  else
    return false, "Module.init failed"
  end
end

function gen_server.start_link(Module, Args, Options, ServerName)
  local co =  VM.spawnlink(function() return init(Module,unpack(Args)) end)
  if VM.status(co) ~= "dead" then
    if ServerName then VM.register(ServerName,co) end
    return co
  else
    return false, "Module.init failed"
  end
end


return gen_server