--local VM = require 'vm'
local luaunit = require 'luaunit'
--Warning uncommenting creates new VM instance in CC
--TODO fix Require to not overwrite this?!?

local gen_server = {}

function gen_server.call(Co, Request, Timeout)
  if not Timeout then Timeout = 5 end--TODO implement receive timeout behaviour
  local Ref = VM.monitor("process",Co)
  VM.send(Co,"sync",Request,VM.running(),Ref)
  while true do
    local Response = {VM.receive(Timeout)}
    if #Response == 2 then 
      local _Ref, Reply = unpack(Response)
      if Ref == _Ref then
        VM.demonitor(Ref)
        return Reply
      end
    elseif #Response == 5 then
      local exit,_Ref,type,_Co,Reason = unpack(Response)
      if exit == "DOWN" and Ref == _Ref and type == "process" and Co == _Co then
        error(Reason,2)
      end
    else
      error("Bad reply to: ".. luaunit.prettystr(Request,true) .."\ngen_server.call GOT: "..luaunit.prettystr(Response,true),2)
    end
  end
end

function gen_server.cast(Co, Request)
  return VM.send(Co,"async",Request)
end

local function loop(Module,State)
  local Response = {VM.receive()}
  --TODO better pattern matching on messages!?!?!?!?!
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

function gen_server.reply(From,Reply)
  if type(From) ~= "table" then error("Bad From",2) end
  local co,Ref = unpack(From)
  VM.send(co,Ref,Reply)
end

local function init(Module, ...)
  return loop(Module,Module.init(...))
end


function gen_server.start(Module, Args, Options, ServerName)
  local co = VM.spawn(function() return init(Module,unpack(Args)) end)
  if ServerName then VM.registerName(ServerName,co) end
  return co
end

function gen_server.start_link(Module, Args, Options, ServerName)
  local co =  VM.spawnlink(function() return init(Module,unpack(Args)) end)
  if ServerName then VM.register(ServerName,co) end
  return co
end


return gen_server