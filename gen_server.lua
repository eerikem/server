--local VM = require 'vm'
--Warning uncommenting creates new VM instance in CC
--TODO fix Require to not overwrite this?!?

local gen_server = {}

function gen_server.call(Co, Request, Timeout)
  if not Timeout then Timeout = 5 end--TODO implement receive timeout behaviour
  local Ref = VM.monitor("process",Co)
  VM.send(Co,"sync",Request,VM.running(),Ref)
  while true do
    local Response = {VM.receive(Timeout)}
    if table.maxn(Response) == 2 then 
      local _Ref, Reply = unpack(Response)
      if Ref == _Ref then
        VM.demonitor(Ref)
        return Reply
      end
    elseif table.maxn(Response) == 5 then
      local exit,_Ref,type,_Co,Reason = unpack(Response)
      if exit == "DOWN" and Ref == _Ref and type == "process" and Co == _Co then
        error(Reason)
      end
    else
      error("Bad reply to gen_server.call")
    end
  end
end

function gen_server.cast(Co, Request)
  return VM.send(Co,"async",Request)
end

local function loop(Module,State)
  local Type, Msg, Co, Ref = VM.receive()
  if Type == "async" then
    return loop(Module,Module.handle_cast(Msg,State))
  elseif Type == "sync" then
    return loop(Module,Module.handle_call(Msg,{Co,Ref},State))
  else
    return loop(Module,Module.handle_info(Type,State))
  end
end

function gen_server.reply(From,Reply)
  if type(From) ~= "table" then error("Bad From",2) end
  local co,Ref = unpack(From)
  VM.send(co,Ref,Reply)
end

local function init(Module, ...)
  return loop(Module,Module.init(unpack(arg)))
end


function gen_server.start(Module, Args, Options, ServerName)
  local co = VM.spawn(function() return init(Module,unpack(Args)) end)
  VM.registerName(ServerName,co)
  return co
end

function gen_server.start_link(Module, Args, Options, ServerName)
  local co =  VM.spawnlink(function() return init(Module,unpack(Args)) end)
  if ServerName then VM.register(ServerName,co) end
  return co
end


return gen_server