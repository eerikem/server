--local VM = require 'vm'

local gen_server = {}

function gen_server.call(Co, Request, Timeout)
  if not Timeout then Timeout = 5 end
  --local Ref = VM.monitor("process",Co)
  local Ref = nil--TODO VM.monitor
  VM.send(Co,"sync",Request,VM.running(),Ref)
  return VM.receive(Timeout)
end

function gen_server.cast(Co, Request)
  VM.send(Co,"async",Request)
end

local function loop(Module,State)
  local Type, Msg, Co, Ref = VM.receive()
  if Type == "async" then
    loop(Module,Module.handle_cast(Msg,State))
  elseif Type == "sync" then
    loop(Module,Module.handle_call(Msg,{Co,Ref},State))
  else
    loop(Module,Module.handle_info(Type,State))
  end
end

local function init(Module, ...)
  loop(Module,Module.init(unpack(arg)))
end


function gen_server.start(Module, Args, Options, ServerName)
  local co = VM.spawn(function() init(Module,unpack(Args)) end)
  VM.registerName(ServerName,co)
  return co
end

function gen_server.start_link(Module, Args, Options, ServerName)
  local co =  VM.spawnlink(function() init(Module,unpack(Args)) end)
  if ServerName then VM.register(ServerName,co) end
  return co
end


return gen_server