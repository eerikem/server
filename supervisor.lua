local gen_server = require 'gen_server'
local Supervisor = {}
local luaunit = require 'luaunit'

--one_for_one = one dies, one restarted
--one_for_all = for dependent workers
--rest for one = chain of dependencies
--simple_one_for_one = one kind of child only..?
sup_flags = {stategy = "one_for_one",
  intensity = 1, period = 5}

--init returns: true, {{RestartStrategy, MaxRestart, MaxTime},[ChildSpec]}
--             |false, Reason
--Childspec: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
  --ChildId: Name of child for debugging(internal to supervisor)
  --StartFunc: what function to start the child eg: start_link
  --Restart: permanent|temporary|transient
  --  eg: always restart, never restart
  --      or always restart unless terminated with normal or shutdown
  --Shutdown: infinity|brutal_kill|Timeout in ms
  --Type: supervisor|worker
  --Modules: the Module or 'dynamic' for unkown cases such as eventHandlers

local function resolve(Child,State)
  if type(Child)=="thread"then
    return Child
  else
    if State.childIds[Child] then
      return State.childIds[Child]
    else
      return nil
    end
  end
end

local function generateChildReport(State)
  local R = {}
  for ChildId,Co in pairs(State.childIds) do
    table.insert(R,{ChildId,Co})
  end
  return R
end

local function removeChild(Child,State)
  for i,child in ipairs(State.children) do
    if child == Child then
      table.remove(State.children,i)
      break
    end
  end
  State.childIds[State.childSpecs[Child][1]]=nil
  State.childSpecs[Child]=nil
end

local function terminateChild(Child,State)
  Child = resolve(Child,State)
  if not Child then return false, "not found" end
  if State.spec.strategy == "simple_one_for_one" then
  
  else
    local ChildSpec = State.childSpecs[Child]
    if ChildSpec[4]=="infinity" then
    
    elseif ChildSpec[4]=="brutal_kill" then
      VM.exit("kill",Child)
    else
      assert(type(ChildSpec[4])=="number")
      VM.exit("shutdown",Child)
--TODO implementing pattern matching receive and timeout abilities in VM.
--      VM.receive(ChildSpec[4])
      
--    if ChildSpec[3]=="temporary" then
--      State.childSpecs[Child]=nil
--    end
      removeChild(Child,State)
    end
  end
  return true
end

local function terminateChildren(State)
  while #State.children > 0 do
    local Child = table.remove(State.children,#State.children)
    terminateChild(Child,State)
  end
end

local function startChild(ChildSpec,State)
    local ChildId, StartFunc,Restart,Shutdown,Type,Modules = unpack(ChildSpec)
    local Mod, Start, Args = unpack(StartFunc)
    local ok, co = VM.exec(Mod,Start,unpack(Args))
    if ok then
      table.insert(State.children,co)
      State.childIds[ChildId]=co
      State.childSpecs[co]=ChildSpec
      return ok, co
    else
      terminateChildren(State)
      return false, {"shutdown",co}
    end
end


local function restartChild(Child,State)
  local ChildSpec = State.childSpecs[Child]
  removeChild(Child,State)
  startChild(ChildSpec,State)
end

function Supervisor.start_link(Module, Args, SupName)
  local ok, co = gen_server.start_link(Supervisor,{Module,Args},nil,SupName)
  return ok, co
end

function Supervisor.count_children(SupRef)
  return gen_server.call(SupRef,{"count"})
end

function Supervisor.which_children(SupRef)
  return gen_server.call(SupRef,"which_children")
end

function Supervisor.start_child(SupRef,ChildSpec)
  return gen_server.call(SupRef,{"start_child",ChildSpec})
end

function Supervisor.terminate_child(SupRef,Child)
  return gen_server.call(SupRef,{"terminate_child",Child})
end

-------
--OTP--
-------

function Supervisor.init(Module,Args)
  --ok,{Restart,Childspecs}
  local ok, Spec = Module.init(unpack(Args))
  if ok then
    VM.process_flag("trap_exit",true)
    local State = {supervisor = true,spec=Spec[1],children={},childIds={},childSpecs={}}
    for _,ChildSpec in ipairs(Spec[2]) do
      local ok, reason = startChild(ChildSpec,State)
      if not ok then
        terminateChildren(State)
      end
    end
    return true, State
  else
    return false, "bad spec"
  end
end

function Supervisor.handle_call(Request,From,State)
  local event = Request[1]
  if event == "start_child" then
    local ChildSpec = Request[2]
    if State.childIds[ChildSpec[1]] then
      gen_server.reply(From,{false,"already_present"})
    else
      local co = startChild(ChildSpec,State)
      gen_server.reply(From,{true,co})
    end
  elseif event == "count" then
    gen_server.reply(From,#State.children)
  elseif Request == "which_children" then
    gen_server.reply(From,generateChildReport(State))
  elseif event == "terminate_child" then
    local Child = Request[2]
    gen_server.reply(From,terminateChild(Child,State))
  else
    VM.log("Sup got: "..event)
  end
  return State
end

function Supervisor.handle_cast(Request,State)

  return State
end

local function handleExit(Child,State)
  if State.spec.strategy == "one_for_one" then
    local Restart = State.childSpecs[Child][5]
    if Restart == "permanent" then
      restartChild(Child,State)
    elseif Restart == "temporary" then
      
    elseif Restart == "transient" then
      
    else
      error("Bad child Restart strategy: ",Restart)
    end
  end
end

function Supervisor.handle_info(Request,State)
  local event = Request[1]
  if event == "EXIT" then
    local _,Child,Reason = unpack(Request)
    handleExit(Child,State)
  end
  return State
end

return Supervisor