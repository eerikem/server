local gen_server = require 'gen_server'
local Supervisor = {}
local luaunit = require 'lib.luaunit'
local queue = require 'lib.queue'
--one_for_one = one dies, one restarted
--one_for_all = for dependent workers
--rest for one = chain of dependencies
--simple_one_for_one = one kind of child only..?
local sup_flags = {strategy = "one_for_one",intensity = 1,period = 5}
local child_flags = {restart = "permanent",shutdown = 500,type = "worker"}

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

--Resolves ChildID to thread or nil if not found.
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
  State.restarts[Child]=nil
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

local function shutdownChild(Child,State)
  gen_server.stop(Child,"shutdown")
end

function Supervisor.terminate(Reason,State)
  while #State.children > 0 do
    local Child = table.remove(State.children,#State.children)
    shutdownChild(Child,State)
  end
end

local function terminateChildren(State)
  while #State.children > 0 do
    local Child = table.remove(State.children,#State.children)
    terminateChild(Child,State)
  end
end

local function initChildSpec(ChildSpec)
  ChildSpec[3] = ChildSpec[3] or child_flags.restart
  ChildSpec[4] = ChildSpec[4] or child_flags.shutdown
  ChildSpec[5] = ChildSpec[5] or child_flags.type
  if not ChildSpec[6] then ChildSpec[6]={ChildSpec[2][1]} end
end

local function initSupSpec(State)
  State.spec.strategy = State.spec[1] or sup_flags.strategy
  State.spec.intensity = State.spec[2] or sup_flags.intensity
  State.spec.period = State.spec[3] or sup_flags.period
end

local function startChild(ChildSpec,State)
    local ChildId, StartFunc,Restart,Shutdown,Type,Modules = unpack(ChildSpec)
    local Mod, Start, Args = unpack(StartFunc)
    local ok, co = VM.exec(Mod,Start,unpack(Args))
    if ok then
      table.insert(State.children,co)
      State.childIds[ChildId]=co
      initChildSpec(ChildSpec)
      State.restarts[co]=queue.new()
      State.childSpecs[co]=ChildSpec
      return ok, co
    else
      terminateChildren(State)
      return false, {"shutdown",co}
    end
end


local function restartChild(Child,State)
  local ChildSpec = State.childSpecs[Child]
  local Q = State.restarts[Child]
  removeChild(Child,State)
  local time = os.time()
  local ok, co
  if not queue.first(Q) then
    queue.push(Q,time)
    local ok, co = startChild(ChildSpec,State)
    if ok then State.restarts[co]=Q end
  else
    while queue.first(Q) do
      local restarts = queue.size(Q)
      if math.abs(queue.first(Q) - time) > State.spec.period then
        queue.pop(Q)
        if queue.size(Q) == 0 then
          local ok, co = startChild(ChildSpec,State)
          queue.push(Q,time)
          if ok then State.restarts[co]=Q end
          break
        end
      else
        if restarts > State.spec.intensity - 1 then
          return VM.log("Child exceeded restart policy.")
        else
          queue.push(Q,time)
          local ok, co = startChild(ChildSpec,State)
          if ok then State.restarts[co]=Q end
          break
        end
      end
    end
  end
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
    local State = {supervisor = true,spec=Spec[1],children={},childIds={},childSpecs={},restarts={}}
    initSupSpec(State)
    for _,ChildSpec in ipairs(Spec[2]) do
      local ok, reason = startChild(ChildSpec,State)
      if not ok then
        terminateChildren(State)
        return false, reason
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

local function handleChildExit(Child,State)
  if State.spec.strategy == "one_for_one" then
    local Restart = State.childSpecs[Child][3]
    if Restart == "permanent" or nil then
      VM.log("Restarting permanent child")
      restartChild(Child,State)
    elseif Restart == "temporary" then
      VM.log("Killing temporary child")
      
    elseif Restart == "transient" then
      VM.log("Restarting transient child?")
      
    else
      error("Bad child Restart strategy: "..Restart)
    end
  else
    VM.log("Unsupported strategy"..State.spec.strategy)
  end
end

function Supervisor.handle_info(Request,State)
  local event = Request[1]
  if event == "EXIT" then
    local _,Co,Reason = unpack(Request)
    if State.childSpecs[Co] then
      handleChildExit(Co,State)
    else
      --TODO Handle linked children who fail during init
      VM.log("supervisor got EXIT from "..tostring(Co))
    end
  end
  return State
end

return Supervisor