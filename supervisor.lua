local gen_server = require 'gen_server'
local Supervisor = {}

--one_for_one = one dies, one restarted
--one_for_all = for dependant workers
--rest for one = chain of dependencies
--simple_one_for_one = one kind of child only..?
sup_flags = {stategy = "one_for_one",
  intensity = 1, period = 5}

--init returns {ok, {{RestartStrategy, MaxRestart, MaxTime},[ChildSpec]}}

--Childspec: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}
  --ChildId: Name of child for debugging(internal to supervisor)
  --StartFunc: what function to start the child eg: start_link
  --Restart: permanent|temporary|transient
  --  eg: always restart, never restart
  --      or always restart unless terminated with normal or shutdown
  --Shutdown: infinity|brutal_kill|Timeout in ms
  --Type: supervisor|worker
  --Modules: the Module or 'dynamic' for unkown cases such as eventHandlers
  
local function startChild(ChildSpec,State)
    local ChildId, StartFunc,Restart,Shutdown,Type,Modules = unpack(ChildSpec)
    local Mod, Start, Args = unpack(StartFunc)
    local co = VM.exec(Mod,Start,unpack(Args))
    table.insert(State.children,co)
    State.childIds[ChildId]=co
    State.childSpecs[co]=ChildSpec
    return co
end  

function Supervisor.start_link(Module, Args, SupName)
  return gen_server.start_link(Supervisor,{Module,Args},nil,SupName)
end

function Supervisor.count_children(SupRef)
  return gen_server.call(SupRef,{"count"})
end

function Supervisor.which_children(SupRef)

end

function Supervisor.start_child(SupRef,ChildSpec)
  return gen_server.call(SupRef,{"start_child",ChildSpec})
end

function Supervisor.init(Module,Args)
  --{ok,{Restart,Childspecs}}
  local Spec = Module.init(unpack(Args))
  if Spec[1] == "ok" then
    local State = {supervisor = true,spec=Spec[2][1],children={},childIds={},childSpecs={}}
    for _,ChildSpec in ipairs(Spec[2][2]) do
      local ok, reason = startChild(ChildSpec,State)
      if not ok then
        return {"error",{"shutdown",reason}}
      end
    end
    return State
  else
    error("bad spec")
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
  else
    VM.log("Sup got: "..event)
  end
  return State
end

function Supervisor.handle_cast(Request,State)

  return State
end

function Supervisor.handleInfo(Request,State)
  local event = Request[1]
  return State
end

return Supervisor