local supervisor = {}

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
  
--TODO necessary?!?
local function init(Module)
  local Spec = Module.init()
  if Spec[1] == "ok" then
    local Children = Spec[2][2]
    for _,ChildSpec in ipairs(Children) do
      local ChildId, StartFunc,Restart,Shutdown,Type,Modules = unpack(ChildSpec)
      local Module, Start, Args = unpack(StartFunc)
      return Module[Start](unpack(Args))
    end
  end
end

function supervisor.start_link(Module, Args, SupName)
  local co  = VM.spawn(function() return init(Module,Args) end)
  if SupName then VM.register(SupName,co) end
  return supervisor.start_link(Module, Args)
end

function supervisor.count_children(SupRef)
  
end

function supervisor.which_children(SupRef)

end

function supervisor.start_child(SupRef,ChildSpec)

end

function supervisor.init(Args)

end

return supervisor