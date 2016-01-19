local VM = {}

local index = 1
local RUNNING = 0
VM.coroutines = {}
VM.co2names = {}

--TODO link
--TODO spawn_link
VM.coroutines[1]=coroutine.running()

function VM.self()
  return 1
end

function VM.running()
  return RUNNING
end

function VM.status(co)
  if not VM.coroutines[co] then error("badarg: Coroutine "..co.." does not exist",2) end
  return coroutine.status(VM.coroutines[co])
end

local function unregisterNames(co)
  for _,name in pairs(VM.co2names[co]) do
    VM.coroutines[name] = nil
  end
  VM.co2names[co]=nil
end

local function registerName(name,co)
  VM.coroutines[name]=VM.coroutines[co]
  if not VM.co2names[co] then
    VM.co2names[co]={}
  end
  table.insert(VM.co2names[co],name)
end

function VM.unregister(name)
  if not VM.coroutines[name] then
    error("badarg: "..name.." not a registered coroutine",2)
  end
  local co = VM.coroutines[name]
  VM.coroutines[name]=nil
  for x,_name in pairs(VM.co2names[co]) do
    if _name == name then
      table.remove(VM.co2names[co],x)
      break
    end
  end
  return true
end

local function removeCo(co)
  --for k,v in pairs(VM.coroutines) do print(k,v) end
  if VM.co2names[co] then
    unregisterNames(co) end
  VM.coroutines[co]=nil
end

local function kill(co)
  removeCo(co)
end


function VM.spawn(fun)
  index = index + 1
  VM.coroutines[index]=coroutine.create(fun)
  VM.resume(index)
  return index
end

--TODO change to private function?
function VM.resume(co,...)
  RUNNING = co
  local thread = VM.coroutines[co]
  local ok, msg = coroutine.resume(thread,unpack(arg))
  if not ok then
    print("ERROR: "..msg)
    --io.stdin:read'*l'
    end
  if coroutine.status(thread)=="dead" then
    removeCo(co)
  end
end

function VM.send(co,...)
  if type(co) == "string" then
    if not VM.coroutines[co] then
      error("badarg: "..co.." not a registered coroutine")
    end
  elseif not (type(co) == "number") then error("badarg: "..co,2) end
  if VM.coroutines[co] then
    VM.resume(co,unpack(arg))
  end
end

function VM.receive()
  local co = RUNNING
  local function terminate(event,...)
    if event == "terminate" then
      kill(co)
    else
      return event,unpack(arg)
    end
  end
  return terminate(coroutine.yield())
end

function VM.register(name,co)
  if VM.coroutines[name] then
    error("badarg: "..name.." already registered",2)
  elseif VM.status(co) == "dead" then
    error("badarg: Cannot register dead coroutine "..co,2)
  elseif not VM.coroutines[co] then
    error("badarg: Cannot find coroutine "..co)
  else
    registerName(name,co)
  end
end

function VM.registered()
  local names = {}
  for key,_ in pairs(VM.coroutines) do
    if type(key)=="string" then table.insert(names,key) end
  end
  return names
end

function VM.link(co)

end

return VM  