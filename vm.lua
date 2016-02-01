local VM = {}

local INDEX = 1
local ROOT = 1
local RUNNING = ROOT
VM.coroutines = {}
VM.co2names = {}
VM.co2flags = {}
VM.links = {}
VM.dead = {}
VM.log = function (msg) print(msg) end
local queue = {}

function VM.init()
  INDEX = 1
  ROOT = 1
  RUNNING = ROOT
  VM.coroutines = {}
  VM.co2names = {}
  VM.co2flags = {{}}
  VM.co2flags[ROOT]={}
  VM.links = {}
  VM.dead = {} 
  queue = {}
end

--TODO spawn_link
VM.coroutines[1]=coroutine.running()

function VM.self()
  return ROOT
end

function VM.running()
  return RUNNING
end

function VM.status(co)
  if VM.dead[co] then return "dead" end
  if not VM.coroutines[co] then error("badarg: Coroutine "..co.." does not exist",2) end
  return coroutine.status(VM.coroutines[co])
end

---------------------
--Utility Functions--
---------------------

local function HashArrayInsert(list,index,item)
  if not list[index] then
    list[index]={item}
  else
    table.insert(list[index],item)
  end
end

--Removes single list item from HashArray
local function HashArrayRemoveValue(list,index,item)
  for x,_item in pairs(list[index]) do
    if _item == item then
      table.remove(list[index],x)
      if table.getn(list[index]) == 0 then
        list[index] = nil
      end
      break
    end
  end
end

--takes a list from hash, clears another list
--of those items, then deletes the list
local function purgeItemsFromHash(hash,index,list)
  for _, item in pairs(hash[index]) do
    list[item] = nil
  end
  hash[index] = nil
end

--similar to above but purges from HashArray
local function purgeItemsFromHashArray(hash,index,list)
  for item, _ in pairs(hash[index]) do
    list[item][index]=nil
  end
  hash[index] = nil
end

---------
--Links--
---------

local function registerLink(co)
  --VM.log("Add "..RUNNING.." to "..co)
  HashArrayInsert(VM.links,co,RUNNING)
  --VM.log("Add "..co.." to "..RUNNING)
  HashArrayInsert(VM.links,RUNNING,co)
  --VM.log("Checking: "..VM.links[RUNNING][1].." "..VM.links[co][1])
end

local function unregisterLinks(co)
  purgeItemsFromHashArray(VM.links,co,VM.links)
end

local function unregisterLink(co)
  --VM.log("Unregistering "..RUNNING.." and "..co)
  HashArrayRemoveValue(VM.links,RUNNING,co)
  HashArrayRemoveValue(VM.links,co,RUNNING)
end

local function queueExit(signal,from,reason)
  table.insert(queue,{signal,from,reason})
end

local function receivedExit(co,msg) end
--TODO fix for loop


local function propogateExit(signal,source,reason)
  while VM.links[source] do
    local co = VM.links[source][1]
    unregisterLink(co)
    --Special case when propogateExit already running in the first coroutine
    if VM.coroutines[co]==coroutine.running() then
      RUNNING = co
      receivedExit(source,reason)
    elseif co == ROOT then
      queueExit(signal,RUNNING,reason)
    else
      VM.send(co,signal,source,reason)
    end
  end
end

function VM.link(co)
  if not VM.coroutines[co] then
    error("badarg: "..co.." is not a registered coroutine",2)
  end
  --VM.log("Linking "..RUNNING.." to "..co)
  registerLink(co)
end

function VM.unlink(co)
  HashArrayRemoveValue(VM.links,co,RUNNING)
  HashArrayRemoveValue(VM.links,RUNNING,co)
end

---------
--Names--
---------

--delete all names registered to coroutine
local function unregisterNames(co)
  purgeItemsFromHash(VM.co2names,co,VM.coroutines)
end

local function registerName(name,co)
  VM.coroutines[name]=VM.coroutines[co]
  HashArrayInsert(VM.co2names,co,name)
end

function VM.unregister(name)
  if not VM.coroutines[name] then
    error("badarg: "..name.." not a registered coroutine",2)
  end
  local co = VM.coroutines[name]
  VM.coroutines[name]=nil
  HashArrayRemoveValue(VM.co2names,co,name)
  return true
end

function VM.registered()
  local names = {}
  for key,_ in pairs(VM.coroutines) do
    if type(key)=="string" then table.insert(names,key) end
  end
  return names
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

--------------
--Coroutines--
--------------

local function removeCo(co)
  --VM.log("Removing "..co)
  --for k,v in pairs(VM.coroutines) do VM.log(k,v) end
  if VM.co2names[co] then
    unregisterNames(co) end
  VM.dead[co] = VM.coroutines[co]
  VM.coroutines[co]=nil
end

receivedExit = function (co,msg)
  if VM.co2flags[RUNNING].trap_exit and msg ~= 'kill' then
    return 'EXIT',co,msg
  elseif msg == "normal" then
      VM.receive()
  else
    if msg == "kill" then msg = "killed" end
    removeCo(RUNNING)
    propogateExit('EXIT',RUNNING,msg)
    --yield from the now dead process to stop execution
    coroutine.yield()
  end
end

function VM.process_flag(signal,value)
  VM.co2flags[RUNNING][signal]=value
end

--Coroutine has yielded an error.
local function catchError(msg)
  VM.log("ERROR in Coroutine "..RUNNING..": "..msg)
  removeCo(RUNNING)
  if VM.links[RUNNING] then
    propogateExit('EXIT',RUNNING,msg)
  end
end

--TODO queue resume till later?
--TODO rehash coroutine lists into single object?


local function init(fun)
  INDEX = INDEX + 1
  --VM.log("Spawning coroutine "..INDEX)
  VM.coroutines[INDEX]=coroutine.create(fun)
  VM.co2flags[INDEX]={}
  local co = INDEX
  return INDEX
end

function VM.spawn(fun)
  if not ("function" == type(fun)) then error("badarg: Not a function",2) end
  local co = init(fun)
  VM.resume(co)
  return co
end

function VM.spawnlink(fun)
  if not ("function" == type(fun)) then error("badarg: Not a function",2) end
  local co = init(fun)
  VM.link(co)
  VM.resume(co)
  return co
end

local function checkQueue()
  for i,e in ipairs(queue) do
    if e[1]=="EXIT" then
      if VM.co2flags[ROOT].trap_exit then
        VM.log('exception exit: '..e[3])
      else
        if e[3] == "normal" then
          break
        else
          VM.init()
          error("exception exit: "..e[3],4)
        end
      end
    else
      VM.log('Received unkown signal: '..e[1])
    end
  end
  queue = {}
end


function VM.exit(reason,co)
  if not co then
    removeCo(RUNNING)
    propogateExit('EXIT',RUNNING,reason)
  elseif reason == "normal" and not co == VM.running() then
    return
  elseif co == ROOT then
    queueExit('EXIT',co,reason)
    checkQueue()
  else
    VM.send(co,'EXIT',co,reason)
  end
end


--TODO change to private function?
function VM.resume(co,...)
  local parent = RUNNING
  RUNNING = co
  local thread = VM.coroutines[co]
  local ok, e = coroutine.resume(thread,unpack(arg))
  if not ok then
    VM.log(RUNNING.." died, returning to "..parent)
    catchError(e)
  elseif coroutine.status(thread)=="dead" then
    removeCo(RUNNING)
    propogateExit('EXIT',RUNNING,"normal")
  end
  RUNNING = parent
  if RUNNING == ROOT then
    checkQueue()
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



local function postYield(event,...)
    --TODO terminate bad behaviour?
    if event == "terminate" then
      removeCo(RUNNING)
      coroutine.yield()
    elseif event == "EXIT" then
      return receivedExit(arg[1],arg[2])
    else
      return event,unpack(arg)
    end
  end

function VM.receive()
  local co = RUNNING
  return postYield(coroutine.yield())
end

---------
--Tests--
---------

local test = function()
  local luaunit = require 'luaunit'
  
  function test_HashArray()
    local hash = {}
    local index = "index"
    local item = "item"
    local item2 = "item2"
    HashArrayInsert(hash,index,item)
    HashArrayInsert(hash,index,item2)
    luaunit.assertEquals(hash,{index={"item","item2"}})
    HashArrayRemoveValue(hash,index,item)
    luaunit.assertEquals(hash,{index={"item2"}})
    hash = {index={"item","item2"}}
    local list = {item="value",item2="value",item3="value"}
    purgeItemsFromHash(hash,index,list)
    luaunit.assertEquals(hash,{})
    luaunit.assertEquals(list,{item3="value"})
    hash = {item={item2="value",item3="value"},item2={item="value",item3="value"},item3={item="value",item2="value"}}
    purgeItemsFromHashArray(hash,item,hash)
    luaunit.assertEquals(hash,{item2={item3="value"},item3={item2="value"}})
  end
  
  os.exit(luaunit.LuaUnit.run())
end

--test()

return VM  