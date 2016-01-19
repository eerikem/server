local VM = {}

local INDEX = 1
local RUNNING = 0
VM.coroutines = {}
VM.co2names = {}
VM.links = {}

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
  HashArrayInsert(VM.links,co,RUNNING)
  HashArrayInsert(VM.links,RUNNING,co)
end

local function unregisterLinks(co)
  purgeItemsFromHashArray(VM.links,co,VM.links)
end

local function unregisterLink(co)
  HashArrayRemoveValue(VM.links,RUNNING,co)
  HashArrayRemoveValue(VM.links,co,RUNNING)
end

local function propogate(e)
  print("Propogating error")
  for _,co in ipairs(VM.links[RUNNING]) do
    unregisterLink(co)
    VM.send(co,"error",e)
  end
end

function VM.link(co)
  if not VM.coroutines[co] then
    error("badarg: "..co.." is not a registered coroutine",2)
  end
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
  --for k,v in pairs(VM.coroutines) do print(k,v) end
  if VM.co2names[co] then
    unregisterNames(co) end
  VM.coroutines[co]=nil
end

local function kill(co)
  removeCo(co)
  coroutine.yield()
end

local function receivedError(msg)
  print("ERROR in Coroutine "..RUNNING..": "..msg)
  if VM.links[RUNNING] then
    propogate(msg)
  end
  kill(RUNNING)
end

--TODO queue resume till later?
function VM.spawn(fun)
  INDEX = INDEX + 1
  VM.coroutines[INDEX]=coroutine.create(fun)
  local co = INDEX
  VM.resume(INDEX)
  return co
end

--TODO change to private function?
function VM.resume(co,...)
  local parent = RUNNING
  RUNNING = co
  local thread = VM.coroutines[co]
  local ok, e = coroutine.resume(thread,unpack(arg))
  if not ok then
    receivedError(e)
    --io.stdin:read'*l'
  elseif coroutine.status(thread)=="dead" then
    removeCo(co)
  end
  RUNNING = parent
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

local function postYield(co,event,...)
  RUNNING = co
    if event == "terminate" then
      kill(RUNNING)
    elseif event == "error" then
      receivedError(arg[1])
    else
      return event,unpack(arg)
    end
  end

function VM.receive()
  local co = RUNNING
  return postYield(co,coroutine.yield())
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