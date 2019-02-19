local VM = {}

local INDEX = 1
local MON_REF = 1
local ROOT = "ROOT"
local RUNNING = ROOT
local queue = {}
local STACK_DEPTH = 0
local MAX_DEPTH = 10
local ROTATOR = 1

function VM.init()
  INDEX = 1
  MON_REF = 1
  ROOT = "ROOT"
  RUNNING = ROOT
  VM.coroutines = {}
  VM.coroutines[ROOT]=ROOT
  VM.co2names = {}
  VM.co2flags = {}
  VM.co2flags[ROOT]={}
  VM.links = {}
  VM.monitors = {}
  VM.watching = {}
  VM.dead = {} 
  VM.log = function (msg) print(msg) end
  VM.mailbox = {}
  VM.mailbox[ROOT]={}
  VM.receiving = {}
  VM.resumes = {[ROOT]=0}
  VM.parents = {[ROOT]=ROOT}
  queue = {}
  STACK_DEPTH = 0
  ROTATOR = 1
end

VM.init()

function VM.self()
  return ROOT
end

function VM.running()
  return RUNNING
end

function VM.depth()
  return STACK_DEPTH
end

local function inc()
  STACK_DEPTH = STACK_DEPTH + 1
  if STACK_DEPTH > MAX_DEPTH then
    --VM.log("Warning: Stack Depth reached "..STACK_DEPTH) end
    error("Stack Depth exceeded "..STACK_DEPTH) end
end

local function dec()
  STACK_DEPTH = STACK_DEPTH - 1
end

function VM.status(co)
  if VM.dead[co] then return "dead" end
  if not VM.coroutines[co] then error("badarg: Coroutine "..tostring(co).." does not exist",2) end
  return coroutine.status(VM.coroutines[co])
end

---------------------
--Utility Functions--
---------------------

function HashArrayInsert(hash,key,item,pos)
  if not hash[key] then
    hash[key]={item}
  else
    if pos then
      table.insert(hash[key],pos,item)
    else
      table.insert(hash[key],item)
    end
  end
end

--Removes single list item from HashArray
function HashArrayRemoveValue(hash,key,item)
  for x,_item in pairs(hash[key]) do
    if _item == item then
      table.remove(hash[key],x)
      if table.getn(hash[key]) == 0 then
        hash[key] = nil
      end
      break
    end
  end
end

--takes a list from hash, clears another list
--of those items, then deletes the list
function purgeItemsFromHash(hash,index,list)
  for _, item in pairs(hash[index]) do
    list[item] = nil
  end
  hash[index] = nil
end

--similar to above but purges from HashArray
function purgeItemsFromHashArray(hash,index,list)
  for item, _ in pairs(hash[index]) do
    list[item][index]=nil
  end
  hash[index] = nil
end

function VM.exec(Module,Function,...)
  if not Module or not Function or not Module[Function] then
    error("Badarg",2)
  else
    return Module[Function](unpack(arg))
  end
end

------------
--Monitors--
------------

local function ref()
  MON_REF = MON_REF + 1
  return "#ref"..MON_REF
end

VM.ref = ref

function VM.monitor(Type,obj)
  if not type(Type) == "string" or not obj then error("badarg: "..luaunit.prettystr(obj,true),2) end
  if Type == "process" then
    local ref = ref()
    VM.monitors[ref]={target = obj, watching = VM.running()}
    HashArrayInsert(VM.monitors,obj,ref)
    HashArrayInsert(VM.watching,VM.running(),ref)
    return ref
  end
end

function VM.demonitor(ref,options)
  if not ref then error("badarg",2) end
  if VM.monitors[ref] then
    local mon = VM.monitors[ref]
    HashArrayRemoveValue(VM.monitors,mon.target,ref)
    HashArrayRemoveValue(VM.watching,mon.watching,ref)
    VM.monitors[ref]=nil
    return true
  end
  return false
end

--removes monitors created by co
local function removeMonitorsBy(co)
  for _,ref in ipairs(VM.watching[co]) do
    local mon = VM.monitors[ref]
    HashArrayRemoveValue(VM.monitors,mon.target,ref)
    VM.monitors[ref] = nil
  end
  VM.watching[co] = nil
end

local function notifyMonitors(co,reason)
  if VM.monitors[co] then
    for _,ref in ipairs(VM.monitors[co]) do
      VM.send(VM.monitors[ref].watching,'DOWN',ref,"process",co,reason)
    end
  end
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

--Overridden below
local function receivedExit(co,msg) end
--TODO fix for loop

--TODO bad shared state?! co's getting killed but VM.links still going?
local function propogateExit(signal,source,reason)
  while VM.links[source] do
    local co = VM.links[source][1]
    unregisterLink(co)
    --Special case when propogateExit already running in the first coroutine
    if co==coroutine.running() then
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

function VM.registered(name)
  if name then
    if VM.coroutines[name] then return true
    else return false end
  else
    local names = {}
    for key,_ in pairs(VM.coroutines) do
      if type(key)=="string" then table.insert(names,key) end
    end
    return names
  end
end

function VM.register(name,co)
  if VM.coroutines[name] then
    error("badarg: "..name.." already registered",2)
  elseif VM.status(co) == "dead" then
    error("badarg: Cannot register dead coroutine",2)
  elseif not VM.coroutines[co] then
    error("badarg: Cannot find coroutine")
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
  VM.mailbox[co]=nil  
  VM.dead[co] = VM.coroutines[co]
  VM.coroutines[co]=nil
  VM.resumes[co]=nil
  VM.parents[co]=nil
end

local function terminateRunning(co,msg)
    removeCo(RUNNING)
    propogateExit('EXIT',RUNNING,msg)
    notifyMonitors(RUNNING,msg)
    --yield from the now dead process to stop execution
    return coroutine.yield()
end

receivedExit = function (co,msg)
  if msg == "kill" then
    msg = "killed"
    return terminateRunning(co,msg)
  elseif VM.co2flags[RUNNING].trap_exit then
    return 'EXIT',co,msg
  elseif msg == "normal" then
    return VM.receive()
  else
    return terminateRunning(co,msg)
  end
end

function VM.process_flag(signal,value)
  VM.co2flags[RUNNING][signal]=value
end

--Coroutine has yielded a new error.
local function catchError(msg)
  VM.log("ERROR in Coroutine: "..msg)
  removeCo(RUNNING)
  if VM.links[RUNNING] then
    propogateExit('EXIT',RUNNING,msg)
    return notifyMonitors(RUNNING,msg)
  end
end

--TODO queue resume till later?
--TODO rehash coroutine lists into single object?


local function init(fun)
  INDEX = INDEX + 1
  local co = coroutine.create(fun)
  VM.coroutines[co]=co
  VM.co2flags[co]={}
  VM.mailbox[co]={}
  VM.resumes[co]=0
  VM.parents[co]=VM.running()
  return co
end

--Initialize coroutine but don't execute initial resume
function VM.queue(fun)
  return init(fun)
end

--Initialize and link coroutine but don't execute initial resume
function VM.queueLink(fun)
  local co = init(fun)
  VM.link(co)
  return co
end

local function catchInitFail(co)
  local ok, reason = VM.resume(co)
  if not ok then
    error(reason)
  else
    return co
  end
end

--TODO queue resumes in spawn function for VM to run just before yielding to ROOT 
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

function VM.spawn_monitor(fun)
  if not ("function"==type(fun)) then error("badarg: Not a function",2) end
  local co = init(fun)
  local ref = VM.monitor("process",co)
  VM.resume(co)
  return co,ref
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
          VM.init()--TODO restart VM?!?
          error("exception exit: "..e[3],5)
        end
      end
    else
      VM.log('Received unkown signal: '..e[1])
    end
  end
  queue = {}
end

---
-- Terminate a thread with a given reason that will be propogated to any listening monitors.
-- @param #string reason "normal" if termination is expected, anything else otherwise.
-- @param #thread co Optionally specify a coroutine to terminate. Stops the running thread if omitted.
function VM.exit(reason,co)
  if not co then
    removeCo(RUNNING)
    propogateExit('EXIT',RUNNING,reason)
    notifyMonitors(RUNNING,reason)
    return coroutine.yield()
  elseif co == ROOT then
    queueExit('EXIT',co,reason)
    return checkQueue()
  else
    return VM.send(co,'EXIT',co,reason)
  end
end


--TODO change to private function?
function VM.resume(co,...)
  local parent = RUNNING
  RUNNING = co
  local thread = VM.coroutines[co]
  VM.resumes[thread]=VM.resumes[thread]+1
  inc()
  if(type(thread)=="string")then error("bad call",4) end
  local ok, e = coroutine.resume(thread,...)
  dec()
  if not ok then
    VM.log(tostring(co).." died, returning to parent: "..tostring(parent))
    catchError(e)
  elseif coroutine.status(thread)=="dead" then
    removeCo(RUNNING)
    propogateExit('EXIT',RUNNING,"normal")
    notifyMonitors(RUNNING,"normal")
  end
  RUNNING = parent
  if RUNNING == ROOT then
    VM.resumes[ROOT] = VM.resumes[ROOT] + 1
    checkQueue()
  end
  return ok, e
end

--TODO Optimize looping search for ready Coroutines.
local function getReadyCo()
  local co = nil
  for n,Co in ipairs(VM.receiving) do
    if Co ~= ROOT then--Cannot resume Root, causes circular resume
      if not VM.coroutines[Co] then --purge dead routines
        table.remove(VM.receiving,n)
      elseif next(VM.mailbox[Co]) then  --fetch coroutine
        return table.remove(VM.receiving,n)
      end
    end
  end
  return nil
end

--Flush all messages queued for ROOT process
function VM.flush()
  local t ={}
  while next(VM.mailbox[ROOT]) do
    table.insert(t,table.remove(VM.mailbox[ROOT],1)) end
  return t
end

--loop until all Receiving coroutines with msgs read have executed
local function flush()
  local co = getReadyCo()
  if co then VM.resume(co,unpack(table.remove(VM.mailbox[co],1)))
    return flush()
  end
--  elseif next(VM.mailbox[ROOT]) then
--    return
--  end
end

VM.flushAll = flush

function VM.send(co,...)
  if type(co) == "string" then
    if not VM.coroutines[co] then
      error("badarg: "..co.." not a registered coroutine",2)
    else
      co = VM.coroutines[co]
    end
  elseif not (type(co) == "thread") then error("VM.send got badarg for co: "..type(co),2)
  elseif not arg then error("badarg: cannot send nil",3) end
  if VM.coroutines[co] then
    HashArrayInsert(VM.mailbox,co,arg)
    --VM.resume(co,...)
    return flush()
  end
end



local function postYield(event,...)
  --TODO terminate bad behaviour?
  if event == "terminate" then
    removeCo(RUNNING)
    return coroutine.yield()
  elseif event == "EXIT" then
    return receivedExit(arg[1],arg[2])
  else
    return event,unpack(arg)
  end
end

--TODO implement Timeout behaviour
function VM.receive()
  if not VM.mailbox[RUNNING] then
    error("Calling receive in dead coroutine: "..tostring(RUNNING),2) end
  if next(VM.mailbox[RUNNING]) then
    return postYield(unpack(table.remove(VM.mailbox[RUNNING],1)))
  end
  
  table.insert(VM.receiving,RUNNING)
  if RUNNING == ROOT then
    if next(VM.mailbox[ROOT]) then
      return postYield(unpack(table.remove(VM.mailbox[ROOT],1)))
    else return end
  end
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