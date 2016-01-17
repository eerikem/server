local VM = {}

VM.coroutines = {}

function VM.spawn(fun)
  local co = coroutine.create(fun)
  table.insert(VM.coroutines,co)
  VM.resume(co)
  return co
end

function VM.resume(co)
  coroutine.resume(co)
end

function VM.send(co,...)
  coroutine.resume(co,unpack(arg))
end

function VM.receive()
  return coroutine.yield()
end

return VM  