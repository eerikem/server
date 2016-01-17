local VM = require 'vm'

luaunit = require 'luaunit'

function test_spawn()
  local co = VM.spawn(function() coroutine.yield() end)
  luaunit.assert_equals(coroutine.status(co),"suspended")
  luaunit.assert_true(table.getn(VM.coroutines)>0)
end

function test_resume()
  local co = VM.spawn(function() coroutine.yield() end)
  VM.resume(co)
  luaunit.assert_equals(coroutine.status(co),"dead")
end

function test_send()
  local msg,msg2
  local co = VM.spawn(function() msg,msg2 = coroutine.yield() end)
  VM.send(co,"a message","second msg")
  luaunit.assert_equals(msg,"a message")
  luaunit.assert_equals(msg2,"second msg")
end

function test_receive()
  local msg = "a msg"
  local msg2
  local co = VM.spawn(function() msg2 = VM.receive() end)
  VM.send(co,msg)
  luaunit.assert_equals(msg,msg2)
end

os.exit(luaunit.LuaUnit.run())