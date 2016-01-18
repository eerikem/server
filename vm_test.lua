local VM = require 'vm'

luaunit = require 'luaunit'

function test_spawn()
  local co = VM.spawn(function() coroutine.yield() end)
  luaunit.assert_equals(VM.status(co),"suspended")
  luaunit.assert_true(VM.coroutines[co])
end

function test_resume()
  local co = VM.spawn(function() coroutine.yield() end)
  VM.resume(co)
  luaunit.assertErrorMsgContains("does not exist",VM.status,co)
end

function test_send()
  local msg,msg2
  local co = VM.spawn(function() msg,msg2 = coroutine.yield() end)
  VM.send(co,"a message","second msg")
  luaunit.assert_equals(msg,"a message")
  luaunit.assert_equals(msg2,"second msg")
  --send doesn't fail except for badarg
  luaunit.assertError(VM.send,{"badarg"},"msg")
  VM.send(-1,"msg")
  luaunit.assertErrorMsgContains("badarg",VM.send,"badname","msg")
end

function test_receive()
  local msg = "a msg"
  local msg2
  local co = VM.spawn(function() msg2 = VM.receive() end)
  VM.send(co,msg)
  luaunit.assert_equals(msg,msg2)
end

function test_terminate()
  local co = VM.spawn(function() VM.receive() end)
  VM.send(co,"terminate")
  luaunit.assertError(VM.status,co)
  --luaunit.assertError(VM.resume,co)
end

function test_register()
  local co = VM.spawn(function() VM.receive() end)
  VM.register("routine",co)
  luaunit.assertEquals(VM.registered(),{"routine"})
  VM.resume(co)
  luaunit.assertEquals(VM.registered(),{})
  luaunit.assertErrorMsgContains("badarg",VM.register,"routine",co)
  local deadCo = VM.spawn(function() end)
  luaunit.assertErrorMsgContains("badarg",VM.register,"imdead",deadCo)
end

function test_registered_co()
  local msg
  local co = VM.spawn(function() msg = VM.receive() end)
  local name = "name"
  VM.register(name,co)
  VM.send(name,"msg")
  luaunit.assertEquals(msg,"msg")
end

os.exit(luaunit.LuaUnit.run())