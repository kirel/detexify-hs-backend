#!/bin/env ruby

require 'ffi'
require 'pp'
require 'json'

module Test
  extend FFI::Library
  ffi_lib 'dist/build/libdetexify.so/libdetexify.so'

  attach_function :hs_init, [:pointer, :pointer], :void
  attach_function :hs_exit, [], :void
  attach_function :init_detexify, [:string], :void

  def self.make_fun(fun_sym)
    attach_function fun_sym, [:string], :string

    proc { |arg|
      pp arg
      JSON.load(method(fun_sym).call(JSON.dump(arg)))
    }
  end
end

classify = Test.make_fun(:classify_export)

Test.hs_init(nil, nil)

puts 'init', Time.now
Test.init_detexify 'snapshot.json'
puts 'classify', Time.now
res = classify.call({strokes:[[{x:1,y:1}]]})
puts 'classify', Time.now
pp res
# puts 'Call2'
# pp classify.call [[[2,2], [1,1]]]

Test.hs_exit
