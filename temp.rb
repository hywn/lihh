#!/usr/bin/env ruby

print ("repl \"" + ARGF.read.gsub(/--.+/, '').gsub("\\", "\\\\\\").gsub(/\n+/, ' ') + "\"")