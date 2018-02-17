require 'json'
require 'socket'

server = ARGV[0] || "localhost"
sock = TCPSocket.new(server, 9999)
sock.write("サンプルAI\n")
myid = sock.gets
puts "自分のID: #{myid}"
loop do
  json = sock.gets
  break unless json
  puts "データ:"
  p JSON.parse(json)
  cmd = %w[UP DOWN LEFT RIGHT STAY].sample
  sock.puts(cmd)
end
