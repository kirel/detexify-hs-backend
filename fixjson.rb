require 'json'
require 'base64'

json = JSON.load(File.open('snapshot.bak.json'))
newjson = json.inject({}) do |h,(k,v)|
  h.merge({
    Base64.decode64(k).gsub('_', "\\") => v
  })
end

puts newjson.keys

File.open('snapshot.json', 'w+') { |f| f.write JSON.dump(newjson) }

