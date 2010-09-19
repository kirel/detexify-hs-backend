task :default => :compile

task :compile do
  sh "ghc --make -O2 -threaded Webserver"
  rm Dir.glob("**/*.{hi,o}")
end
