spec = Gem::Specification.new do |s| 
  s.name = "telegraph"
  s.version = "0.0.1"
  s.author = "Jonathan Palley"
  s.email = "jpalley@idapted.com"
  s.homepage = "http://gems.idapted.com/"
  s.platform = Gem::Platform::RUBY
  s.summary = "Lib for FreeSWITCH/Rails Interfacing"
  %w{data lib}.each{|folder|
    s.files += Dir["#{folder}/**/*"]
  }
  s.require_path = "lib"
  s.autorequire = "telegraph"
  s.test_files = Dir["{test}/**/*test.rb"]
  s.has_rdoc = false
  s.extra_rdoc_files = ["README"]
  s.add_dependency("idp_lib")
end

