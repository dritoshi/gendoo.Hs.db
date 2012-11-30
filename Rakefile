desc "Build the package"
task :build do
  sh "R CMD build gendoo.Hs.db"
end

desc "Install the package"
task :install do
  sh "sudo R CMD INSTALL gendoo.Hs.db_0.99.0.tar.gz"
end