require 'json'
require 'open-uri'

cmcontinue = ""
members = []

begin
  while cmcontinue
    url = "http://bots.snpedia.com/api.php?format=json&action=query&list=categorymembers&cmtitle=Category:Is_a_snp&cmlimit=5000&cmcontinue=#{cmcontinue}"
    body = JSON.load(open(url))
    members << body["query"]["categorymembers"]
    cmcontinue = body["query-continue"]["categorymembers"]["cmcontinue"].gsub('|', "%7C")
  end
rescue Exception => e
  puts e
ensure
  rsids = members.inject({}) {|h,r| h[r["title"].downcase] = r["pageid"].to_s; h }
  File.write("rsids", JSON.pretty_generate(rsids))
end
