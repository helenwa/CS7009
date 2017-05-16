# CS7009
Using Haskel + Github API + neo4J to crawl github for information about users and repos.
Using HAskel + Yesod + d3 to display info.
start neo4J

start yesod webpage;
cd yesodBlob
stack exec yesodBlob
you need to login at least once before starting the crawler or the rate limit will be deadful.

start crawler;
  cd crawler
  stack exec crawler-exe
  get request to;
  "localhost:8080/startC/username/hops"
  eg localhost:8080/startC/helenwa/4

access on
localhost:3000
