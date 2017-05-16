# CS7009
Using Haskel + Github API + neo4J to crawl github for information about users and repos.
Using HAskel + Yesod + d3 to display info.
start neo4J

start yesod webpage;
cd yesodBlob
stack exec yesodBlob

start crawler;
  cd crawler
  stack exec crawler-exe
  access webpage on localhost:3000
  login using github at this point.
  get request to;
  "localhost:8080/startC/username/hops"
  eg localhost:8080/startC/helenwa/4

