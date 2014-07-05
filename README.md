erlastica
=========

Erlang library to use ElasticSearch. This is a work in progress.

How to use it
=============
 * Start the main supervisor [erlastica_sup](https://github.com/marcelog/erlastica/blob/master/src/egetter_sup.erl) from your own supervisor
tree. Or application:start(erlastica).

 * Configure the application, here's a sample:
```
{erlastica, [
  {ibrowse_options, []},
  {indices, [
    {index_1, [         % Index name, will map to a given ES name and host/port.
      {index, "index"}, % ElasticSearch index name
      {host, elastic_1} % Host mapping
    ]}
  ]},
  {hosts, [
    {elastic_1, [
      {host, "127.0.0.1"},
      {port, 9200}
    ]}
  ]}
]}
```

In this example we are setting up a fantasy name for an index (index_1) that will 
map to a specific ElasticSearch index name and host.

The main entry point for the librar is the module [erlastica](https://github.com/marcelog/erlastica/blob/master/src/erlastica.erl).
