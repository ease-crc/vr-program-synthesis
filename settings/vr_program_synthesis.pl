%	Plugins
setting(knowrob:plugins, []).

%	Triple DB
setting(lang_db:drop_graphs, [user]). % erase "user" graph on start-up

%	Disable logging of TF messages
setting(tf:use_logger, false).
setting(tf:use_publisher, false).

%	Mongo DB name
setting(mng_client:db_name, roslog).
%	Flag for read only mongo databases
setting(mng_client:read_only, false).
%	A prefix used for all collection names.
setting(mng_client:collection_prefix, '').
