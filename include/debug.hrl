
-define(DEBUG(Msg),eproxy_server:debug(?MODULE,?LINE,Msg)).
-define(DEBUG(Format,Args),eproxy_server:debug(?MODULE,?LINE,Format,Args)).
