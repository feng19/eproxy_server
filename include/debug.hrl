
-define(DEBUG(Msg),io:format("[~p:~p] ~p~n",[?MODULE,?LINE,Msg])).

-define(DEBUG(Format,Args),io:format("[~p:~p] "++Format++"~n",[?MODULE,?LINE|Args])).