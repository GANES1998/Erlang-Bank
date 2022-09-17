%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2022 2:17 pm
%%%-------------------------------------------------------------------
-module(bank_sup).
-author("ganesonravichandran").

%% API
-export([start/0, stop/0, init/0]).

%% These functions are like APIs and will be exposed by the module
start() ->
  io:format(".............Starting Bank Supervisor.............~n"),
  PID = spawn(?MODULE, init, []),
  register(?MODULE, PID).

stop() ->
  ?MODULE ! terminate.

init() ->
  process_flag(trap_exit, true),
  {ok, BankPid} = bank:start_link(),
  io:format("Bank process running in ~p ~n", [BankPid]),
  main_loop(BankPid).

main_loop(BankPid) ->
  receive
    {'EXIT', BankPid, _} ->
      io:format("Bank Running on ~p died ~n", [BankPid]),
%%      Terminated and needs to be restarted by the supervisor
      {ok, NewBankPid} = bank:start_link(),
      main_loop(NewBankPid);
    terminate ->
      bank:stop()
  end.

