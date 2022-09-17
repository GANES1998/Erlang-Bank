%%%-------------------------------------------------------------------
%%% @author ganesonravichandran
%%% @copyright (C) 2022, UFL
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2022 11:27 am
%%%-------------------------------------------------------------------
-module(bank).
-author("ganesonravichandran").

%% API
-export([start/0, stop/0, deposit/2, balance/1, withdraw/2, init/0]).
-record(state, {
  accounts
}).


%%APIs and will be exposed
start() ->
  io:format(".............Starting Bank............."),
  BankProcessPid = spawn(?MODULE, init, []),
  register(?MODULE, BankProcessPid).

stop() ->
  ?MODULE ! terminate.

deposit(AccountId, Amount) ->
  ?MODULE ! {deposit, self(), AccountId, Amount},
  receive
    Reply -> Reply
    after 5000 -> {error, timeout}
  end.

balance(AccountId) ->
  ?MODULE ! {balance, self(), AccountId},
  receive
    Reply -> Reply
    after 5000 -> {error, timeout}
  end.

withdraw(AccountId, Amount) ->
  ?MODULE ! {withdraw, self(), AccountId, Amount},
  receive
    Reply -> Reply
    after 5000 -> {error, timeout}
  end.


%%Functions called only by this module and are not exposed

init() ->
  State = #state{accounts = dict:new()},
  main_loop(State).

get_current_balance(AccountId, Accounts) ->
  case dict:find(AccountId, Accounts) of
    error -> 0;
    {ok, Value} -> Value
  end.

main_loop(#state{accounts = Accounts} = State) ->
  receive
    terminate ->
      io:format("..........Closing Bank..........");
    {deposit, CallerPid, AccountId, Amount} ->
      CurrentBalance = get_current_balance(AccountId, Accounts),
      NewBalance = CurrentBalance + Amount,
      NewState = #state{accounts = dict:store(AccountId, NewBalance, Accounts)},
      CallerPid ! ok,
      main_loop(NewState);
    {balance, CallerPid, AccountId} ->
      CurrentBalance = get_current_balance(AccountId, Accounts),
      CallerPid ! {ok, CurrentBalance},
      main_loop(State);
    {withdraw, CallerPid, AccountId, Amount} ->
      CurrentBalance = get_current_balance(AccountId, Accounts),
      case CurrentBalance >= Amount of
        true ->
          NewState = #state{accounts = dict:store(AccountId, CurrentBalance - Amount, Accounts)},
          CallerPid ! {ok, CurrentBalance - Amount},
          main_loop(NewState);
        false ->
          CallerPid ! {error, insufficient_funds},
          main_loop(State)
      end
  end.



