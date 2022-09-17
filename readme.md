## Erlang Based Bank.

Sample erlang bank program, which supports the following operations

1. Deposit - Deposit by providing account id and amount. 
   1. If account id is available adds the amount to existing balance.
   2. If account id is not available, creates a account, stores in the erlang module state with amount as initial balance

*usage*

```shell
>1 bank:deposit(1, 1000).
{ok}
```

2. Balance - Checks Balance using account id, returns the value
   1. Returns 0 if no such account exists in the system.
   2. Returns the value if such an account exist.

*usage*

```shell
>2 bank:balance(1).
{ok, 1000}
```

3. Withdraw - Withdraws balance from the accountId mentioned.
    1. If the given amount is greater than existing balance, It errors, saying in_sufficient_balance.
    2. If the given amount is lesser or equal to current balance, the current balance for the account will be updated.

*usage*

```shell
>3 bank:balance(1).
{ok, 1000}
>4 bank:withdraw(1, 700).
{ok, 300)
>5 bank:withdraw(1, 500).
{error, in_sufficien_funds}
```

The bank is started using a supervisor `bank_sup` which takes care of starting and stoping the bank including 
restarting it when it crashes.

**Note**
* Each module needs to be compiled before calling any functions in it.

```shell
~ erl
1> c(bank), c(bank_sup).
{ok, bank_sup}
```

* Before calling any bank operations on the bank module, we need to start the bank module.

```shell
1> bank_sup:start().
............Starting Supervisor.............
...............Starting bank................
```

**Example**

![](doc/assets/Sample%20Input%20Output%20With%20Supervisor.png)
