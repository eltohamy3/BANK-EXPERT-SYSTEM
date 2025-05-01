% ----------------------------
% BANK EXPERT SYSTEM CORE
% ----------------------------

:- dynamic customer/4.          % ID, Name, ContactInfo, KYC
:- dynamic account/4.           % AccountID, CustomerID, Type, Balance
:- dynamic transaction/6.       % TransactionID, FromAccount, ToAccount, Amount, Timestamp, Description
:- dynamic user/3.              % Username, Password, Roles
:- dynamic current_user/2.      % Username, Roles

% Main entry point
start_bank_expert :-
    write('======= BANK EXPERT SYSTEM ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    initialize_system,
    authenticate_user.

% Initialize with sample data
initialize_system :-
    retractall(customer(_,_,_,_)),
    retractall(account(_,_,_,_)),
    retractall(transaction(_,_,_,_,_,_)),
    retractall(user(_,_,_)),
    retractall(current_user(_,_)),
    
    % Sample customers
    assert(customer('cust001', 'John Doe', '555-1234', verified)),
    assert(customer('cust002', 'Jane Smith', '555-5678', verified)),
    assert(customer('cust003', 'Bob Johnson', '555-9012', unverified)),
    
    % Sample accounts (customers can have multiple accounts)
    assert(account('acct001', 'cust001', 'checking', 5000.0)),
    assert(account('acct002', 'cust001', 'savings', 15000.0)),
    assert(account('acct003', 'cust002', 'checking', 2500.0)),
    assert(account('acct004', 'cust002', 'savings', 10000.0)),
    assert(account('acct005', 'cust003', 'checking', 1000.0)),
    
    % Sample users
    assert(user(admin, 'bank', [admin])),
    assert(user(teller1, 'teller123', [teller])),
    assert(user(manager1, 'mgr456', [manager])).

% Authentication system
authenticate_user :-
    write('Please authenticate'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Username: '), read(Username),
    write('Password: '), read(Password),
    (user(Username, Password, Roles) -> 
        asserta(current_user(Username, Roles)),
        show_main_menu(Username)
    ;
        write('Invalid credentials. Try again.'), nl,nl,
        write('*****************************************************'), nl,nl,
        authenticate_user
    ).

% Main menu
show_main_menu(Username) :-
    nl,nl, write('======= MAIN MENU ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Logged in as: '), write(Username), nl,nl, nl,nl,
    write('1. Customer Management'), nl,nl,
    write('2. Account Management'), nl,nl,
    write('3. Transaction Processing'), nl,nl,
    write('4. Reports & Analytics'), nl,nl,
    write('0. Logout'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Select option (0-4): '),
    read(Choice),
    handle_service(Choice, Username).

% Service routing
handle_service(0, _) :- 
    write('Logging out...'), nl,nl, nl,nl,
    write('*****************************************************'), nl,nl,
    retractall(current_user(_, _)),
    start_bank_expert.

handle_service(1, User) :- customer_management(User).
handle_service(2, User) :- account_management(User).
handle_service(3, User) :- transaction_processing(User).
handle_service(4, User) :- reports_analytics(User).
handle_service(_, User) :- 
    write('Invalid choice'), nl,nl, 
    write('*****************************************************'), nl,nl,
    show_main_menu(User).

% ----------------------------
% CUSTOMER MANAGEMENT MODULE
% ----------------------------

customer_management(User) :-
    check_access(User, [admin,manager]),
    nl,nl, write('======= CUSTOMER MANAGEMENT ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('1. Add Customer'), nl,nl,
    write('2. View All Customers'), nl,nl,
    write('3. Search Customers'), nl,nl,
    write('4. Update Customer'), nl,nl,
    write('5. Delete Customer'), nl,nl,
    write('6. Back to Main Menu'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 6 -> show_main_menu(User) ; handle_customer_choice(Choice, User)).

handle_customer_choice(1, User) :- add_customer, customer_management(User).
handle_customer_choice(2, User) :- view_all_customers, customer_management(User).
handle_customer_choice(3, User) :- search_customers, customer_management(User).
handle_customer_choice(4, User) :- update_customer, customer_management(User).
handle_customer_choice(5, User) :- delete_customer, customer_management(User).
handle_customer_choice(_, User) :- 
    write('Invalid choice'), nl,nl, 
    write('*****************************************************'), nl,nl,
    customer_management(User).

add_customer :-
    write('Enter Customer ID: '), read(ID),
    (customer(ID, _, _, _) ->
        write('Error: Customer ID already exists!'), nl,nl,
        write('*****************************************************'), nl
    ;
        write('Full Name: '), read(Name),
        write('Contact Info: '), read(Contact),
        write('KYC Status (verified/unverified): '), read(KYC),
        assert(customer(ID, Name, Contact, KYC)),
        write('Customer added successfully!'), nl,nl,
        write('*****************************************************'), nl
    ).

view_all_customers :-
    nl,nl, write('======= ALL CUSTOMERS ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    findall(customer(ID, Name, Contact, KYC),
           customer(ID, Name, Contact, KYC),
           Customers),
    (Customers = [] -> 
        write('No customers found.'), nl,nl,
        write('*****************************************************'), nl
    ;
        print_customers(Customers),
        length(Customers, Count),
        nl,nl, write('Total customers: '), write(Count), nl,nl,
        write('*****************************************************'), nl
    ).

print_customers([]).
print_customers([customer(ID, Name, Contact, KYC)|Rest]) :-
    nl,nl, write('ID: '), write(ID), nl,nl,
    write('Name: '), write(Name), nl,nl,
    write('Contact: '), write(Contact), nl,nl,
    write('KYC Status: '), write(KYC), nl,nl,
    % Show customer's accounts
    findall(account(AccID, Type, Balance),
            (account(AccID, ID, Type, Balance)),
            Accounts),
    (Accounts = [] ->
        write('Accounts: None'), nl
    ;
        write('Accounts:'), nl,nl,
        print_accounts(Accounts)
    ),
    write('*****************************************************'), nl,nl,
    print_customers(Rest).

print_accounts([]).
print_accounts([account(AccID, Type, Balance)|Rest]) :-
    write('  - '), write(AccID), write(' ('), write(Type), 
    write('): $'), format('~2f', [Balance]), nl,nl,
    print_accounts(Rest).

search_customers :-
    write('Enter search term (ID or Name): '), read(Term),
    findall(customer(ID, Name, Contact, KYC),
           (customer(ID, Name, Contact, KYC),
            (sub_atom(ID, _, _, _, Term) ; sub_atom(Name, _, _, _, Term))),
           Customers),
    (Customers = [] -> 
        write('No matching customers found.'), nl,nl,
        write('*****************************************************'), nl
    ;
        nl,nl, write('======= SEARCH RESULTS ======='), nl,nl,
        write('*****************************************************'), nl,nl,
        print_customers(Customers)
    ).

update_customer :-
    write('Enter Customer ID to update: '), read(ID),
    (customer(ID, Name, Contact, KYC) ->
        nl,nl, write('Current details:'), nl,nl,
        write('*****************************************************'), nl,nl,
        write('1. Name: '), write(Name), nl,nl,
        write('2. Contact: '), write(Contact), nl,nl,
        write('3. KYC Status: '), write(KYC), nl,nl,
        write('*****************************************************'), nl,nl,
        write('Select field to update (1-3, 0 to cancel): '), read(Choice),
        (Choice =:= 0 -> 
            write('Update canceled.'), nl,nl,
            write('*****************************************************'), nl
        ; Choice =:= 1 ->
            write('New Name: '), read(NewName),
            retract(customer(ID, _, Contact, KYC)),
            assert(customer(ID, NewName, Contact, KYC)),
            write('Name updated.'), nl,nl,
            write('*****************************************************'), nl
        ; Choice =:= 2 ->
            write('New Contact: '), read(NewContact),
            retract(customer(ID, Name, _, KYC)),
            assert(customer(ID, Name, NewContact, KYC)),
            write('Contact updated.'), nl,nl,
            write('*****************************************************'), nl
        ; Choice =:= 3 ->
            write('New KYC Status: '), read(NewKYC),
            retract(customer(ID, Name, Contact, _)),
            assert(customer(ID, Name, Contact, NewKYC)),
            write('KYC status updated.'), nl,nl,
            write('*****************************************************'), nl
        ;
            write('Invalid choice.'), nl,nl,
            write('*****************************************************'), nl
        )
    ;
        write('Customer not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

delete_customer :-
    write('Enter Customer ID to delete: '), read(ID),
    (customer(ID, _, _, _) ->
        % First delete all accounts
        findall(AccID, account(AccID, ID, _, _), Accounts),
        foreach(member(Acc, Accounts), retract(account(Acc, ID, _, _))),
        % Then delete customer
        retract(customer(ID, _, _, _)),
        write('Customer and all associated accounts deleted.'), nl,nl,
        write('*****************************************************'), nl
    ;
        write('Customer not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

% ----------------------------
% ACCOUNT MANAGEMENT MODULE
% ----------------------------

account_management(User) :-
    check_access(User, [admin,manager,teller]),
    nl,nl, write('======= ACCOUNT MANAGEMENT ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('1. Open New Account'), nl,nl,
    write('2. Close Account'), nl,nl,
    write('3. View Customer Accounts'), nl,nl,
    write('4. View All Accounts'), nl,nl,
    write('5. Back to Main Menu'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 5 -> show_main_menu(User) ; handle_account_choice(Choice, User)).

handle_account_choice(1, User) :- open_account, account_management(User).
handle_account_choice(2, User) :- close_account, account_management(User).
handle_account_choice(3, User) :- view_customer_accounts, account_management(User).
handle_account_choice(4, User) :- view_all_accounts, account_management(User).
handle_account_choice(_, User) :- 
    write('Invalid choice'), nl,nl, 
    write('*****************************************************'), nl,nl,
    account_management(User).

open_account :-
    write('Customer ID: '), read(CustID),
    (customer(CustID, _, _, _) ->
        generate_account_id(AccID),
        write('Account Type (checking/savings): '), read(Type),
        write('Initial Deposit: '), read(Amount),
        (Amount >= 0 ->
            assert(account(AccID, CustID, Type, Amount)),
            write('Account '), write(AccID), 
            write(' opened successfully with $'), 
            format('~2f', [Amount]), nl,nl,
            write('*****************************************************'), nl
        ;
            write('Initial deposit must be non-negative.'), nl,nl,
            write('*****************************************************'), nl
        )
    ;
        write('Customer not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

generate_account_id(AccID) :-
    findall(Num, account(Acc, _, _, _), Accounts),
    length(Accounts, Count),
    NextNum is Count + 1,
    format(atom(AccID), 'acct~d', [NextNum]).

close_account :-
    write('Account ID to close: '), read(AccID),
    (account(AccID, CustID, _, Balance) ->
        (Balance =:= 0.0 ->
            retract(account(AccID, CustID, _, _)),
            write('Account closed successfully.'), nl,nl,
            write('*****************************************************'), nl
        ;
            write('Cannot close account with non-zero balance ($'),
            format('~2f', [Balance]), write(').'), nl,nl,
            write('*****************************************************'), nl
        )
    ;
        write('Account not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

view_customer_accounts :-
    write('Customer ID: '), read(CustID),
    (customer(CustID, Name, _, _) ->
        findall(account(AccID, Type, Balance),
                account(AccID, CustID, Type, Balance),
                Accounts),
        (Accounts = [] ->
            write('No accounts found for '), write(Name), nl,nl,
            write('*****************************************************'), nl
        ;
            nl,nl, write('======= ACCOUNTS FOR '), write(Name), write(' ======='), nl,nl,
            write('*****************************************************'), nl,nl,
            print_account_balances(Accounts)
        )
    ;
        write('Customer not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

view_all_accounts :-
    findall(account(AccID, CustID, Type, Balance),
            account(AccID, CustID, Type, Balance),
            Accounts),
    (Accounts = [] ->
        write('No accounts in system.'), nl,nl,
        write('*****************************************************'), nl
    ;
        nl,nl, write('======= ALL ACCOUNTS ======='), nl,nl,
        write('*****************************************************'), nl,nl,
        print_all_accounts(Accounts)
    ).

print_account_balances([]).
print_account_balances([account(AccID, Type, Balance)|Rest]) :-
    write('Account: '), write(AccID), 
    write(' ('), write(Type), write(')'), nl,nl,
    write('Balance: $'), format('~2f', [Balance]), nl,nl, nl,nl,
    write('*****************************************************'), nl,nl,
    print_account_balances(Rest).

print_all_accounts([]).
print_all_accounts([account(AccID, CustID, Type, Balance)|Rest]) :-
    customer(CustID, Name, _, _),
    write('Account: '), write(AccID), nl,nl,
    write('Customer: '), write(Name), write(' ('), write(CustID), write(')'), nl,nl,
    write('Type: '), write(Type), nl,nl,
    write('Balance: $'), format('~2f', [Balance]), nl,nl, nl,nl,
    write('*****************************************************'), nl,nl,
    print_all_accounts(Rest).

% ----------------------------
% TRANSACTION PROCESSING MODULE
% ----------------------------

transaction_processing(User) :-
    check_access(User, [admin,manager,teller]),
    nl,nl, write('======= TRANSACTION PROCESSING ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('1. Deposit'), nl,nl,
    write('2. Withdrawal'), nl,nl,
    write('3. Transfer Between Accounts'), nl,nl,
    write('4. View Transaction History'), nl,nl,
    write('5. Back to Main Menu'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 5 -> show_main_menu(User) ; handle_transaction_choice(Choice, User)).

handle_transaction_choice(1, User) :- process_deposit, transaction_processing(User).
handle_transaction_choice(2, User) :- process_withdrawal, transaction_processing(User).
handle_transaction_choice(3, User) :- process_transfer, transaction_processing(User).
handle_transaction_choice(4, User) :- view_transaction_history, transaction_processing(User).
handle_transaction_choice(_, User) :- 
    write('Invalid choice'), nl,nl, 
    write('*****************************************************'), nl,nl,
    transaction_processing(User).

process_deposit :-
    write('Account ID: '), read(AccID),
    (account(AccID, CustID, Type, Balance) ->
        write('Amount to deposit: '), read(Amount),
        (Amount > 0 ->
            NewBalance is Balance + Amount,
            retract(account(AccID, CustID, Type, _)),
            assert(account(AccID, CustID, Type, NewBalance)),
            generate_transaction_id(TxnID),
            get_time(TimeStamp),
            assert(transaction(TxnID, AccID, 'deposit', Amount, TimeStamp, 'Deposit to account')),
            write('Deposit successful. New balance: $'), 
            format('~2f', [NewBalance]), nl,nl,
            write('*****************************************************'), nl
        ;
            write('Deposit amount must be positive.'), nl,nl,
            write('*****************************************************'), nl
        )
    ;
        write('Account not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

process_withdrawal :-
    write('Account ID: '), read(AccID),
    (account(AccID, CustID, Type, Balance) ->
        write('Amount to withdraw: '), read(Amount),
        (Amount > 0 ->
            (Balance >= Amount ->
                NewBalance is Balance - Amount,
                retract(account(AccID, CustID, Type, _)),
                assert(account(AccID, CustID, Type, NewBalance)),
                generate_transaction_id(TxnID),
                get_time(TimeStamp),
                assert(transaction(TxnID, AccID, 'withdrawal', Amount, TimeStamp, 'Withdrawal from account')),
                write('Withdrawal successful. New balance: $'), 
                format('~2f', [NewBalance]), nl,nl,
                write('*****************************************************'), nl
            ;
                write('Insufficient funds. Current balance: $'),
                format('~2f', [Balance]), nl,nl,
                write('*****************************************************'), nl
            )
        ;
            write('Withdrawal amount must be positive.'), nl,nl,
            write('*****************************************************'), nl
        )
    ;
        write('Account not found!'), nl,nl,
        write('*****************************************************'), nl
    ).

process_transfer :-
    write('From Account ID: '), read(FromAcc),
    write('To Account ID: '), read(ToAcc),
    (FromAcc == ToAcc ->
        write('Cannot transfer to the same account!'), nl,nl,
        write('*****************************************************'), nl
    ;
        (account(FromAcc, FromCust, FromType, FromBalance),
         account(ToAcc, ToCust, ToType, ToBalance) ->
            write('Amount to transfer: '), read(Amount),
            (Amount > 0 ->
                (FromBalance >= Amount ->
                    % Update source account
                    NewFromBalance is FromBalance - Amount,
                    retract(account(FromAcc, FromCust, FromType, _)),
                    assert(account(FromAcc, FromCust, FromType, NewFromBalance)),
                    
                    % Update destination account
                    NewToBalance is ToBalance + Amount,
                    retract(account(ToAcc, ToCust, ToType, _)),
                    assert(account(ToAcc, ToCust, ToType, NewToBalance)),
                    
                    % Record transactions
                    generate_transaction_id(TxnID),
                    get_time(TimeStamp),
                    assert(transaction(TxnID, FromAcc, ToAcc, Amount, TimeStamp, 'Account transfer')),
                    
                    write('Transfer successful!'), nl,nl,
                    write('From Account: '), write(FromAcc), 
                    write(' | New Balance: $'), format('~2f', [NewFromBalance]), nl,nl,
                    write('To Account: '), write(ToAcc), 
                    write(' | New Balance: $'), format('~2f', [NewToBalance]), nl,nl,
                    write('*****************************************************'), nl
                ;
                    write('Insufficient funds in source account.'), nl,nl,
                    write('Available balance: $'), format('~2f', [FromBalance]), nl,nl,
                    write('*****************************************************'), nl
                )
            ;
                write('Transfer amount must be positive.'), nl,nl,
                write('*****************************************************'), nl
            )
        ;
            write('One or both accounts not found!'), nl,nl,
            write('*****************************************************'), nl
        )
    ).

% Fixed transaction ID generation
generate_transaction_id(TxnID) :-
    findall(Num, transaction(Txn, _, _, _, _, _), Transactions),
    length(Transactions, Count),
    NextNum is Count + 1,
    format(atom(TxnID), 'txn~d', [NextNum]).

view_transaction_history :-
    nl,nl, write('======= TRANSACTION HISTORY ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('1. View all transactions'), nl,nl,
    write('2. View transactions for account'), nl,nl,
    write('3. View transactions for customer'), nl,nl,
    write('4. Back to Transaction Menu'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 1 -> show_all_transactions
    ; Choice =:= 2 -> show_account_transactions
    ; Choice =:= 3 -> show_customer_transactions
    ; Choice =:= 4 -> true
    ; write('Invalid choice'), nl,nl, 
      write('*****************************************************'), nl,nl,
      view_transaction_history
    ).

show_all_transactions :-
    findall(transaction(TxnID, From, To, Amount, Time, Desc),
            transaction(TxnID, From, To, Amount, Time, Desc),
            Transactions),
    (Transactions = [] ->
        write('No transactions found.'), nl,nl,
        write('*****************************************************'), nl
    ;
        nl,nl, write('======= ALL TRANSACTIONS ======='), nl,nl,
        write('*****************************************************'), nl,nl,
        print_transactions(Transactions)
    ).

show_account_transactions :-
    write('Account ID: '), read(AccID),
    findall(transaction(TxnID, From, To, Amount, Time, Desc),
            (transaction(TxnID, From, To, Amount, Time, Desc),
             (From = AccID ; To = AccID)),
            Transactions),
    (Transactions = [] ->
        write('No transactions found for account '), write(AccID), nl,nl,
        write('*****************************************************'), nl
    ;
        nl,nl, write('======= TRANSACTIONS FOR ACCOUNT '), write(AccID), write(' ======='), nl,nl,
        write('*****************************************************'), nl,nl,
        print_transactions(Transactions)
    ).

show_customer_transactions :-
    write('Customer ID: '), read(CustID),
    findall(transaction(TxnID, From, To, Amount, Time, Desc),
            (account(From, CustID, _, _),
             transaction(TxnID, From, To, Amount, Time, Desc)),
            Outgoing),
    findall(transaction(TxnID, From, To, Amount, Time, Desc),
            (account(To, CustID, _, _),
             transaction(TxnID, From, To, Amount, Time, Desc)),
            Incoming),
    append(Outgoing, Incoming, Transactions),
    (Transactions = [] ->
        write('No transactions found for customer '), write(CustID), nl,nl,
        write('*****************************************************'), nl
    ;
        nl,nl, write('======= TRANSACTIONS FOR CUSTOMER '), write(CustID), write(' ======='), nl,nl,
        write('*****************************************************'), nl,nl,
        print_transactions(Transactions)
    ).

print_transactions([]).
print_transactions([transaction(TxnID, From, To, Amount, Time, Desc)|Rest]) :-
    stamp_date_time(Time, DateTime, local),
    format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', DateTime),
    (atom(To) ->
        % Transfer transaction
        write(FormattedTime), write(' - '), write(TxnID), nl,nl,
        write('  Transfer: '), write(From), write(' -> '), write(To), nl,nl,
        write('  Amount: $'), format('~2f', [Amount]), nl,nl,
        write('  Description: '), write(Desc), nl
    ;
        % Deposit/withdrawal
        write(FormattedTime), write(' - '), write(TxnID), nl,nl,
        write('  Account: '), write(From), nl,nl,
        write('  Type: '), write(To), nl,nl,
        write('  Amount: $'), format('~2f', [Amount]), nl,nl,
        write('  Description: '), write(Desc), nl
    ),
    nl,nl,
    write('*****************************************************'), nl,nl,
    print_transactions(Rest).

% ----------------------------
% REPORTS & ANALYTICS MODULE
% ----------------------------

reports_analytics(User) :-
    check_access(User, [admin,manager]),
    nl,nl, write('======= REPORTS & ANALYTICS ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('1. Customer Summary Report'), nl,nl,
    write('2. Account Balance Report'), nl,nl,
    write('3. Transaction Summary Report'), nl,nl,
    write('4. KYC Status Report'), nl,nl,
    write('5. Back to Main Menu'), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 5 -> show_main_menu(User) ; handle_report_choice(Choice, User)).

handle_report_choice(1, User) :- customer_summary_report, reports_analytics(User).
handle_report_choice(2, User) :- account_balance_report, reports_analytics(User).
handle_report_choice(3, User) :- transaction_summary_report, reports_analytics(User).
handle_report_choice(4, User) :- kyc_status_report, reports_analytics(User).
handle_report_choice(_, User) :- 
    write('Invalid choice'), nl,nl, 
    write('*****************************************************'), nl,nl,
    reports_analytics(User).

customer_summary_report :-
    findall(customer(ID, Name, _, _),
            customer(ID, Name, _, _),
            Customers),
    length(Customers, CustomerCount),
    findall(account(_, _, _, _), account(_, _, _, _), Accounts),
    length(Accounts, AccountCount),
    AvgAccounts is AccountCount / CustomerCount,
    
    nl,nl, write('======= CUSTOMER SUMMARY REPORT ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Total Customers: '), write(CustomerCount), nl,nl,
    write('Total Accounts: '), write(AccountCount), nl,nl,
    write('Average Accounts per Customer: '), format('~2f', [AvgAccounts]), nl,nl,
    
    % Customers with most accounts
    findall(cust_accounts(ID, Name, Count),
            (customer(ID, Name, _, _),
             aggregate(count, account(A, ID, _, _), Count)),
            CustomerAccounts),
    sort_customer_accounts(CustomerAccounts, Sorted),
    reverse(Sorted, [Top|_]),
    Top = cust_accounts(_, TopName, TopCount),
    write('Customer with most accounts: '), write(TopName), nl,nl,
    write('*****************************************************'), nl.

sort_customer_accounts(List, Sorted) :-
    predsort(compare_customer_accounts, List, Sorted).

compare_customer_accounts(>, cust_accounts(_, _, C1), cust_accounts(_, _, C2)) :- C1 < C2.
compare_customer_accounts(<, cust_accounts(_, _, C1), cust_accounts(_, _, C2)) :- C1 > C2.
compare_customer_accounts(=, _, _).

account_balance_report :-
    findall(Balance, account(_, _, _, Balance), Balances),
    sum_list(Balances, TotalBalance),
    length(Balances, AccountCount),
    AvgBalance is TotalBalance / AccountCount,
    min_list(Balances, MinBalance),
    max_list(Balances, MaxBalance),
    
    nl,nl, write('======= ACCOUNT BALANCE REPORT ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Total Balance Across All Accounts: $'), format('~2f', [TotalBalance]), nl,nl,
    write('Average Account Balance: $'), format('~2f', [AvgBalance]), nl,nl,
    write('Minimum Account Balance: $'), format('~2f', [MinBalance]), nl,nl,
    write('Maximum Account Balance: $'), format('~2f', [MaxBalance]), nl,nl,
    
    % Account types breakdown
    findall(Type, account(_, _, Type, _), Types),
    aggregate_all(count, member(checking, Types), CheckingCount),
    aggregate_all(count, member(savings, Types), SavingsCount),
    write('Checking Accounts: '), write(CheckingCount), nl,nl,
    write('Savings Accounts: '), write(SavingsCount), nl,nl,
    write('*****************************************************'), nl.

transaction_summary_report :-
    findall(Amount, transaction(_, _, _, Amount, _, _), Amounts),
    (Amounts = [] ->
        write('No transactions to report.'), nl,nl,
        write('*****************************************************'), nl
    ;
        sum_list(Amounts, TotalAmount),
        length(Amounts, TransactionCount),
        AvgAmount is TotalAmount / TransactionCount,
        min_list(Amounts, MinAmount),
        max_list(Amounts, MaxAmount),
        
        nl,nl, write('======= TRANSACTION SUMMARY REPORT ======='), nl,nl,
        write('*****************************************************'), nl,nl,
        write('Total Transactions: '), write(TransactionCount), nl,nl,
        write('Total Amount Transacted: $'), format('~2f', [TotalAmount]), nl,nl,
        write('Average Transaction Amount: $'), format('~2f', [AvgAmount]), nl,nl,
        write('Smallest Transaction: $'), format('~2f', [MinAmount]), nl,nl,
        write('Largest Transaction: $'), format('~2f', [MaxAmount]), nl,nl,
        
        % Transaction type breakdown
        findall(Type, transaction(_, _, Type, _, _, _), Types),
        aggregate_all(count, member(deposit, Types), DepositCount),
        aggregate_all(count, member(withdrawal, Types), WithdrawalCount),
        aggregate_all(count, transaction(_, _, To, _, _, _), atom(To), TransferCount),
        write('Deposits: '), write(DepositCount), nl,nl,
        write('Withdrawals: '), write(WithdrawalCount), nl,nl,
        write('Transfers: '), write(TransferCount), nl,nl,
        write('*****************************************************'), nl
    ).

kyc_status_report :-
    findall(KYC, customer(_, _, _, KYC), KYCs),
    aggregate_all(count, member(verified, KYCs), VerifiedCount),
    aggregate_all(count, member(unverified, KYCs), UnverifiedCount),
    TotalCustomers is VerifiedCount + UnverifiedCount,
    VerifiedPct is (VerifiedCount / TotalCustomers) * 100,
    UnverifiedPct is (UnverifiedCount / TotalCustomers) * 100,
    
    nl,nl, write('======= KYC STATUS REPORT ======='), nl,nl,
    write('*****************************************************'), nl,nl,
    write('Verified Customers: '), write(VerifiedCount), 
    write(' ('), format('~2f', [VerifiedPct]), write('%)'), nl,nl,
    write('Unverified Customers: '), write(UnverifiedCount),
    write(' ('), format('~2f', [UnverifiedPct]), write('%)'), nl,nl,
    write('Total Customers: '), write(TotalCustomers), nl,nl,
    write('*****************************************************'), nl.

% ----------------------------
% HELPER PREDICATES
% ----------------------------

check_access(User, RequiredRoles) :-
    current_user(User, Roles),
    (intersection(Roles, RequiredRoles, []) ->
        write('Access denied!'), nl,nl, 
        write('*****************************************************'), nl,nl,
        fail
    ;
        true
    ).

intersection([], _, []).
intersection([X|Xs], Ys, [X|Zs]) :- member(X, Ys), intersection(Xs, Ys, Zs).
intersection([X|Xs], Ys, Zs) :- \+ member(X, Ys), intersection(Xs, Ys, Zs).

% Start the system
:- initialization(start_bank_expert).