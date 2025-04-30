% consult("/Users/ahmedbahy/MyMac/Education/Academic/prolog/final.pl").

% ----------------------------
% BANK EXPERT SYSTEM CORE
% ----------------------------

:- dynamic customer/6.
:- dynamic transaction/5.
:- dynamic account/4.
:- dynamic user/3.

% Main entry point
start_bank_expert :-
    write('=== BANK EXPERT SYSTEM ==='), nl,
    initialize_system,
    authenticate_user.

% Initialize with sample data
initialize_system :-
    retractall(customer(_,_,_,_,_,_)),
    retractall(account(_,_,_,_)),
    retractall(transaction(_,_,_,_,_)),
    retractall(user(_,_,_)),
    
    % Sample customers
    assert(customer('cust001', 'John Doe', '1980-05-15', '123-Main-St', '555-1234', verified)),
    assert(customer('cust002', 'Jane Smith', '1990-08-22', '456-Oak-Ave', '555-5678', verified)),
	assert(customer('cust003', 'Bahy', '1990-08-22', '456-asd-Ave', '555-5678', verified)),
    
    % Sample accounts
    assert(account('SAV001', 'cust001', savings, 1000)),
    assert(account('SAV002', 'cust002', checking, 5000)),
    assert(account('SAV003', 'cust003', savings, 10000)),
    
    % Sample users
    assert(user(admin, 'bank', [admin])),
    assert(user(manager, 'secure456', [manager,reports])),
    assert(user(staff, 'staff789', [staff])).

% Authentication system
authenticate_user :-
    write('Please authenticate'), nl,
    write('Username: '), read(Username),
    write('Password: '), read(Password),
    (user(Username, Password, _) -> 
        show_main_menu(Username)
    ;
        write('Invalid credentials. Try again.'), nl,
        authenticate_user
    ).

% Main menu
show_main_menu(Username) :-
    nl, write('=== MAIN MENU ==='), nl,
    write('Logged in as: '), write(Username), nl, nl,
    write('1. Customer Management'), nl,
    write('2. Account Services'), nl,
    write('3. Transaction Processing'), nl,
    write('4. Fraud Detection'), nl,
    write('5. Investment Advisory'), nl,
    write('6. Reports & Analytics'), nl,
    write('0. Logout'), nl,
    write('Select option (0-6): '),
    read(Choice),
    handle_service(Choice, Username).

% Service routing
handle_service(0, _) :- 
    write('Logging out...'), nl, nl,
    start_bank_expert.

handle_service(1, User) :- customer_management(User).
handle_service(2, User) :- account_services(User).
handle_service(3, User) :- transaction_processing(User).
handle_service(4, User) :- fraud_detection(User).
handle_service(5, User) :- investment_advisory(User).
handle_service(6, User) :- reports_analytics(User).
handle_service(_, User) :- 
    write('Invalid choice'), nl, 
    show_main_menu(User).

% ----------------------------
% CUSTOMER MANAGEMENT MODULE
% ----------------------------

customer_management(User) :-
    check_access(User, [admin,manager]),
    nl, write('=== CUSTOMER MANAGEMENT ==='), nl,
    write('1. Add Customer'), nl,
    write('2. Search Customers'), nl,
    write('3. Update Customer'), nl,
    write('4. Back to Main Menu'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 4 -> show_main_menu(User) ; handle_customer_choice(Choice, User)).

handle_customer_choice(1, User) :- add_customer, customer_management(User).
handle_customer_choice(2, User) :- search_customers, customer_management(User).
handle_customer_choice(3, User) :- update_customer, customer_management(User).
handle_customer_choice(_, User) :- 
    write('Invalid choice'), nl, 
    customer_management(User).

add_customer :-
    write('Enter Customer ID: '), read(ID),
    write('Full Name: '), read(Name),
    write('Date of Birth (YYYY-MM-DD): '), read(DOB),
    write('Address: '), read(Address),
    write('Phone: '), read(Phone),
    write('KYC Status (verified/unverified): '), read(KYC),
    assert(customer(ID, Name, DOB, Address, Phone, KYC)),
    write('Customer added successfully!'), nl.

% NEW: Search customers by ID
search_customers :-
    nl, write('=== SEARCH CUSTOMERS ==='), nl,
    write('1. Search by ID'), nl,
    write('2. Back to Customer Management'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 2 -> true ; handle_search_choice(Choice)).

handle_search_choice(1) :-
    write('Enter Customer ID to search: '), read(ID),
    findall(customer(ID,Name,DOB,Address,Phone,KYC), 
        customer(ID,Name,DOB,Address,Phone,KYC), 
        Customers),
    (Customers = [] -> 
        write('No customers found with that ID.'), nl
    ;
        print_customers(Customers)
    ),
    search_customers.
    
handle_search_choice(_) :-
    write('Invalid choice'), nl,
    search_customers.

print_customers([]).
print_customers([customer(ID,Name,DOB,Address,Phone,KYC)|Rest]) :-
    nl, write('=== CUSTOMER DETAILS ==='), nl,
    write('ID: '), write(ID), nl,
    write('Name: '), write(Name), nl,
    write('DOB: '), write(DOB), nl,
    write('Address: '), write(Address), nl,
    write('Phone: '), write(Phone), nl,
    write('KYC Status: '), write(KYC), nl, nl,
    print_customers(Rest).

% NEW: Update customer information
update_customer :-
    write('Enter Customer ID to update: '), read(ID),
    (customer(ID, OldName, OldDOB, OldAddress, OldPhone, OldKYC) ->
        nl, write('Current customer details:'), nl,
        write('1. Name: '), write(OldName), nl,
        write('2. Date of Birth: '), write(OldDOB), nl,
        write('3. Address: '), write(OldAddress), nl,
        write('4. Phone: '), write(OldPhone), nl,
        write('5. KYC Status: '), write(OldKYC), nl, nl,
        
        write('Enter field number to update (1-5, 0 to cancel): '), read(Field),
        (Field =:= 0 -> 
            write('Update canceled.'), nl
        ; Field >= 1, Field =< 5 ->
            update_customer_field(ID, Field),
            write('Customer updated successfully!'), nl
        ;
            write('Invalid field number.'), nl,
            update_customer()
        )
    ;
        write('Customer not found!'), nl
    ).

update_customer_field(ID, 1) :-
    write('Enter new Name: '), read(NewName),
    customer(ID, _, DOB, Address, Phone, KYC),
    retract(customer(ID, _, _, _, _, _)),
    assert(customer(ID, NewName, DOB, Address, Phone, KYC)).

update_customer_field(ID, 2) :-
    write('Enter new Date of Birth (YYYY-MM-DD): '), read(NewDOB),
    customer(ID, Name, _, Address, Phone, KYC),
    retract(customer(ID, _, _, _, _, _)),
    assert(customer(ID, Name, NewDOB, Address, Phone, KYC)).

update_customer_field(ID, 3) :-
    write('Enter new Address: '), read(NewAddress),
    customer(ID, Name, DOB, _, Phone, KYC),
    retract(customer(ID, _, _, _, _, _)),
    assert(customer(ID, Name, DOB, NewAddress, Phone, KYC)).

update_customer_field(ID, 4) :-
    write('Enter new Phone: '), read(NewPhone),
    customer(ID, Name, DOB, Address, _, KYC),
    retract(customer(ID, _, _, _, _, _)),
    assert(customer(ID, Name, DOB, Address, NewPhone, KYC)).

update_customer_field(ID, 5) :-
    write('Enter new KYC Status (verified/unverified): '), read(NewKYC),
    customer(ID, Name, DOB, Address, Phone, _),
    retract(customer(ID, _, _, _, _, _)),
    assert(customer(ID, Name, DOB, Address, Phone, NewKYC)).

% ----------------------------
% ACCOUNT SERVICES MODULE
% ----------------------------

account_services(User) :-
    check_access(User, [admin,manager,staff]),
    nl, write('=== ACCOUNT SERVICES ==='), nl,
    write('1. Open Account'), nl,
    write('2. Close Account'), nl,
    write('3. View Account'), nl,
    write('4. List All Accounts'), nl,
    write('5. Back to Main Menu'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 5 -> show_main_menu(User) ; handle_account_choice(Choice, User)).

handle_account_choice(1, User) :- open_account, account_services(User).
handle_account_choice(2, User) :- close_account, account_services(User).
handle_account_choice(3, User) :- view_account, account_services(User).
handle_account_choice(4, User) :- list_accounts, account_services(User).
handle_account_choice(_, User) :- 
    write('Invalid choice'), nl, 
    account_services(User).

open_account :-
    write('Enter Customer ID: '), read(CustomerID),
    (customer(CustomerID, _, _, _, _, _) ->
        write('Account Type (savings/checking/business): '), read(Type),
        write('Initial Deposit: '), read(Balance),
        generate_account_number(Type, AccountNumber),
        assert(account(AccountNumber, CustomerID, Type, Balance)),
        write('Account opened successfully! Number: '), write(AccountNumber), nl
    ;
        write('Customer not found!'), nl
    ).
% Close account predicate
close_account :-
    write('Enter Account Number to close: '), read(Account),
    (account(Account, CustomerID, Type, Balance) ->
        (Balance =:= 0 ->
            retract(account(Account, CustomerID, Type, Balance)),
            write('Account closed successfully!'), nl
        ;
            write('Cannot close account with non-zero balance. Current balance: '), 
            write(Balance), nl,
            write('Please withdraw all funds before closing.'), nl
        )
    ;
        write('Account not found!'), nl
    ).

% View account details
view_account :-
    write('Enter Account Number: '), read(Account),
    (account(Account, CustomerID, Type, Balance) ->
        nl, write('=== ACCOUNT DETAILS ==='), nl,
        write('Account Number: '), write(Account), nl,
        write('Customer ID: '), write(CustomerID), nl,
        write('Account Type: '), write(Type), nl,
        write('Balance: '), write(Balance), nl
    ;
        nl, write('Account not found!'), nl
    ).


% List all accounts
list_accounts :-
    findall(account(Account, CustomerID, Type, Balance), 
        account(Account, CustomerID, Type, Balance), 
        Accounts),
    (Accounts = [] -> 
        write('No accounts found.'), nl
    ;
        nl, write('=== ALL ACCOUNTS ==='), nl,
        print_accounts(Accounts)
    ).

% Helper to print account list
print_accounts([]).
print_accounts([account(Account, CustomerID, Type, Balance)|Rest]) :-
    write('Account: '), write(Account), 
    write(' | Customer: '), write(CustomerID),
    write(' | Type: '), write(Type),
    write(' | Balance: '), write(Balance), nl,
    print_accounts(Rest).


generate_account_number(Type, AccountNumber) :-
    get_time(TimeStamp),
    format_time(atom(RandomPart), '%S%M', TimeStamp),
    atom_concat(Type, RandomPart, AccountNumber).

% ----------------------------
% TRANSACTION PROCESSING MODULE
% ----------------------------

transaction_processing(User) :-
    check_access(User, [admin,manager,staff]),
    nl, write('=== TRANSACTION PROCESSING ==='), nl,
    write('1. Process Deposit'), nl,
    write('2. Process Withdrawal'), nl,
    write('3. Process Transfer'), nl,
    write('4. View Transaction History'), nl,
    write('5. Back to Main Menu'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 5 -> show_main_menu(User) ; handle_transaction_choice(Choice, User)).

handle_transaction_choice(1, User) :- process_deposit, transaction_processing(User).
handle_transaction_choice(2, User) :- process_withdrawal, transaction_processing(User).
handle_transaction_choice(3, User) :- process_transfer, transaction_processing(User).
handle_transaction_choice(4, User) :- view_transactions, transaction_processing(User).
handle_transaction_choice(_, User) :- 
    write('Invalid choice'), nl, 
    transaction_processing(User).

process_deposit :-
    write('Account Number: '), read(Account),
    write('Amount: '), read(Amount),
    (account(Account, CustomerID, Type, Balance) ->
        NewBalance is Balance + Amount,
        retract(account(Account, CustomerID, Type, Balance)),
        assert(account(Account, CustomerID, Type, NewBalance)),
        get_time(TimeStamp),
        assert(transaction(Account, deposit, Amount, TimeStamp, 'completed')),
        write('Deposit successful. New balance: '), write(NewBalance), nl
    ;
        write('Account not found!'), nl
    ).

process_withdrawal :-
    write('Account Number: '), read(InputAccount),
    write('Amount: '), read(Amount),
    (   account(InputAccount, CustomerID, Type, Balance)
    ->  write('Current balance: '), write(Balance), nl,
        (   Amount > 0
        ->  (   Balance >= Amount
            ->  NewBalance is Balance - Amount,
                retract(account(InputAccount, CustomerID, Type, Balance)),
                assert(account(InputAccount, CustomerID, Type, NewBalance)),
                get_time(TimeStamp),
                assert(transaction(InputAccount, withdrawal, Amount, TimeStamp, 'completed')),
                write('Withdrawal successful. New balance: '), write(NewBalance), nl,
                write('Account: '), write(InputAccount), nl
            ;   write('ERROR: Insufficient funds!'), nl,
                write('Withdrawal amount: '), write(Amount), nl,
                write('Your balance remains: '), write(Balance), nl
            )
        ;   write('ERROR: Invalid withdrawal amount!'), nl,
            write('Your balance remains: '), write(Balance), nl
        )
    ;   write('ERROR: Account not found!'), nl
    ).


% 3. Process Transfer
process_transfer :-
    write('From Account (source): '), read(FromAccount),
    write('To Account (destination): '), read(ToAccount),
    write('Amount: '), read(Amount),
    (   FromAccount \== ToAccount,
        Amount > 0,
        account(FromAccount, FromCustomerID, FromType, FromBalance),
        account(ToAccount, ToCustomerID, ToType, ToBalance),
        FromBalance >= Amount
    ->  % Withdraw from source account
        NewFromBalance is FromBalance - Amount,
        retract(account(FromAccount, FromCustomerID, FromType, FromBalance)),
        assert(account(FromAccount, FromCustomerID, FromType, NewFromBalance)),
        
        % Deposit to destination account
        NewToBalance is ToBalance + Amount,
        retract(account(ToAccount, ToCustomerID, ToType, ToBalance)),
        assert(account(ToAccount, ToCustomerID, ToType, NewToBalance)),
        
        % Record both transactions
        get_time(TimeStamp),
        assert(transaction(FromAccount, transfer_out, Amount, TimeStamp, 'completed')),
        assert(transaction(ToAccount, transfer_in, Amount, TimeStamp, 'completed')),
        
        % Show success message
        nl, write('Transfer successful!'), nl,
        write('From Account: '), write(FromAccount), 
        write(' | New Balance: '), write(NewFromBalance), nl,
        write('To Account: '), write(ToAccount), 
        write(' | New Balance: '), write(NewToBalance), nl
    ;   FromAccount == ToAccount
    ->  write('ERROR: Cannot transfer to the same account!'), nl
    ;   write('ERROR: Transfer failed! Check account numbers and balances.'), nl
    ).

% 4. View Transaction History
view_transactions :-
    nl, write('=== TRANSACTION HISTORY OPTIONS ==='), nl,
    write('1. View all transactions'), nl,
    write('2. View transactions for specific account'), nl,
    write('3. Back to Transaction Menu'), nl,
    write('Select option: '),
    read(Choice),
    (   Choice == 1 -> show_all_transactions
    ;   Choice == 2 -> show_account_transactions
    ;   Choice == 3 -> true
    ;   write('Invalid choice'), nl, view_transactions
    ).

% Helper to show all transactions
show_all_transactions :-
    findall(transaction(Account, Type, Amount, TimeStamp, Status),
            transaction(Account, Type, Amount, TimeStamp, Status),
            Transactions),
    (   Transactions == []
    ->  write('No transactions found.'), nl
    ;   format_transactions(Transactions)
    ).

% Helper to show transactions for a specific account
show_account_transactions :-
    write('Enter Account Number: '), read(Account),
    findall(transaction(Account, Type, Amount, TimeStamp, Status),
            transaction(Account, Type, Amount, TimeStamp, Status),
            Transactions),
    (   Transactions == []
    ->  write('No transactions found for account '), write(Account), nl
    ;   format_transactions(Transactions)
    ).

% Helper to format and display transactions
format_transactions([]).
format_transactions([transaction(Account, Type, Amount, TimeStamp, Status)|Rest]) :-
    format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', TimeStamp),
    write('Account: '), write(Account),
    write(' | Type: '), write(Type),
    write(' | Amount: '), write(Amount),
    write(' | Time: '), write(FormattedTime),
    write(' | Status: '), write(Status), nl,
    format_transactions(Rest).

% ----------------------------
% HELPER PREDICATES
% ----------------------------

check_access(User, RequiredRoles) :-
    user(User, _, Roles),
    (intersection(Roles, RequiredRoles, []) ->
        write('Access denied!'), nl, show_main_menu(User)
    ;
        true
    ).

intersection([], _, []).
intersection([X|Xs], Ys, [X|Zs]) :- member(X, Ys), intersection(Xs, Ys, Zs).
intersection([X|Xs], Ys, Zs) :- \+ member(X, Ys), intersection(Xs, Ys, Zs).

% Start the system
:- initialization(start_bank_expert).