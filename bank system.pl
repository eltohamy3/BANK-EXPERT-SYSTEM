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
    assert(customer('cust001', 'John Doe', '1980-05-15', '123 Main St', '555-1234', verified)),
    assert(customer('cust002', 'Jane Smith', '1990-08-22', '456 Oak Ave', '555-5678', verified)),
    
    % Sample accounts
    assert(account('SAV001', 'cust001', savings, 5000)),
    assert(account('CHK001', 'cust001', checking, 2500)),
    assert(account('SAV002', 'cust002', savings, 15000)),
    
    % Sample users
    assert(user(admin, 'bank123', [admin])),
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
    write('2. View Customer'), nl,
    write('3. Search Customers'), nl,
    write('4. Update Customer'), nl,
    write('5. Back to Main Menu'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 5 -> show_main_menu(User) ; handle_customer_choice(Choice, User)).

handle_customer_choice(1, User) :- add_customer, customer_management(User).
handle_customer_choice(2, User) :- view_customer, customer_management(User).
handle_customer_choice(3, User) :- search_customers, customer_management(User).
handle_customer_choice(4, User) :- update_customer, customer_management(User).
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

view_customer :-
    write('Enter Customer ID: '), read(ID),
    (customer(ID, Name, DOB, Address, Phone, KYC) ->
        nl, write('=== CUSTOMER DETAILS ==='), nl,
        write('ID: '), write(ID), nl,
        write('Name: '), write(Name), nl,
        write('DOB: '), write(DOB), nl,
        write('Address: '), write(Address), nl,
        write('Phone: '), write(Phone), nl,
        write('KYC Status: '), write(KYC), nl
    ;
        write('Customer not found!'), nl
    ).

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