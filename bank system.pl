% ----------------------------
% BANK EXPERT SYSTEM CORE
% ----------------------------

:- dynamic customer/7.  % ID, Name, DOB, Address, Phone, KYC, Balance
:- dynamic transaction/5. % CustomerID, Type, Amount, Timestamp, Status
:- dynamic user/3. % Username, Password, Roles

% Main entry point
start_bank_expert :-
    write('=== BANK EXPERT SYSTEM ==='), nl,
    initialize_system,
    authenticate_user.

% Initialize with sample data
initialize_system :-
    retractall(customer(_,_,_,_,_,_,_)),
    retractall(transaction(_,_,_,_,_)),
    retractall(user(_,_,_)),
    
    % Sample customers with balances
    assert(customer('cust001', 'John Doe', '1980-05-15', '123-Main-St', '555-1234', verified, 15000)),
    assert(customer('cust002', 'Jane Smith', '1990-08-22', '456-Oak-Ave', '555-5678', verified, 5000)),
    assert(customer('cust003', 'Bahy', '1990-08-22', '456-asd-Ave', '555-5678', verified, 10000)),
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
    write('2. Transaction Processing'), nl,
    write('3. Reports & Analytics'), nl,
    write('0. Logout'), nl,
    write('Select option (0-3): '),
    read(Choice),
    handle_service(Choice, Username).

% Service routing
handle_service(0, _) :- 
    write('Logging out...'), nl, nl,
    start_bank_expert.

handle_service(1, User) :- customer_management(User).
handle_service(2, User) :- transaction_processing(User).
handle_service(3, User) :- reports_analytics(User).
handle_service(_, User) :- 
    write('Invalid choice'), nl, 
    show_main_menu(User).
% ----------------------------
% CUSTOMER MANAGEMENT MODULE (updated with view all)
% ----------------------------

customer_management(User) :-
    check_access(User, [admin,manager]),
    nl, write('=== CUSTOMER MANAGEMENT ==='), nl,
    write('1. Add Customer'), nl,
    write('2. Search Customers'), nl,
    write('3. Update Customer'), nl,
    write('4. Delete Customer'), nl,
    write('5. View All Customers'), nl,  % New option
    write('6. Back to Main Menu'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 6 -> show_main_menu(User) ; handle_customer_choice(Choice, User)).

handle_customer_choice(1, User) :- add_customer, customer_management(User).
handle_customer_choice(2, User) :- search_customers, customer_management(User).
handle_customer_choice(3, User) :- update_customer, customer_management(User).
handle_customer_choice(4, User) :- delete_customer, customer_management(User).
handle_customer_choice(5, User) :- view_all_customers, customer_management(User).  % New handler
handle_customer_choice(_, User) :- 
    write('Invalid choice'), nl, 
    customer_management(User).

% New predicate to view all customers
view_all_customers :-
    nl, write('=== ALL CUSTOMERS ==='), nl,
    findall(customer(ID, Name, DOB, Address, Phone, KYC, Balance),
           customer(ID, Name, DOB, Address, Phone, KYC, Balance),
           Customers),
    (Customers = [] -> 
        write('No customers found in the system.'), nl
    ;
        print_customers(Customers),
        length(Customers, Count),
        nl, write('Total customers: '), write(Count), nl
    ).

add_customer :-
    % Get all customer details except balance
        get_unique_id(ID),
    write('Full Name: '), read(Name),
    write('Date of Birth (YYYY-MM-DD): '), read(DOB),
    write('Address: '), read(Address),
    write('Phone: '), read(Phone),
    write('KYC Status (verified/unverified): '), read(KYC),
    
    % Get balance with validation
    get_valid_balance(Balance),
    
    % Assert the customer if balance is valid
    assert(customer(ID, Name, DOB, Address, Phone, KYC, Balance)),
    write('Customer added successfully!'), nl.

get_unique_id(ID) :-
    repeat,
    write('Enter Customer ID: '), 
    read(ID),
    (   customer(ID, _, _, _, _, _, _)
    ->  write('Error: Customer ID already exists! Try again.'), nl,
        fail
    ;   !  % Cut to commit to this unique ID
    ).


% Helper predicate to get valid balance
get_valid_balance(Balance) :-
    repeat,  % Creates a choice point for retry
    write('Initial Balance (must be positive): '), 
    read(Balance),
    (   number(Balance), Balance > 0 
    ->  !  % Cut to commit to this solution
    ;   write('Error: Balance must be positive. Please try again.'), nl,
        fail  % Triggers repeat
    ).



search_customers :-
    nl, write('=== SEARCH CUSTOMERS ==='), nl,
    write('1. Search by ID'), nl,
    write('2. Back to Customer Management'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 2 -> true ; handle_search_choice(Choice)).

handle_search_choice(1) :-
    write('Enter Customer ID to search: '), read(ID),
    findall(customer(ID,Name,DOB,Address,Phone,KYC,Balance), 
        customer(ID,Name,DOB,Address,Phone,KYC,Balance), 
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
print_customers([customer(ID,Name,DOB,Address,Phone,KYC,Balance)|Rest]) :-
    nl, write('=== CUSTOMER DETAILS ==='), nl,
    write('ID: '), write(ID), nl,
    write('Name: '), write(Name), nl,
    write('DOB: '), write(DOB), nl,
    write('Address: '), write(Address), nl,
    write('Phone: '), write(Phone), nl,
    write('KYC Status: '), write(KYC), nl,
    write('Balance: $'), format('~2f', [Balance]), nl, nl,
    print_customers(Rest).

update_customer :-
    write('Enter Customer ID to update: '), read(ID),
    (customer(ID, OldName, OldDOB, OldAddress, OldPhone, OldKYC, OldBalance) ->
        nl, write('Current customer details:'), nl,
        write('1. Name: '), write(OldName), nl,
        write('2. Date of Birth: '), write(OldDOB), nl,
        write('3. Address: '), write(OldAddress), nl,
        write('4. Phone: '), write(OldPhone), nl,
        write('5. KYC Status: '), write(OldKYC), nl,

        
        write('Enter field number to update (1-5, 0 to cancel): '), read(Field),
        (Field =:= 0 -> 
            write('Update canceled.'), nl
        ; Field >= 1, Field =< 6 ->
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
    customer(ID, _, DOB, Address, Phone, KYC, Balance),
    retract(customer(ID, _, _, _, _, _, _)),
    assert(customer(ID, NewName, DOB, Address, Phone, KYC, Balance)).

update_customer_field(ID, 2) :-
    write('Enter new Date of Birth (YYYY-MM-DD): '), read(NewDOB),
    customer(ID, Name, _, Address, Phone, KYC, Balance),
    retract(customer(ID, _, _, _, _, _, _)),
    assert(customer(ID, Name, NewDOB, Address, Phone, KYC, Balance)).

update_customer_field(ID, 3) :-
    write('Enter new Address: '), read(NewAddress),
    customer(ID, Name, DOB, _, Phone, KYC, Balance),
    retract(customer(ID, _, _, _, _, _, _)),
    assert(customer(ID, Name, DOB, NewAddress, Phone, KYC, Balance)).

update_customer_field(ID, 4) :-
    write('Enter new Phone: '), read(NewPhone),
    customer(ID, Name, DOB, Address, _, KYC, Balance),
    retract(customer(ID, _, _, _, _, _, _)),
    assert(customer(ID, Name, DOB, Address, NewPhone, KYC, Balance)).

update_customer_field(ID, 5) :-
    write('Enter new KYC Status (verified/unverified): '), read(NewKYC),
    customer(ID, Name, DOB, Address, Phone, _, Balance),
    retract(customer(ID, _, _, _, _, _, _)),
    assert(customer(ID, Name, DOB, Address, Phone, NewKYC, Balance)).


delete_customer :-
    write('Enter Customer ID to delete: '), read(ID),
    (customer(ID, _, _, _, _, _, _) ->
        % Delete all transactions for this customer first
        retractall(transaction(ID, _, _, _, _)),
        % Then delete the customer
        retract(customer(ID, _, _, _, _, _, _)),
        write('Customer and all their transactions deleted successfully!'), nl
    ;
        write('Customer not found!'), nl
    ).

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
    write('Customer ID: '), read(CustomerID),
    (customer(CustomerID, _, _, _, _, _, Balance) ->
        write('Amount: '), read(Amount),
        (Amount > 0 ->
            NewBalance is Balance + Amount,
            retract(customer(CustomerID, Name, DOB, Address, Phone, KYC, _)),
            assert(customer(CustomerID, Name, DOB, Address, Phone, KYC, NewBalance)),
            get_time(TimeStamp),
            assert(transaction(CustomerID, deposit, Amount, TimeStamp, 'completed')),
            write('Deposit successful. New balance: $'), 
            format('~2f', [NewBalance]), nl
        ;
            write('ERROR: Deposit amount must be positive!'), nl
        )
    ;
        write('Customer not found!'), nl
    ).

process_withdrawal :-
    write('Customer ID: '), read(CustomerID),
    (customer(CustomerID, _, _, _, _, _, Balance) ->
        write('Amount: '), read(Amount),
        (Amount =< 0 ->
            write('ERROR: Withdrawal amount must be positive!'), nl,
            write('Your balance remains: $'), format('~2f', [Balance]), nl
        ; Balance >= Amount ->
            NewBalance is Balance - Amount,
            retract(customer(CustomerID, Name, DOB, Address, Phone, KYC, _)),
            assert(customer(CustomerID, Name, DOB, Address, Phone, KYC, NewBalance)),
            get_time(TimeStamp),
            assert(transaction(CustomerID, withdrawal, Amount, TimeStamp, 'completed')),
            write('Withdrawal successful. New balance: $'), 
            format('~2f', [NewBalance]), nl
        ;
            write('ERROR: Insufficient funds!'), nl,
            write('Withdrawal amount: $'), format('~2f', [Amount]), nl,
            write('Your balance remains: $'), format('~2f', [Balance]), nl
        )
    ;
        write('Customer not found!'), nl
    ).

process_transfer :-
    write('From Customer ID: '), read(FromCustomer),
    write('To Customer ID: '), read(ToCustomer),
    write('Amount: '), read(Amount),
    (FromCustomer == ToCustomer ->
        write('ERROR: Cannot transfer to same customer!'), nl
    ; customer(FromCustomer, _, _, _, _, _, FromBalance),
      customer(ToCustomer, _, _, _, _, _, ToBalance) ->
        (Amount =< 0 ->
            write('ERROR: Transfer amount must be positive!'), nl
        ; FromBalance < Amount ->
            write('ERROR: Insufficient funds for transfer!'), nl,
            write('Available balance: $'), format('~2f', [FromBalance]), nl
        ;
            % Update sender balance
            NewFromBalance is FromBalance - Amount,
            retract(customer(FromCustomer, FName, FDOB, FAddress, FPhone, FKYC, _)),
            assert(customer(FromCustomer, FName, FDOB, FAddress, FPhone, FKYC, NewFromBalance)),
            
            % Update receiver balance
            NewToBalance is ToBalance + Amount,
            retract(customer(ToCustomer, TName, TDOB, TAddress, TPhone, TKYC, _)),
            assert(customer(ToCustomer, TName, TDOB, TAddress, TPhone, TKYC, NewToBalance)),
            
            % Record transactions
            get_time(TimeStamp),
            assert(transaction(FromCustomer, transfer_out, Amount, TimeStamp, 'completed')),
            assert(transaction(ToCustomer, transfer_in, Amount, TimeStamp, 'completed')),
            
            write('Transfer successful!'), nl,
            write('From Customer: '), write(FromCustomer), 
            write(' | New Balance: $'), format('~2f', [NewFromBalance]), nl,
            write('To Customer: '), write(ToCustomer), 
            write(' | New Balance: $'), format('~2f', [NewToBalance]), nl
        )
    ;
        write('One or both customers not found!'), nl
    ).

% Updated view_transactions predicate
view_transactions :-
    nl, write('=== TRANSACTION HISTORY OPTIONS ==='), nl,
    write('1. View all transactions'), nl,
    write('2. View transactions for specific customer'), nl,
    write('3. Back to Transaction Menu'), nl,
    write('Select option: '),
    read(Choice),
    (   Choice == 1 -> show_all_transactions
    ;   Choice == 2 -> show_customer_transactions
    ;   Choice == 3 -> true
    ;   write('Invalid choice'), nl, view_transactions
    ).

% Fixed show_all_transactions predicate
show_all_transactions :-
    findall(transaction(Customer, Type, Amount, TimeStamp, Status),
            transaction(Customer, Type, Amount, TimeStamp, Status),
            Transactions),
    (   Transactions == []
    ->  write('No transactions found.'), nl
    ;   write('=== ALL TRANSACTIONS ==='), nl,
        format_transactions(Transactions)
    ).

% Fixed show_customer_transactions predicate
show_customer_transactions :-
    write('Enter Customer ID: '), read(CustomerID),
    findall(transaction(CustomerID, Type, Amount, TimeStamp, Status),
            transaction(CustomerID, Type, Amount, TimeStamp, Status),
            Transactions),
    (   Transactions == []
    ->  write('No transactions found for customer '), write(CustomerID), nl
    ;   write('=== TRANSACTIONS FOR CUSTOMER '), write(CustomerID), write(' ==='), nl,
        format_transactions(Transactions)
    ).

% Fixed format_transactions predicate with proper date handling
format_transactions([]).
format_transactions([transaction(Customer, Type, Amount, TimeStamp, Status)|Rest]) :-
    (   float(TimeStamp) 
    ->  stamp_date_time(TimeStamp, DateTime, local),
        format_time(atom(FormattedTime), '%Y-%m-%d %H:%M:%S', DateTime)
    ;   FormattedTime = TimeStamp  % Fallback if not a timestamp
    ),
    write('Customer: '), write(Customer),
    write(' | Type: '), write(Type),
    write(' | Amount: $'), format('~2f', [Amount]),
    write(' | Time: '), write(FormattedTime),
    write(' | Status: '), write(Status), nl,
    format_transactions(Rest).

% ----------------------------
% REPORTS & ANALYTICS MODULE
% ----------------------------

reports_analytics(User) :-
    check_access(User, [admin,manager]),
    nl, write('=== REPORTS & ANALYTICS ==='), nl,
    write('1. Customer balances report'), nl,
    write('2. Transaction summary'), nl,
    write('3. KYC status report'), nl,
    write('4. Back to Main Menu'), nl,
    write('Select option: '),
    read(Choice),
    (Choice =:= 4 -> show_main_menu(User) ; handle_report_choice(Choice, User)).

handle_report_choice(1, User) :-
    findall(customer(ID, Name, _, _, _, _, Balance),
            customer(ID, Name, _, _, _, _, Balance),
            Customers),
    sort_customers_by_balance(Customers, SortedCustomers),
    nl, write('=== CUSTOMER BALANCES REPORT ==='), nl,
    print_balance_report(SortedCustomers),
    reports_analytics(User).

handle_report_choice(2, User) :-
    findall(Amount,
            transaction(_, _, Amount, _, 'completed'),
            Amounts),
    (Amounts = [] ->
        write('No transactions to report.'), nl
    ;
        sum_list(Amounts, Total),
        length(Amounts, Count),
        min_list(Amounts, Min),
        max_list(Amounts, Max),
        Avg is Total / Count,
        nl, write('=== TRANSACTION SUMMARY ==='), nl,
        write('Total transactions: '), write(Count), nl,
        write('Total amount: $'), format('~2f', [Total]), nl,
        write('Average transaction: $'), format('~2f', [Avg]), nl,
        write('Smallest transaction: $'), format('~2f', [Min]), nl,
        write('Largest transaction: $'), format('~2f', [Max]), nl
    ),
    reports_analytics(User).

handle_report_choice(3, User) :-
    findall(KYC, customer(_, _, _, _, _, KYC, _), KYCs),
    count_kyc_status(KYCs, Verified, Unverified),
    Total is Verified + Unverified,
    VerifiedPct is (Verified / Total) * 100,
    UnverifiedPct is (Unverified / Total) * 100,
    nl, write('=== KYC STATUS REPORT ==='), nl,
    write('Verified customers: '), write(Verified), 
    write(' ('), format('~2f', [VerifiedPct]), write('%)'), nl,
    write('Unverified customers: '), write(Unverified),
    write(' ('), format('~2f', [UnverifiedPct]), write('%)'), nl,
    write('Total customers: '), write(Total), nl,
    reports_analytics(User).

handle_report_choice(_, User) :-
    write('Invalid choice'), nl,
    reports_analytics(User).

sort_customers_by_balance(Customers, Sorted) :-
    predsort(compare_balances, Customers, Sorted).

compare_balances(>, customer(_, _, _, _, _, _, B1), customer(_, _, _, _, _, _, B2)) :- B1 < B2.
compare_balances(<, customer(_, _, _, _, _, _, B1), customer(_, _, _, _, _, _, B2)) :- B1 > B2.
compare_balances(=, _, _).

print_balance_report([]).
print_balance_report([customer(ID, Name, _, _, _, _, Balance)|Rest]) :-
    write('ID: '), write(ID),
    write(' | Name: '), write(Name),
    write(' | Balance: $'), format('~2f', [Balance]), nl,
    print_balance_report(Rest).

count_kyc_status([], 0, 0).
count_kyc_status([verified|Rest], V, U) :-
    count_kyc_status(Rest, V1, U),
    V is V1 + 1.
count_kyc_status([unverified|Rest], V, U) :-
    count_kyc_status(Rest, V, U1),
    U is U1 + 1.

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