% ----------------------------
% BANK EXPERT SYSTEM CORE
% ----------------------------

% Main entry point
start_bank_expert :-
    write('Welcome to Bank Expert System'), nl,
    write('Available services:'), nl,
    write('1. Account recommendation'), nl,
    write('2. Fraud detection'), nl,
    write('3. Investment advice'), nl,
    write('4. Customer service routing'), nl,
    write('5. Transaction validation'), nl,
    write('Select service (1-5): '),
    read(Choice),
    handle_service(Choice).

% ----------------------------
% SERVICE HANDLERS
% ----------------------------

handle_service(1) :- account_recommendation.
handle_service(2) :- fraud_detection.
handle_service(3) :- investment_advice.
handle_service(4) :- customer_service_routing.
handle_service(5) :- transaction_validation.
handle_service(_) :- write('Invalid choice'), nl, start_bank_expert.

% ----------------------------
% ACCOUNT RECOMMENDATION MODULE
% ----------------------------

account_recommendation :-
    write('=== ACCOUNT RECOMMENDATION ==='), nl,
    ask_customer_profile(CustomerType, ActivityLevel, Balance),
    recommend_account(CustomerType, ActivityLevel, Balance).

ask_customer_profile(CustomerType, ActivityLevel, Balance) :-
    write('Customer type (individual/business/student/senior): '),
    read(CustomerType),
    write('Transaction activity level (low/medium/high): '),
    read(ActivityLevel),
    write('Average monthly balance: '),
    read(Balance).

recommend_account(individual, low, Balance) :-
    Balance < 1000,
    write('Recommended: Basic Savings Account with no fees').

recommend_account(individual, medium, Balance) :-
    Balance >= 1000, Balance < 5000,
    write('Recommended: Premium Checking with debit rewards').

recommend_account(business, high, _) :-
    write('Recommended: Business Advantage Account with unlimited transactions').

recommend_account(student, _, _) :-
    write('Recommended: Student Account with no monthly fees').

% ----------------------------
% FRAUD DETECTION MODULE
% ----------------------------

fraud_detection :-
    write('=== FRAUD DETECTION ==='), nl,
    write('Enter transaction details'), nl,
    ask_transaction_details(Amount, Location, Recipient, Frequency),
    analyze_fraud_risk(Amount, Location, Recipient, Frequency).

ask_transaction_details(Amount, Location, Recipient, Frequency) :-
    write('Transaction amount: '),
    read(Amount),
    write('Transaction location: '),
    read(Location),
    write('Recipient (known/new): '),
    read(Recipient),
    write('Transaction frequency (first/repeat): '),
    read(Frequency).

analyze_fraud_risk(Amount, Location, Recipient, Frequency) :-
    risk_score(Amount, Location, Recipient, Frequency, Score),
    (Score > 70 ->
        write('HIGH RISK: Transaction flagged for review'), nl,
        write('Recommended action: Contact customer and verify transaction');
     Score > 40 ->
        write('MEDIUM RISK: Additional verification recommended');
     write('LOW RISK: Transaction appears normal')).

risk_score(Amount, _, new, first, Score) :-
    Score is min(100, Amount / 1000 * 70).
risk_score(Amount, foreign, _, _, Score) :-
    Score is min(100, Amount / 2000 * 80).
risk_score(_, _, _, _, 20). % Default low risk

% ----------------------------
% INVESTMENT ADVICE MODULE
% ----------------------------

investment_advice :-
    write('=== INVESTMENT ADVICE ==='), nl,
    ask_investment_profile(RiskTolerance, Horizon, Amount),
    recommend_investment(RiskTolerance, Horizon, Amount).

ask_investment_profile(RiskTolerance, Horizon, Amount) :-
    write('Risk tolerance (low/medium/high): '),
    read(RiskTolerance),
    write('Investment horizon (short/medium/long): '),
    read(Horizon),
    write('Investment amount: '),
    read(Amount).

recommend_investment(low, short, _) :-
    write('Recommended: Money market funds or CDs').

recommend_investment(low, long, _) :-
    write('Recommended: Government bonds or index funds').

recommend_investment(high, long, Amount) :-
    Amount >= 10000,
    write('Recommended: Diversified stock portfolio with international exposure').

% ----------------------------
% CUSTOMER SERVICE ROUTING
% ----------------------------

customer_service_routing :-
    write('=== CUSTOMER SERVICE ROUTING ==='), nl,
    write('Select issue category:'), nl,
    write('1. Account issues'), nl,
    write('2. Card problems'), nl,
    write('3. Loan inquiries'), nl,
    write('4. Fraud concerns'), nl,
    write('5. General questions'), nl,
    read(Category),
    route_to_department(Category).

route_to_department(1) :- write('Routing to Account Services: 1-800-ACC-1234').
route_to_department(2) :- write('Routing to Card Department: 1-800-CRD-5678').
route_to_department(3) :- write('Routing to Loan Officers: 1-800-LON-9012').
route_to_department(4) :- write('URGENT: Routing to Fraud Department: 1-800-FRD-3456').
route_to_department(5) :- write('Routing to Customer Service: 1-800-BNK-7890').

% ----------------------------
% TRANSACTION VALIDATION
% ----------------------------

transaction_validation :-
    write('=== TRANSACTION VALIDATION ==='), nl,
    write('Enter account type (checking/savings/business): '),
    read(AccountType),
    write('Enter transaction amount: '),
    read(Amount),
    write('Enter current balance: '),
    read(Balance),
    validate_transaction(AccountType, Amount, Balance).

validate_transaction(checking, Amount, Balance) :-
    Balance >= Amount,
    write('Transaction approved for checking account').

validate_transaction(savings, Amount, Balance) :-
    Balance - Amount >= 100, % Maintain minimum balance
    write('Transaction approved for savings account').

validate_transaction(business, Amount, Balance) :-
    Balance >= Amount * 1.1, % Business accounts keep 10% buffer
    write('Transaction approved for business account').

% ----------------------------
% UTILITIES
% ----------------------------

% Run the expert system
:- initialization(start_bank_expert).
