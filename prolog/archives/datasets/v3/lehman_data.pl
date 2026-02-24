% --- PROLOG AUDIT: LEHMAN BROTHERS (LBHI) ---
% DATA SOURCE: lehman.pdf (Examiner's Report)
% TEMPORAL SCALE: T=0 (Jan 2006), T=24 (Jan 2008), T=32 (Sept 2008)

% --- 1. CORE ENTITIES ---
entity(lbhi, organizational).
entity(sec, organizational).
entity(frbny, organizational).
entity(rating_agencies, class).
entity(repo_105, structural).
entity(risk_appetite_limit, structural).
entity(liquidity_pool, scaffold).

% --- 2. INTERVALS AND EVENTS ---
interval(growth_strategy, 0, 12).
interval(crisis_incubation, 12, 24).
interval(terminal_phase, 24, 32).

% Fixed schema: event(ID, Kind, Time, Properties).
event(bear_stearns_collapse, market_shock, 26, [liquidity_freeze]).
event(bankruptcy_filing, terminal_collapse, 32, [chapter_11]).

% --- 3. CONSTRAINTS AND CLAIMS ---
constraint_claim(repo_105, rope).
constraint_claim(risk_appetite_limit, mountain).
constraint_claim(liquidity_pool, mountain).

% Current state metrics (at T_end)
constraint_metric(repo_105, extractiveness, 0.85).
constraint_metric(repo_105, suppression_requirement, 0.95).
constraint_metric(repo_105, snap_back_potential, 1.0).

constraint_metric(risk_appetite_limit, extractiveness, 0.75).
constraint_metric(risk_appetite_limit, suppression_requirement, 0.20).

constraint_metric(liquidity_pool, extractiveness, 0.95).
constraint_metric(liquidity_pool, suppression_requirement, 0.90).

% --- 4. TEMPORAL MEASUREMENTS (PAIRED) ---
% Repo 105: Evolution from tool (Rope) to manipulation (Snare)
measurement(m1, repo_105, extractiveness, 24, 0.20).
measurement(m2, repo_105, suppression_requirement, 24, 0.85).
measurement(m3, repo_105, extractiveness, 29, 0.85).
measurement(m4, repo_105, suppression_requirement, 29, 0.95).

% Liquidity Pool: Evolution from fortress (Mountain) to encumbered asset (Snare)
measurement(m5, liquidity_pool, extractiveness, 24, 0.10).
measurement(m6, liquidity_pool, suppression_requirement, 24, 0.05).
measurement(m7, liquidity_pool, extractiveness, 32, 0.95).
measurement(m8, liquidity_pool, suppression_requirement, 32, 0.90).

% --- 5. DEPENDENCIES ---
affects_constraint(repo_105, net_leverage_ratio).
affects_constraint(net_leverage_ratio, rating_agency_confidence).
affects_constraint(rating_agency_confidence, counterparty_funding).
affects_constraint(liquidity_pool, survival).

% --- 6. INTENT AND CAPTURE ---
intent_beneficiary_class(growth_strategy, management_compensation).
% FIXED: Added power change delta for the beneficiary class
intent_power_change(growth_strategy, management_compensation, 0.8). 
intent_power_change(growth_strategy, risk_management_function, -0.8).

% Logical consistency: Alternatives must be viable before they are rejected
intent_viable_alternative(crisis_incubation, madelyn_antoncic, 'Opposed risky investments').
intent_alternative_rejected(crisis_incubation, madelyn_antoncic, 'Opposed risky investments').

% --- 7. RECOMMENDATIONS ---
recommendation(r1, 'Terminate Repo 105 usage and expose true leverage').
affects_constraint(r1, repo_105).

recommendation(r2, 'Deploy SpinCo scaffold for illiquid commercial assets').
affects_constraint(r2, liquidity_pool).

veto_actor(fsa_uk).
veto_exposed(fsa_uk, r2).
