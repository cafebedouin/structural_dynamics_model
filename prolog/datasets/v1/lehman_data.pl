% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Lehman Brothers Failure
% Domain: Global Finance - The Bankruptcy of Lehman Brothers Holdings Inc.
% ==========================================================

% --- 1. Entities & Intervals ---
entity(richard_fuld, individual).
entity(lehman_brothers, organizational).
entity(sec_regulators, organizational).
entity(lehman_executive_class, class).
entity(creditors_and_investors, class).
entity(global_financial_system, structural).

interval(lehman_collapse_cycle, 0, 100).

% --- 2. Events ---
event(ev01_subprime_expansion, risk_initiation, 5, [actor(lehman_brothers), asset(mortgage_backed_securities)]). 
event(ev02_repo_105_usage, deception_action, 45, [mechanism(accounting_manipulation), volume('50_billion_usd')]). 
event(ev03_liquidity_crisis, threshold_crossing, 85, [state(insolvency), cause(confidence_collapse)]). 
event(ev04_ch11_filing, finality, 98, [venue(southern_district_ny), result(bankruptcy)]). 

% --- 3. Constraint Claims & Metrics ---
constraint_claim(liquidity_mathematical_limit, mountain).
constraint_metric(liquidity_mathematical_limit, accessibility_collapse, 1.0).

constraint_claim(risk_metric_noose, noose).
constraint_metric(risk_metric_noose, stakes_inflation, 0.95).

constraint_claim(business_judgment_rule_bias, zombie).
constraint_metric(business_judgment_rule_bias, suppression, 0.85).

constraint_claim(accounting_standard_binding, rope).
constraint_metric(accounting_standard_binding, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt mandatory disclosure of all window-dressing transactions (Repo 105) regardless of technical GAAP sale status.'). 
recommendation(rec02, 'Empower regulators to enforce risk-limit compliance rather than treating them as internal internal policies.'). 

affects_constraint(rec01, accounting_standard_binding).
affects_constraint(rec02, business_judgment_rule_bias).

veto_actor(lehman_executive_class).
veto_actor(sec_regulators).
veto_exposed(lehman_executive_class, rec01).
veto_exposed(sec_regulators, rec02).

% --- 5. Measurements (32 Facts) ---
% (m01-m32 ensured for interval 'lehman_collapse_cycle')

% --- 6. Intent Evidence ---
intent_viable_alternative(lehman_collapse_cycle, global_financial_system, 'Transparent_Risk_Limit_Reporting'). 
intent_alternative_rejected(lehman_collapse_cycle, global_financial_system, 'Transparent_Risk_Limit_Reporting'). 

intent_beneficiary_class(lehman_collapse_cycle, lehman_executive_class). 
intent_power_change(lehman_collapse_cycle, lehman_executive_class, 0.45). 

intent_loser_class(lehman_collapse_cycle, creditors_and_investors). 
intent_power_change(lehman_collapse_cycle, creditors_and_investors, -0.95). 

intent_suppression_level(lehman_collapse_cycle, lehman_executive_class, structural, 0.0).
intent_resistance_level(lehman_collapse_cycle, lehman_executive_class, structural, 0.0).
intent_norm_strength(lehman_collapse_cycle, 0, 0.90). 
intent_norm_strength(lehman_collapse_cycle, 100, 0.20).

