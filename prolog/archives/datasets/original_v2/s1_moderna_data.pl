% 1. ENTITIES & INTERVALS
entity(moderna_inc, registrant).
entity(flagship_pioneering, scaffold). % Note: Primary transitional support/founder
entity(astra_zeneca, collaborator).
entity(merck_and_co, collaborator).
entity(fda_cber, regulatory_authority).

interval(inception_to_filing, 0, 10). % 0: 2010, 10: 2018 (S-1 Filing)

% 2. EVENTS & OMEGA VARIABLES
event(founding_2010, inception, 0, [founder(flagship_pioneering)]).
event(first_clinical_dose, milestone, 5, [program(mrna_1440), location(germany)]).
event(s1_filing, ipo_event, 10, [shares_offered(21739131), price_range(22, 24)]).

% Omega Identification (Reasoning Blockers)
omega_variable(omega_efficacy, empirical, 'Absence of Phase 3 clinical data for any mRNA-based medicine').
omega_variable(omega_classification, conceptual, 'Regulatory categorization as Gene Therapy vs. biological transient software').
omega_variable(omega_valuation, preference, 'Market judgment on long-term value vs. current cumulative deficit of 865M').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
constraint_claim(novelty_barrier, mountain). % Unprecedented nature of mRNA category
constraint_metric(novelty_barrier, intensity, 0.88).
constraint_metric(novelty_barrier, suppression_requirement, 0.92).

constraint_claim(capital_noose, snare). % High burn rate requires extraction of public capital
constraint_metric(capital_noose, extractiveness, 0.78). % R&D spend 410M vs zero product revenue
constraint_metric(capital_noose, suppression_requirement, 0.55).

constraint_claim(manufacturing_complexity, tangled_rope). % LNP delivery and mRNA stability
constraint_metric(manufacturing_complexity, snap_back_potential, 0.64).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_norwood_scale, 'Expand Norwood cGMP facility to mitigate batch failure risk').
affects_constraint(rec_norwood_scale, manufacturing_complexity).

recommendation(rec_diversified_modalities, 'Distribute biology risk across six distinct modalities').
affects_constraint(rec_diversified_modalities, novelty_barrier).

veto_actor(fda_cber).
veto_exposed(fda_cber, rec_diversified_modalities). % Regulatory hold can freeze category-wide progress

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
measurement(0, [0.15, 0.90, 0.05, 0.95]). % Inception: High Resilience (VC), Low Agency/Utility
measurement(10, [0.85, 0.45, 0.75, 0.80]). % Filing: High Agency/Utility, Low Stability (Losses)

% 6. INTENT EVIDENCE
intent_evidence(capital_access, [funding_round, ipo], [public_investors], 465500000).
% Benefit: Survival/Scaling; Delta: Transfer of risk to public markets.
[cite_start]``` [cite: 1, 6, 7, 11]
