% 1. ENTITIES & INTERVALS
entity(king_hammurabi, person).
entity(babylonian_state, organization).
entity(lex_talionis_foundation, concept).
entity(debt_slavery_system, structure).
entity(judicial_accountability, protocol).
entity(bankruptcy_scaffold, scaffold). % Transitional support for Snare dissolution

% Time interval represents the estimated reign: 2285 to 2242 B.C. (normalized to 0-43)
interval(reign_duration, 0, 43).

% 2. EVENTS & OMEGA VARIABLES
event(promulgation, decree, 0, [origin_at_sippara, divine_sanction_shamash]).
event(audit_snapshot, analysis, 43, [terminal_state_assessment]).

% Omega Identification (Ω)
% Ω_C: Ambiguity of social stratification terms (Gentleman vs. Poor Man vs. Slave)
omega_variable(stratification_definition, conceptual, 'Ambiguity in the legal boundary between amēlu and mushkēnu classes').
% Ω_E: Missing data regarding the actual rate of "Holy River" survival in §2
omega_variable(ordeal_survival_rate, empirical, 'Lack of archaeological/statistical data on river ordeal outcomes').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS

% MOUNTAIN (■C): Lex Talionis (§196 - Eye for an Eye)
constraint_claim(lex_talionis, mountain).
constraint_metric(lex_talionis, intensity, 1.0).
constraint_metric(lex_talionis, suppression_requirement, 0.04). % Verified <= 0.05
constraint_metric(lex_talionis, snap_back_potential, 0.0).

% ROPE (⊞C): Contractual Witness Requirements (§7)
constraint_claim(contract_witness, rope).
constraint_metric(contract_witness, extractiveness, 0.25). % Verified <= 0.35
constraint_metric(contract_witness, suppression_requirement, 0.10). % Verified <= 0.15

% TANGLED ROPE (⊠T): Class-Based Penalties (§198-201)
% Creates systemic friction by varying costs for the same injury based on class.
constraint_claim(class_variance, tangled_rope).
constraint_metric(class_variance, extractiveness, 0.55). % Triggering Reform range
constraint_metric(class_variance, suppression_requirement, 0.30).

% NOOSE (⊠C): Perpetual Debt Slavery (§117)
% Extreme extraction of labor/agency from family members.
constraint_claim(debt_slavery, snare).
constraint_metric(debt_slavery, extractiveness, 0.85). % Verified >= 0.66
constraint_metric(debt_slavery, suppression_requirement, 0.70). % Verified >= 0.46

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_01, 'Implement three-year term limit as a scaffold for debt release (§117).').
recommendation(rec_02, 'Reform class-based fines to uniform standards to reduce systemic friction.').

affects_constraint(rec_01, debt_slavery).
affects_constraint(rec_02, class_variance).

veto_actor(king_hammurabi).
veto_exposed(king_hammurabi, rec_02). % The King maintains class distinctions for social order.

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% t=0: High instability post-conquest, High Divine Utility
measurement(0, [0.2, 0.4, 0.8, 0.5]).
% t=43: Low Individual Agency, Maximum Systemic Stability
measurement(43, [0.1, 0.9, 0.7, 0.8]).

% 6. INTENT EVIDENCE
% Alternatives: Tribal blood feuds (replaced by State Lex Talionis).
% Beneficiaries: The Palace and Temple (centralized wealth and justice).
% Power Deltas: Shift from local community arbitration to centralized Royal "Judgments of Righteousness."
