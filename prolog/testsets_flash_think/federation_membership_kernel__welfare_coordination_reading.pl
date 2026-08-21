% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement via National Welfare Coordination
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the EU's approach to free movement as operating
 *   through the coordination of national welfare systems, rather than full
 *   supranational harmonization. It focuses on the EU's role in enforcing
 *   anti-social-dumping rules to mitigate negative externalities of free
 *   movement, while preserving member states' autonomy in welfare design.
 *   This reading acknowledges that while coordination aims for collective
 *   benefit, it also generates specific costs and benefits, leading to an
 *   extractive dynamic for certain groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.75).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via National Welfare Coordination").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, 'c06d293e-239a-4386-a0cc-f052c2043ffa').
narrative_ontology:cs_kernel_codification('c06d293e-239a-4386-a0cc-f052c2043ffa', formalized).
narrative_ontology:cs_authority_grounding('c06d293e-239a-4386-a0cc-f052c2043ffa', lineage).
narrative_ontology:cs_interpretation_layer_present('c06d293e-239a-4386-a0cc-f052c2043ffa').
narrative_ontology:cs_reading_relation('c06d293e-239a-4386-a0cc-f052c2043ffa', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('c06d293e-239a-4386-a0cc-f052c2043ffa', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c06d293e-239a-4386-a0cc-f052c2043ffa', foundational, national_welfare_autonomy_preserved).
narrative_ontology:cs_axiom_status(national_welfare_autonomy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('c06d293e-239a-4386-a0cc-f052c2043ffa', national_welfare_autonomy_preserved, conventional).
narrative_ontology:cs_axiom('c06d293e-239a-4386-a0cc-f052c2043ffa', foundational, free_movement_with_social_safeguards).
narrative_ontology:cs_axiom_status(free_movement_with_social_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('c06d293e-239a-4386-a0cc-f052c2043ffa', free_movement_with_social_safeguards, instrumental).
narrative_ontology:cs_reference_frame('c06d293e-239a-4386-a0cc-f052c2043ffa', subsidiarity_principle_in_social_policy).
narrative_ontology:cs_drift_state('c06d293e-239a-4386-a0cc-f052c2043ffa', contemporary_challenges_of_social_dumping, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c06d293e-239a-4386-a0cc-f052c2043ffa', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_commission).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, national_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, employers_of_posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_member_states_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_member_states_fiscal_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces anti-social-dumping rules, aiming to balance free movement with national welfare autonomy. Benefits from maintaining the integrity of the single market and its legal framework.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Experience pressure from posted workers undercutting local wages and permanent migrants displacing local labor. Bear social costs without full fiscal compensation, leading to internal political tension.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_member_states_labor_markets, payer,
    organized, biographical, constrained, national).

% Lose skilled workers to other EU countries, impacting their tax base and social security contributions, without adequate fiscal transfers to compensate for the brain drain.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_member_states_fiscal_capacity, payer,
    organized, generational, constrained, national).

% Benefit from free movement but are often subject to wage undercutting and social levy exemptions (e.g., 2-year rule), making them vulnerable to exploitation and creating unfair competition in receiving states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, constrained, regional).

% Their design autonomy is preserved, avoiding supranational harmonization that might undermine national social contracts. This allows for diverse welfare models across the EU.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, national_welfare_systems, beneficiary,
    institutional, generational, constrained, national).

% Benefit from lower labor costs due to social levy exemptions and wage undercutting, enhancing their competitiveness, particularly in sectors like construction and transport (cabotage).
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, employers_of_posted_workers, beneficiary,
    powerful, biographical, mobile, regional).

% Interprets EU law related to free movement and social security coordination, shaping the boundaries of national autonomy and EU enforcement. Its rulings define the operational space of this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, employers_of_posted_workers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the principle of free movement of persons within the EU by allowing national welfare systems to retain their distinct designs, while attempting to prevent 'social dumping' through targeted enforcement.
% TRANSFER_FUNCTION: Transfers the benefits of labor mobility and market access to individuals and businesses, while transferring the costs of social dumping (wage undercutting, fiscal strain) to posted workers and receiving/sending member states.
% ABSENT_VOICES: Fully harmonized EU social security advocates, who would argue for a single, unified welfare system to eliminate social dumping entirely, are structurally absent from the current coordination-based policy debate.
% DISAPPEARANCE_RATIONALE: If this coordination framework vanished, free movement would either collapse due to unmanageable social dumping and national protectionism, or it would force rapid, potentially disruptive, supranational harmonization of welfare systems.
% FOUNDING_PROBLEM: To enable free movement of workers across the EU while respecting the diversity and autonomy of national welfare states, preventing a 'race to the bottom' in social standards.
% FOUNDING_PROBLEM_CORROBORATION: EU institutions, national governments, and academic researchers consistently highlight the ongoing challenge of balancing free movement with social protection, citing persistent issues like social dumping and labor market distortions. This is corroborated by numerous policy debates and legislative initiatives.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the current coordination model, despite anti-social-dumping rules, still allows for significant cost-competition posting, wage undercutting, and fiscal strain on member states. Suppression (0.75) is substantial as the EU actively enforces rules to prevent outright social dumping, but also suppresses full welfare harmonization. Theater ratio is low (0.20) as the enforcement mechanisms are genuinely active, though their effectiveness in fully mitigating extraction is debated. The increasing trend in extractiveness and suppression reflects the growing challenges and enforcement efforts over time, particularly with EU enlargements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions and national welfare systems, this is a necessary coordination mechanism. However, from the perspective of posted workers and affected labor markets, it functions as a system that enables significant extraction and social costs. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The EU Commission and national welfare systems are beneficiaries, as the former maintains market integrity and the latter preserves autonomy. Employers of posted workers are also beneficiaries, gaining from lower labor costs. Posted workers, receiving state labor markets, and sending state fiscal capacities are victims, bearing the costs of wage undercutting, displacement, and worker loss respectively. The structural delta highlights these specific victim groups.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainability_of_coordination_model,
    'Is the current coordination model, with its anti-social-dumping rules, genuinely sustainable in preventing a ''race to the bottom'' in social standards, or does it inherently lead to persistent social dumping and labor market distortions?',
    'Longitudinal empirical studies comparing labor market outcomes and social protection levels in member states under the current regime versus hypothetical scenarios of either full harmonization or stricter national controls.',
    'If unsustainable, the constraint''s effective extractiveness is higher than measured, and its claimed coordination function is largely theatrical, pushing it towards a Snare. If sustainable, the coordination function is robust, supporting a Rope or Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_of_coordination_model, empirical, 'Whether the coordination model effectively mitigates social dumping or merely manages its symptoms.').

omega_variable(
    fiscal_compensation_adequacy,
    'Are the fiscal compensation mechanisms for sending and receiving member states adequate to offset the costs associated with free movement and posted workers?',
    'Detailed economic analysis of fiscal flows and social costs, including a comparison of current EU funds and national budgets against estimated costs of worker loss and social service provision.',
    'If compensation is inadequate, the extraction from sending/receiving states is higher than currently accounted for, strengthening the Snare-like aspects of the constraint. If adequate, it supports the coordination narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_compensation_adequacy, empirical, 'Adequacy of fiscal compensation for free movement externalities.').

omega_variable(
    framing_of_free_movement_benefits,
    'Does the ''welfare coordination'' framing genuinely prioritize the balance of national autonomy and free movement, or does it serve as a cover for maintaining a flexible labor market that benefits specific employers?',
    'Analysis of legislative intent, lobbying efforts, and the distribution of economic gains from free movement, particularly focusing on the influence of employer groups versus social partners.',
    'If primarily a cover, the constraint''s claimed coordination function is largely theatrical, and its true nature is closer to a Snare, with the ''coordination'' narrative masking extraction. If genuine, it supports the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_free_movement_benefits, conceptual, 'Conceptual ambiguity between genuine coordination and extractive framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1992, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(fede_tr_t2000, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t1992, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(fede_be_t2000, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1992, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(fede_su_t2000, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
