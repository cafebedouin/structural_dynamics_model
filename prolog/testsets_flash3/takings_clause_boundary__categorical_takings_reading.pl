% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Takings Clause Boundary: Categorical Takings Reading
 *   domain: constitutional_law/property_rights
 *
 * SUMMARY:
 *   This constraint represents the 'categorical takings' reading of the Fifth
 *   Amendment's Takings Clause, which holds that permanent physical
 *   occupations and regulations that eliminate all economically beneficial
 *   use of property are 'per se' takings requiring compensation. All other
 *   regulations are evaluated under the more flexible, fact-intensive Penn
 *   Central balancing test. This reading attempts to provide bright-line
 *   rules for extreme cases while preserving regulatory flexibility for the
 *   vast majority of government actions. It is one reading of the broader
 *   'takings_clause_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.45).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.3).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Takings Clause Boundary: Categorical Takings Reading").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'b0fe8645-85cc-4e4b-9c61-5070510b6611').
narrative_ontology:cs_kernel_codification('b0fe8645-85cc-4e4b-9c61-5070510b6611', fixed_text).
narrative_ontology:cs_authority_grounding('b0fe8645-85cc-4e4b-9c61-5070510b6611', lineage).
narrative_ontology:cs_interpretation_layer_present('b0fe8645-85cc-4e4b-9c61-5070510b6611').
narrative_ontology:cs_reading_relation('b0fe8645-85cc-4e4b-9c61-5070510b6611', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0fe8645-85cc-4e4b-9c61-5070510b6611', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('b0fe8645-85cc-4e4b-9c61-5070510b6611', foundational, property_rights_fundamental_but_not_absolute).
narrative_ontology:cs_axiom_status(property_rights_fundamental_but_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b0fe8645-85cc-4e4b-9c61-5070510b6611', property_rights_fundamental_but_not_absolute, deontological).
narrative_ontology:cs_axiom('b0fe8645-85cc-4e4b-9c61-5070510b6611', foundational, regulatory_power_essential_for_public_welfare).
narrative_ontology:cs_axiom_status(regulatory_power_essential_for_public_welfare, holdable).
narrative_ontology:cs_axiom_grounding('b0fe8645-85cc-4e4b-9c61-5070510b6611', regulatory_power_essential_for_public_welfare, instrumental).
narrative_ontology:cs_reference_frame('b0fe8645-85cc-4e4b-9c61-5070510b6611', penn_central_balancing_with_exceptions).
narrative_ontology:cs_drift_state('b0fe8645-85cc-4e4b-9c61-5070510b6611', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b0fe8645-85cc-4e4b-9c61-5070510b6611', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_extreme_cases).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators_flexibility).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_penn_central_cases).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, local_governments_regulatory_uncertainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These property owners benefit from clear, bright-line rules that guarantee compensation for permanent physical occupations or total value elimination, providing certainty in extreme scenarios.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_extreme_cases, beneficiary,
    moderate, biographical, constrained, local).

% These property owners face uncertainty and high litigation costs when their property is regulated but does not fall into the categorical takings rules, requiring a fact-intensive Penn Central analysis to determine if compensation is due.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_penn_central_cases, payer,
    moderate, biographical, constrained, local).

% Government regulators benefit from the flexibility to enact a wide range of regulations without triggering automatic compensation, as long as they avoid permanent physical occupations or total value elimination. This allows for broad public welfare regulation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators_flexibility, beneficiary,
    institutional, generational, constrained, national).

% Local governments bear the cost of litigation and uncertainty when their regulations are challenged under the Penn Central factors, as the outcome is less predictable than with categorical rules. This can chill legitimate regulatory action.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, local_governments_regulatory_uncertainty, payer,
    organized, biographical, constrained, local).

% The federal judiciary, particularly the Supreme Court, sets and interprets the rules for takings jurisprudence. They administer the framework, balancing property rights with public welfare, and their decisions shape the application of this reading.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Legal scholars analyze the coherence, fairness, and economic impact of takings jurisprudence, critiquing its application and proposing alternative frameworks. They influence future legal thought and potential judicial shifts.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for balancing private property rights against the government's power to regulate for public welfare, offering some predictability for property owners and flexibility for regulators.
% TRANSFER_FUNCTION: Transfers the cost of certain extreme regulatory burdens (permanent physical occupations, total value elimination) from individual property owners to the public (via compensation), while leaving the cost of other regulations on property owners.
% ABSENT_VOICES: Advocates for a broader interpretation of regulatory takings (e.g., 'goes too far' standard for all regulations) are present in legal discourse but their preferred bright-line rules for all significant value diminution are not adopted by this reading, leaving them to navigate the Penn Central factors.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for property rights and government regulation would become highly uncertain. Property owners would lose clear protections against extreme government actions, and regulators would face unpredictable challenges to their authority, leading to widespread litigation and a re-evaluation of land use and environmental laws.
% FOUNDING_PROBLEM: The problem of how to justly balance private property rights with the government's inherent power to regulate for public good, ensuring that individuals are not forced to bear public burdens alone.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, property rights advocates, and government attorneys all attest that the fundamental tension between private property and public regulation remains a live and complex problem, requiring ongoing judicial interpretation and legislative action.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).
:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while some property owners benefit from clear compensation rules, many others face the uncertainty and cost of the Penn Central test. Suppression is low (0.3) as property owners retain significant avenues for legal challenge, though the burden of proof can be high. Theater ratio is low (0.1) because the framework genuinely functions to adjudicate claims, even if its application is complex. The metrics reflect a system that provides some coordination (predictability for extreme cases) but also involves significant transfers and enforcement costs.
 *
 * PERSPECTIVAL GAP:
 *   Property owners whose regulations fall into the categorical rules perceive the constraint as a clear protection (rope-like), while those subject to Penn Central factors experience it as a complex, potentially extractive burden (tangled_rope-like). Regulators see it as a necessary balance, allowing most public welfare regulations to proceed (rope-like), but local governments facing Penn Central challenges may see it as an unpredictable snare. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners facing permanent physical occupations or total value elimination are beneficiaries (low d) due to guaranteed compensation. Government regulators benefit from broad flexibility (low d). Property owners in the Penn Central middle ground are payers (high d) due to litigation costs and uncertainty. Local governments also act as payers (high d) due to the risk of having to pay compensation or defend regulations. The federal judiciary acts as the agenda-setter, defining and enforcing the rules.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_predictability,
    'To what extent does the Penn Central balancing test provide predictable outcomes for property owners and regulators, versus remaining an ad hoc, unpredictable standard?',
    'Empirical analysis of Penn Central litigation outcomes over time, assessing consistency of judicial application and success rates for different types of regulatory challenges.',
    'If Penn Central is highly unpredictable, the ''categorical takings'' reading''s coordination function is weaker, increasing its effective extractiveness for property owners in the middle ground and making it more snare-like. If it offers reasonable predictability, its coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_predictability, empirical, 'Predictability of the Penn Central balancing test.').

omega_variable(
    scope_of_total_value_elimination,
    'How narrowly or broadly is ''total value elimination'' interpreted by courts, and does this interpretation effectively create a loophole for near-total but not absolute value loss?',
    'Judicial review of cases involving significant but not total economic impact, observing whether courts consistently find ''less than total'' value elimination to fall outside the categorical rule, or if they expand the definition.',
    'A narrow interpretation of ''total value elimination'' increases the effective extractiveness for property owners whose property value is severely diminished but not entirely eliminated, pushing the constraint towards a snare for them. A broader interpretation would reduce this extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_total_value_elimination, conceptual, 'Interpretation of ''total value elimination'' in categorical takings.').

omega_variable(
    mandatrophy_of_penn_central,
    'Has the Penn Central test, over time, become so complex and costly to litigate that its original function of balancing interests has atrophied into a de facto barrier to compensation for all but the wealthiest property owners?',
    'Analysis of litigation costs, duration, and success rates for Penn Central claims, particularly for small vs. large property owners, compared to the original intent of the test.',
    'If Penn Central has atrophied into a barrier, the constraint''s effective extractiveness for ''middle ground'' property owners is higher than measured, and its classification for them shifts closer to a snare or piton, as the process itself becomes the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_penn_central, empirical, 'Atrophy of the Penn Central balancing test into a barrier to compensation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.05).
narrative_ontology:measurement(taki_tr_t1992, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(taki_tr_t2005, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(taki_be_t1992, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1992, 0.4).
narrative_ontology:measurement(taki_be_t2005, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.25).
narrative_ontology:measurement(taki_su_t1992, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1992, 0.28).
narrative_ontology:measurement(taki_su_t2005, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2005, 0.29).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'takings_clause_boundary' kernel. Its bright-line rules for extreme cases and Penn Central balancing for others differentiate it from readings that focus solely on physical appropriation or broad regulatory impact. All readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
