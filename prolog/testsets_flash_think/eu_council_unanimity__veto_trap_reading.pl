% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity: Veto Trap Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story analyzes the EU Council's unanimity rule through
 *   the 'veto trap' reading, where it functions as a structural vulnerability
 *   enabling minoritarian extraction. While the rule is formally presented as
 *   a safeguard for national sovereignty, this reading focuses on its
 *   observed effect: systematically transferring value (policy concessions,
 *   opt-outs, financial benefits) from the majority preference to a blocking
 *   minority through credible threats. The high extractiveness and
 *   suppression metrics reflect the coercive power of the veto threat in
 *   practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.8).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.85).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity: Veto Trap Reading").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'a266de9c-275a-4ff2-ba04-22b0d7ebca64').
narrative_ontology:cs_kernel_codification('a266de9c-275a-4ff2-ba04-22b0d7ebca64', formalized).
narrative_ontology:cs_authority_grounding('a266de9c-275a-4ff2-ba04-22b0d7ebca64', practice).
narrative_ontology:cs_interpretation_layer_present('a266de9c-275a-4ff2-ba04-22b0d7ebca64').
narrative_ontology:cs_reading_relation('a266de9c-275a-4ff2-ba04-22b0d7ebca64', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('a266de9c-275a-4ff2-ba04-22b0d7ebca64', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('a266de9c-275a-4ff2-ba04-22b0d7ebca64', foundational, unanimity_enables_minority_extraction).
narrative_ontology:cs_axiom_status(unanimity_enables_minority_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a266de9c-275a-4ff2-ba04-22b0d7ebca64', unanimity_enables_minority_extraction, empirically_contingent).
narrative_ontology:cs_axiom('a266de9c-275a-4ff2-ba04-22b0d7ebca64', secondary, credible_blocking_threat_transfers_value).
narrative_ontology:cs_axiom_status(credible_blocking_threat_transfers_value, holdable).
narrative_ontology:cs_axiom_grounding('a266de9c-275a-4ff2-ba04-22b0d7ebca64', credible_blocking_threat_transfers_value, empirically_contingent).
narrative_ontology:cs_reference_frame('a266de9c-275a-4ff2-ba04-22b0d7ebca64', minority_leverage_framework).
narrative_ontology:cs_drift_state('a266de9c-275a-4ff2-ba04-22b0d7ebca64', contemporary_eu_policy_making, gap(stable, minor, false)).
narrative_ontology:cs_created_at('a266de9c-275a-4ff2-ba04-22b0d7ebca64', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, majority_coalition_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A single member state that uses the unanimity rule to block proposed EU legislation or policy, thereby extracting concessions, opt-outs, or other benefits from the majority coalition. Their leverage comes from the credible threat of preventing any collective action.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter,
    institutional, biographical, arbitrage, national).

% The group of member states that support a proposed policy but are forced to make concessions to a blocking state to achieve any collective action. They bear the cost of delayed or diluted policy outcomes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, majority_coalition_member_states, payer,
    organized, biographical, constrained, continental).

% Proposes legislation and seeks to advance the EU's collective agenda, but its proposals are subject to the unanimity rule in the Council. It often acts as a mediator in disputes arising from veto threats.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission, observer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, eu_commission, agenda_setter).

% Bear the diffuse costs of policy inaction, delays, or suboptimal compromises resulting from the veto trap. They experience the EU as less effective or responsive due to minoritarian blocking.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens, payer,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all member states, particularly on matters touching upon national sovereignty or vital interests, must consent to collective action, theoretically fostering deeper integration through unanimous buy-in.
% TRANSFER_FUNCTION: Transfers policy concessions, derogations, or other political and economic benefits from the collective will of the majority to the specific interests of a blocking minority (often a single state).
% ABSENT_VOICES: EU citizens who desire more efficient, unified, and responsive European action, unhindered by minoritarian blocking. Their voices are often aggregated through national political systems, but the unanimity rule can override these aggregated preferences.
% DISAPPEARANCE_RATIONALE: If the unanimity rule in the Council vanished overnight, decision-making would fundamentally shift towards qualified majority voting, drastically altering power dynamics, accelerating policy adoption, and removing the leverage of single states to block collective action. The EU's institutional balance would be profoundly reorganized.
% FOUNDING_PROBLEM: To prevent majoritarian coercion and safeguard the national sovereignty of member states within a supranational political entity, ensuring that no state is forced into collective action against its vital interests.
% FOUNDING_PROBLEM_CORROBORATION: Blocking member states and their political allies consistently attest that the founding problem of sovereignty protection remains live and critical. However, EU institutions, academic analyses, and majority member states frequently argue that the rule is now primarily used for rent-seeking and minoritarian extraction, indicating the founding problem is either dead or its application has drifted significantly from its original intent.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.80) is high because the veto threat forces the majority to compromise on its preferred policies, often leading to diluted or delayed outcomes that benefit the blocking state at the expense of the collective. Suppression (0.85) is also high, as the unanimity rule effectively suppresses alternative policy paths that do not accommodate the blocking minority's demands. The theater ratio (0.15) is low, indicating that the threat of veto is a real and functional mechanism, not merely performative. Resistance (0.70) is substantial, as the majority coalition actively seeks ways to circumvent or mitigate the veto power, often through complex negotiations or 'coalitions of the willing' outside formal EU structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the blocking member state, the unanimity rule is a legitimate defense of national interests and sovereignty. From the perspective of the majority coalition and EU citizens, it is an extractive mechanism that hinders effective governance. The engine's classification will highlight this divergence, showing a Snare from the payer seats and a more benign type (e.g., Rope or even Mountain) from the blocking state's perspective, if that state's internal framing were modeled.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking member state is the clear beneficiary and agenda-setter, leveraging the rule to achieve its specific policy goals (low d). The majority coalition member states and EU citizens are the primary targets, bearing the costs of policy compromises and inaction (high d). The EU Commission, while an agenda-setter in proposing legislation, is constrained by the veto trap and acts more as an observer/mediator in this context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    purpose_ambiguity,
    'Is the EU Council''s unanimity rule primarily a safeguard for national sovereignty, or a tool for minoritarian extraction?',
    'Empirical analysis of veto usage patterns over time, quantifying the nature of concessions granted versus the stated sovereignty concerns, and comparing policy outcomes in unanimity vs. QMV domains.',
    'If primarily an extraction tool, the Snare classification is strongly reinforced. If primarily a sovereignty safeguard, the classification would shift towards a Rope or even a Mountain (from the perspective of national interest protection).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(purpose_ambiguity, conceptual, 'Ambiguity regarding the primary function of the unanimity rule.').

omega_variable(
    quantification_of_concessions,
    'How can the ''value'' of extracted concessions (policy changes, opt-outs, financial benefits) be consistently quantified across diverse policy domains?',
    'Development of a standardized methodology for valuing policy concessions, potentially using counterfactual modeling or expert elicitation, to provide a more precise measure of extractiveness.',
    'More precise quantification of concessions would refine the extractiveness metric, potentially revealing higher or lower levels of extraction than currently estimated, and strengthening the empirical basis for the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantification_of_concessions, empirical, 'Challenge in empirically measuring the value transferred by veto threats.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent is the suppression of alternative policies due to the formal unanimity rule itself, versus an internalized political culture among member states to avoid confrontation and seek consensus at all costs?',
    'Comparative institutional analysis with other international bodies having similar voting rules but different political cultures, or detailed qualitative studies of negotiation dynamics within the Council.',
    'If suppression is largely internalized, the constraint''s effective suppression might be higher than the structural measure suggests, as states self-censor proposals before they even reach the veto stage. If purely structural, removing the rule would immediately open policy space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for policy alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t4, eu_council_unanimity__veto_trap_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(eu_c_tr_t8, eu_council_unanimity__veto_trap_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__veto_trap_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__veto_trap_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(eu_c_be_t4, eu_council_unanimity__veto_trap_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(eu_c_be_t8, eu_council_unanimity__veto_trap_reading, base_extractiveness, 8, 0.73).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__veto_trap_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__veto_trap_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(eu_c_su_t4, eu_council_unanimity__veto_trap_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(eu_c_su_t8, eu_council_unanimity__veto_trap_reading, suppression_requirement, 8, 0.79).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__veto_trap_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(eu_c_su_t16, eu_council_unanimity__veto_trap_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
