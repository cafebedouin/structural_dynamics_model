% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold (Minoritarian Veto Reading)
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint represents a 'minoritarian veto' reading of a
 *   supermajority threshold, where the threshold, originally intended to
 *   safeguard consensus, has become a tool for blocking minorities to
 *   entrench the status quo. This reading views the threshold as a snare,
 *   actively extracting from contemporary majorities by denying them the
 *   ability to enact necessary reforms. The claimed type 'snare' reflects
 *   this interpretation, while the metrics capture the high extractiveness
 *   and suppression inherent in this dynamic. This is one reading of the
 *   'supermajority_threshold' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.85).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold (Minoritarian Veto Reading)").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '8e3343c1-ca09-4e34-a784-5b6004e29aeb').
narrative_ontology:cs_kernel_codification('8e3343c1-ca09-4e34-a784-5b6004e29aeb', fixed_text).
narrative_ontology:cs_authority_grounding('8e3343c1-ca09-4e34-a784-5b6004e29aeb', lineage).
narrative_ontology:cs_interpretation_layer_present('8e3343c1-ca09-4e34-a784-5b6004e29aeb').
narrative_ontology:cs_reading_relation('8e3343c1-ca09-4e34-a784-5b6004e29aeb', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e3343c1-ca09-4e34-a784-5b6004e29aeb', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('8e3343c1-ca09-4e34-a784-5b6004e29aeb', foundational, majority_rule_is_foundational).
narrative_ontology:cs_axiom_status(majority_rule_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('8e3343c1-ca09-4e34-a784-5b6004e29aeb', majority_rule_is_foundational, deontological).
narrative_ontology:cs_axiom('8e3343c1-ca09-4e34-a784-5b6004e29aeb', foundational, constitutional_amendment_must_be_responsive_to_contemporary_will).
narrative_ontology:cs_axiom_status(constitutional_amendment_must_be_responsive_to_contemporary_will, holdable).
narrative_ontology:cs_axiom_grounding('8e3343c1-ca09-4e34-a784-5b6004e29aeb', constitutional_amendment_must_be_responsive_to_contemporary_will, deontological).
narrative_ontology:cs_reference_frame('8e3343c1-ca09-4e34-a784-5b6004e29aeb', democratic_responsiveness_framework).
narrative_ontology:cs_drift_state('8e3343c1-ca09-4e34-a784-5b6004e29aeb', contemporary_political_gridlock_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8e3343c1-ca09-4e34-a784-5b6004e29aeb', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit directly from the existing distribution of power and resources, which the supermajority threshold protects from change. They actively lobby against reforms and leverage their institutional positions to maintain the status quo, effectively wielding a veto power.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    institutional, generational, arbitrage, national).

% Various organized interests (e.g., specific industries, regional blocs) that gain from the current legal and economic framework. They may not be 'elites' but their interests are aligned with preventing change, and they contribute to the blocking minority.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    organized, biographical, constrained, national).

% The majority of the populace whose will is consistently thwarted by the supermajority requirement. They bear the costs of unmet policy needs and democratic frustration, with no effective means to overcome the entrenched veto.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, immediate, trapped, national).

% Activists, political movements, and parties pushing for constitutional or systemic reforms. They expend significant resources and political capital attempting to build supermajorities, often unsuccessfully, leading to burnout and disillusionment.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_advocates, payer,
    moderate, generational, constrained, national).

% Analyze the democratic legitimacy and functional consequences of supermajority rules, often highlighting the disjuncture between original intent and contemporary effect. Their analysis informs public debate but does not directly alter the constraint.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The supermajority threshold nominally coordinates broad political consensus for fundamental changes, ensuring stability and protecting minority rights against transient majorities.
% TRANSFER_FUNCTION: Transfers effective veto power over constitutional or fundamental legal changes from a simple majority to a blocking minority, thereby preserving existing distributions of power and resources for the beneficiaries.
% ABSENT_VOICES: Future generations, who are bound by the entrenched status quo but have no voice in its amendment, are fundamentally excluded. Disenfranchised or marginalized groups whose interests are consistently overridden by the blocking minority are also effectively absent from the decision-making process.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished, the political landscape would immediately shift. Majorities would be empowered to enact reforms, potentially leading to significant changes in policy, resource allocation, and institutional structures. The entrenched elites would lose their veto power, forcing a renegotiation of political power.
% FOUNDING_PROBLEM: The supermajority threshold was designed to prevent hasty, ill-considered changes to fundamental law, ensuring that constitutional amendments reflect a broad and enduring consensus, thereby protecting fundamental rights and institutional stability.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (often aligned with entrenched elites) argue the problem of protecting fundamental law from transient majorities is still live. Critics (contemporary majorities, reform advocates, and many constitutional scholars) argue that while the original problem was valid, the threshold has become an instrument of minoritarian entrenchment, making the original problem 'dead' in its current application, or at least 'contested' in its effect.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the threshold effectively transfers power from the majority to a minority, allowing the latter to maintain beneficial arrangements at the cost of the former. Suppression (0.78) is also high, as the mechanism actively prevents the majority's will from being enacted, suppressing alternative policy outcomes. Theater ratio is low (0.20) because the constraint is highly functional in its extractive role, even if its stated purpose (consensus building) is largely performative in this reading. The rising extractiveness and suppression over time reflect the increasing entrenchment of the status quo and the growing frustration of majorities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of entrenched elites, the supermajority threshold is a legitimate safeguard of stability and property rights (a 'rope' or even 'mountain' of constitutional design). From the perspective of contemporary majorities, it is a 'snare' that actively extracts their democratic agency. The engine's classification will highlight this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites and status quo beneficiaries are clear beneficiaries (d near 0.0), as the threshold protects their interests. Contemporary majorities and reform advocates are the primary victims/targets (d near 1.0), as their efforts to enact change are consistently blocked. Constitutional scholars act as observers, analyzing the mechanism without direct participation in its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_contemporary_effect,
    'Is the supermajority threshold still serving its original purpose of ensuring broad consensus and protecting minorities, or has its primary effect shifted to entrenching the status quo and empowering blocking minorities?',
    'Empirical analysis of legislative outcomes over time, comparing the types of proposals blocked by supermajority rules against the stated intent of the threshold''s designers. Examination of the demographic and economic profiles of blocking minorities versus the broader populace.',
    'If the effect has shifted, it strengthens the ''snare'' classification and calls for institutional reform. If it still primarily serves its original purpose, it would weaken the ''snare'' classification and support a ''rope'' or ''tangled_rope'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_contemporary_effect, empirical, 'Assesses the functional drift of the supermajority threshold from its original design intent.').

omega_variable(
    democratic_legitimacy_framing,
    'Is the protection of a blocking minority''s interests, even against a contemporary majority, a legitimate function of constitutional design, or does it fundamentally undermine democratic principles?',
    'Conceptual analysis of competing theories of democracy (e.g., majoritarian vs. consensual models, constitutionalism vs. popular sovereignty). This is a normative choice, not an empirical one.',
    'If protecting blocking minorities is deemed legitimate, the ''snare'' classification might be re-evaluated as a ''tangled_rope'' (coordination with extraction, but extraction is justified by a higher-order value). If it undermines democracy, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_framing, preference, 'Examines the normative justification for supermajority rules in a democratic context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'supermajority_threshold' kernel. This 'minoritarian_veto_reading' focuses on the extractive and anti-democratic aspects, contrasting with the 'consensus_safeguard_reading' (which emphasizes stability) and the 'adaptive_gradient_reading' (which focuses on functional calibration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
