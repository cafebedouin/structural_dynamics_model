% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria (Declaratory Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story describes the 'declaratory' reading of the
 *   Montevideo Convention criteria for statehood. Under this reading,
 *   statehood is an objective legal fact established by meeting four criteria
 *   (permanent population, defined territory, government, capacity to enter
 *   into relations with other states), irrespective of recognition by
 *   existing states. This reading minimizes extraction by existing states, as
 *   they lose the power to arbitrarily grant or deny statehood. It functions
 *   as a coordination mechanism for international law, providing clear rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.2).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.1).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '67eb9e29-5010-4315-ad43-41bd9b4995fe').
narrative_ontology:cs_kernel_codification('67eb9e29-5010-4315-ad43-41bd9b4995fe', formalized).
narrative_ontology:cs_authority_grounding('67eb9e29-5010-4315-ad43-41bd9b4995fe', lineage).
narrative_ontology:cs_interpretation_layer_present('67eb9e29-5010-4315-ad43-41bd9b4995fe').
narrative_ontology:cs_reading_relation('67eb9e29-5010-4315-ad43-41bd9b4995fe', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_reading_relation('67eb9e29-5010-4315-ad43-41bd9b4995fe', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('67eb9e29-5010-4315-ad43-41bd9b4995fe', foundational, statehood_is_objective_fact).
narrative_ontology:cs_axiom_status(statehood_is_objective_fact, holdable).
narrative_ontology:cs_axiom_grounding('67eb9e29-5010-4315-ad43-41bd9b4995fe', statehood_is_objective_fact, deontological).
narrative_ontology:cs_axiom('67eb9e29-5010-4315-ad43-41bd9b4995fe', foundational, recognition_is_declaratory_only).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_only, holdable).
narrative_ontology:cs_axiom_grounding('67eb9e29-5010-4315-ad43-41bd9b4995fe', recognition_is_declaratory_only, conventional).
narrative_ontology:cs_reference_frame('67eb9e29-5010-4315-ad43-41bd9b4995fe', montevideo_convention_text).
narrative_ontology:cs_drift_state('67eb9e29-5010-4315-ad43-41bd9b4995fe', contemporary_international_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('67eb9e29-5010-4315-ad43-41bd9b4995fe', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_law_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, existing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities, often emerging from secession or civil conflict, benefit from the declaratory reading as it grants them legal statehood upon meeting objective criteria, regardless of external recognition. This strengthens their claim to sovereignty and international legal personality.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    moderate, biographical, constrained, local).

% Existing states, particularly those with historical ties or strategic interests, may find their leverage to condition recognition diminished. They are 'payers' in the sense that they lose a tool of foreign policy and influence, as statehood is not contingent on their consent.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states, payer,
    institutional, generational, mobile, global).

% Scholars who advocate for a rules-based international order benefit from this reading as it emphasizes objective legal criteria over political discretion, reinforcing the self-executing nature of international law.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_scholars, beneficiary,
    analytical, generational, analytical, universal).

% These bodies, such as the UN, are tasked with applying international law. The declaratory reading simplifies their task by providing clear, objective criteria for statehood, reducing political disputes over membership and legal status.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_organizations, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, objective, and universal standard for determining statehood, reducing arbitrary political decisions and fostering stability in international relations by defining who is a state.
% TRANSFER_FUNCTION: Transfers the power to confer statehood from the political discretion of existing states to the objective fulfillment of legal criteria. It transfers legal personality and rights to entities meeting the criteria.
% ABSENT_VOICES: Existing states that wish to retain political leverage over recognition, or those who believe statehood should be contingent on normative criteria (e.g., human rights record), would object. Their voices are often present in debates but are structurally sidelined by the declaratory principle.
% DISAPPEARANCE_RATIONALE: If the declaratory reading vanished, statehood would revert to being purely a matter of political recognition, leading to increased instability, arbitrary decisions, and a lack of clear legal status for many entities. The international system would lose a foundational principle for order.
% FOUNDING_PROBLEM: The problem of arbitrary and politically motivated recognition of states, leading to instability, conflicts, and a lack of clear legal status for entities that objectively controlled territory and population.
% FOUNDING_PROBLEM_CORROBORATION: International legal texts (e.g., Montevideo Convention), UN General Assembly resolutions, and the consistent practice of many states and international organizations attest to the ongoing relevance of objective criteria. Legal scholars widely corroborate the problem of arbitrary recognition.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).
:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because this reading primarily serves to coordinate international legal understanding and limits the power of existing states to extract political concessions for recognition. Suppression is also low (0.1) as it's a legal principle, not actively enforced coercion. Theater ratio is minimal (0.05) as the criteria are generally applied directly. The slight increase in extractiveness and suppression over time reflects the ongoing political contestation and attempts by some states to reassert a constitutive element, even against the declaratory principle.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of de facto authorities, this is a pure Rope, providing essential legal status. From the perspective of existing states that prefer to retain political leverage, it might be seen as a constraint on their foreign policy, though still a net benefit for international order. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities are beneficiaries (d near 0.0) as they gain legal status without needing external political approval. International law scholars also benefit by having a clear, objective framework. Existing states, particularly those with an interest in conditioning recognition, are 'payers' (d near 0.5) as they lose a tool of influence, though they also benefit from the overall stability of clear rules.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_legal_fact,
    'To what extent is statehood truly a legal fact, and to what extent does political recognition still de facto determine an entity''s international standing?',
    'Empirical analysis of entities meeting Montevideo criteria but lacking widespread recognition (e.g., Taiwan, Kosovo, Palestine): track their participation in international forums, treaty-making capacity, and diplomatic relations.',
    'If political recognition consistently overrides objective criteria, the declaratory reading''s effective extractiveness (from de facto authorities) and suppression (of their full participation) would be higher, pushing it towards a Tangled Rope or Snare in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_vs_legal_fact, empirical, 'Ambiguity between legal principle and political reality in statehood.').

omega_variable(
    normative_criteria_creep,
    'Is there an unacknowledged ''creep'' of normative criteria (e.g., democracy, human rights) into the objective Montevideo criteria, effectively creating a de facto hybrid reading?',
    'Content analysis of state practice and international legal pronouncements regarding new state formations, looking for implicit or explicit conditioning of recognition on normative factors.',
    'If normative criteria are implicitly applied, the declaratory reading''s purity as a Rope would be compromised, and it would function more like a Tangled Rope or Snare for entities failing those unstated normative tests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_creep, conceptual, 'Implicit inclusion of normative criteria in objective statehood assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.1).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.05).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Montevideo Statehood Criteria' kernel, alongside the 'constitutive_reading' and 'hybrid_reading'. Each reading defines statehood differently, leading to distinct structural implications for international law and political actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
