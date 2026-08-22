% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The Second Amendment's operative clause ('the right of the people to keep
 *   and bear Arms, shall not be infringed') is read by the individual-right
 *   reading as guaranteeing a personal right to possess and carry firearms
 *   for self-defense, independent of the prefatory militia clause. This
 *   constraint story models that reading as an active constitutional
 *   doctrine: it coordinates individual gun owners by providing a uniform
 *   legal shield against prohibitory regulation, while extracting from
 *   prohibited persons (felons, domestic abusers) who are categorically
 *   disarmed and from state legislatures whose regulatory authority is
 *   judicially suppressed. The reading competes with the collective-security
 *   reading (which conditions the right on militia service) and the
 *   originalist civic-virtue reading (which ties the right to citizen-soldier
 *   capacity). The constraint is not a natural law but a constructed legal
 *   interpretation that requires active judicial enforcement to maintain its
 *   preemptive effect.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/national) â gain legal protection for possession and carry
 *   - prohibited_persons: Primary target (powerless/trapped) â categorically excluded from the right, bear criminal penalties
 *   - federal_judiciary: Agenda-setter (institutional/national) â interprets and enforces the individual right against legislation
 *   - state_legislatures: Payer (institutional/constrained) â lose regulatory authority over firearms policy
 *   - gun_control_advocates: Excluded voice (organized/constrained) â seek prohibitory regimes foreclosed by the reading
 *   - constitutional_scholars: Analytical observer â debate original meaning and doctrinal coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, 'f4b00690-3b74-4b1e-977b-159e47e08afb').
narrative_ontology:cs_kernel_codification('f4b00690-3b74-4b1e-977b-159e47e08afb', fixed_text).
narrative_ontology:cs_authority_grounding('f4b00690-3b74-4b1e-977b-159e47e08afb', lineage).
narrative_ontology:cs_interpretation_layer_present('f4b00690-3b74-4b1e-977b-159e47e08afb').
narrative_ontology:cs_reading_relation('f4b00690-3b74-4b1e-977b-159e47e08afb', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('f4b00690-3b74-4b1e-977b-159e47e08afb', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('f4b00690-3b74-4b1e-977b-159e47e08afb', foundational, operative_clause_independent_right).
narrative_ontology:cs_axiom_status(operative_clause_independent_right, holdable).
narrative_ontology:cs_axiom_grounding('f4b00690-3b74-4b1e-977b-159e47e08afb', operative_clause_independent_right, conventional).
narrative_ontology:cs_axiom('f4b00690-3b74-4b1e-977b-159e47e08afb', foundational, personal_self_defense_core_purpose).
narrative_ontology:cs_axiom_status(personal_self_defense_core_purpose, holdable).
narrative_ontology:cs_axiom_grounding('f4b00690-3b74-4b1e-977b-159e47e08afb', personal_self_defense_core_purpose, deontological).
narrative_ontology:cs_reference_frame('f4b00690-3b74-4b1e-977b-159e47e08afb', individual_armed_self_defense_framework).
narrative_ontology:cs_drift_state('f4b00690-3b74-4b1e-977b-159e47e08afb', contemporary_post_bruen, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f4b00690-3b74-4b1e-977b-159e47e08afb', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, prohibited_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise the right to possess and carry firearms for self-defense; benefit from judicial invalidation of prohibitory laws and permit regimes; politically organized to defend the reading.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, mobile, national).

% Categorically excluded from the right by federal and state prohibitions targeting felons and domestic abusers; subject to criminal penalties for possession; no individualized rehabilitation pathway in many jurisdictions.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, prohibited_persons, payer,
    powerless, immediate, trapped, national).

% Interprets the Second Amendment to invalidate laws inconsistent with an individual right; sets doctrinal tests that constrain legislative innovation; administers the boundary of protected versus prohibited conduct.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Enact firearms regulations that are struck down or chilled by the individual right framework; lose authority to impose may-issue regimes, assault weapon bans, and sensitive-place restrictions.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_legislatures, payer,
    institutional, biographical, constrained, national).

% Advocate for permit requirements, registration, and categorical bans; structurally excluded from winning policy outcomes under the individual right reading because the constitutional frame forecloses their preferred regulatory alternatives.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% Analyze the original public meaning, history, and doctrinal implications; debate whether the reading is faithful to text or a modern construction.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform national legal framework for individual firearms possession for self-defense, preempting a patchwork of state prohibitions and creating legal certainty for gun owners.
% TRANSFER_FUNCTION: Moves legal authority and protective status from state legislatures and prohibited persons to individual gun owners, shielding possession from democratic regulation.
% ABSENT_VOICES: Gun control advocates and jurisdictions favoring may-issue permitting or blanket bans are structurally excluded; their preferred policies are foreclosed by the doctrinal framework. Prohibited persons lack political representation to challenge categorical exclusion.
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, state legislatures would regain authority to enact prohibitory regimes, may-issue permitting, and sensitive-place restrictions; the firearms policy landscape would fragment into divergent state regimes; individual gun owners would lose constitutional shield.
% FOUNDING_PROBLEM: Prevention of federal disarmament of the citizenry and preservation of state militia capacity against federal overreach.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the Founding era corroborate the militia-centric problem. The individual right reading's self-defense reframing is attested by some legal historians but contested by others; public health scholars and comparative law scholars outside the benefiting parties contest that the current arrangement solves a live self-defense problem relative to its costs.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the reading preempts democratic regulatory alternatives and categorically excludes a class of persons from a constitutional right without individualized process. Suppression (0.75) reflects the active judicial suppression of may-issue regimes, assault weapon bans, and sensitive-place restrictions. Theater ratio (0.32) captures the performative dimension of originalist rhetoric that sometimes substitutes for historical demonstration. Accessibility collapse (0.60) measures how completely regulatory alternatives collapse once the individual right frame is accepted. Resistance (0.80) is very high due to sustained state-level non-compliance, legislative workarounds, and political backlash.
 *
 * PERSPECTIVAL GAP:
 *   From the individual gun owner seat, the constraint is protective coordination that secures a fundamental right against majoritarian infringement. From the prohibited person seat, it is categorical exclusion backed by criminal penalties. From the state legislature seat, it is an externally imposed extraction of democratic authority. The engine computes this divergence from the structural data: same constitutional text, opposite directionalities depending on whether the agent is shielded or constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners are beneficiaries (d near 0.0): the constraint subsidizes their legal position and expands their option set. Prohibited persons are targets (d near 1.0): they are trapped by legal status and bear the costs of categorical disarmament. State legislatures are also targets (d ~0.75): their regulatory power is extracted by judicial review. The federal judiciary sits as agenda-setter with analytical exit, directionality near 0.5 symmetric because it both administers the constraint and is bound by interpretive methodology.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the Second Amendment was originally to preserve state militias against federal interference. The individual right reading extends that mandate to personal self-defense, a contested expansion. Mandatrophy is not declared resolved because the original militia-coordination function is arguably dead, while the self-defense coordination function is contested. The classification as tangled_rope captures that a genuine coordination function (legal certainty for self-defense) coexists with asymmetric extraction (disarmed populations, suppressed legislatures). A pure snare classification would miss the coordination; a pure rope classification would miss the extraction and categorical exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_empirical_status,
    'Does the historical record support the claim that the Second Amendment''s original public meaning protected an individual right to self-defense independent of militia service?',
    'Corpus linguistics analysis of 18th-century ''bear arms'' usage, militia statutes, and state constitutional analogues.',
    'If the empirical claim fails, the reading''s conventional authority weakens and its extraction profile rises (it becomes a modern construction imposed on the text). If it holds, the reading''s lineage grounding is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_empirical_status, empirical, 'Empirical uncertainty about original public meaning').

omega_variable(
    prohibited_persons_victim_status,
    'Are prohibited persons (felons, domestic abusers) structural victims of the individual right reading, or are they legitimately excluded from a privilege that creates no correlative duty?',
    'Cross-jurisdictional comparison of recidivism and domestic violence rates under different disarmament regimes; philosophical analysis of right versus privilege.',
    'If legitimately excluded, the victim set empties and the constraint shifts toward rope or mountain. If structurally victimized, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibited_persons_victim_status, conceptual, 'Whether categorical disarmament is victimization or legitimate boundary').

omega_variable(
    kernel_reading_position,
    'This constraint is the individual_right_reading of the second_amendment_text kernel; how would classification change under the collective_security_reading or originalist_civic_virtue_reading?',
    'Generate sibling constraint stories and compare structural deltas: collective_security would likely show state_legislatures as beneficiary and individual_gun_owners as constrained; civic virtue would show a different beneficiary set (citizen-soldiers).',
    'Under collective_security, the directionality reverses; the same constitutional text produces inverted extraction. Under civic virtue, the coordination function shifts from self-defense to martial capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel sibling reading structural delta').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__individual_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t2, second_amendment_text__individual_right_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(seco_tr_t4, second_amendment_text__individual_right_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(seco_tr_t8, second_amendment_text__individual_right_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(seco_tr_t12, second_amendment_text__individual_right_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(seco_tr_t16, second_amendment_text__individual_right_reading, theater_ratio, 16, 0.32).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__individual_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(seco_be_t2, second_amendment_text__individual_right_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(seco_be_t4, second_amendment_text__individual_right_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(seco_be_t8, second_amendment_text__individual_right_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(seco_be_t12, second_amendment_text__individual_right_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(seco_be_t16, second_amendment_text__individual_right_reading, base_extractiveness, 16, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__individual_right_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(seco_su_t2, second_amendment_text__individual_right_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(seco_su_t4, second_amendment_text__individual_right_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(seco_su_t8, second_amendment_text__individual_right_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(seco_su_t12, second_amendment_text__individual_right_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(seco_su_t16, second_amendment_text__individual_right_reading, suppression_requirement, 16, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_text kernel. The kernel decomposes into structurally distinct claims because the same text supports divergent epsilon profiles: the individual right reading (high extraction from prohibited persons and state legislatures), the collective security reading (high extraction from individual gun owners), and the civic virtue reading (different beneficiary set). Each reading is a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
