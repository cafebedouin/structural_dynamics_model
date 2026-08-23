% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Sovereignty-Maximalist Reading
 *   domain: international_relations/law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-maximalist reading of
 *   the contested RBIO (Rules-Based International Order) practice norm
 *   complex. Under this reading, state sovereignty is absolute and
 *   non-derogable; RBIO norms are legitimate solely when they protect states
 *   from external interference, and humanitarian exceptions are treated as
 *   pretexts for regime change. The constraint coordinates state behavior
 *   around non-interference, solving the collective-action problem of
 *   preventing constant great-power intervention, but asymmetrically extracts
 *   from repressed populations by denying them external recourse and
 *   shielding authoritarian regimes. The story is authored as a kernel
 *   reading: the structural data reflect only this reading's
 *   beneficiary/victim structure, not an average over sibling readings.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes (beneficiary): Shielded from intervention by absolute sovereignty claims.
 *   - Repressed populations (payer): Bear the cost of blocked external recourse under repressive governments.
 *   - Great-power veto holders (agenda_setter): Administer the constraint via UNSC and diplomatic enforcement.
 *   - Liberal democratic states (observer): Blocked humanitarian intervention agenda, bear political costs of inaction.
 *   - Human rights advocates (excluded): Structurally muted in sovereignty-adjudicating institutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.78).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Sovereignty-Maximalist Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '03074958-09f5-4c53-a32b-0b179627394c').
narrative_ontology:cs_kernel_codification('03074958-09f5-4c53-a32b-0b179627394c', fixed_text).
narrative_ontology:cs_authority_grounding('03074958-09f5-4c53-a32b-0b179627394c', lineage).
narrative_ontology:cs_interpretation_layer_present('03074958-09f5-4c53-a32b-0b179627394c').
narrative_ontology:cs_reading_relation('03074958-09f5-4c53-a32b-0b179627394c', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('03074958-09f5-4c53-a32b-0b179627394c', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('03074958-09f5-4c53-a32b-0b179627394c', foundational, state_sovereignty_as_supreme_norm).
narrative_ontology:cs_axiom_status(state_sovereignty_as_supreme_norm, holdable).
narrative_ontology:cs_axiom_grounding('03074958-09f5-4c53-a32b-0b179627394c', state_sovereignty_as_supreme_norm, conventional).
narrative_ontology:cs_axiom('03074958-09f5-4c53-a32b-0b179627394c', foundational, humanitarian_intervention_pretext_thesis).
narrative_ontology:cs_axiom_status(humanitarian_intervention_pretext_thesis, holdable).
narrative_ontology:cs_axiom_grounding('03074958-09f5-4c53-a32b-0b179627394c', humanitarian_intervention_pretext_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('03074958-09f5-4c53-a32b-0b179627394c', westphalian_nonintervention_order).
narrative_ontology:cs_drift_state('03074958-09f5-4c53-a32b-0b179627394c', contemporary_rbio_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('03074958-09f5-4c53-a32b-0b179627394c', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, repressed_populations).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_intervention_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shielded from external intervention by the absolute sovereignty norm. They invoke territorial integrity and non-interference to deflect humanitarian scrutiny, conditionality, and accountability. Their statehood is the credential that bars external recourse.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, global).

% Live under repressive governments with no external recourse because the sovereignty-maximalist reading delegitimizes humanitarian intervention and conditions any international engagement on state consent that the regime can withhold. Exit via asylum is costly and uncertain; internal exit is blocked by the regime.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, repressed_populations, payer,
    powerless, immediate, trapped, national).

% Administer the constraint through UNSC veto power and diplomatic enforcement of non-intervention. They set the agenda for what counts as legitimate international order, using sovereignty maximalism to block enforcement actions against aligned authoritarian states while preserving freedom of action in their own spheres.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, great_power_veto_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Advance norms of humanitarian intervention and responsibility to protect, but are blocked by the sovereignty-maximalist veto coalition. They bear the political cost of watching atrocities persist without a legal pathway to act, yet retain power to shape discourse outside the veto chamber.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_democratic_states, observer,
    institutional, generational, analytical, global).

% Would argue for humanitarian exceptions and international recourse for repressed populations, but are structurally excluded from the agenda-setting institutions where sovereignty claims are adjudicated. Their voices are present in discourse but absent from the veto chamber.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior around mutual non-interference, preventing constant great-power intervention and territorial revisionism by establishing a default rule of absolute sovereignty.
% TRANSFER_FUNCTION: Transfers the cost of repression from the international communityâwhich is barred from actingâto populations living under authoritarian regimes, while transferring the benefit of impunity to the regimes that rule them.
% ABSENT_VOICES: Repressed populations are not represented in the institutions that adjudicate sovereignty claims; human rights advocates are present in discourse but excluded from the veto-wielding agenda-setting bodies. Liberal states voice objections but are overridden.
% DISAPPEARANCE_RATIONALE: If the sovereignty-maximalist reading vanished, the legal barrier to humanitarian intervention would drop, authoritarian regimes would lose their shield against external conditionality, and the international order would shift toward liberal institutional or hegemonic readingsârepressed populations would gain external recourse, while veto holders and authoritarian regimes would lose a foundational tool of insulation.
% FOUNDING_PROBLEM: The Thirty Years' War and subsequent great-power conflicts demonstrated that unchecked interventionism produces endemic warfare and territorial instability; the Westphalian system was built to solve the problem of constant religious and dynastic intervention across borders.
% FOUNDING_PROBLEM_CORROBORATION: Historians of international relations attest the founding problemâintervention-driven warfareâwas real. However, human rights organizations and liberal states attest the problem has mutated: the arrangement now protects internal repression more than it prevents interstate war. The International Commission on Intervention and State Sovereignty documented the tension from outside the pure beneficiary camp.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the sovereignty-maximalist reading systematically transfers the cost of repression to trapped populations while granting impunity to authoritarian regimes. Suppression is higher (0.78) because the constraint's persistence depends on actively excluding humanitarian intervention alternatives through UNSC veto and diplomatic enforcement. Theater is moderate-high (0.55): the coordination functionâpreventing interstate war via non-interventionâis real, but an increasing share of sovereignty rhetoric performs regime insulation rather than genuine territorial defense. The measurement series run on a shared grid from 1975â2025, capturing the post-Cold War liberal challenge and subsequent authoritarian resurgence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (veto holders) and the beneficiary seat (authoritarian regimes) experience the constraint as protective and legitimate; the payer seat (repressed populations) experiences it as an externalized prison. The engine computes this divergence from the structural data: identical sovereignty norms produce negative effective extraction for shielded regimes and severe effective extraction for trapped populations. The liberal observer seat computes yet another type, seeing coordination decayed into extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes are the structural beneficiaries (low d, subsidized by the constraint's shield). Repressed populations are the structural targets (high d, extraction amplified by trapped exit and national scope). Great-power veto holders sit near the agenda-setter/beneficiary boundary: they administer the constraint and gain strategic flexibility from it, though their formal role is enforcement rather than direct rent collection. Liberal democratic states are observers whose humanitarian policy preferences are blocked; their d is derived from their non-beneficiary, non-target position. Human rights advocates are excluded, bearing no direct cost but structurally muted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemârunaway intervention producing endemic warâwas genuine, but the arrangement has outlived its corrective function and now primarily insulates domestic repression. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags that the constraint persists beyond its original mandate. However, because the coordination function (non-intervention) remains partially live and the extraction is asymmetric rather than pure, the computed type is tangled_rope rather than snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is this constraint a genuine normative commitment to sovereignty, or a constructed shield for authoritarian extraction?',
    'Comparative analysis across the three sibling readings of the rbio_practice_norm_complex kernel; empirical tracking of whether sovereignty-invocations correlate with regime type and domestic repression severity.',
    'If the sovereignty-maximalist reading is predominantly a cover for authoritarian regime security, effective extractiveness is higher than the coordination framing suggests; if it genuinely prevents great-power war, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the sovereignty-maximalist reading is principled non-intervention or authoritarian extraction.').

omega_variable(
    humanitarian_pretext_or_exception,
    'Are humanitarian exceptions structurally pretextual under sovereignty maximalism, or do genuine protective interventions still occur despite the norm?',
    'Case-by-case review of interventions labeled humanitarian versus non-intervention cases to establish whether the exception is applied strategically rather than principledly.',
    'If exceptions are consistently strategic, the constraint is more extractive; if principled exceptions persist, the extraction is moderated by genuine protective function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_pretext_or_exception, empirical, 'Whether humanitarian exceptions are pretextual or genuine.').

omega_variable(
    sovereignty_naturalness,
    'Is the sovereignty-maximalist norm an emergent feature of anarchic international politics, or an actively constructed and enforced ideology?',
    'Historical genealogy of sovereignty norms: track whether the norm persisted independently of great-power enforcement, or required repeated institutional reinforcement.',
    'If emergent, directionality is more symmetric; if constructed and enforced by great powers, directionality is asymmetrically extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_naturalness, conceptual, 'Whether sovereignty maximalism is constructed or emergent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(rbio_tr_t50, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(rbio_be_t50, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(rbio_su_t50, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested rbio_practice_norm_complex kernel. The kernel decomposes into at least three structurally distinct claims: a sovereignty-maximalist reading (high extraction, authoritarian beneficiaries), a liberal-institutional reading (coordination through consent), and a hegemonic-extraction reading (frozen institutional lock-in). Each reading carries a distinct epsilon, beneficiary structure, and classification. This decomposition follows the epsilon-invariance principle: the natural-language label 'RBIO' conflates multiple structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
