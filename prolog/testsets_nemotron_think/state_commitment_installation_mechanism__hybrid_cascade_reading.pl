% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade: State-Initiated Commitment Installation Requiring Fringe Validation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the hybrid cascade mechanism of state commitment
 *   installation: apex actors (monarchs, revolutionary regimes, colonial
 *   administrations) install new commitments downward, but stabilization
 *   structurally requires fringe actors (local elites, intermediaries,
 *   communities) to validate, interpret, and enforce them. The coordination
 *   function is real — without fringe validation, commitments remain paper
 *   edicts; without apex initiation, fringe actors lack a common framework.
 *   The extraction is asymmetric — fringe actors bear disproportionate
 *   legitimation labor and coercion costs, while apex actors collect
 *   compliance rents. Partial resistance is absorbed through local
 *   interpretation, which both stabilizes the cascade and masks extraction.
 *   The claimed type is tangled_rope: genuine coordination + asymmetric
 *   extraction + active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.42).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade: State-Initiated Commitment Installation Requiring Fringe Validation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '04d9fa3c-a550-4d5d-93c9-8e7488281aff').
narrative_ontology:cs_kernel_codification('04d9fa3c-a550-4d5d-93c9-8e7488281aff', distributed).
narrative_ontology:cs_authority_grounding('04d9fa3c-a550-4d5d-93c9-8e7488281aff', practice).
narrative_ontology:cs_interpretation_layer_present('04d9fa3c-a550-4d5d-93c9-8e7488281aff').
narrative_ontology:cs_reading_relation('04d9fa3c-a550-4d5d-93c9-8e7488281aff', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('04d9fa3c-a550-4d5d-93c9-8e7488281aff', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_axiom('04d9fa3c-a550-4d5d-93c9-8e7488281aff', foundational, apex_installation_requires_fringe_validation_for_stabilization).
narrative_ontology:cs_axiom_status(apex_installation_requires_fringe_validation_for_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('04d9fa3c-a550-4d5d-93c9-8e7488281aff', apex_installation_requires_fringe_validation_for_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('04d9fa3c-a550-4d5d-93c9-8e7488281aff', secondary, local_interpretation_absorbs_resistance_and_enables_extraction).
narrative_ontology:cs_axiom_status(local_interpretation_absorbs_resistance_and_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('04d9fa3c-a550-4d5d-93c9-8e7488281aff', local_interpretation_absorbs_resistance_and_enables_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('04d9fa3c-a550-4d5d-93c9-8e7488281aff', classical_state_formation_bidirectional_legitimation).
narrative_ontology:cs_drift_state('04d9fa3c-a550-4d5d-93c9-8e7488281aff', contemporary_historical_sociology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('04d9fa3c-a550-4d5d-93c9-8e7488281aff', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex_actors).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimated_fringe_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, validating_fringe_actors).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, resistant_local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimated_fringe_elites).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, state_formation_requires_bidirectional_legitimation).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__hybrid_cascade_reading, cultural_authority_cascades_require_local_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monarchs, central bureaucracies, or revolutionary regimes that install new ideological, legal, or administrative commitments at the apex of the social order. They initiate cascades but cannot stabilize them without downstream acceptance. They collect compliance revenue and political stability when validation succeeds.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex_actors, agenda_setter,
    institutional, generational, arbitrage, national).

% Regional notables, religious intermediaries, or professional corps who adapt apex commitments to local conditions. They gain status and resources from successful validation but bear the cost of translation, enforcement, and absorbing local resistance. Their position depends on the cascade continuing.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimated_fringe_elites, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, legitimated_fringe_elites, payer).

% Local officials, clergy, guild masters, or village elders whose cooperation is structurally required for the commitment to take root. They must perform validation rituals, adjust customary practice, and suppress dissent — costs they bear without proportional gain. Exit means abandoning communal role-identity.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, validating_fringe_actors, payer,
    moderate, biographical, identity_locked, local).

% Peasantries, urban poor, or minority groups subjected to the installed commitment without meaningful voice. They bear extraction (taxes, labor, cultural suppression) and face coercion if they resist. Their exit options are near-zero; resistance is sporadic and crushed.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, resistant_local_communities, payer,
    powerless, immediate, trapped, local).

% Analysts who trace cascade patterns across cases (early modern Europe, Meiji Japan, postcolonial states). They see the structural necessity of fringe validation but disagree on whether the apex or the fringe is the primary causal motor. Their readings instantiate the kernel contest.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how a new commitment (religious orthodoxy, legal code, administrative reform) achieves society-wide stabilization without either pure imposition (which triggers rebellion) or pure emergence (which takes centuries). The two-phase cascade coordinates apex initiative with fringe absorption.
% TRANSFER_FUNCTION: Moves legitimation labor and adaptation costs from apex to fringe; moves stabilized authority and resource extraction rights from fringe to apex. Fringe actors pay with interpretive work and coercion of their subordinates; apex collects compliance and symbolic capital.
% ABSENT_VOICES: Subaltern groups who experience the commitment only as extraction — colonized peoples, enslaved populations, stateless minorities. They are structurally excluded from the validation bargain; their resistance is recorded as 'disorder' rather than as a veto on legitimacy.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade mechanism vanished, new commitments would either fail at installation (apex lacks enforcement reach) or stabilize only through centuries of endogenous climb (fringe lacks initiation capacity). State formation timelines would lengthen dramatically; revolutionary regimes would collapse or harden into pure coercion.
% FOUNDING_PROBLEM: Early modern states needed to install uniform commitments (tax codes, religious settlements, legal systems) across heterogeneous territories where apex enforcement capacity was thin. Pure imposition failed; pure emergence was too slow. The hybrid cascade was the discovered workaround.
% FOUNDING_PROBLEM_CORROBORATION: Tilly (1990) and Mann (1986) attest the coordination problem from state-centered sociology; Scott (1998) and Subrahmanyam (1990) attest the fringe perspective from below. No single school corroborates both phases simultaneously — the contested status reflects the kernel split.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the apex's net gain of stabilized authority at fringe expense. Suppression (0.42) is moderate — the cascade works partly through voluntary validation, not pure force, but resistance from trapped communities requires coercion. Theater ratio (0.28) captures the performative dimension: validation rituals, ceremonial compliance, and 'local interpretation' that increasingly serves apex interests. Accessibility collapse (0.45) is middling — alternatives (endogenous climb, exogenous imposition) remain thinkable but are structurally disadvantaged. Resistance (0.52) is significant — fringe actors and subaltern groups contest the terms of validation. Metrics measured at interval end (t=200) after cascade maturation.
 *
 * PERSPECTIVAL GAP:
 *   From the apex seat, the cascade looks like necessary coordination — the only way to achieve uniform commitment across distance and diversity. From the validating fringe seat, it looks like imposed labor — they must make the commitment work locally or lose their intermediary position. From the resistant community seat, it looks like predation — a foreign commitment enforced by local collaborators. The engine computes these as different constraint types per seat; the authored claim (tangled_rope) describes the aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   State apex actors are structural beneficiaries (d ~ 0.15): they initiate, control the commitment content, and collect compliance. Legitimated fringe elites sit near symmetric (d ~ 0.45): they gain status but pay validation costs. Validating fringe actors are targets (d ~ 0.75): identity-locked into validation roles, they bear labor and coercion costs. Resistant communities are full targets (d ~ 0.95): trapped, no voice, pure extraction. Historical sociologists are analytical (d = 0.5 by definition). The engine derives d from these structural positions + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uniform commitment installation across heterogeneous terrain with thin enforcement) is contested: state-centered scholars say it persists (new states still face it); subaltern scholars say it was never the real problem (the problem was extraction, not coordination). The mandate has not clearly atrophied — modern development bureaucracies still use hybrid cascades — but the extraction/coordination ratio has drifted upward (rising extractiveness, rising theater). Not yet a piton; the coordination function remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid cascade a distinct structural mechanism, or a descriptive blend of the endogenous and exogenous readings?',
    'Case comparison: if cascades exist where apex initiation fails without fringe validation AND fringe validation fails without apex initiation (neither phase alone suffices), the hybrid is structurally distinct. If all cases decompose into one phase doing the work, the hybrid is a blend.',
    'If distinct, the kernel has three irreducible readings; if blend, the kernel reduces to two poles with the hybrid as epiphenomenal. Affects whether cs_structure.reading_relations should be coexists_with or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the hybrid reading names a structurally independent mechanism.').

omega_variable(
    validation_vs_consent_ambiguity,
    'Does ''fringe validation'' mean active consent (fringe actors judge the commitment worthy) or coerced performance (fringe actors perform validation under threat)?',
    'Micro-historical analysis of validation episodes: measure fringe actor autonomy in interpretation choices, exit attempts, and post-validation complaint patterns.',
    'If validation = consent, extraction is lower and coordination higher (closer to rope). If validation = coerced performance, extraction is higher and coordination is cover (closer to snare). The current metrics assume a mixed case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(validation_vs_consent_ambiguity, empirical, 'Whether fringe validation is genuine coordination or extraction cover.').

omega_variable(
    cascade_completion_criteria,
    'What counts as ''stabilized'' — universal compliance, elite consensus, or merely absence of overt rebellion?',
    'Define stabilization thresholds per case and measure cascade outcomes against them. Compare time-to-stabilization across reading types.',
    'A low threshold (no rebellion) makes the cascade look more successful (lower extractiveness). A high threshold (universal internalization) reveals persistent extraction. The metric values assume a mid-range threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_completion_criteria, conceptual, 'Operationalization of the cascade''s success condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scim_hc_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(scim_hc_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(scim_hc_tr_t80, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(scim_hc_tr_t120, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(scim_hc_tr_t160, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 160, 0.27).
narrative_ontology:measurement(scim_hc_tr_t200, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(scim_hc_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scim_hc_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(scim_hc_be_t80, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement(scim_hc_be_t120, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 120, 0.53).
narrative_ontology:measurement(scim_hc_be_t160, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 160, 0.56).
narrative_ontology:measurement(scim_hc_be_t200, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(scim_hc_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(scim_hc_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(scim_hc_su_t80, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement(scim_hc_su_t120, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 120, 0.38).
narrative_ontology:measurement(scim_hc_su_t160, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 160, 0.4).
narrative_ontology:measurement(scim_hc_su_t200, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.1).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the state commitment installation kernel into three readings differing on the causal locus of legitimation: apex-driven (exogenous), fringe-driven (endogenous), and bidirectional (hybrid). The hybrid reading claims both phases are structurally necessary; the siblings claim one phase is primary. All three readings share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, organized, 0.45).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__hybrid_cascade_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
