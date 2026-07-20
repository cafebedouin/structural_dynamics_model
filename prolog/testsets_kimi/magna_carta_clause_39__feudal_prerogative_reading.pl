% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 â Feudal Prerogative Reading
 *   domain: constitutional/law/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) is interpreted here through the feudal
 *   prerogative reading: a narrow procedural safeguard for elite peers within
 *   an established hierarchical order. It constrains the crown from arbitrary
 *   imprisonment and dispossession but extends only to free men, preserving
 *   rather than challenging the feudal pyramid. This constraint is one
 *   reading of a contested kernel; sibling readings construe the same text as
 *   a liberal due process guarantee or as an originalist limitation on
 *   specific 1215 abuses.
 *
 * KEY AGENTS:
 *   - crown: Traditional authority constrained by the clause (powerful/constrained) â bears the loss of arbitrary detention power
 *   - feudal_elite_peers: Primary beneficiary and enforcer (powerful/constrained) â gains procedural protection and administers enforcement through baronial councils
 *   - non_elite_subjects: Excluded population (powerless/trapped) â remains subject to arbitrary power and absent from the constitutional bargain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.22).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.35).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 â Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '3d5b559a-8265-45f9-a440-cc2813e2539e').
narrative_ontology:cs_kernel_codification('3d5b559a-8265-45f9-a440-cc2813e2539e', fixed_text).
narrative_ontology:cs_authority_grounding('3d5b559a-8265-45f9-a440-cc2813e2539e', lineage).
narrative_ontology:cs_interpretation_layer_present('3d5b559a-8265-45f9-a440-cc2813e2539e').
narrative_ontology:cs_reading_relation('3d5b559a-8265-45f9-a440-cc2813e2539e', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d5b559a-8265-45f9-a440-cc2813e2539e', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('3d5b559a-8265-45f9-a440-cc2813e2539e', foundational, peer_judgment_as_privilege).
narrative_ontology:cs_axiom_status(peer_judgment_as_privilege, holdable).
narrative_ontology:cs_axiom_grounding('3d5b559a-8265-45f9-a440-cc2813e2539e', peer_judgment_as_privilege, conventional).
narrative_ontology:cs_axiom('3d5b559a-8265-45f9-a440-cc2813e2539e', foundational, hierarchy_preservation_mandate).
narrative_ontology:cs_axiom_status(hierarchy_preservation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('3d5b559a-8265-45f9-a440-cc2813e2539e', hierarchy_preservation_mandate, conventional).
narrative_ontology:cs_reference_frame('3d5b559a-8265-45f9-a440-cc2813e2539e', feudal_reciprocal_obligations).
narrative_ontology:cs_drift_state('3d5b559a-8265-45f9-a440-cc2813e2539e', post_feudal_centralization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3d5b559a-8265-45f9-a440-cc2813e2539e', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, feudal_elite_peers).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_reciprocity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains overarching feudal authority but loses the arbitrary power to imprison, dispossess, or exile elite peers without lawful judgment. The clause structurally extracts from royal prerogative by forcing the crown to proceed through peer judgment or the law of the land.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown, payer,
    powerful, generational, constrained, national).

% Barons and free men who receive procedural protection against arbitrary royal seizure. They enforce the clause through baronial councils and the threat of collective action, coordinating feudal reciprocity while preserving their privileged standing within the hierarchy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, feudal_elite_peers, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, feudal_elite_peers, agenda_setter).

% Unfree peasants, villeins, and lower-status persons excluded from the clause's protection. They remain fully subject to arbitrary lordly and royal power and would demand universal application if included in the constitutional conversation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, non_elite_subjects, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, feudal_elite_peers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents arbitrary royal confiscation and imprisonment of elite vassals, stabilizing reciprocal feudal obligations and reducing the risk of baronial rebellion by guaranteeing that seizure requires lawful peer judgment.
% TRANSFER_FUNCTION: Transfers the authority to judge and punish elite free men from the crown's arbitrary discretion to a process requiring lawful judgment by equals or the law of the land, preserving baronial property and personal liberty.
% ABSENT_VOICES: Non-elite subjectsâvilleins, unfree peasants, and those outside the class of free menâare structurally excluded; they would object to the narrow scope and demand protection from arbitrary power by lords and crown alike.
% DISAPPEARANCE_RATIONALE: If the clause vanished, the crown could resume arbitrary imprisonment and dispossession of barons, dissolving the procedural safeguard that stabilizes the feudal hierarchy and likely provoking immediate baronial resistance.
% FOUNDING_PROBLEM: Crown arbitrary imprisonment, dispossession, and exile of barons and elite vassals without trial or peer judgment, violating feudal reciprocal obligations and threatening baronial revolt.
% FOUNDING_PROBLEM_CORROBORATION: Chroniclers and petitioners outside the immediate beneficiary class, including clerical observers and subsequent common-law jurists, attest that arbitrary royal seizure was the precipitating grievance of 1215; modern historians corroborate that the specific feudal crisis was resolved by the mid-13th century.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the clause only modestly constrains royal prerogative, leaving the crown with broad feudal authority. Suppression (0.35) reflects the suppression of arbitrary royal power over elites but not its elimination. Theater ratio is low (0.15) because the peer-judgment mechanism is functional within its narrow scope. Accessibility collapse (0.55) is moderate: within the feudal frame, alternatives to legal process collapse for elites, but the frame itself is contestable. Resistance (0.40) captures persistent royal attempts to annul or evade the charter.
 *
 * PERSPECTIVAL GAP:
 *   The crown experiences the clause as extraction of prerogative; the baronial seat experiences it as essential coordination guaranteeing feudal reciprocity. The engine computes this divergence from the structural asymmetry in power and exit options: both are powerful but the crown is uniquely targeted by the constraint's limitation.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown is the structural target (d near 1.0) because the constraint directly extracts from royal arbitrary power. Feudal elite peers are structural beneficiaries (d near 0.0) because the constraint subsidizes their security and property claims. Non-elite subjects have no directionality assignment because they are excluded from the constraint's operation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâarbitrary royal abuse of baronsâwas substantially resolved by the charter and subsequent feudal settlements. The clause persisted beyond its original function, but within the authored interval it retains genuine coordinating force among elites and is not yet a piton. Later centuries would see it become performative or be reinterpreted under liberal readings, but the feudal prerogative reading tracks its operative phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is Clause 39 a feudal privilege mechanism, a universal due process right, or merely a limitation on specific 1215 abuses?',
    'Comparative legal-historical analysis of 13th-century practice versus subsequent constitutional reception; textual analysis of ''liber homo'' and ''per legem terrae'' in contemporaneous sources.',
    'Resolution would determine whether the constraint''s beneficiary set is restricted to elites or universal, radically altering directionality and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading ambiguity for Clause 39').

omega_variable(
    feudal_scope_exclusion,
    'Does the exclusion of non-elite subjects from Clause 39''s protection constitute an extractive asymmetry or merely a jurisdictional boundary consistent with feudal social structure?',
    'Analysis of contemporaneous legal treatment of villeins and unfree persons versus free men in royal and manorial courts.',
    'If exclusion is extractive, the constraint has a hidden victim set (non-elites) that would raise extractiveness and shift classification toward snare; if jurisdictional, the victim set remains restricted to the crown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_scope_exclusion, conceptual, 'Exclusion of non-elites as extraction or jurisdictional boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magn_tr_t50, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(magn_be_t50, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 50, 0.24).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(magn_su_t50, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
