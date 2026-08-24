% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium: State/Traditional Legitimacy Partition
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   This constraint describes the dual practice equilibrium observed in Meiji
 *   Japan (1868-1912) and analogous cases: state authority governs
 *   public/administrative domains (taxation, bureaucracy, military,
 *   education) using Western standards, while traditional authority governs
 *   private/ritual domains (festivals, agriculture, family rites, dress at
 *   home) using indigenous standards. The partition is presented as a stable
 *   equilibrium — not a transitional phase — with compliance being strategic
 *   (code-switching) rather than internalized. The constraint is claimed as a
 *   tangled rope: it coordinates by assigning domains (reducing conflict) but
 *   extracts via the dual-compliance burden on the populace. Active
 *   enforcement is required on both sides: the state polices public
 *   conformity, ritual elites police ritual conformity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.48).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium: State/Traditional Legitimacy Partition").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'b558d90b-96c4-440b-bead-f56029a65e02').
narrative_ontology:cs_kernel_codification('b558d90b-96c4-440b-bead-f56029a65e02', formalized).
narrative_ontology:cs_authority_grounding('b558d90b-96c4-440b-bead-f56029a65e02', practice).
narrative_ontology:cs_reading_relation('b558d90b-96c4-440b-bead-f56029a65e02', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('b558d90b-96c4-440b-bead-f56029a65e02', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('b558d90b-96c4-440b-bead-f56029a65e02', foundational, domain_partition_is_stable_equilibrium).
narrative_ontology:cs_axiom_status(domain_partition_is_stable_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('b558d90b-96c4-440b-bead-f56029a65e02', domain_partition_is_stable_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('b558d90b-96c4-440b-bead-f56029a65e02', secondary, strategic_compliance_not_internalized).
narrative_ontology:cs_axiom_status(strategic_compliance_not_internalized, holdable).
narrative_ontology:cs_axiom_grounding('b558d90b-96c4-440b-bead-f56029a65e02', strategic_compliance_not_internalized, empirically_contingent).
narrative_ontology:cs_reference_frame('b558d90b-96c4-440b-bead-f56029a65e02', meiji_dual_practice_settlement).
narrative_ontology:cs_drift_state('b558d90b-96c4-440b-bead-f56029a65e02', post_meiji_restoration, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b558d90b-96c4-440b-bead-f56029a65e02', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrators).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ritual_elites).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, general_populace).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, domain_partition_prevents_legitimacy_conflict).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, strategic_compliance_sustains_dual_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce public/administrative standards (Gregorian calendar for taxation, Western dress for officials, metric weights). They gain administrative legibility and international recognition. Their authority depends on maintaining the partition so traditional elites do not obstruct bureaucratic penetration.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Preserve traditional authority over private/ritual domains (lunar calendar for festivals, kimono for ceremonies, ancestral rites). They retain cultural legitimacy and social cohesion functions. Their position is fused with communal identity; exit would mean loss of ritual status and collective meaning.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ritual_elites, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ritual_elites, beneficiary).

% Must navigate two practice regimes daily: use Gregorian calendar for tax filing and work schedules, lunar calendar for planting and festivals; wear Western suits in government offices and factories, kimono at home and shrines. Compliance is strategic — they switch codes to avoid sanctions from either authority. The cognitive and material cost of maintaining dual repertoires falls on them.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, general_populace, payer,
    moderate, biographical, constrained, national).

% Advocate for full convergence (either complete Westernization or nativist restoration). They are excluded from the settled partition because their proposals threaten the compromise that sustains both state and traditional authority. Some emigrate or join opposition movements; others are co-opted into the bureaucracy.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, reformers, excluded,
    moderate, biographical, mobile, national).

% Analyze the partition as a stable equilibrium or a transitional phase. Their work shapes retrospective legitimacy but does not affect the constraint's operation in real time.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions legitimacy domains between state and traditional authorities to prevent open conflict over practice standardization, allowing each to govern its sphere without the other's interference.
% TRANSFER_FUNCTION: Transfers the burden of maintaining two distinct practice repertoires (calendar, dress, measurement, ritual) from the authorities to the general populace, who must strategically comply with both.
% ABSENT_VOICES: Reformers seeking full convergence (radical modernizers and nativist restorationists) are structurally excluded; they would object to the perpetuation of dual practices but are kept out by the mutual interest of state and traditional elites in maintaining the partition.
% DISAPPEARANCE_RATIONALE: If the partition vanished, the state would likely impose uniform administrative practices across all domains (exogenous override), provoking resistance from ritual elites and populace, or traditional authorities would expand claims into public administration, undermining state capacity. The equilibrium prevents either outcome.
% FOUNDING_PROBLEM: The Meiji state needed to adopt Western administrative standards (calendar, dress, weights) for fiscal/military modernization without triggering mass resistance from a populace whose identity and livelihood were organized around traditional practices, and without alienating traditional elites whose cooperation was necessary for social order.
% FOUNDING_PROBLEM_CORROBORATION: Meiji oligarchs' own memoranda (e.g., Iwakura Mission reports) attest to the deliberate partition strategy. Scholars outside the beneficiary set — Marius Jansen (The Making of Modern Japan), Carol Gluck (Japan's Modern Myths), and Andrew Gordon (A Modern History of Japan) — document the strategic compromise and its contested durability. No single account from the benefiting parties alone corroborates the status.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the ongoing cost to individuals of maintaining two practice repertoires. Suppression (0.48) is moderate: alternatives (full convergence) are discouraged by both authorities but not violently suppressed; the partition itself is the suppression mechanism. Theater ratio (0.22) is low because the coordination function is genuine and the enforcement is functional, not performative. Accessibility collapse (0.52) is partial: individuals can and do exit specific practices (e.g., adopt Western dress at home) but the structural partition persists. Resistance (0.38) is modest: most compliance is strategic, not resistant; organized resistance appears only when one authority encroaches on the other's domain.
 *
 * PERSPECTIVAL GAP:
 *   From the state_administrators' seat, the constraint is a rope (coordination enabling modernization). From the ritual_elites' seat, it is a rope (coordination preserving tradition). From the general_populace's seat, it is a snare (extraction via dual compliance). The engine will compute these per-seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   State administrators and ritual elites are beneficiaries (d near 0.0) — they gain legitimacy and control in their domains. General populace are payers (d near 1.0) — they bear the compliance costs with constrained exit. Reformers are excluded (d undefined) — they are kept out of the arrangement. Historical sociologists are observers (d=0.5). The directionality derivation from beneficiary/victim declarations plus exit options captures this structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling the partition as pure coordination (rope) because the extraction on the populace is structural and persistent. It prevents mislabeling as pure extraction (snare) because the coordination function (conflict avoidance between authorities) is genuine and valued by both authority groups. The mandate (modernize without revolution) has not atrophied — the partition continues to serve both authorities — so mandatrophy_resolved is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_transition,
    'Is the domain partition a stable equilibrium or a prolonged transitional phase before full convergence?',
    'Longitudinal comparative study of cases with similar partitions (Meiji Japan, Atatürk''s Turkey, Reza Shah''s Iran, post-colonial states): measure duration and whether convergence eventually occurs.',
    'If transitional, the constraint is a scaffold with a delayed sunset; if stable, it is a tangled rope. Classification and predicted persistence change accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_transition, conceptual, 'Whether the dual practice equilibrium is an endpoint or a waypoint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of convergence structural (active policing by both authorities) or internalized (populace accepts the partition as natural)?',
    'Survey and ethnographic work on whether individuals experience the partition as imposed or as a natural order; compare with cases where partition collapsed.',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint may persist even if active enforcement relaxes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in a dual-authority context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, meiji_calendar_reform).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, meiji_dress_regulation).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, household_registration_system).

% DUAL FORMULATION NOTE:
% This reading (dual_practice_equilibrium) and its siblings (endogenous_displacement, exogenous_override) form a constraint family decomposing the kernel 'legitimacy_of_practice_standardization'. Each reading has a distinct ε: this reading ε≈0.42 (extraction from dual compliance); endogenous_displacement ε≈0.15 (low extraction, voluntary change); exogenous_override ε≈0.75 (high extraction, state imposition). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
