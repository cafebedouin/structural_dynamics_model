% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__behavioral_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: aneyoshi_land_use_prohibition__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Land-Use Prohibition â Behavioral Competence Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone inscribes a prohibition against building
 *   houses below its elevation, encoding the 1896 Meiji-Sanriku tsunami
 *   inundation limit. In the behavioral competence reading, this prohibition
 *   functioned as a live land-use rule for 78 years, enforced through
 *   community social practice rather than centralized authority. The 2011
 *   TÅhoku tsunami validated the rule: the village suffered minimal damage
 *   while neighboring settlements built lower were destroyed. This reading
 *   treats the constraint as tracking an irreducible physical limit (tsunami
 *   run-up physics) with negligible extraction, no beneficiary structure, and
 *   no active suppression of alternatives â the danger itself eliminates
 *   the alternative. The claim is Mountain because the underlying physical
 *   constraint would persist regardless of the stone's presence, and the
 *   social practice adds zero extractive overhead.
 *
 * KEY AGENTS:
 *   - aneyoshi_residents: Symmetrically positioned community members who coordinate land use through the inscribed boundary (moderate/local).
 *   - future_settlers: Latent beneficiaries of the preserved boundary who inherit the risk-avoidance information (powerless/local).
 *   - external_observers: Disaster anthropologists and physical scientists who corroborate the rule's functional status (analytical/global).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Stone Land-Use Prohibition â Behavioral Competence Reading").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'de92ad89-702b-4da2-9f93-a8ffaaeccf89').
narrative_ontology:cs_kernel_codification('de92ad89-702b-4da2-9f93-a8ffaaeccf89', fixed_text).
narrative_ontology:cs_authority_grounding('de92ad89-702b-4da2-9f93-a8ffaaeccf89', practice).
narrative_ontology:cs_interpretation_layer_present('de92ad89-702b-4da2-9f93-a8ffaaeccf89').
narrative_ontology:cs_reading_relation('de92ad89-702b-4da2-9f93-a8ffaaeccf89', aneyoshi_land_use_prohibition__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('de92ad89-702b-4da2-9f93-a8ffaaeccf89', foundational, tsunami_inscription_governs_settlement).
narrative_ontology:cs_axiom_status(tsunami_inscription_governs_settlement, holdable).
narrative_ontology:cs_axiom_grounding('de92ad89-702b-4da2-9f93-a8ffaaeccf89', tsunami_inscription_governs_settlement, empirically_contingent).
narrative_ontology:cs_reference_frame('de92ad89-702b-4da2-9f93-a8ffaaeccf89', live_inundation_competence).
narrative_ontology:cs_drift_state('de92ad89-702b-4da2-9f93-a8ffaaeccf89', post_2011_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('de92ad89-702b-4da2-9f93-a8ffaaeccf89', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates settlement location by marking the maximum credible tsunami inundation, aligning individual building decisions with collective survival.
% TRANSFER_FUNCTION: Transfers risk-information from past disaster experience to present and future settlement decisions; no asymmetric material transfer.
% ABSENT_VOICES: Coastal developers and prospective settlers from outside the community are absent from the local decision frame; their interest in low-elevation land is rendered irrelevant by the physical boundary rather than by active exclusion.
% DISAPPEARANCE_RATIONALE: The stone and its prohibition are a marker and social aid; the underlying tsunami physics and inundation boundary would remain unchanged if the stone vanished. The community might eventually forget the precise limit, but the physical constraint itself would persist, and disaster would re-teach it.
% FOUNDING_PROBLEM: The 1896 Meiji-Sanriku tsunami destroyed low-elevation settlements and killed over 22,000 people; the stone was erected to prevent recurrence of fatal settlement in the inundation zone.
% FOUNDING_PROBLEM_CORROBORATION: Geological records of recurrent tsunami inundation and the 2011 TÅhoku tsunami event corroborate that the hazard persists; disaster researchers and oceanographers outside the community attest the founding problem is live, while the community's own survival provides endogenous validation.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_land_use_prohibition__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because no party collects rents or asymmetric benefits from the prohibition; the arrangement moves risk information, not wealth or status. Suppression is minimal (0.05) because compliance is driven by shared recognition of physical danger rather than by coercive enforcement; the 'enforcement' is social practice, not suppression of exit. Accessibility collapse is high (0.90) because once the tsunami risk is understood, building in the inundation zone becomes practically unthinkable for rational agents â the alternative collapses without external coercion. Resistance is negligible (0.02) because the constraint aligns with survival interest; there is no active movement to build lower. Theater ratio is zero because the rule is functionally intact and validated by disaster outcomes. The measurement series is flat across 78 years, indicating stable, drift-free operation.
 *
 * PERSPECTIVAL GAP:
 *   From the community seat, the constraint is experienced as common-sense survival coordination with no extraction. From an external analytical seat, it appears as a rare case of persistent low-level institutional memory successfully tracking a natural hazard. There is no seat divergence in type because all indices face the same physical limit; degrees of freedom are uniformly zero.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim asymmetry is declared. All agents who interact with the constraint are structurally symmetric: the information flows from the stone to the community, and the community's compliance is self-benefiting. Because there is no asymmetric extraction, directionality derivation yields near-symmetric values for all seats, and effective extraction remains negligible across all indices.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate â avoid building below the tsunami limit â remains live because the underlying hazard (subduction-zone tsunami recurrence) is unchanged. The 2011 event provided empirical corroboration that the founding problem is not solved. There is no mandatrophy because the constraint's function (preventing fatal settlement patterns) is still required, and the mechanism (the inscribed boundary plus social practice) still delivers it without decay into theatrical performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_practice_naturality,
    'Is the prohibition a naturally emerging limit or a socially constructed coordination mechanism?',
    'Comparative analysis of other tsunami stones with varying compliance rates; if compliance varies by social context, the constraint is constructed.',
    'If constructed, reclassify as Rope or Piton; if naturalized, the Mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_practice_naturality, conceptual, 'Ambiguity between natural-law and social-construction framing').

omega_variable(
    operational_enforcement_scope,
    'Was the prohibition uniformly behaviorally enforced across the full 78-year interval, or did enforcement fluctuate regionally or temporally?',
    'Archival land-use records and oral history interviews tracking building permits and community sanctions.',
    'If enforcement was patchy, extractiveness may be higher than modeled due to localized coercion; if uniform, the low-extraction Mountain profile is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_enforcement_scope, empirical, 'Temporal and spatial uniformity of behavioral enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_behavioral_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 13, 0.0).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 26, 0.0).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 39, 0.0).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 52, 0.0).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 65, 0.0).
narrative_ontology:measurement(aneyoshi_behavioral_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.0).

% Extraction over time
narrative_ontology:measurement(aneyoshi_behavioral_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(aneyoshi_behavioral_be_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 13, 0.02).
narrative_ontology:measurement(aneyoshi_behavioral_be_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 26, 0.02).
narrative_ontology:measurement(aneyoshi_behavioral_be_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 39, 0.02).
narrative_ontology:measurement(aneyoshi_behavioral_be_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 52, 0.02).
narrative_ontology:measurement(aneyoshi_behavioral_be_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 65, 0.02).
narrative_ontology:measurement(aneyoshi_behavioral_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_land_use_prohibition__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and commemorative_husk_reading are sibling readings of the aneyoshi_land_use_prohibition kernel. They share the same referent (the stone inscription and associated village prohibition) but diverge structurally: this reading claims the rule is live, low-extraction, and tracks a physical limit; the sibling reading claims it is an atrophied husk sustained by symbolic performance. The Îµ values differ accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
