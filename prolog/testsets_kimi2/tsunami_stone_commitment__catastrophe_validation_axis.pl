% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: 2011 Tsunami as Catastrophe Validation Axis
 *   domain: disaster_anthropology/commitment_system_analysis
 *
 * SUMMARY:
 *   The 2011 TÅhoku tsunami serves as a physical catastrophe that
 *   independently adjudicates competing readings of Japan's tsunami stone
 *   commitment system. As a natural event, the wave's magnitude and coastal
 *   inundation were structurally independent of human will, providing binary
 *   empirical evidence: communities who had maintained intergenerational
 *   compliance with stone evacuation instructions and survived, versus those
 *   who had not. This constraint story treats the tsunami not as a social
 *   arrangement but as a mountain constraintâa physical limit that, by
 *   occurring, collapses interpretive ambiguity about whether the inscribed
 *   warnings retained live behavioral force. It functions as an upstream
 *   adjudication device feeding into both the behavioral competence and
 *   commemorative husk readings of the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "2011 Tsunami as Catastrophe Validation Axis").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_system_analysis").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'a6175511-daa5-4854-b8d1-965898120bec').
narrative_ontology:cs_kernel_codification('a6175511-daa5-4854-b8d1-965898120bec', fixed_text).
narrative_ontology:cs_authority_grounding('a6175511-daa5-4854-b8d1-965898120bec', practice).
narrative_ontology:cs_interpretation_layer_present('a6175511-daa5-4854-b8d1-965898120bec').
narrative_ontology:cs_reading_relation('a6175511-daa5-4854-b8d1-965898120bec', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('a6175511-daa5-4854-b8d1-965898120bec', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('a6175511-daa5-4854-b8d1-965898120bec', foundational, catastrophe_adjudicates_commitment_validity).
narrative_ontology:cs_axiom_status(catastrophe_adjudicates_commitment_validity, holdable).
narrative_ontology:cs_axiom_grounding('a6175511-daa5-4854-b8d1-965898120bec', catastrophe_adjudicates_commitment_validity, empirically_contingent).
narrative_ontology:cs_axiom('a6175511-daa5-4854-b8d1-965898120bec', secondary, physical_event_overrides_interpretive_drift).
narrative_ontology:cs_axiom_status(physical_event_overrides_interpretive_drift, holdable).
narrative_ontology:cs_axiom_grounding('a6175511-daa5-4854-b8d1-965898120bec', physical_event_overrides_interpretive_drift, empirically_contingent).
narrative_ontology:cs_reference_frame('a6175511-daa5-4854-b8d1-965898120bec', catastrophe_testable_commitment).
narrative_ontology:cs_drift_state('a6175511-daa5-4854-b8d1-965898120bec', post_2011_tohoku_event, gap(stable, substantial, true)).
narrative_ontology:cs_created_at('a6175511-daa5-4854-b8d1-965898120bec', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

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
% COORDINATION_FUNCTION: None; the constraint is a physical catastrophe, not a coordination mechanism. Its occurrence may secondarily validate or invalidate prior coordination arrangements, but it does not itself coordinate.
% TRANSFER_FUNCTION: No social transfer occurs; physical energy is released and survival is distributed stochastically by proximity to hazard, elevation, and individual compliance with warning signals.
% ABSENT_VOICES: Modern civil defense authorities who attribute survival to telemetry-based early warning rather than to inscribed stones, and geomorphologists who emphasize random spatial variation in inundation patterns, are underrepresented in the commitment-system framing.
% DISAPPEARANCE_RATIONALE: Had the 2011 tsunami not occurred, the physical adjudication event would simply be absent; the world's social arrangements would not rearrange themselves around its absence. The commitment system would remain empirically unresolved by this particular catastrophic instance.
% FOUNDING_PROBLEM: The need to empirically validate whether long-term intergenerational commitments to coastal hazard avoidance, encoded in stone inscriptions, retain live behavioral force over centuries when confronted with actual catastrophic events.
% FOUNDING_PROBLEM_CORROBORATION: Disaster anthropologists and geomorphologists outside the tradition-holding communities attest that traditional ecological knowledge requires periodic empirical calibration against catastrophic events; this corroborating perspective is documented in cross-cultural hazard studies independent of the commitment holders.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.0, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because a natural catastrophe does not extract rents; it destroys indiscriminately without rent-seeking or surplus transfer. Suppression is zero because the wave requires no active enforcement to persist. Accessibility collapse is near-total (0.95): once the wave front is recognized, all alternatives to evacuation or perishing collapse. Resistance is negligible (0.05) at the physical levelânature meets no human resistance, though humans may resist warnings. The metrics are authored independently of the mountain claim; they describe a physical event without beneficiaries, victims, or performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   As a single-index physical constraint, there is no perspectival divergence in directionality. All agents facing the wave occupy the same structural relationship to the physical limit. The sibling readings of the kernel diverge only in how they interpret the constraint's adjudicative output, not in their physical exposure to it.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the tsunami is a physical event without directional extraction. Every agent's structural d-value converges toward the symmetric center only in the trivial sense that physics governs all; effective extraction remains zero because there is no social mechanism translating the event into asymmetric transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable. The constraint is a physical event, not an institutional mandate. There is no coordination function that could atrophy, no sunset clause, and no performance-maintenance gap. The mountain classification prevents misreading the tsunami as a snare or tangled rope by explicitly declaring its natural emergence and zero extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_attribution_ambiguity,
    'Is the 2011 tsunami''s survival outcome correctly attributable to heeding stone inscriptions rather than modern early-warning systems, elevation, or random spatial variation?',
    'Micro-level demographic and geospatial analysis correlating stone locations, community oral-history compliance, and survivor outcomes, controlling for elevation and warning-system access.',
    'If survival is attributable primarily to non-stone factors, this mountain constraint''s adjudicative power over the commitment kernel is weakened without dissolving the constraint itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_attribution_ambiguity, empirical, 'Ambiguity in attributing survival outcomes to stone compliance versus confounding factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_validation_axis reading decomposes the tsunami_stone_commitment kernel by isolating the 2011 physical event as an independent mountain constraint. It is upstream of both sibling readings in the constraint family, supplying the empirical adjudication that both must interpret.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
