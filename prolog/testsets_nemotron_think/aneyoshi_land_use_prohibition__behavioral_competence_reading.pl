% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Aneyoshi Tsunami Stone Behavioral Prohibition
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone, erected after the 1896 Meiji-Sanriku tsunami,
 *   marks a land-use prohibition line: 'Do not build your homes below this
 *   point.' For 78 years (1896-1974, spanning through the 1933 Showa-Sanriku
 *   tsunami and beyond), the Aneyoshi community operationally enforced this
 *   prohibition through social practice — no homes were built below the stone
 *   line. The constraint is a physical reality (tsunami hydrodynamics)
 *   encoded into a social rule via a stone marker and intergenerational
 *   transmission. There is no beneficiary extracting value from this
 *   arrangement; the community members who comply are neither victims nor
 *   beneficiaries in an extractive sense — they are participants in a
 *   coordination system that aligns with an irreducible physical limit. The
 *   stone itself is the kernel; this reading asserts the kernel remains
 *   behaviorally operative.
 *
 * KEY AGENTS:
 *   - Aneyoshi community members: constrained by physical reality, coordinated by social practice (powerless/identity_locked/local)
 *   - Stone marker: physical kernel fixing the boundary (non-agent entity)
 *   - Intergenerational transmitters: elders who convey the prohibition's meaning (organized/biographical/local)
 *   - Tsunami physics: the irreducible natural law that makes the constraint a mountain (universal/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.05).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__behavioral_competence_reading, "Aneyoshi Tsunami Stone Behavioral Prohibition").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

domain_priors:emerges_naturally(aneyoshi_land_use_prohibition__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__behavioral_competence_reading, '066fe99e-e9af-46e5-912a-19933dad78ac').
narrative_ontology:cs_kernel_codification('066fe99e-e9af-46e5-912a-19933dad78ac', fixed_text).
narrative_ontology:cs_authority_grounding('066fe99e-e9af-46e5-912a-19933dad78ac', practice).
narrative_ontology:cs_interpretation_layer_present('066fe99e-e9af-46e5-912a-19933dad78ac').
narrative_ontology:cs_reading_relation('066fe99e-e9af-46e5-912a-19933dad78ac', aneyoshi_land_use_prohibition__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('066fe99e-e9af-46e5-912a-19933dad78ac', foundational, stone_mark_operational_boundary).
narrative_ontology:cs_axiom_status(stone_mark_operational_boundary, holdable).
narrative_ontology:cs_axiom_grounding('066fe99e-e9af-46e5-912a-19933dad78ac', stone_mark_operational_boundary, empirically_contingent).
narrative_ontology:cs_axiom('066fe99e-e9af-46e5-912a-19933dad78ac', secondary, intergenerational_transmission_preserves_behavioral_force).
narrative_ontology:cs_axiom_status(intergenerational_transmission_preserves_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('066fe99e-e9af-46e5-912a-19933dad78ac', intergenerational_transmission_preserves_behavioral_force, empirically_contingent).
narrative_ontology:cs_reference_frame('066fe99e-e9af-46e5-912a-19933dad78ac', operational_tsunami_boundary).
narrative_ontology:cs_drift_state('066fe99e-e9af-46e5-912a-19933dad78ac', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('066fe99e-e9af-46e5-912a-19933dad78ac', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, tsunami_physics_enforces_land_use_boundary).
narrative_ontology:constraint_vindicates(aneyoshi_land_use_prohibition__behavioral_competence_reading, intergenerational_knowledge_transmission_works).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates community land-use decisions with tsunami hydrodynamics: the stone marks the physical boundary beyond which building is unsurvivable, solving the coordination problem of communicating an intergenerational survival threshold without requiring each generation to rediscover it through catastrophe.
% TRANSFER_FUNCTION: Transfers nothing extractive. The arrangement moves survival probability from 'low if built below line' to 'high if built above line' — a coordination gain, not a transfer from one party to another. No money, status, or labor flows between agents via this constraint.
% ABSENT_VOICES: Future generations who would inherit the constraint; the 2011 tsunami dead who could not testify to the stone's accuracy. Neither are structurally excluded — the constraint's logic includes them by design (intergenerational transmission).
% DISAPPEARANCE_RATIONALE: If the stone and its prohibition vanished overnight, the community would lose its intergenerationally transmitted survival boundary. Without the marker, some members might build in the inundation zone, leading to fatalities in the next tsunami. The world rearranges because the constraint prevents a physical catastrophe that would otherwise occur.
% FOUNDING_PROBLEM: After the 1896 Meiji-Sanriku tsunami destroyed the village, survivors needed a permanent, non-technological marker to communicate the safe building line to all future generations — a boundary that would persist beyond memory, literacy, or institutional continuity.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tohoku tsunami provided empirical corroboration from outside the community: the inundation line matched the stone's position, confirming the founding problem (tsunami survival) remains live and the constraint's solution remains accurate. Geological surveys and tsunami modeling by external researchers (Tohoku University, JMA) corroborate the stone's boundary alignment with physical hazard zones.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.02) because no party collects from the constraint's operation — the prohibition prevents building in a tsunami inundation zone, which is a physical necessity, not a rent extraction. Suppression is minimal (0.05) because compliance requires no coercion; the alternative (building below the line) is physically suicidal, not merely prohibited. Theater ratio is negligible (0.03) because the constraint's performative and functional aspects are identical — the stone marks the real boundary. Accessibility collapse is very high (0.92) because once the tsunami physics is understood, alternatives genuinely collapse — you cannot negotiate with hydrodynamics. Resistance is near-zero (0.04) because the constraint meets no active opposition; compliance is rational survival behavior. The measurement series shows remarkable stability across the 78-year interval, consistent with a mountain constraint whose referent is physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   All community members sit at d ≈ 0.5 (symmetric) — the constraint costs them nothing (they would not build in the inundation zone anyway) and benefits them existentially (survival). There are no beneficiaries in the extractive sense (no one collects rents) and no victims (no one bears imposed costs). The directionality derivation from beneficiary/victim declarations yields symmetric d for all seats because both arrays are empty. The stone marker and tsunami physics are non-agent entities (agent: false) and do not enter directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (tsunami survival) remains live — the 2011 Tohoku tsunami confirmed the stone's boundary was correct (the 2011 inundation stopped at the stone line). The mandate has not atrophied; the constraint's function is continuously vindicated by physical reality. No mandatrophy resolution is needed because the constraint's justification is the persistent physical threat, not a solved historical problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Aneyoshi stone a live operational land-use rule (behavioral competence reading) or a commemorative husk where the prohibition has decayed to symbol (commemorative husk reading)?',
    'Empirical observation of contemporary building patterns relative to the stone line; ethnographic documentation of whether community members actively reference and comply with the prohibition in current land-use decisions.',
    'If behavioral competence reading holds, the constraint is a mountain with near-zero extraction. If commemorative husk reading holds, the constraint is a piton (degraded coordination maintained theatrically) with higher theater_ratio and lower accessibility_collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Structural ambiguity between live operational rule vs. decayed commemorative symbol for the same physical marker.').

omega_variable(
    extraction_referent_stability,
    'Does the 78-year enforcement record represent continuous behavioral compliance, or are there periods of lapse that would indicate the constraint''s mountain status is intermittent?',
    'Historical land-use records, aerial photography time series, and oral history triangulation across the full interval.',
    'If enforcement was continuous, the mountain classification is robust. If significant lapses exist, the constraint may be a scaffold (temporary coordination) or piton (atrophied) rather than a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_referent_stability, empirical, 'Continuity of operational enforcement across the claimed 78-year interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 13, 0.03).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 26, 0.03).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 39, 0.03).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 52, 0.03).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 65, 0.03).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, theater_ratio, 78, 0.03).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(aney_be_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 13, 0.02).
narrative_ontology:measurement(aney_be_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 26, 0.02).
narrative_ontology:measurement(aney_be_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 39, 0.02).
narrative_ontology:measurement(aney_be_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 52, 0.02).
narrative_ontology:measurement(aney_be_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 65, 0.02).
narrative_ontology:measurement(aney_be_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, base_extractiveness, 78, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(aney_su_t13, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 13, 0.05).
narrative_ontology:measurement(aney_su_t26, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 26, 0.05).
narrative_ontology:measurement(aney_su_t39, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 39, 0.05).
narrative_ontology:measurement(aney_su_t52, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 52, 0.05).
narrative_ontology:measurement(aney_su_t65, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 65, 0.05).
narrative_ontology:measurement(aney_su_t78, aneyoshi_land_use_prohibition__behavioral_competence_reading, suppression_requirement, 78, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__behavioral_competence_reading, aneyoshi_land_use_prohibition__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling commemorative_husk_reading form a constraint family decomposing the 'Aneyoshi stone' natural-language label. This reading (behavioral competence) treats the stone as a live operational boundary with near-zero extraction (mountain). The sibling reading treats it as a decayed commemorative symbol (piton). They are linked via affects_constraints because the commemorative reading's claim about decay is a direct challenge to this reading's claim about continuous enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
