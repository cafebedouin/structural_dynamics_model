% ============================================================================
% CONSTRAINT STORY: behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: behavioral_competence_reading
 *   human_readable: Stone Directive as Behavioral Competence Constraint (Aneyoshi Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The stone directive at Aneyoshi, Japan, is a 600-year-old marker
 *   instructing residents not to build below a certain elevation. For 78
 *   years (1945–2023), this directive persisted as a binding land-use
 *   constraint without formal scientific validation — no published geological
 *   survey, no official government mandate, no institutional enforcement
 *   mechanism. Yet when the 2011 Tōhoku tsunami struck, the directive proved
 *   accurate: the tsunami reached precisely to the marker's location, and
 *   Aneyoshi was the only settlement in the Sanriku region with zero
 *   casualties. This constraint story instantiates the behavioral competence
 *   reading of the contested kernel 'aneyoshi_stone_directive': the directive
 *   is binding because it encodes accurate behavioral guidance derived from
 *   physical geography (tsunami reach), not because it is enforced by
 *   institutions or because anyone benefits from compliance. The constraint
 *   emerges naturally from the subduction zone geometry and historical
 *   tsunami patterns. The 78-year absence of formal validation is an
 *   institutional gap, not an empirical one — the directive's accuracy was
 *   validated implicitly through compliance and disaster outcomes, but never
 *   formally documented in scientific literature.
 *
 * KEY AGENTS:
 *   - Aneyoshi Residents: Primary agent (powerless/trapped) — constrained by the directive to build above the marker; experience the constraint as alignment with observable reality, not as extraction
 *   - Regional Disaster Management Authority: Secondary agent (moderate/constrained) — responsible for disaster preparedness; recognize the directive's accuracy but lack formal institutional mandate to enforce it
 *   - Geological/Oceanographic Processes: Non-agent entity (physical geography) — the subduction zone, tsunami generation, and topographic amplification that make the directive accurate
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as emerging from physical geography, not from institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_competence_reading, 0.08).
domain_priors:suppression_score(behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(behavioral_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_competence_reading, mountain).
narrative_ontology:human_readable(behavioral_competence_reading, "Stone Directive as Behavioral Competence Constraint (Aneyoshi Reading)").
narrative_ontology:topic_domain(behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_competence_reading, '33b9d72d-fba7-4119-9c1f-492922c6b0fb').
narrative_ontology:cs_kernel_codification('33b9d72d-fba7-4119-9c1f-492922c6b0fb', fixed_text).
narrative_ontology:cs_authority_grounding('33b9d72d-fba7-4119-9c1f-492922c6b0fb', practice).
narrative_ontology:cs_interpretation_layer_present('33b9d72d-fba7-4119-9c1f-492922c6b0fb').
narrative_ontology:cs_reading_relation('33b9d72d-fba7-4119-9c1f-492922c6b0fb', behavioral_competence_reading__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('33b9d72d-fba7-4119-9c1f-492922c6b0fb', foundational, directive_accuracy_derives_from_physical_geography).
narrative_ontology:cs_axiom_status(directive_accuracy_derives_from_physical_geography, holdable).
narrative_ontology:cs_axiom_grounding('33b9d72d-fba7-4119-9c1f-492922c6b0fb', directive_accuracy_derives_from_physical_geography, empirically_contingent).
narrative_ontology:cs_axiom('33b9d72d-fba7-4119-9c1f-492922c6b0fb', foundational, binding_force_independent_of_institutional_enforcement).
narrative_ontology:cs_axiom_status(binding_force_independent_of_institutional_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('33b9d72d-fba7-4119-9c1f-492922c6b0fb', binding_force_independent_of_institutional_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('33b9d72d-fba7-4119-9c1f-492922c6b0fb', physical_geography_constraint).
narrative_ontology:cs_drift_state('33b9d72d-fba7-4119-9c1f-492922c6b0fb', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('33b9d72d-fba7-4119-9c1f-492922c6b0fb', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(behavioral_competence_reading, aneyoshi_stone_directive).

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
% COORDINATION_FUNCTION: The directive solves the coordination problem of identifying safe building locations in a tsunami-prone region. Residents need to know where to build to avoid tsunami risk; the directive provides this information through a physical marker (the stone) that encodes the maximum reach of historical tsunamis.
% TRANSFER_FUNCTION: The directive transfers constraint (not value): it restricts where residents can build, but this restriction aligns with their safety interests. No value flows from one agent to another; the constraint is a shared alignment with physical reality.
% ABSENT_VOICES: No voices are absent from this constraint. The directive applies equally to all residents of Aneyoshi. The only absent voice is that of future residents who will inherit the directive without understanding its origin — but this is a temporal absence, not a social one.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared and all institutional memory of it were lost, residents would need to rediscover the safe building boundary through trial and error (or through scientific study of tsunami risk). The world would rearrange itself around the loss of this behavioral guidance — settlements would likely rebuild in the tsunami zone, leading to casualties in future events. The directive's disappearance would have catastrophic consequences, demonstrating that arrangements depend on it.
% FOUNDING_PROBLEM: The founding problem was the need to identify safe building locations in a tsunami-prone region. Historical tsunamis (documented in oral tradition and physical evidence) reached a certain elevation; the stone directive marks this elevation to guide future building decisions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is corroborated by the 2011 Tōhoku tsunami, which reached precisely to the marker's location. This empirical validation confirms that the founding problem (identifying safe building locations) remains live and that the directive's solution remains accurate. The corroboration comes from the physical event itself, not from institutional authority.
narrative_ontology:disappearance_verdict(behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(behavioral_competence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANEYOSHI RESIDENTS (MOUNTAIN) — The stone directive encodes a behavioral constraint that emerges from repeated empirical observation of tsunami risk: do not build below the marker. This is not a rule imposed by authority but a physical-behavioral fact: the marker's location reflects the maximum reach of historical tsunamis. Compliance is not extraction — it is alignment with observable reality. The constraint persists because the underlying physical geography (tsunami reach) persists, not because anyone enforces it or benefits from it.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational timescale, the stone directive is a constraint that emerges from the physical geography of the Sanriku coast: the subduction zone geometry, the historical frequency and magnitude of tsunamis, and the topography of the Aneyoshi valley. These are not socially constructed — they are features of the Earth's structure. The directive's persistence across 78 years without formal validation reflects that the underlying physical constraint has not changed. The constraint would persist even if the stone were destroyed, because the geography persists.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL DISASTER MANAGEMENT (MOUNTAIN) — From the perspective of tsunami science and regional disaster management, the stone directive is a constraint that emerges from geological and oceanographic facts: the subduction zone's slip rate, the historical tsunami record, and the topographic amplification in the Sanriku valleys. The directive's binding force is not institutional authority but empirical accuracy. The constraint persists because the underlying geological processes persist, not because the directive is enforced or because anyone benefits from compliance.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_competence_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(behavioral_competence_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_competence_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(behavioral_competence_reading),
    narrative_ontology:constraint_metric(behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The directive does not extract value from residents — it constrains where they can build, but this constraint aligns with their safety interests. No agent collects rents from compliance. The low value reflects that the constraint is a physical fact, not an institutional arrangement. Suppression (0.12): Low. Residents are not coerced into compliance — the directive is binding because it is accurate, not because alternatives are suppressed. Some residents may face economic costs from the building restriction, but these are costs of alignment with physical reality, not costs of institutional suppression. Theater ratio (0.15): Very low. The directive has minimal performative content — it is a simple instruction with clear physical referent (the stone marker). The small theater component reflects the cultural/commemorative dimension (the directive is also a historical marker), but the primary function is behavioral guidance, not ritual.
 *
 * PERSPECTIVAL GAP:
 *   The behavioral competence reading produces a mountain classification from all three perspectives because the underlying constraint is physical geography, not institutional arrangement. The powerless residents, the regional authority, and the analytical observer all see the same constraint: a binding limit on building location that emerges from tsunami risk. There is no perspectival gap because there is no extraction mechanism — no agent benefits from the constraint, and no agent is coerced into compliance. The constraint is binding because it is accurate, not because it is enforced. This uniformity across perspectives is diagnostic of a genuine natural law constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The behavioral competence reading has no beneficiaries and no victims because the constraint is not an institutional arrangement. The directive constrains residents' building choices, but this constraint aligns with their safety interests — it is not extraction. The regional authority has no institutional interest in the directive's persistence (no rents, no power consolidation). The constraint persists because the underlying physical geography persists, not because any agent enforces it or benefits from it. Directionality is not applicable to this reading because there is no extraction flow.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_reading,
    'Is the stone directive binding because it encodes accurate behavioral guidance derived from physical geography (behavioral competence reading), or is it binding primarily because it functions as a commemorative marker that sustains collective memory of past disasters (commemorative husk reading)?',
    'Empirical test: measure compliance rates and disaster outcomes in regions where the directive is treated as behavioral guidance vs. regions where it is treated as historical monument. If behavioral reading is correct, compliance should correlate with reduced tsunami casualties. If commemorative reading is correct, compliance should correlate with cultural continuity and collective memory strength, independent of actual tsunami risk.',
    'If behavioral reading is correct: the constraint is a mountain (physical geography constraint with no beneficiary). If commemorative reading is correct: the constraint is a piton (degraded institutional memory mechanism maintained theatrically). The two readings foreclose each other at the level of the constraint''s primary function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_reading, empirical, 'Whether the directive''s binding force derives from physical geography or from commemorative function').

omega_variable(
    validation_gap_significance,
    'Does the 78-year absence of formal scientific validation of the stone directive''s location represent a genuine epistemic gap (the directive''s accuracy is unverified), or does it represent a gap in institutional documentation (the directive''s accuracy is empirically validated through implicit compliance and disaster outcomes, but never formally published)?',
    'Historical analysis of tsunami records, geological surveys, and disaster outcomes in Aneyoshi vs. comparable unprotected settlements. If the directive''s location matches the maximum reach of documented tsunamis, the validation gap is institutional (lack of formal documentation), not empirical (lack of accuracy). If the directive''s location diverges from tsunami records, the validation gap is empirical (the directive may be inaccurate).',
    'If institutional gap: the constraint''s binding force is undiminished by lack of formal validation — the directive is accurate regardless of whether scientists have published it. If empirical gap: the constraint''s accuracy is genuinely uncertain, and the 78-year persistence without validation is a structural risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(validation_gap_significance, empirical, 'Whether the validation gap is institutional (lack of documentation) or empirical (lack of accuracy)').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the stone directive a natural law (a constraint that emerges from physical geography and would persist regardless of human institutions), or is it a constructed constraint (a rule that persists because of institutional and cultural mechanisms, and could be abandoned if those mechanisms failed)?',
    'Counterfactual analysis: if the stone were destroyed and all institutional memory of the directive were lost, would residents independently rediscover the same location as the safe building boundary? If yes, the constraint is natural law. If no, the constraint is constructed (dependent on institutional transmission).',
    'If natural law: the constraint is a mountain (no beneficiary, no enforcement needed). If constructed: the constraint is a piton or snare (dependent on institutional maintenance, possibly extractive). The classification hinges on whether the constraint''s binding force is independent of human institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the constraint emerges from physical geography or from institutional/cultural mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behcomp_theater_t0, behavioral_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(behcomp_theater_t20, behavioral_competence_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(behcomp_theater_t40, behavioral_competence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(behcomp_theater_t78, behavioral_competence_reading, theater_ratio, 78, 0.15).

% Extraction over time
narrative_ontology:measurement(behcomp_extract_t0, behavioral_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(behcomp_extract_t20, behavioral_competence_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(behcomp_extract_t40, behavioral_competence_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(behcomp_extract_t78, behavioral_competence_reading, base_extractiveness, 78, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(behcomp_suppress_t0, behavioral_competence_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(behcomp_suppress_t20, behavioral_competence_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(behcomp_suppress_t40, behavioral_competence_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(behcomp_suppress_t78, behavioral_competence_reading, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_directive kernel decomposes into two structurally distinct constraint stories: behavioral_competence_reading (this file) and commemorative_husk_reading. The behavioral competence reading treats the directive as a physical geography constraint (mountain, ε ≈ 0.08); the commemorative husk reading treats it as an institutional memory mechanism (piton, ε ≈ 0.45). The two readings have different ε values because they measure different observables: behavioral competence measures alignment with tsunami risk, while commemorative function measures cultural transmission and collective memory. Per the ε-invariance principle, these are two distinct constraints, not two measurements of the same constraint. They are linked via network.affects_constraints because they share a kernel (the stone directive) and because the behavioral competence reading's accuracy (or inaccuracy) affects the commemorative reading's legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
