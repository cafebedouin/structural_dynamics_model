% ============================================================================
% CONSTRAINT STORY: behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Directive as Behavioral Competence Constraint
 *   domain: disaster_anthropology/land_use_governance/institutional_memory
 *
 * SUMMARY:
 *   In 1933, following a devastating tsunami, the village of Aneyoshi erected
 *   a stone marker at the high-water line with the directive: 'Do not build
 *   homes below this point.' The directive was retained as a binding land-use
 *   constraint for 78 years without formal re-validation. In 2011, the Tohoku
 *   tsunami inundated the coast up to but not beyond the stone line — every
 *   structure above the marker survived; every structure below it was
 *   destroyed. This constraint story models the BEHAVIORAL COMPETENCE reading
 *   of the stone directive: the directive encodes a physical geography
 *   constraint (tsunami inundation elevation) in durable institutional form,
 *   and its 78-year retention without validation reflects appropriate
 *   stability rather than institutional inertia. The directive is not a rule
 *   that extracts from residents but a translation mechanism that preserves
 *   geological knowledge across generational timescales. The 2011 tsunami
 *   provides the critical empirical test: if the directive's threshold
 *   corresponded to actual inundation risk, the behavioral competence reading
 *   is validated. The measurements show theater_ratio and extractiveness
 *   rising slightly during the middle period (1973-1993, when tsunami memory
 *   faded and compliance may have become more ritualistic) before dropping in
 *   2011 when the directive's function was empirically confirmed.
 *
 * KEY AGENTS:
 *   - Coastal Residents: Constrained by physical geography (powerless/trapped) — the directive does not extract from them but coordinates their behavior around a natural limit
 *   - Municipal Planning Authority: Enforces the directive (institutional/mobile) — enforcement is not extraction but prevention of construction in the inundation zone
 *   - Analytical Observer: Sees the directive as information-preservation technology (analytical/analytical) — the stone translates geological knowledge into durable behavioral constraint
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
narrative_ontology:constraint_metric(behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_competence_reading, mountain).
narrative_ontology:human_readable(behavioral_competence_reading, "Aneyoshi Stone Directive as Behavioral Competence Constraint").
narrative_ontology:topic_domain(behavioral_competence_reading, "disaster_anthropology/land_use_governance/institutional_memory").

domain_priors:emerges_naturally(behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_competence_reading, 'cfe93e78-3eac-4544-afa6-556f68ac2a12').
narrative_ontology:cs_kernel_codification('cfe93e78-3eac-4544-afa6-556f68ac2a12', fixed_text).
narrative_ontology:cs_authority_grounding('cfe93e78-3eac-4544-afa6-556f68ac2a12', practice).
narrative_ontology:cs_reading_relation('cfe93e78-3eac-4544-afa6-556f68ac2a12', behavioral_competence_reading__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('cfe93e78-3eac-4544-afa6-556f68ac2a12', foundational, directive_encodes_physical_geography).
narrative_ontology:cs_axiom_status(directive_encodes_physical_geography, holdable).
narrative_ontology:cs_axiom_grounding('cfe93e78-3eac-4544-afa6-556f68ac2a12', directive_encodes_physical_geography, empirically_contingent).
narrative_ontology:cs_axiom('cfe93e78-3eac-4544-afa6-556f68ac2a12', secondary, geological_timescale_stability_permits_unvalidated_retention).
narrative_ontology:cs_axiom_status(geological_timescale_stability_permits_unvalidated_retention, holdable).
narrative_ontology:cs_axiom_grounding('cfe93e78-3eac-4544-afa6-556f68ac2a12', geological_timescale_stability_permits_unvalidated_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('cfe93e78-3eac-4544-afa6-556f68ac2a12', post_1933_tsunami_geological_knowledge).
narrative_ontology:cs_drift_state('cfe93e78-3eac-4544-afa6-556f68ac2a12', pre_2011_validation, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('cfe93e78-3eac-4544-afa6-556f68ac2a12', '').
narrative_ontology:cs_kernel_id(behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL RESIDENT (MOUNTAIN) — The stone directive encodes a physical geography constraint: tsunami inundation reaches this elevation. Compliance is not extraction but recognition of natural law. The resident is 'trapped' by geography, not by the directive.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL PLANNING AUTHORITY (MOUNTAIN) — The directive coordinates land use around a physical constraint. The authority enforces the directive not to extract but to prevent construction in the inundation zone. The 78-year retention without validation reflects that the underlying physical constraint (tsunami reach) has not changed.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — The directive is a behavioral competence mechanism: it translates geological knowledge (tsunami inundation elevation) into a durable land-use rule. The 78-year persistence without validation is not institutional inertia but appropriate stability — the physical constraint the directive encodes has not changed, so the directive should not change. This reading sees the stone as a successful information-preservation technology.
constraint_indexing:constraint_classification(behavioral_competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.08): Very low. The directive imposes a land-use constraint, but the constraint encodes a physical geography limit rather than extracting rents. Residents who comply avoid tsunami risk; residents who violate the directive face natural consequences, not institutional punishment. The slight extractiveness reflects the opportunity cost of foregone coastal construction, but this is not extraction in the DR sense — it is the cost of avoiding a natural hazard. Suppression (0.12): Very low. The directive is enforced through municipal planning authority, but enforcement is minimal because compliance is in residents' self-interest once the physical constraint is understood. The directive does not suppress alternatives (residents can build above the line) or prevent exit (residents can leave the village). Theater ratio (0.15): Very low. The directive's function is genuine: it prevents construction in the inundation zone. The theater component reflects the period (1973-1993) when tsunami memory faded and compliance may have become ritualistic rather than risk-based, but the 2011 tsunami re-validated the directive's function and reduced theater. Accessibility collapse (0.92): Very high. Once the physical constraint (tsunami inundation elevation) is understood, alternatives collapse — there is no other elevation threshold that would be safer. Resistance (0.08): Very low. The directive meets minimal resistance because it encodes a physical limit that residents can verify through historical memory and geological evidence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap — all agents classify it as mountain. The coastal resident, the municipal planning authority, and the analytical observer all recognize the directive as encoding a physical geography constraint. The uniformity of classification reflects that the directive successfully translates geological knowledge into institutional form: the stone makes the physical constraint (tsunami inundation elevation) legible and durable across generational timescales. The absence of a perspectival gap is itself diagnostic: genuine natural law constraints produce consensus across observation sites because the constraint is invariant to the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the directive does not extract from any agent. Coastal residents bear the opportunity cost of foregone coastal construction, but this is not extraction — it is the cost of avoiding a natural hazard. The municipal planning authority enforces the directive but does not benefit from enforcement. The directive coordinates behavior around a physical constraint; it does not transfer resources from one agent to another. All perspectives classify as mountain because all agents recognize the directive as encoding a natural limit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all long-retained institutional rules are pitons. The directive's 78-year retention without validation is not institutional inertia but appropriate stability: the underlying physical constraint (tsunami inundation elevation) has not changed, so the directive should not change. The 2011 Tohoku tsunami provides the critical empirical test — the directive's threshold corresponded to actual inundation risk, validating the behavioral competence reading. The mandate (prevent construction in the inundation zone) remains aligned with the function (encode physical geography constraint). This is the opposite of mandatrophy: the directive's persistence reflects that its function has NOT atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Aneyoshi stone directive a behavioral competence constraint (encoding physical geography) or a commemorative husk (institutional inertia maintaining a rule whose function has atrophied)?',
    'Empirical test: Does the directive''s elevation threshold correspond to actual tsunami inundation risk? If the 2011 Tohoku tsunami validated the threshold (inundation stopped below the stone line), the behavioral competence reading is correct. If the threshold is arbitrary or the inundation exceeded it, the commemorative husk reading is correct.',
    'If behavioral competence: the constraint is a genuine mountain (natural law encoded in institutional form). If commemorative husk: the constraint is a piton (degraded function maintained theatrically). The 2011 Tohoku tsunami provides the critical test case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the stone directive encodes physical geography or institutional inertia').

omega_variable(
    validation_necessity_threshold,
    'At what interval does a land-use constraint encoding physical geography require re-validation to remain legitimate?',
    'Comparative analysis of stable vs changing physical constraints: geological timescales (tsunami inundation zones, floodplains) vs human timescales (economic conditions, technology). If the underlying physical constraint is stable across centuries, validation may be unnecessary.',
    'If validation interval > 78 years for geological constraints: the directive''s retention without validation is appropriate (mountain). If validation interval < 78 years: the directive''s retention is institutional inertia (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_necessity_threshold, conceptual, 'Validation interval for physical geography constraints').

omega_variable(
    sibling_reading_coexistence,
    'Can the behavioral competence reading and the commemorative husk reading coexist as simultaneous descriptions of the same directive at different historical moments?',
    'Temporal decomposition: the directive may have been behavioral competence at T0 (1933, encoding fresh tsunami memory) and degraded into commemorative husk at T1 (1980s, when the memory faded and compliance became ritualistic) before being re-validated as behavioral competence at T2 (2011, when the tsunami confirmed the threshold).',
    'If readings can coexist temporally: the kernel is not a single constraint but a constraint family with different ε values at different time points. If readings are mutually exclusive: one reading is structurally false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sibling readings describe different temporal phases of the same directive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_theater_1933, behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aneyoshi_theater_1953, behavioral_competence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(aneyoshi_theater_1973, behavioral_competence_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(aneyoshi_theater_1993, behavioral_competence_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(aneyoshi_theater_2011, behavioral_competence_reading, theater_ratio, 78, 0.15).

% Extraction over time
narrative_ontology:measurement(aneyoshi_extract_1933, behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aneyoshi_extract_1953, behavioral_competence_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(aneyoshi_extract_1973, behavioral_competence_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement(aneyoshi_extract_1993, behavioral_competence_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(aneyoshi_extract_2011, behavioral_competence_reading, base_extractiveness, 78, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_competence_reading, information_standard).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone directive kernel decomposes into two readings with substantially different ε values. The behavioral competence reading (this constraint) has very low extractiveness (0.08) because the directive encodes a physical geography constraint. The commemorative husk reading has higher extractiveness because it models the directive as institutional inertia extracting compliance costs without providing risk-reduction function. The 2011 Tohoku tsunami provides the empirical test: the directive's threshold corresponded to actual inundation risk, supporting the behavioral competence reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
