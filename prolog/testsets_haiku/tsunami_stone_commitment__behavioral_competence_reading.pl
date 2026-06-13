% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Intergenerational Tsunami Stone Norm Transmission (Behavioral Competence Reading)
 *   domain: institutional/disaster_anthropology
 *
 * SUMMARY:
 *   In coastal Japan and similar tsunami-prone regions, ancestral communities
 *   encoded disaster knowledge in stone markers and sustained transmission
 *   through intergenerational practice. Under the BEHAVIORAL COMPETENCE
 *   READING, the stone norms operated as a live institutional constraint:
 *   knowledge-keepers actively maintained and transmitted evacuation
 *   procedures, younger residents learned embodied disaster response, and the
 *   constraint functioned to protect lives across centuries. The 2011 Tōhoku
 *   tsunami provided a decisive empirical test: villages that retained the
 *   norms evacuated successfully; villages that had abandoned them suffered
 *   catastrophic casualties. This reading treats the constraint as a piton —
 *   a degraded but historically effective institution that functioned as
 *   intended. The claim and metrics are intentionally independent: the
 *   constraint is CLAIMED as piton (mostly performative maintenance in modern
 *   era, with low extraction) while the underlying structural data describe a
 *   coordination mechanism that produced real protective outcomes.
 *
 * KEY AGENTS:
 *   - coastal_community_descendants: beneficiaries of the evacuation knowledge and protection without bearing extraction costs
 *   - village_knowledge_keepers: custodians of the norms, maintaining and transmitting them across generations without profit motive
 *   - younger_generation_residents: passive beneficiaries, learning disaster competence through embodied practice
 *   - historical_stone_placers: ancestral agenda-setters who designed the constraint (analytical reference, not active)
 *   - geological_hazard_distribution: the recurrent hazard pattern the constraint encodes (analytical reference, non-agent)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Intergenerational Tsunami Stone Norm Transmission (Behavioral Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "institutional/disaster_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, 'b7f3b9da-c7f7-449e-a137-c7a120133a93').
narrative_ontology:cs_kernel_codification('b7f3b9da-c7f7-449e-a137-c7a120133a93', fixed_text).
narrative_ontology:cs_authority_grounding('b7f3b9da-c7f7-449e-a137-c7a120133a93', practice).
narrative_ontology:cs_interpretation_layer_present('b7f3b9da-c7f7-449e-a137-c7a120133a93').
narrative_ontology:cs_reading_relation('b7f3b9da-c7f7-449e-a137-c7a120133a93', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('b7f3b9da-c7f7-449e-a137-c7a120133a93', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('b7f3b9da-c7f7-449e-a137-c7a120133a93', foundational, intergenerational_knowledge_transmission_remains_robust).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_transmission_remains_robust, holdable).
narrative_ontology:cs_axiom_grounding('b7f3b9da-c7f7-449e-a137-c7a120133a93', intergenerational_knowledge_transmission_remains_robust, empirically_contingent).
narrative_ontology:cs_axiom('b7f3b9da-c7f7-449e-a137-c7a120133a93', foundational, stone_norm_behavioral_force_produces_measurable_protective_outcomes).
narrative_ontology:cs_axiom_status(stone_norm_behavioral_force_produces_measurable_protective_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('b7f3b9da-c7f7-449e-a137-c7a120133a93', stone_norm_behavioral_force_produces_measurable_protective_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('b7f3b9da-c7f7-449e-a137-c7a120133a93', ancestral_protective_knowledge_system).
narrative_ontology:cs_drift_state('b7f3b9da-c7f7-449e-a137-c7a120133a93', contemporary_institutional_warning_system_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7f3b9da-c7f7-449e-a137-c7a120133a93', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 at interval end) because the constraint transfers knowledge and protection benefit with no rent collection, no coercive overhead, and no asymmetric extraction. Knowledge-keepers do not profit; they bear identity-locked custodial costs. Suppression is negligible (0.12) because compliance emerges from embodied practice and genuine survival value, not coercion. Theater ratio rises over the interval (0.05 to 0.18) as modern institutional alternatives (warning systems, evacuation orders, written guides) emerge, making the stone norms increasingly performative rather than functionally necessary — but even at interval end, the theater ratio remains below 0.5, indicating the norms retain substantive protective function. Accessibility collapse is high (0.92) because alternatives to the embodied knowledge require complete geographic relocation or abandonment of ancestral lands; for identity-locked community members, the exit cost is total. Resistance is minimal (0.05) because the norms are transmitted as cultural knowledge, not imposed by force. The measurement series show stable extractiveness with rising theater (degradation from coordinating mechanism to cultural artifact), particularly from t=100 onward as institutional warning systems and modern infrastructure become available.
 *
 * PERSPECTIVAL GAP:
 *   From the knowledge-keepers' seat, the constraint is custodial duty without extraction; from the younger generation's seat, it is embodied cultural competence acquired without awareness of enforcement; from outside observers (geologists, disaster response professionals), it is an effective institutional technology that reduces casualties. All three perspectives compute the same classification (piton: low extraction, no beneficiary rent-collection, performative maintenance in modern era) but from different bases — this is appropriate piton structure. The constraint's behavioral force persists not through coercion but through the genuine protective value survivors observe during seismic events.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder occupies an extractive position. Knowledge-keepers bear custodial cost (high time_horizon commitment, identity-locked exit). Younger residents are beneficiaries with constrained exit (embodied knowledge is high-cost to unlearn). The constraint does not concentrate gains in any seat; benefits are distributed across the community and across generations. This is the defining feature of a piton: no party extracts, so no party maintains it against benefit-cost calculus; persistence depends on cultural transmission and the constraint's demonstrated protective value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving tsunami knowledge across generations in pre-literate and post-disaster environments) is LIVE: coastal communities still face multi-generational tsunami hazards, intergenerational knowledge loss remains a real failure mode, and written records can be destroyed. However, the SOLUTION (stone norms as the primary mechanism) has partially atrophied: modern institutional alternatives (seismic warning systems, written evacuation procedures, institutional disaster response) now carry more of the protective load. The theater ratio rises accordingly as the constraint shifts from functional necessity to cultural reinforcement. This is the defining trajectory of a piton: the founding problem persists, the constraint retains some behavioral force (as demonstrated in 2011), but its primary function has migrated to institutional alternatives. The constraint persists through cultural transmission momentum and demonstrated protective value, not through active enforcement or beneficiary stake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transmission_mechanism_vitality,
    'Is the intergenerational transmission of stone-norm knowledge currently robust, weakening, or stalled among coastal residents born after 1980?',
    'Ethnographic survey tracking knowledge of evacuation procedures, stone locations, and hazard interpretation among age cohorts; comparison of transmission rates in villages with high vs. low institutional warning system deployment.',
    'If transmission is weakening substantially, the constraint is shifting from live behavioral enforcement toward piton status (cultural artifact retained through momentum). If transmission remains robust despite modern alternatives, the constraint retains greater behavioral force than piton classification suggests. This affects the theater_ratio trajectory and the time-horizon of the constraint''s eventual obsolescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_mechanism_vitality, empirical, 'Empirical status of intergenerational knowledge transmission in contemporary coastal communities.').

omega_variable(
    behavioral_causation_2011_tsunami,
    'In villages that evacuated successfully during the 2011 Tōhoku tsunami, did evacuation occur BECAUSE residents followed stone norms, or was it simply spatially coordinated with stone locations while driven by other cues (earthquake intensity, institutional warnings, social observation)?',
    'Post-disaster ethnographic interviews with evacuees documenting decision-making: did they consciously recall and follow stone-marking guidance, or did institutional warning systems dominate their response? Triangulation with institutional warning system timing and coverage data.',
    'If stone norms were the primary behavioral driver, the constraint''s protective efficacy is direct and the behavioral competence reading is strongly supported. If institutional warnings dominated and stone compliance was epiphenomenal, the constraint''s functional force is weaker than this reading claims and the commemorative_husk reading gains plausibility. This directly affects ε estimation: measured extractiveness assumes behavioral causation; if causation is weak, the constraint''s real coordination function is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_causation_2011_tsunami, empirical, 'Causal attribution for evacuation behavior in the 2011 tsunami: norm-driven vs. warning-system-driven vs. social mimicry.').

omega_variable(
    knowledge_keeper_motivation_evolution,
    'Do contemporary village knowledge-keepers maintain the stone norms out of genuine belief in their protective function, cultural obligation and identity-lock, institutional pressure, or touristic performance for external audiences?',
    'Ethnographic interviews with knowledge-keepers documenting their stated motivations, maintenance effort, and relationship to institutional alternatives. Observation of whether maintenance intensity correlates with hazard cycles (increases after earthquakes) or external attention.',
    'If motivation is genuine belief in protective function, the constraint retains behavioral force and custodial legitimacy. If motivation is pure identity-lock (no functional belief), the constraint approaches performative maintenance. If touristic performance dominates, the theater_ratio is higher than authored. This affects classification stability: genuine-belief maintenance is more resilient to institutional substitution; identity-lock or touristic motivation suggests the constraint is more vulnerable to decay as identity contexts change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_keeper_motivation_evolution, empirical, 'Motivational structure sustaining knowledge-keeper participation in norm transmission.').

omega_variable(
    reading_foreclosure_via_2011_causation,
    'Does the empirical resolution of the behavioral_causation_2011_tsunami omega necessitate foreclosure of one reading and holdability of the other, or is the evidence ambiguous enough that both readings remain coexistent?',
    'Post-disaster evidence combined with institutional warning data and survivor accounts is examined for clarity: if 2011 outcomes clearly track stone-norm knowledge and institutional warning system coverage shows gaps, behavioral competence is supported; if both systems tracked together, causation is ambiguous; if institutional systems dominated, commemorative_husk gains.',
    'If the 2011 evidence clearly forecloses one reading, the sibling reading must be reclassified as logically incompatible within a single framework (relation shifts from coexists_with to forecloses). If evidence remains ambiguous, the readings remain coexistent, each with a different evidence weight. This affects the terminal-state prediction: foreclosure routes toward one dominant reading and potential institutional settlement; coexistence maintains indefinite interpretive contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_2011_causation, conceptual, 'Whether empirical resolution of behavioral causation logically forecloses the sibling reading or leaves both readings coexistent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsun_tr_t50, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(tsun_tr_t150, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 150, 0.18).
narrative_ontology:measurement(tsun_tr_t200, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement(tsun_tr_t250, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 250, 0.18).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(tsun_be_t50, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(tsun_be_t150, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 150, 0.09).
narrative_ontology:measurement(tsun_be_t200, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 200, 0.08).
narrative_ontology:measurement(tsun_be_t250, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 250, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.06).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into at least three structurally distinct constraints: (1) behavioral_competence_reading (this story) — stone norms retain live behavioral force, intergenerational transmission is robust, protective outcomes measurable; (2) commemorative_husk_reading (sibling) — stone norms have decayed to symbolic artifact, transmission is weakening, protective effects are incidental; (3) catastrophe_validation_axis (sibling) — the 2011 tsunami as a decisive empirical test that validates or invalidates one of the above readings. The readings foreclose or coexist depending on empirical resolution of behavioral causation (see omega: behavioral_causation_2011_tsunami). All three stories share the same kernel text (the stones themselves) but instantiate radically different constraints because they measure different aspects: transmission robustness, functional efficacy, and empirical validation. ε estimates differ markedly: behavioral_competence is piton-range (0.08), commemorative_husk would be much lower (approaching zero), and catastrophe_validation is a binary outcome detector rather than a steady-state constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
