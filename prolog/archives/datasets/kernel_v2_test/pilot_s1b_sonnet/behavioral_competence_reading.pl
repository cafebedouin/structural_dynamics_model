% ============================================================================
% CONSTRAINT STORY: behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
 *   human_readable: Aneyoshi Stone as Live Regulatory Mechanism (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/land_use_regulation
 *
 * SUMMARY:
 *   In 1933, survivors of the Shōwa Sanriku tsunami erected a stone marker in
 *   the village of Aneyoshi (Iwate Prefecture, Japan) inscribed with a
 *   directive: 'High dwellings are the peace and harmony of our descendants.
 *   Remember the calamity of the great tsunamis. Do not build any homes below
 *   this point.' The stone was placed at approximately 60 meters elevation.
 *   For 78 years (1933-2011), the stone functioned as a live land-use rule:
 *   residents built homes above the marker, and municipal planning
 *   incorporated the directive. When the 2011 Tōhoku tsunami struck, every
 *   structure in Aneyoshi above the stone line survived; structures below it
 *   (built in defiance of the directive or predating it) were destroyed. Zero
 *   residents died. This constraint story models the stone AS IT OPERATED
 *   from 1933-2011 under the behavioral competence reading: the stone
 *   retained operational force as a regulatory mechanism, and the 2011
 *   survival outcome is causal evidence of compliance. The sibling reading
 *   (commemorative_husk_reading) models the alternative hypothesis that the
 *   stone degraded into a memorial with negligible regulatory force and 2011
 *   survival was coincidental. The structural delta between readings is
 *   substantial (ε_behavioral ≈ 0.12 vs ε_commemorative ≈ 0.65), reflecting
 *   fundamentally different claims about what the stone DID during the
 *   78-year interval.
 *
 * KEY AGENTS:
 *   - Post-2011 Aneyoshi Residents: Primary beneficiaries (powerless to moderate / constrained to mobile, depending on cohort) — survived the 2011 tsunami because the stone directive kept them out of the inundation zone; experienced the stone as coordination rather than constraint
 *   - 1933-2011 Aneyoshi Residents (Generational Cohort): Coordination participants (powerless / constrained) — followed the stone directive across generational turnover; the constraint coordinated building decisions without significant extraction
 *   - Municipal Planning Authority: Coordination agent (moderate / mobile) — incorporated the stone directive into official land-use planning; benefited from the stability of a simple, durable rule
 *   - Downstream Coastal Communities: Indirect beneficiaries (powerless to institutional / varies) — the Aneyoshi case provides empirical evidence that stone-marker commitments can work, influencing post-2011 disaster mitigation policy across the Tōhoku region
 *   - Analytical Observer: The disaster anthropology and commitment-systems research community studying the stone as a successful case of long-duration institutional memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_competence_reading, 0.12).
domain_priors:suppression_score(behavioral_competence_reading, 0.18).
domain_priors:theater_ratio(behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(behavioral_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(behavioral_competence_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_competence_reading, rope).
narrative_ontology:human_readable(behavioral_competence_reading, "Aneyoshi Stone as Live Regulatory Mechanism (Behavioral Competence Reading)").
narrative_ontology:topic_domain(behavioral_competence_reading, "disaster_anthropology/commitment_systems/land_use_regulation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behavioral_competence_reading, '1a9b8585-3750-4bbd-8f12-c49e804eae70').
narrative_ontology:cs_kernel_codification('1a9b8585-3750-4bbd-8f12-c49e804eae70', fixed_text).
narrative_ontology:cs_authority_grounding('1a9b8585-3750-4bbd-8f12-c49e804eae70', practice).
narrative_ontology:cs_interpretation_layer_present('1a9b8585-3750-4bbd-8f12-c49e804eae70').
narrative_ontology:cs_reading_relation('1a9b8585-3750-4bbd-8f12-c49e804eae70', behavioral_competence_reading__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('1a9b8585-3750-4bbd-8f12-c49e804eae70', foundational, directive_causally_constrained_building_decisions).
narrative_ontology:cs_axiom_status(directive_causally_constrained_building_decisions, holdable).
narrative_ontology:cs_axiom_grounding('1a9b8585-3750-4bbd-8f12-c49e804eae70', directive_causally_constrained_building_decisions, empirically_contingent).
narrative_ontology:cs_axiom('1a9b8585-3750-4bbd-8f12-c49e804eae70', secondary, id_2011_survival_outcome_demonstrates_compliance).
narrative_ontology:cs_axiom_status(id_2011_survival_outcome_demonstrates_compliance, holdable).
narrative_ontology:cs_axiom_grounding('1a9b8585-3750-4bbd-8f12-c49e804eae70', id_2011_survival_outcome_demonstrates_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('1a9b8585-3750-4bbd-8f12-c49e804eae70', founding_directive_1933).
narrative_ontology:cs_drift_state('1a9b8585-3750-4bbd-8f12-c49e804eae70', pre_2011_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1a9b8585-3750-4bbd-8f12-c49e804eae70', '').
narrative_ontology:cs_kernel_id(behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_competence_reading, post_2011_aneyoshi_residents).
narrative_ontology:constraint_beneficiary(behavioral_competence_reading, downstream_coastal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL RESIDENT ACROSS 78 YEARS (ROPE) — The stone directive solves a genuine coordination problem: where to build in a tsunami-prone valley. Residents face constrained exit (cannot easily relocate) but experience the stone as coordination rather than extraction. The directive provides clear, persistent guidance that prevents drift into hazardous areas. Low extraction: the constraint coordinates without asymmetric cost-bearing.
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: MUNICIPAL PLANNING AUTHORITY (ROPE) — The stone functions as a stable regulatory input that simplifies land-use planning. The authority has moderate power and mobile exit options (can adopt alternative planning frameworks) but retains the stone directive because it encodes verified hazard knowledge. The coordination function is clear: the stone preserves institutional memory across administrative turnover. No significant extraction — the planning authority benefits from the stability.
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL DISASTER MANAGEMENT FRAMEWORK (ROPE) — At the institutional/civilizational level, the Aneyoshi stone represents a success case for community-scale hazard mitigation. The framework has arbitrage-level exit (can promote alternative mitigation strategies) but references the stone as evidence that simple, durable commitments can preserve life-saving knowledge across generational timescales. The stone coordinates without extraction — it demonstrates that low-tech institutional memory works.
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From the analytical position, the stone is a rare example of commitment-system success: a 1933 directive that retained operational force through 78 years and demonstrably saved lives in 2011. The observable is building location decisions, and the stone's causal role is empirically verified (structures above the stone line survived; structures below it did not). Extraction is minimal — the coordination function dominates. The stone shows that disaster mitigation commitments CAN persist without degrading into theater or extraction, given the right structural conditions (simple directive, clear physical marker, local custodianship, empirical vindication).
constraint_indexing:constraint_classification(behavioral_competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_competence_reading_tests).
:- end_tests(behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The stone directive solves a genuine coordination problem (where to build in a tsunami-prone valley) with minimal asymmetric cost. Residents above the line are constrained from building in more convenient low-lying areas, but the constraint is symmetric (applies to all equally) and the cost is modest relative to the survival benefit. No identifiable beneficiary collects rents from the arrangement — the original stone-erectors are long dead, and no institution extracts from the directive's operation. The small residual extraction reflects the opportunity cost of constrained land use and the custodianship labor (maintaining the stone, transmitting its meaning), but these are coordination costs rather than asymmetric extraction. Suppression (0.18): Low. The stone directive is not enforced coercively — residents can and occasionally did build below the line (structures predating the stone, or built in defiance of it). The suppression value reflects social pressure and municipal planning integration, not legal prohibition. Alternatives (building below the line for convenience, ignoring the directive) were available but socially costly. Theater ratio (0.05): Very low. The stone's function was operational, not performative. The directive guided actual building decisions, and maintenance was functional (preserving legibility and transmitting the directive) rather than theatrical. The 2011 survival outcome is empirical evidence that the constraint's function was real, not performance. The measurements show stable low theater across the interval — no drift toward ritualization. Accessibility collapse (0.92): Very high. Once the 2011 tsunami validated the stone's directive, alternative land-use frameworks (building below the line, dismissing the stone as superstition) collapsed almost completely. Before 2011, alternatives were available but socially discouraged; after 2011, they became structurally unthinkable. Resistance (0.08): Very low. The stone directive met minimal active resistance. The primary resistance came from convenience (low-lying land is easier to build on and closer to the shore) and occasional skepticism across generational turnover, but no organized opposition formed. The low resistance after 1933 suggests the directive was understood as coordination rather than imposition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits MINIMAL perspectival gap — all four perspectives classify as Rope. The uniformity is diagnostically significant: it distinguishes genuine coordination (uniform Rope) from naturalized extraction (false summit: claimed Mountain with hidden beneficiaries). The Aneyoshi stone has no hidden extraction — the 1933 survivors who erected it are dead, no institution collects rents, and the coordination function is empirically verified by the 2011 outcome. The absence of a Snare or Tangled Rope perspective confirms that the constraint lacks an asymmetric extraction mechanism. The minimal gap differentiates this constraint from typical disaster-mitigation policies, which often drift into extraction (zoning rules that benefit property owners, insurance schemes that subsidize risk-takers, building codes that favor contractors). The Aneyoshi stone avoided these failure modes. The structural question is WHY: what design features (physical durability, simplicity, local custodianship, lack of external enforcement) enabled 78 years of coordination without extraction or theater?
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as Rope because all agents are net beneficiaries or neutral relative to the constraint. The stone directive coordinates without extracting. Individual residents (powerless/constrained) experience low effective extraction because they benefit from the survival guarantee the directive provides — the constraint runs TOWARD them (protection) rather than away from them (cost). The municipal planning authority (moderate/mobile) benefits from a stable, simple rule that encodes verified hazard knowledge. The national disaster framework (institutional/arbitrage) benefits from a success case that demonstrates low-tech institutional memory works. The analytical observer sees a rare example of commitment persistence without degradation. No agent is a victim — the constraint has no identifiable extraction target. The directionality derivation produces near-zero or negative d (net beneficiary) for all agents, yielding low chi across all perspectives. The Rope classification is uniform not because the constraint is a natural law but because the coordination function genuinely dominates and no asymmetric extraction exists.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination (Rope) can persist across generational timescales without degrading into theater (Piton) or accumulating extraction (Tangled Rope → Snare drift). The mandate (protect descendants from tsunami) has NOT outlived its function — the 2011 outcome empirically confirmed the function remains live. The stone exemplifies coordination that works: simple directive, clear physical marker, local custodianship, empirical vindication, no external enforcement, no rent-seeking intermediaries. The classification is stable across the 78-year interval (measurements show minimal drift), which is rare for human institutions. Most disaster-mitigation commitments either attenuate (forgotten after a few generations) or accumulate extraction (captured by contractors, insurers, or bureaucrats). The Aneyoshi stone did neither. The mandatrophy question ('Has the mandate outlived its function?') has a clear answer here: No. The stone's mandate is to keep residents above the inundation line, and the 2011 tsunami validated that the mandate remains relevant. The constraint is NOT a case of mandatrophy but a case of mandate-function alignment persisting across time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the behavioral_competence_reading of the aneyoshi_stone_commitment kernel. The commemorative_husk_reading is a sibling constraint treating the same stone as a degraded memorial with negligible operational force. What structural features distinguish these readings, and under what conditions does one reading displace the other?',
    'Longitudinal observation of building location decisions relative to the stone line; interview data with residents and municipal planners about the stone''s role in decision-making; comparison of pre-2011 vs post-2011 land-use patterns. If the stone causally constrained building decisions from 1933-2011 and the 2011 survival pattern reflects that constraint, the behavioral_competence_reading is structurally accurate. If the stone was commemorative-only and 2011 survival was coincidental or explained by other factors (topography, recent municipal zoning independent of the stone), the commemorative_husk_reading is accurate.',
    'Classification delta: behavioral_competence_reading → rope (ε ≈ 0.12); commemorative_husk_reading → piton (ε ≈ 0.65). The sibling readings differ on whether the 78-year interval shows persistent regulatory force or gradual atrophy into performance. The engine does not adjudicate between readings — it models each as a distinct constraint with its own ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, empirical, 'Committer-frame uncertainty: which reading of the Aneyoshi stone kernel is structurally accurate').

omega_variable(
    custodianship_mechanism_necessity,
    'Is local custodianship (the community maintaining the stone and transmitting its directive across generations) a necessary condition for the stone''s regulatory force, or would the physical marker alone have been sufficient?',
    'Comparative analysis of other tsunami stones in the Tōhoku region: stones with active custodianship vs stones that became unattended landmarks. If unattended stones retained no regulatory force (building patterns ignored them) while attended stones retained force, custodianship is necessary. If both categories retained force, the physical marker suffices.',
    'If custodianship is necessary, the low extraction (ε = 0.12) is conditional on sustained community labor — the Rope classification holds only while the custodianship mechanism persists. If the marker alone suffices, the Rope classification is more robust (coordination persists even if the community disengages). This bears on generalizability: can other communities replicate the Aneyoshi model by erecting stones, or does replication require also building custodianship structures?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodianship_mechanism_necessity, empirical, 'Whether local custodianship is necessary for the stone''s regulatory force').

omega_variable(
    post_2011_commitment_drift,
    'Has the 2011 empirical vindication (zero deaths in Aneyoshi, massive casualties in non-compliant communities) shifted the stone from a coordination mechanism to an extractive monument — a site that now collects tourism, media attention, and institutional prestige for the community?',
    'Temporal measurements post-2011: does extractiveness increase as the stone becomes a pilgrimage site for disaster researchers and media? Does the community begin performing maintenance theatrically (for external audiences) rather than functionally (for internal coordination)? Does the stone''s directive shift from ''live rule we follow'' to ''proof of our wisdom that others should admire''?',
    'If extraction rises post-2011, the constraint drifts from Rope toward Piton — the original coordination function atrophies as the stone becomes a monument to past success rather than a guide for present decisions. The behavioral_competence_reading would then be time-bounded (accurate for 1933-2011 but degrading after 2011), and a third reading (post_vindication_monument) might be needed for the post-2011 interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2011_commitment_drift, empirical, 'Whether 2011 vindication initiates drift from coordination to extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_theater_1933, behavioral_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aneyoshi_theater_1953, behavioral_competence_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(aneyoshi_theater_1973, behavioral_competence_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(aneyoshi_theater_1993, behavioral_competence_reading, theater_ratio, 60, 0.06).
narrative_ontology:measurement(aneyoshi_theater_2011, behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aneyoshi_extraction_1933, behavioral_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(aneyoshi_extraction_1953, behavioral_competence_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(aneyoshi_extraction_1973, behavioral_competence_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(aneyoshi_extraction_1993, behavioral_competence_reading, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(aneyoshi_extraction_2011, behavioral_competence_reading, base_extractiveness, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The behavioral_competence_reading and commemorative_husk_reading are dual formulations of the same 1933 stone marker, evaluated via different observables. The behavioral reading uses building location decisions as the observable and finds low extraction (stone functions as regulatory coordination). The commemorative reading uses ritual maintenance and community memory as the observable and finds high extraction (stone degrades into performance). Both readings cannot be simultaneously true of the same interval — they represent competing hypotheses about what the stone DID from 1933-2011. The 2011 empirical outcome (zero deaths, survival pattern matching the stone line) is evidence for the behavioral reading, but the commemorative reading could accommodate this as coincidence or as explained by other factors (topography, recent zoning independent of the stone). The framework does not adjudicate — it models both as distinct constraints and lets the corpus analysis reveal which reading the structural data supports.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
