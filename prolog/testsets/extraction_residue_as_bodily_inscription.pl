% ============================================================================
% CONSTRAINT STORY: extraction_residue_as_bodily_inscription
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extraction_residue_as_bodily_inscription, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: extraction_residue_as_bodily_inscription
 *   human_readable: Extraction Residue as Bodily Inscription
 *   domain: institutional_violence/medical_authority/labor_extraction
 *
 * SUMMARY:
 *   The constraint that productive labor leaves measurable physiological
 *   traces in worker bodies is a physical law, not a social arrangement. This
 *   is the mountain that extractive labor systems are built upon — the
 *   thermodynamic fact that energy transfer through biological tissue at
 *   sufficient intensity produces irreversible structural changes. The
 *   constraint is not the extraction itself (the decision to subject workers
 *   to damaging conditions) but the physical relationship between work
 *   intensity, exposure duration, and tissue degradation. Medical
 *   documentation systems (occupational disease registries, workers'
 *   compensation claims, epidemiological studies) measure this relationship
 *   with high precision: black lung progression correlates with coal dust
 *   exposure years, carpal tunnel severity correlates with repetitive motion
 *   cycles, hearing loss correlates with cumulative decibel-hours. The
 *   constraint's low extractiveness (0.08) reflects that it is a measurement
 *   relationship, not an extractive mechanism — the extraction happens in the
 *   labor system built on top of this physical substrate. The constraint's
 *   low suppression (0.03) reflects that the physical relationship is
 *   accessible to all observers with measurement tools — it cannot be hidden,
 *   only its documentation can be suppressed. The constraint's low theater
 *   ratio (0.12) reflects that the measurement is functional, not
 *   performative — tissue damage is real, and its correlation with exposure
 *   is empirically robust.
 *
 * KEY AGENTS:
 *   - The Worker's Body: Universal subject (powerless/trapped at the physical level) — accumulates damage as a function of exposure regardless of social position
 *   - Medical Documentation System: Institutional observer (institutional/arbitrage) — measures and records the damage trajectory with clinical precision
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes the thermodynamic substrate: work is entropy production, damage is residue
 *   - Labor Union: Organized agents (organized/constrained) — can negotiate exposure limits and compensation but cannot remove the underlying physical constraint
 *   - Capital: Powerful agents (powerful/mobile) — can externalize costs and suppress documentation but cannot change the physics of tissue damage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extraction_residue_as_bodily_inscription, 0.08).
domain_priors:suppression_score(extraction_residue_as_bodily_inscription, 0.03).
domain_priors:theater_ratio(extraction_residue_as_bodily_inscription, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, extractiveness, 0.08).
narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extraction_residue_as_bodily_inscription, mountain).
narrative_ontology:human_readable(extraction_residue_as_bodily_inscription, "Extraction Residue as Bodily Inscription").
narrative_ontology:topic_domain(extraction_residue_as_bodily_inscription, "institutional_violence/medical_authority/labor_extraction").

domain_priors:emerges_naturally(extraction_residue_as_bodily_inscription).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WORKER'S BODY (MOUNTAIN) — The physiological constraint that labor extraction leaves measurable traces in tissue, bone density, respiratory capacity, and cellular damage is invariant to the worker's structural position. Whether trapped in a specific job or mobile across industries, the body accumulates damage as a function of exposure duration and intensity. This is not extraction by the constraint itself but the physical law that makes extraction visible.
constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MEDICAL DOCUMENTATION SYSTEM (MOUNTAIN) — Occupational medicine observes the same invariant relationship: cumulative exposure produces predictable pathology. Black lung progression correlates with coal dust exposure years. Repetitive strain injuries correlate with motion cycles. Hearing loss correlates with decibel-hours. The measurement apparatus is institutional but the measured phenomenon is a physical law — tissue damage accumulates regardless of the observer's institutional position.
constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational perspective, the constraint is thermodynamic: productive work transfers energy through human bodies, and energy transfer at sufficient intensity produces irreversible structural changes in biological tissue. This is not a social arrangement but a consequence of the second law of thermodynamics applied to biological systems. The body is a dissipative structure; work is entropy production; damage is the residue.
constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: LABOR UNION (MOUNTAIN) — Organized labor recognizes the constraint as immutable: you cannot negotiate away the physics of tissue damage. What is negotiable is exposure duration, intensity, protective equipment, and compensation — but the underlying relationship between work and bodily degradation is a natural law. The union's power is constrained precisely because the constraint itself is a mountain — they can only route around it, not remove it.
constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CAPITAL (MOUNTAIN) — Even from the position of maximum structural power, the constraint is invariant. Capital can externalize the cost of bodily damage, can suppress documentation, can lobby against regulation — but cannot change the fact that high-intensity repetitive labor produces cumulative physiological damage. The mountain classification reflects that the constraint binds all agents equally at the physical level, even as its costs are distributed asymmetrically at the social level.
constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extraction_residue_as_bodily_inscription_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraction_residue_as_bodily_inscription, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, ExtMetricName, E),
    domain_priors:suppression_score(extraction_residue_as_bodily_inscription, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(extraction_residue_as_bodily_inscription),
    narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(extraction_residue_as_bodily_inscription, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(extraction_residue_as_bodily_inscription_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint itself does not extract — it is the physical law that makes extraction visible. The extraction happens in the labor arrangements that subject workers to damaging conditions; the constraint is the measurement relationship that reveals this extraction as bodily inscription. The small non-zero value reflects that measurement itself has costs (medical examination, documentation labor, epidemiological infrastructure) but these are coordination costs, not extractive overhead. Suppression (0.03): Very low. The physical relationship between work and tissue damage is accessible to any observer with measurement tools. Suppression occurs at the social level (hiding medical records, blocking occupational disease registries, defunding research) but the underlying constraint — that damage accumulates and is measurable — cannot be suppressed. The small non-zero value reflects measurement barriers (access to workers, longitudinal tracking, autopsy data) but these are practical limits, not structural suppression. Theater ratio (0.12): Very low. The measurement is functional. Tissue damage is real, its progression is predictable, and its correlation with exposure is empirically robust. The small non-zero value reflects that some occupational health assessments are performative (cursory exams that miss early-stage damage, industry-funded studies with methodological bias) but the core measurement relationship is not theatrical. Accessibility collapse (0.92): Very high. All observers with measurement capacity converge on the same relationship: cumulative exposure produces predictable pathology. The constraint is not observer-dependent. Resistance (0.08): Very low. Attempts to deny or suppress the constraint fail when confronted with longitudinal medical data. The constraint reasserts itself in every cohort study, every workers' compensation claim, every autopsy finding.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification — all perspectives return mountain. This is the expected signature of a genuine natural law constraint. The gap that exists is not in classification but in response: the worker experiences the constraint as bodily suffering; the union experiences it as a bargaining limit; capital experiences it as an externality to be managed; the medical system experiences it as a measurement target; the analytical observer experiences it as a thermodynamic inevitability. But all agree that the constraint itself is immutable. The absence of a classification gap is itself diagnostic — it confirms that the constraint is not a social arrangement naturalized as law (false summit) but an actual physical limit.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain constraint with no beneficiaries or victims declared because the constraint itself is a physical law, not an extractive mechanism. All agents experience the constraint as immutable regardless of their structural position. The worker's body accumulates damage; the medical system measures it; the union negotiates around it; capital externalizes its cost; the analytical observer recognizes its thermodynamic substrate. The directionality values for all perspectives derive from the canonical fallback for their power atoms, but because the constraint is a mountain, all perspectives classify identically. The perspectival invariance is the diagnostic signature of a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that not all constraints involving extraction are themselves extractive. The labor systems built on top of this physical substrate are extractive (and would be modeled as separate constraint stories — e.g., 'coal_mining_labor_regime' as a snare, 'workers_compensation_suppression' as a tangled rope). But the underlying physical law — that work leaves measurable traces in tissue — is a mountain. The mandatrophy resolution is structural: the constraint is the measurement relationship, not the decision to subject workers to damaging conditions. Confusing the two would misclassify a physical law as a social arrangement, which is exactly the error the mountain/snare distinction exists to prevent. The constraint's low extractiveness and low suppression confirm that it is not hiding extraction behind coordination — it is revealing extraction through measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extraction_residue_as_bodily_inscription, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extraction_residue_as_bodily_inscription, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the physical substrate upon which extractive labor constraints are built. Separate stories would model the extractive mechanisms themselves (e.g., coal mining labor regimes, textile factory conditions, agricultural labor systems). Those stories would have high extractiveness and would link to this constraint via network.affects_constraints to indicate that they exploit the physical law documented here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
