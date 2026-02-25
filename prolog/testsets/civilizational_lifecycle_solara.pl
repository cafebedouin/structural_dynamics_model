% ============================================================================
% CONSTRAINT STORY: civilizational_lifecycle_solara
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilizational_lifecycle_solara, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: civilizational_lifecycle_solara
 *   human_readable: The Lifecycle of Solaran Civilization
 *   domain: social/political
 *
 * SUMMARY:
 *   This constraint models the emergent 'iron law' governing the complete
 *   lifecycle of the Solaran civilization, from unification and expansion to
 *   zenith and eventual decline. The constraint is not a single policy but
 *   the entire socio-political operating system, including its path
 *   dependencies, cultural norms, and structural incentives. This story
 *   serves as a diagnostic exemplar, demonstrating how a single complex
 *   system with fixed base properties can be classified as all six constraint
 *   types depending on the observer's structural position and temporal
 *   location within the lifecycle.
 *
 * KEY AGENTS:
 *   - The Unifying Founder (Solem): Primary beneficiary (institutional/arbitrage) - Experiences the system as pure coordination (Rope).
 *   - The Imperial Elite/Warrior Caste: Beneficiaries (powerful/constrained) - Experience a mix of coordination and extraction that benefits them (Tangled Rope).
 *   - Subjugated Species/Early Peasantry: Primary victims (powerless/trapped) - Experience the system as pure coercive extraction (Snare).
 *   - Constitutional Reformers: Change agents (organized/mobile) - View the old system as a temporary, coercive structure to be replaced (Scaffold).
 *   - Late-Empire Bureaucracy: Inertial actors (institutional/constrained) - Maintain the rituals of a system whose function has decayed (Piton).
 *   - The Galactic Historian: Analytical observer (analytical/analytical) - Risks naturalizing the contingent lifecycle as an inevitable law (Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilizational_lifecycle_solara, 0.72).
domain_priors:suppression_score(civilizational_lifecycle_solara, 0.65).
domain_priors:theater_ratio(civilizational_lifecycle_solara, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, extractiveness, 0.72).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(civilizational_lifecycle_solara, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilizational_lifecycle_solara, tangled_rope).
narrative_ontology:human_readable(civilizational_lifecycle_solara, "The Lifecycle of Solaran Civilization").
narrative_ontology:topic_domain(civilizational_lifecycle_solara, "social/political").

domain_priors:requires_active_enforcement(civilizational_lifecycle_solara).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, imperial_elite).
narrative_ontology:constraint_beneficiary(civilizational_lifecycle_solara, warrior_caste).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, early_peasant_caste).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, subjugated_species).
narrative_ontology:constraint_victim(civilizational_lifecycle_solara, late_empire_citizenry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJUGATED SPECIES (SNARE) — For a conquered world or the early peasant caste, the Solaran system is pure coercive extraction. They are trapped, with resources and labor systematically removed to benefit the imperial core. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.92.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE FOUNDER (ROPE) — From the perspective of the unifier Solem, the imperial structure is a pure coordination mechanism to end planetary wars, enable interstellar expansion, and ensure stability. The costs are seen as necessary investments for collective benefit. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.10. A net subsidy.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: IMPERIAL ADMINISTRATOR (TANGLED ROPE) — A high-level official during the empire's zenith perceives both the genuine coordination function (managing a galactic empire) and the necessary, asymmetric extraction (taxes, conscription) required to maintain it. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORMER (SCAFFOLD) — The agents who abolished the original caste system viewed that rigid hierarchy as a temporary scaffold. Their constitution acts as a sunset clause, intended to transition the civilization to a more equitable state. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.54. The classification is scaffold due to the sunset clause, despite high chi.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LATE-EMPIRE BUREAUCRAT (PITON) — In the civilization's decline, a bureaucrat sees the grand rituals of the sacred monarchy and state functions as largely performative. The original function has atrophied, but the theater persists due to institutional inertia. theater_ratio=0.75 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GALACTIC HISTORIAN (MOUNTAIN) — An external, long-term observer may view the entire lifecycle as an inevitable, quasi-natural law of civilizational dynamics. From this distance, the rise and fall appears as an unchangeable pattern, a mountain of social physics. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(civilizational_lifecycle_solara, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_lifecycle_solara_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_lifecycle_solara, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civilizational_lifecycle_solara, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilizational_lifecycle_solara, TR),
    TR >= 0.70.

:- end_tests(civilizational_lifecycle_solara_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high, reflecting the system's ultimate reliance on resource and labor extraction from subjugated populations and lower classes to fuel its expansion and maintain the elite. Suppression (0.65) is high due to the initial caste system and the military enforcement required for galactic conquest. Theater Ratio (0.75) is high, reflecting the 'sacred monarchy' and the tendency for institutions to become performative and inertial in the civilization's later stages. The temporal measurements show both extractiveness and theater increasing as the civilization matures and then declines, a classic sign of lifecycle drift.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal. The same system is a Rope to its founder, a Snare to its victims, a Tangled Rope to its managers, a Scaffold to its reformers, a Piton to its late-stage inhabitants, and a Mountain to the distant observer. This demonstrates that the constraint's 'type' is not an intrinsic property but an emergent feature of the interaction between the system's structure and the observer's indexical position (P,T,E,S).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the explicit beneficiary/victim declarations. The Imperial Elite (beneficiary, arbitrage/constrained exit) experience low or negative effective extraction. The Subjugated Species (victim, trapped exit) experience maximally amplified extraction, pushing chi into the Snare category. The reformers (organized, mobile exit) have enough agency to perceive a path to change, classifying the system as a temporary Scaffold. The analytical historian has no stake, but their universal scope and civilizational timeline predisposes them to see an immutable pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by embracing it. The question 'Which type is the Solaran lifecycle really?' is ill-posed. The system IS the presheaf of all its perspectival classifications. The mandatrophy is resolved not by picking one 'correct' type, but by understanding that the system's complexity is captured by the full set of indexed observations. The high base extractiveness (0.72) forces this analysis, preventing a simplistic classification and revealing the deep structural conflicts within the civilization's lifecycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_driver,
    'Was the primary driver of Solaran decline internal decay (e.g., elite overproduction, institutional sclerosis) or external pressure (e.g., insurmountable threats, resource depletion)?',
    'Analysis of late-empire records, focusing on resource allocation, military deployments, and internal political stability metrics.',
    'Internal decay points to the system being a Snare that consumed itself. External pressure suggests the system was a Rope that was simply overcome by a Mountain of external reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_driver, empirical, 'Distinguishing between internal vs. external drivers of civilizational collapse.').

omega_variable(
    constitutional_effectiveness,
    'Did the constitutional reforms genuinely dismantle the extractive caste system or merely provide a new theatrical justification for it?',
    'Comparative analysis of wealth and power distribution pre- and post-constitution.',
    'If effective, the Scaffold perspective is validated. If ineffective, the reform was merely a shift in the Piton''s theatrical script, and the underlying Snare remained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_effectiveness, empirical, 'Assessing the real-world impact of the constitutional reforms on social stratification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_lifecycle_solara, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civi_tr_t0, civilizational_lifecycle_solara, theater_ratio, 0, 0.2).
narrative_ontology:measurement(civi_tr_t500, civilizational_lifecycle_solara, theater_ratio, 500, 0.5).
narrative_ontology:measurement(civi_tr_t1000, civilizational_lifecycle_solara, theater_ratio, 1000, 0.75).

% Extraction over time
narrative_ontology:measurement(civi_be_t0, civilizational_lifecycle_solara, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(civi_be_t500, civilizational_lifecycle_solara, base_extractiveness, 500, 0.6).
narrative_ontology:measurement(civi_be_t1000, civilizational_lifecycle_solara, base_extractiveness, 1000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_lifecycle_solara, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
