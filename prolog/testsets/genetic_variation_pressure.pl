% ============================================================================
% CONSTRAINT STORY: genetic_variation_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_variation_pressure, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: genetic_variation_pressure
 *   human_readable: Genetic Variation Pressure in Evolutionary Dynamics
 *   domain: evolutionary_biology/population_genetics
 *
 * SUMMARY:
 *   Genetic variation pressure is the force exerted by phenotypic
 *   heterogeneity within a population on each individual organism's fitness
 *   and adaptive options. In any sexually reproducing population with
 *   heritable variation, individuals face competition from others with
 *   different phenotypes and genotypes. This pressure is mathematically
 *   inevitable given: (1) reproduction produces heritable offspring
 *   variation, (2) finite resources limit population size, (3) differential
 *   survival and reproduction occur based on phenotype. The constraint
 *   operates at the population level but binds individual fitness outcomes.
 *   No organism can opt out of genetic variation pressure — it is the
 *   background condition against which all individual adaptations must be
 *   calibrated. The constraint exhibits zero degrees of freedom across all
 *   evolutionary contexts: prokaryotes, eukaryotes, plants, animals,
 *   parasites. The accessibility of this constraint is near-total
 *   (accessibility_collapse = 0.92): every sexually reproducing population
 *   must manage variation or face extinction; the constraint is intrinsic to
 *   evolution itself. Resistance is minimal (0.08): no known biological
 *   mechanism permits escape from variation pressure; all adaptations are
 *   responses to it, not alternatives to it.
 *
 * KEY AGENTS:
 *   - Individual Organism: Trapped (powerless/trapped) — experiences variation pressure as immutable competitive context; cannot exit or modify the constraint
 *   - Breeding Population: Organized beneficiary (organized/arbitrage) — genetic variation is the substrate for selection; breeders depend on variation pressure to enable directional evolution
 *   - Conservation Authority: Institutional manager (institutional/analytical) — must operate under variation pressure as an invariant; limited to managing its effects, not eliminating it
 *   - Evolutionary Process: Analytical observer (analytical/analytical) — variation pressure is a logical necessity of population genetics under selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_variation_pressure, 0.18).
domain_priors:suppression_score(genetic_variation_pressure, 0.02).
domain_priors:theater_ratio(genetic_variation_pressure, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_variation_pressure, extractiveness, 0.18).
narrative_ontology:constraint_metric(genetic_variation_pressure, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(genetic_variation_pressure, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genetic_variation_pressure, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(genetic_variation_pressure, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_variation_pressure, mountain).
narrative_ontology:human_readable(genetic_variation_pressure, "Genetic Variation Pressure in Evolutionary Dynamics").
narrative_ontology:topic_domain(genetic_variation_pressure, "evolutionary_biology/population_genetics").

domain_priors:emerges_naturally(genetic_variation_pressure).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ORGANISM (MOUNTAIN) — The organism experiences genetic variation pressure as an immutable constraint: phenotypic diversity within the population creates competitive pressure that cannot be escaped. An individual cannot opt out of population-level genetic heterogeneity. The constraint operates regardless of the organism's agency or awareness. Zero degrees of freedom — the pressure exists at the population level and binds each member.
constraint_indexing:constraint_classification(genetic_variation_pressure, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational perspective, genetic variation pressure is a logical-mathematical necessity of population genetics under selection. Given: (1) heritable phenotypic variation exists in populations, (2) differential reproduction occurs based on phenotypes, (3) offspring inherit parental traits. Therefore: variation pressure is inevitable. This is not contingent on measurement method or institutional arrangement — it follows from the axioms of inheritance and selection. The pressure exists in all sexually reproducing populations, all evolutionary time scales, all ecological contexts. The accessibility of this constraint is near-total: no population can sustain itself without managing variation; no organism can thrive without responding to the phenotypic diversity in its local environment.
constraint_indexing:constraint_classification(genetic_variation_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: BREEDING POPULATION (MOUNTAIN) — Even from the perspective of an organized collective (domesticated breeding populations, wildlife management programs), genetic variation pressure appears as an invariant constraint. The breeders' goal — selecting for specific traits — REQUIRES the variation pressure to exist. Remove genetic variation and artificial selection becomes impossible. The constraint is experienced as the condition enabling intentional breeding, but it remains immutable: variation pressure cannot be dissolved, only channeled. Even artificial selection experiments that dramatically narrow genetic variation eventually hit hard boundaries (fixation limits, inbreeding depression) that reflect the underlying constraint.
constraint_indexing:constraint_classification(genetic_variation_pressure, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSERVATION AUTHORITY (MOUNTAIN) — Wildlife managers and conservation agencies operate under genetic variation pressure as an immutable structural fact: endangered species face the constraint that reduced population size amplifies genetic drift and reduces variation, making populations more vulnerable. The pressure cannot be eliminated; it can only be managed via population connectivity, translocations, and reproductive strategies that maintain variation. The constraint persists regardless of funding, technology, or institutional design. Even state-level conservation programs cannot escape the mathematical pressure of variation in finite populations.
constraint_indexing:constraint_classification(genetic_variation_pressure, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_variation_pressure_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(genetic_variation_pressure, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_variation_pressure, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(genetic_variation_pressure, ExtMetricName, E),
    domain_priors:suppression_score(genetic_variation_pressure, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(genetic_variation_pressure),
    narrative_ontology:constraint_metric(genetic_variation_pressure, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(genetic_variation_pressure, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(genetic_variation_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Genetic variation pressure is not extractive in the classical sense — no beneficiary is siphoning resources or labor from victims. The pressure operates symmetrically across all individuals in a population: every organism faces competition from phenotypic variation. The 0.18 value reflects the baseline structural 'cost' of maintaining variation (energy expense of genetic expression diversity, developmental plasticity required to respond to varying genetic backgrounds). This is coordination overhead, not extraction. Suppression (0.02): Minimal. There are no external barriers preventing organisms from responding to genetic variation pressure — all organisms have the capacity to develop, reproduce, and adapt. The constraint is not maintained by coercion or restricted information; it is simply the inevitable result of heritable variation and differential reproduction. Theater ratio (0.05): Minimal. Genetic variation pressure exhibits no performative component; the mechanism is purely functional. Population-level variation mathematically produces individual-level competitive pressure — there is no theatrical maintenance required, no symbolic legitimacy needed. The constraint is transparent in its operation.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on mountain classification, indicating this constraint is a uniform natural law. The gap that emerges is not between classification types but between how different agents EXPERIENCE the immutability: the individual organism experiences it as an external pressure they cannot escape; the breeding population experiences it as a resource they depend on; the conservation authority experiences it as a boundary condition they must work within; the analytical observer recognizes it as a logical inevitability. These are different phenomenological experiences of the same immutable structure. There is no perspectival disagreement on classification — the constraint is invariant across all positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Genetic variation pressure does not decompose into beneficiary and victim groups because it is not extractive. The pressure operates symmetrically: all individuals in a population experience variation-driven competition. The breeding population's 'beneficiary' status (arbitrage exit option) reflects that organized actors can intentionally use variation pressure as a tool for selection — but they cannot escape the pressure itself. This is distinct from true extraction: beneficiaries in extractive constraints accumulate resources while imposing costs on victims; here, the breeding population's ability to use variation pressure productively does not reduce the variation pressure experienced by other organisms. The constraint is not zero-sum. Therefore, no directionality override is needed — the constraint is properly classified as a neutral-pressure mountain rather than asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN CONSTRAINT — NATURAL LAW INVARIANT. This constraint presents no mandatrophy risk because all perspectives produce mountain classification. The potential false summit is the analytical perspective that naturalizes variation pressure as an immutable law of evolutionary biology: the risk is that this framing obscures the possibility that future technologies (genetic editing, synthetic reproduction, artificial selection protocols) could reduce the force of variation pressure. However, the omega variables identify this risk explicitly — if technological escape becomes feasible, the mountain classification would be demoted to piton (inertial institutional constraint). Currently, the evidence supports mountain classification: genetic variation pressure is logically inevitable given heritable reproduction and selection, applies across all sexual organisms and all time scales, and has not been escaped or significantly reduced by any known biological or technological intervention. The constraint's immutability is not institutional or contingent; it is structural to the inheritance system itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_vs_variation_pressure_directionality,
    'Does genetic variation pressure act primarily as a constraint (forcing adaptation) or as a resource (enabling diversity)?',
    'Population genetics simulation: compare selection efficiency under high vs low variation conditions; empirical data on trait heritability and response to selection in populations with restricted vs abundant variation',
    'If primarily constraining: classification remains mountain (immutable pressure). If primarily enabling: the ''constraint'' reframes as a necessary precondition (rope-like coordination). The distinction matters for conservation policy — scarcity narrative vs resilience narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_vs_variation_pressure_directionality, empirical, 'Whether variation pressure acts as constraint or resource').

omega_variable(
    technological_escape_feasibility,
    'Could future technologies (genomic editing, artificial reproduction, genetic rescue from cryobanks) reduce or eliminate the practical force of genetic variation pressure?',
    'Prospective analysis of CRISPR-based trait fixing, synthetic biology reproduction methods, and genetic rescue timelines; whether technological escape requires overcoming fundamental biological limits or just current technical barriers',
    'If escape is feasible: mountain classification may be demoted to piton (inertial institutional constraint) as technology supersedes natural law. If fundamental limits prevent escape: mountain classification confirmed — no technology can eliminate the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_escape_feasibility, empirical, 'Whether future technologies could eliminate variation pressure').

omega_variable(
    asexual_reproduction_boundary,
    'Does genetic variation pressure apply to asexual and parthenogenetic populations, or only to sexual reproducers?',
    'Comparative analysis of variation dynamics in parthenogenetic organisms, bacterial colonies, and clonal plants; whether drift and mutation pressure create equivalent constraint in non-sexual contexts',
    'If variation pressure is universal: mountain classification holds even for asexual lineages. If specific to sexual reproduction: the constraint is less fundamental than claimed — contingent on reproductive mode, suggesting piton reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asexual_reproduction_boundary, empirical, 'Scope of variation pressure across reproductive modes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_variation_pressure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gvp_tr_t0, genetic_variation_pressure, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gvp_tr_t10, genetic_variation_pressure, theater_ratio, 10, 0.05).
narrative_ontology:measurement(gvp_tr_t20, genetic_variation_pressure, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(gvp_be_t0, genetic_variation_pressure, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gvp_be_t10, genetic_variation_pressure, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(gvp_be_t20, genetic_variation_pressure, base_extractiveness, 20, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_variation_pressure, information_standard).
narrative_ontology:affects_constraint(genetic_variation_pressure, inbreeding_depression).
narrative_ontology:affects_constraint(genetic_variation_pressure, fitness_landscape_navigation).
narrative_ontology:affects_constraint(genetic_variation_pressure, speciation_branching).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
