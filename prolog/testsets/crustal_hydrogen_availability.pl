% ============================================================================
% CONSTRAINT STORY: crustal_hydrogen_availability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crustal_hydrogen_availability, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: crustal_hydrogen_availability
 *   human_readable: Crustal Hydrogen Availability and Distribution
 *   domain: geochemistry/planetary_science
 *
 * SUMMARY:
 *   Crustal hydrogen availability represents a fundamental geochemical limit
 *   on human civilization's ability to extract and utilize hydrogen as an
 *   energy carrier or chemical feedstock. The constraint emerges from
 *   planetary formation history and stellar nucleosynthesis: Earth's crustal
 *   inventory of hydrogen (distributed among water, hydrous silicates,
 *   carbonates, and organic compounds) is finite and fixed over human
 *   timescales. Extraction rates from water splitting, hydrocarbon
 *   processing, and mineral leaching are negligible relative to total crustal
 *   inventory (~1.5-2.5% of crustal mass), but they operate at timescales
 *   orders of magnitude faster than natural hydrogen replenishment via mantle
 *   outgassing or weathering. This creates an asymmetry: the absolute crustal
 *   hydrogen ceiling is immutable (mountain), while policies, technologies,
 *   and institutional arrangements can only redistribute or conserve the
 *   fixed stock. No observational position can escape this constraint because
 *   it is rooted in the physical fact of planetary composition.
 *
 * KEY AGENTS:
 *   - Industrial hydrogen consumers (powerless/trapped): Fertilizer synthesis, petroleum refining, steelmaking, desulfurization — all depend on crustal hydrogen availability; cannot exit the constraint
 *   - Energy sector policy makers (institutional/arbitrage): Can choose hydrogen pathways vs alternatives but face the same absolute ceiling; arbitrage is possible within the constraint, not around it
 *   - Hydrogen economy advocates (organized/constrained): Technological optimists pushing fuel cell infrastructure; can shift which forms of hydrogen they extract but cannot exceed crustal limits
 *   - Geochemical science community (analytical/analytical): Measures and models crustal hydrogen inventory; observes the constraint as a law of planetary composition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crustal_hydrogen_availability, 0.18).
domain_priors:suppression_score(crustal_hydrogen_availability, 0.03).
domain_priors:theater_ratio(crustal_hydrogen_availability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crustal_hydrogen_availability, extractiveness, 0.18).
narrative_ontology:constraint_metric(crustal_hydrogen_availability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(crustal_hydrogen_availability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(crustal_hydrogen_availability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(crustal_hydrogen_availability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crustal_hydrogen_availability, mountain).
narrative_ontology:human_readable(crustal_hydrogen_availability, "Crustal Hydrogen Availability and Distribution").
narrative_ontology:topic_domain(crustal_hydrogen_availability, "geochemistry/planetary_science").

domain_priors:emerges_naturally(crustal_hydrogen_availability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HYDROGEN-DEPENDENT SECTORS (MOUNTAIN) — Aggregate planetary crustal hydrogen availability is a fixed physical limit. No industrial process can escape the constraint. The distribution of hydrogen isotopes and their geochemical availability are immutable at the civilizational timescale. Total crustal hydrogen is finite; reorganization cannot create new supply. Extraction and cost shifts are possible, but the absolute ceiling is an unchangeable natural law.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENERGY SECTOR POLICY (MOUNTAIN) — Hydrogen availability as a planetary constraint is invariant across all policy frameworks and technological approaches. Whether nations pursue hydrogen economy, ammonia synthesis, desulfurization, or petrochemical feedstock strategies, they face the same finite crustal reservoir. Exit via technological arbitrage (fuel cells, alternative sources, synthetic biology) does not change the constraint — it merely shifts which forms of hydrogen extraction they employ. The ceiling remains immutable.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — Crustal hydrogen availability is determined by stellar nucleosynthesis history and planetary formation. The total inventory of hydrogen in the crust (approximately 1.5-2.5% by mass in hydrous minerals, water, and organics) is set by the initial composition of the accretion disk and has only marginally changed over 4.5 billion years via outgassing and solar wind implantation. This is a natural law of planetary composition. No observer position can relativize the absolute bound.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / EXTRACTION TIMESCALE (MOUNTAIN) — Hydrogen removal via industrial extraction, ocean water splitting, or atmospheric processes operates at timescales (decades to centuries) vastly shorter than hydrogen replenishment via weathering, metamorphic dehydration, or mantle outgassing (millions of years). The effective extraction rate relative to replenishment rate is negligible over human timescales, creating a de facto depletion ceiling. This asymmetry is rooted in geochemical reaction kinetics — a natural law of planetary chemistry.
constraint_indexing:constraint_classification(crustal_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crustal_hydrogen_availability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(crustal_hydrogen_availability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crustal_hydrogen_availability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(crustal_hydrogen_availability, ExtMetricName, E),
    domain_priors:suppression_score(crustal_hydrogen_availability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(crustal_hydrogen_availability),
    narrative_ontology:constraint_metric(crustal_hydrogen_availability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(crustal_hydrogen_availability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(crustal_hydrogen_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The crustal hydrogen inventory is vast relative to current industrial consumption (~110 million tonnes of pure hydrogen annually equivalent). The effective extraction rate is <0.001% per century of total crustal reserves. The constraint is not currently binding in the sense of creating immediate scarcity, so base extractiveness reflects not current pressure but the eventual ceiling imposed by planetary finite stock. Suppression (0.03): Minimal. The constraint operates transparently — all agents understand that hydrogen must come from water, hydrocarbons, or minerals. There is no coercive or informational suppression; the limit is simply physical. Theater ratio (0.05): Minimal. The constraint exhibits no performative content. Hydrogen availability is directly measurable via geochemistry; there is no ritual or proxy needed to verify it. Accessibility collapse (0.92): Very high. The constraint is accessible to verification via direct geochemical measurement, isotopic analysis, and mass balance accounting. No observer position can deny the finiteness of crustal hydrogen. Resistance (0.08): Very low. The constraint emerges from fundamental planetary physics; no technological, policy, or institutional arrangement can resist it. Adaptation (substitution with other energy vectors) is possible; resistance to the constraint itself is not.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify the constraint as mountain because the constraint is truly invariant across observational positions. The hydrogen-dependent industrial sectors perceive an immutable ceiling; policy makers perceive the same ceiling regardless of strategy choice; analytical observers perceive the constraint as a law of planetary composition. There is no perspectival gap because the constraint has zero degrees of freedom — it is equally binding from all power levels, timescales, and exit options. This uniformity is itself diagnostic: when a natural-language concept produces identical classification from powerless/trapped, institutional/arbitrage, and analytical/analytical perspectives at the civilizational timescale, the constraint is likely a true mountain, not a contingent institutional arrangement. The constraint cannot be reclassified by changing the observation method because the observation method (geochemical mass balance) is the correct one.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to mountain constraints. No beneficiary or victim structure exists because the constraint does not extract from or benefit specific agents — it is universally binding. The entire human economy operates within a finite hydrogen budget set by planetary chemistry. This is the defining characteristic of a mountain: it is not a mechanism of inter-agent extraction but a structural limit that all agents face identically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy trivially because it is a mountain — there is no risk of misclassifying coordination as extraction (or vice versa) since no coordination function exists. The constraint is a pure limit, not a mechanism. The analytical observer cannot naturalize contingent institutional arrangements as laws of nature because the constraint genuinely is a law of nature (planetary composition). The constraint is invariant across all observables and measurement methodologies — the total crustal hydrogen inventory is the same whether measured via direct geochemistry, isotopic tracing, or remote sensing. This stability across observables is the ε-invariance principle: no alternative measurement basis can change the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mantle_hydrogen_influx_rate,
    'What is the quantitative rate of hydrogen delivery to the crust from mantle outgassing and metamorphic dehydration, and how does it compare to modern extraction rates?',
    'Geochemical models of volatile cycling; isotopic tracer analysis of mantle-derived fluids; quantitative mass balance across plate boundaries and subduction zones',
    'If influx significantly exceeds extraction: the constraint may be weakly binding in the long term (reclassify toward rope/scaffold at multi-generational timescales). If influx is negligible: crustal hydrogen is effectively non-renewable at human timescales, strengthening mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mantle_hydrogen_influx_rate, empirical, 'Rate of hydrogen replenishment from deep Earth sources').

omega_variable(
    hydrogen_accessibility_variability,
    'Does the geographic and depth-dependent variability in hydrogen availability create functionally distinct sub-constraints with different extractiveness values, or is crustal hydrogen adequately modeled as a single unified limit?',
    'Spatial analysis of hydrogen-bearing mineral deposits and water resources; evaluation of extraction cost functions by region; determination of whether local depletion (low accessibility) differs structurally from global depletion',
    'If sub-constraints are structurally distinct: decompose into separate stories (crustal_hydrogen_global_inventory vs regional_hydrogen_scarcity). If unified: single mountain constraint is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hydrogen_accessibility_variability, conceptual, 'Whether crustal hydrogen is one constraint or multiple by geography').

omega_variable(
    technological_hydrogen_recycling_ceiling,
    'Can closed-loop hydrogen recycling and synthetic regeneration approaches fundamentally change the effective constraint, or do they merely redistribute existing crustal hydrogen?',
    'Energy accounting for hydrogen recycling systems; analysis of whether synthetic hydrogen production (from electricity and water) circumvents crustal depletion or creates secondary energy constraints; thermodynamic limits on recycling efficiency',
    'If recycling enables genuine supply multiplication: the constraint may reclassify as rope (coordination of recycling infrastructure) at institutional timescales. If recycling merely conserves finite stock: mountain classification remains valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_hydrogen_recycling_ceiling, empirical, 'Whether hydrogen recycling circumvents or merely manages the crustal limit').

omega_variable(
    water_as_hydrogen_proxy,
    'Is crustal hydrogen availability best modeled as a direct physical limit, or is it primarily a function of water accessibility and desalination technology?',
    'Comparative analysis of hydrogen extraction from water vs other sources; technological roadmaps for desalination and electrolysis; determination of whether water, not hydrogen atoms, is the binding constraint',
    'If water is the binding constraint: the story should decompose and link to a constraint on freshwater access. If hydrogen specifically is limiting: mountain classification is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(water_as_hydrogen_proxy, empirical, 'Whether the constraint is on hydrogen atoms or on water molecules as hydrogen sources').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crustal_hydrogen_availability, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crust_h_tr_t0, crustal_hydrogen_availability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(crust_h_tr_t2, crustal_hydrogen_availability, theater_ratio, 2, 0.05).
narrative_ontology:measurement(crust_h_tr_t4, crustal_hydrogen_availability, theater_ratio, 4, 0.05).

% Extraction over time
narrative_ontology:measurement(crust_h_be_t0, crustal_hydrogen_availability, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(crust_h_be_t2, crustal_hydrogen_availability, base_extractiveness, 2, 0.17).
narrative_ontology:measurement(crust_h_be_t4, crustal_hydrogen_availability, base_extractiveness, 4, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(crustal_hydrogen_availability, industrial_hydrogen_production_cost).
narrative_ontology:affects_constraint(crustal_hydrogen_availability, green_hydrogen_scaling_timescale).
narrative_ontology:affects_constraint(crustal_hydrogen_availability, ammonia_synthesis_availability).

% DUAL FORMULATION NOTE:
% Crustal hydrogen availability is upstream of multiple downstream constraints in the energy and chemical economy. Industrial hydrogen production costs reflect both the crustal availability ceiling and technological access costs. Green hydrogen scaling is constrained both by the crustal hydrogen ceiling and by electricity availability. Ammonia synthesis availability depends on both the hydrogen input constraint and the nitrogen fixation constraint. This story models the geochemical ceiling; downstream stories model institutional and technological mediations of that ceiling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
