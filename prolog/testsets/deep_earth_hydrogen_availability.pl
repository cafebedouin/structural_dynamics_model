% ============================================================================
% CONSTRAINT STORY: deep_earth_hydrogen_availability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deep_earth_hydrogen_availability, []).

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
 *   constraint_id: deep_earth_hydrogen_availability
 *   human_readable: Deep Earth Hydrogen Availability Limit
 *   domain: technological/energy_resources/geophysics
 *
 * SUMMARY:
 *   The deep Earth hydrogen availability limit is a natural law: the total
 *   hydrogen content of Earth's core and mantle is fixed by planetary
 *   accretion history, solar nebula composition, and 4.5 billion years of
 *   geochemical differentiation. The constraint emerges from first-principles
 *   planetary physics, not from institutional arrangements, scarcity
 *   narratives, or human competition. Deep mantle hydrogen remains
 *   inaccessible to current technology (mantle drilling depth limit ~12 km;
 *   Mohorovičić discontinuity at 6-70 km). This constraint is significant for
 *   long-term hydrogen economy planning because it forces technological
 *   choices toward accessible sources: crustal hydrogen (via
 *   serpentinization, metamorphic reactions), electrolytic hydrogen (powered
 *   by renewable electricity), biological hydrogen (fermentation), or
 *   volcanic outgassing. The constraint does NOT create extraction or
 *   scarcity unless mantle access becomes technologically feasible and is
 *   then monopolized. Currently, it functions as a coordination mechanism — a
 *   geophysical reality check that prevents fantasy energy strategies.
 *
 * KEY AGENTS:
 *   - Hydrogen technologists: Constrained by the limit but adapt through alternative sources (mobile exit)
 *   - Planetary scientists: Measure and characterize the limit; benefit from understanding deep Earth composition (analytical exit)
 *   - Energy planners: Use the limit as a boundary condition for realistic scenario modeling (analytical/organized exit)
 *   - Deep-drilling technology developers: Face the limit as an engineering constraint; constrained but not victimized (constrained exit)
 *   - Future fusion researchers: If fusion succeeds, the hydrogen limit becomes economically inert (arbitrage exit via energy substitution)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deep_earth_hydrogen_availability, 0.18).
domain_priors:suppression_score(deep_earth_hydrogen_availability, 0.03).
domain_priors:theater_ratio(deep_earth_hydrogen_availability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, extractiveness, 0.18).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deep_earth_hydrogen_availability, mountain).
narrative_ontology:human_readable(deep_earth_hydrogen_availability, "Deep Earth Hydrogen Availability Limit").
narrative_ontology:topic_domain(deep_earth_hydrogen_availability, "technological/energy_resources/geophysics").

domain_priors:emerges_naturally(deep_earth_hydrogen_availability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY TECHNOLOGISTS — Any hydrogen extraction strategy that relies on accessing Earth's mantle or core faces an absolute availability limit determined by planetary geochemistry. This is not a negotiable resource constraint but a structural property of Earth's composition and outgassing history. No technology, policy, or economic reorganization can increase the total hydrogen reservoir below the crust-mantle boundary. The constraint is universal and immutable.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER — The deep Earth hydrogen inventory is determined by solar nebula composition, planetary accretion, differentiation, and 4.5 billion years of chemical evolution. The hydrogen content of the core is constrained by iron-hydrogen phase diagrams and mantle outgassing rates. These are physical limits, not institutional or economic constraints. No policy or innovation can change the solar nebula's initial composition or planetary differentiation. The constraint is a natural law.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: RESOURCE PLANNERS — Deep Earth hydrogen is inaccessible for current and foreseeable technology (mantle drilling remains impractical). From a resource-planning perspective, the deep Earth hydrogen inventory is effectively a zero — not because it doesn't exist, but because extraction is beyond technological capacity. However, this does not create a constraint on hydrogen technology itself. Alternative sources (solar water splitting, microbial fermentation, electrolysis from renewable electricity, crustal hydrogen) are available. Organized agents have exit options: substitute crustal or atmospheric hydrogen. The constraint becomes inert for practical planning horizons.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: HYDROGEN ECONOMY ARCHITECTS — The deep Earth hydrogen limit functions as a coordination signal. Hydrogen energy strategies must be grounded in realistic source assumptions — crustal hydrogen, electrolytic hydrogen, biological hydrogen. The constraint enforces intellectual honesty: plans cannot rely on deep mantle mining. This coordination function is beneficial; it prevents resource fantasy. Extraction is minimal because the constraint benefits the entire field by anchoring assumptions to geophysical reality. No agent exploits the limit; all agents benefit from knowing it.
constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deep_earth_hydrogen_availability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deep_earth_hydrogen_availability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, ExtMetricName, E),
    domain_priors:suppression_score(deep_earth_hydrogen_availability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deep_earth_hydrogen_availability),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deep_earth_hydrogen_availability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deep_earth_hydrogen_availability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The deep Earth hydrogen inventory is a fixed physical property — it is not extracted from anyone, nor does it extract from anyone. It is a boundary condition. The non-zero value (not quite zero) reflects the fact that knowledge about the limit is imperfect, and uncertainty in total inventory creates modest informational asymmetry. As measurement improves (mineralogy, geochemistry, seismic imaging), uncertainty collapses and extractiveness approaches zero. Suppression (0.03): Nearly zero. There are no alternatives to 'obey planetary physics' — this is not suppression of human choice but description of physical law. The residual value reflects measurement difficulty and the time required for data to propagate through the scientific community. Theater ratio (0.15): Very low. The constraint is straightforward geophysical fact. Minimal performative content. Scientists do not perform compliance with planetary differentiation; they measure it. The residual theater reflects legitimate epistemic gaps (mantle sampling difficulty, compositional uncertainty) but not institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives yield Mountain classification. There is no perspectival gap because the constraint is invariant across all observer positions. Energy technologists, planners, scientists, and civilization itself all face the same immutable fact: Earth's interior contains a fixed amount of hydrogen. The constraint's universality is the defining mark of a natural law. The only perspectival variation is in RELEVANCE: from resource planners' view, the constraint is practically irrelevant (inaccessible = zero for planning). From energy architects' view, it is relevant as a reality check (prevents over-optimism about mantle hydrogen). From planetary scientists' view, it is central (measures planetary formation). But all perspectives recognize it as law, not policy or institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable. Mountains have no beneficiaries or victims — the constraint benefits and harms no agent relative to alternatives, because there is no alternative. All agents are subject equally and universally. The constraint emerges naturally with no human-control leverage. Each perspective's d value is effectively undefined or universally neutral (0.5 by default) because no agent can exit or exploit the constraint. The canonical analytical perspective (analytical power, civilizational horizon, analytical exit, universal scope) derives d ≈ 0.72 (observational asymmetry), producing f(d) ≈ 1.15, but this reflects measurement uncertainty, not power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Is the deep Earth hydrogen limit a mountain (immutable law) or a snare (scarcity engineering)? The constraint is GENUINE MOUNTAIN. All omega variables resolve in favor of mountain classification: (1) Mantle hydrogen inventory is fixed by planetary physics — inaccessible to human intervention. (2) Mantle hydrogen accessibility is and will remain negligible for foreseeable technology. (3) Alternative hydrogen sources are sufficient to support hydrogen economy without deep mantle mining. Therefore: the constraint is not a scarcity mechanism imposed by institutions or extractors — it is a boundary condition of planetary geochemistry. The hydrogen economy does not fail due to deep Earth limits; it succeeds or fails based on crustal hydrogen availability, renewable electricity, and alternative technologies. The mountain classification prevents mislabeling planetary physics as technological scarcity, which would risk false narratives about 'scarcity-driven' energy solutions when the real limiting factors are economic and institutional, not geological.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mantle_hydrogen_total_inventory,
    'What is the total hydrogen inventory of Earth''s core and mantle, and how much is sequestered vs. mobile?',
    'High-pressure mineral physics experiments (iron-hydrogen phase diagrams, hydrogen solubility in silicate minerals); cosmochemical modeling of solar nebula hydrogen budget; seismic detection of hydrogen-bearing minerals; volatilization mass balance from volcanic outgassing',
    'Current estimates place mantle hydrogen at 1-10 ocean masses, but uncertainty spans ~2 orders of magnitude. Higher estimates might suggest larger accessible crustal hydrogen from mantle outgassing; lower estimates constrain deep-source hydrogen more strictly. Does not change the mountain classification but affects evaluation timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mantle_hydrogen_total_inventory, empirical, 'Total hydrogen inventory of Earth''s interior').

omega_variable(
    mantle_hydrogen_accessibility,
    'Can directed deep drilling or future technology access mantle hydrogen before it outgasses naturally?',
    'Technological trajectory analysis of deep drilling (current limit ~12 km; Mohorovičić discontinuity at ~6-70 km depending on location); fusion energy viability (if fusion arrives, hydrogen becomes less scarce regardless of mantle access); cost curves for ultra-deep drilling vs. alternative hydrogen sources',
    'If mantle drilling becomes practical within 100 years: the constraint shifts from natural law to resource availability (Snare or Tangled Rope, depending on who controls access). If it remains perpetually impractical: constraint remains Mountain. If fusion arrives: constraint becomes economically inert.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mantle_hydrogen_accessibility, empirical, 'Whether mantle hydrogen becomes technologically accessible').

omega_variable(
    hydrogen_source_portfolio_sufficiency,
    'Are non-mantle hydrogen sources (electrolytic, biological, volcanic outgassing, crustal hydrolysis) sufficient to support a hydrogen economy at scales matching or exceeding current global energy demand?',
    'Energy balance calculations for global electrolysis using renewable electricity; efficiency roadmaps for hydrogen production methods; long-term crustal hydrogen availability (ore hydration, serpentinization); biological hydrogen production capacity at industrial scales',
    'If alternative sources are sufficient: deep Earth hydrogen limit is academically interesting but practically irrelevant (constraint becomes inert piton). If they are insufficient: the constraint becomes a genuine scarcity limit (shifts toward Snare from energy-dependent sectors). This is the key mandatrophy resolution: does the mountain limit matter for technology?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hydrogen_source_portfolio_sufficiency, empirical, 'Whether non-mantle hydrogen sources suffice for global energy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deep_earth_hydrogen_availability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dehyd_tr_t0, deep_earth_hydrogen_availability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dehyd_tr_t50, deep_earth_hydrogen_availability, theater_ratio, 50, 0.15).
narrative_ontology:measurement(dehyd_tr_t100, deep_earth_hydrogen_availability, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(dehyd_be_t0, deep_earth_hydrogen_availability, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(dehyd_be_t50, deep_earth_hydrogen_availability, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(dehyd_be_t100, deep_earth_hydrogen_availability, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deep_earth_hydrogen_availability, global_infrastructure).
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, crustal_hydrogen_availability).
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, electrolytic_hydrogen_scalability).
narrative_ontology:affects_constraint(deep_earth_hydrogen_availability, fusion_energy_feasibility).

% DUAL FORMULATION NOTE:
% Deep Earth hydrogen is a boundary condition for three upstream constraints: crustal hydrogen availability (constrained by mantle-to-crust fluxes), electrolytic hydrogen scalability (unlimited if renewable electricity is available), and fusion (orthogonal energy source that bypasses hydrogen scarcity entirely). The network relationship is diagnostic: if any upstream constraint becomes tight, this mountain becomes relevant; if all upstream constraints ease, this mountain remains theoretically true but practically inert.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
