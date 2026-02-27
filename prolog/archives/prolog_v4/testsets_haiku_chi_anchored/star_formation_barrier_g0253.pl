% ============================================================================
% CONSTRAINT STORY: star_formation_barrier_g0253
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_star_formation_barrier_g0253, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: star_formation_barrier_g0253
 *   human_readable: Star Formation Barrier in the 'Brick' Cloud (G0.253+0.016)
 *   domain: astrophysics/star_formation
 *
 * SUMMARY:
 *   The Brick cloud (G0.253+0.016) presents a paradox: despite having
 *   sufficient mass, density, and magnetic support to be a prolific stellar
 *   nursery, it forms stars at a suppressed rate and exhibits a strong bias
 *   toward high-mass star formation at the expense of lower-mass stars. This
 *   constraint exhibits a structural tension between magnetic field
 *   coordination (enabling hierarchical fragmentation and organized gas
 *   dynamics) and magnetic suppression (preventing bulk gas from reaching
 *   star-forming density). The same physical mechanisms—magnetic pressure,
 *   flux freezing, and turbulent cascade—operate simultaneously as both a
 *   coordination function (for the magnetic field institutional structure)
 *   and an extraction mechanism (for the bulk molecular gas). The
 *   theater_ratio reflects that much of the interpretive effort focuses on
 *   fitting the star formation efficiency puzzle (defining it, parameterizing
 *   it, explaining why it is ~5%) rather than on identifying the fundamental
 *   physical process responsible. Modern observational astronomy, through
 *   improved interferometry and dust polarimetry, is building alternative
 *   measurement frameworks that may reframe or resolve the barrier
 *   classification—a classic scaffold sunset signature.
 *
 * KEY AGENTS:
 *   - Bulk Molecular Gas: Primary victim (powerless/trapped) — cannot escape magnetic confinement; redirected toward high-mass channels; suppressed from lower-mass star formation
 *   - Magnetic Field Organization: Primary beneficiary (institutional/arbitrage) — benefits from the constraint as a coordination mechanism enabling ordered hierarchical fragmentation and energy dissipation
 *   - Star-Forming Population (Protostars/Accretion Flows): Secondary agent (organized/constrained) — experiences mixed coordination and extraction; constrained by suppression but enabled by fragmentation structure
 *   - Observational Astronomy Community: Organized agent (organized/mobile) — building alternative measurement frameworks (interferometry, polarimetry) that may bypass the efficiency puzzle framing
 *   - Star Formation Efficiency Paradigm: Institutional structure (institutional/constrained) — maintains the ~5% efficiency concept through modeling and analysis despite conceptual ambiguities; theater_ratio reflects this inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — at risk of naturalizing a contingent magnetic/turbulent state as a fundamental physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(star_formation_barrier_g0253, 0.38).
domain_priors:suppression_score(star_formation_barrier_g0253, 0.62).
domain_priors:theater_ratio(star_formation_barrier_g0253, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(star_formation_barrier_g0253, extractiveness, 0.38).
narrative_ontology:constraint_metric(star_formation_barrier_g0253, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(star_formation_barrier_g0253, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(star_formation_barrier_g0253, tangled_rope).
narrative_ontology:human_readable(star_formation_barrier_g0253, "Star Formation Barrier in the 'Brick' Cloud (G0.253+0.016)").
narrative_ontology:topic_domain(star_formation_barrier_g0253, "astrophysics/star_formation").

domain_priors:requires_active_enforcement(star_formation_barrier_g0253).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(star_formation_barrier_g0253, magnetic_field_structures).
narrative_ontology:constraint_beneficiary(star_formation_barrier_g0253, high_mass_star_formation).
narrative_ontology:constraint_victim(star_formation_barrier_g0253, bulk_cloud_star_formation).
narrative_ontology:constraint_victim(star_formation_barrier_g0253, stellar_mass_distribution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BULK MOLECULAR GAS (SNARE) — Gas in the bulk of the cloud cannot escape magnetic confinement and turbulent fragmentation; faces extraction through redirection toward high-mass star formation channels while suppressed from forming lower-mass stars. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.42.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STAR-FORMING POPULATION (TANGLED ROPE) — Organized agents (protostars, accretion flows) experience mixed effects: constrained by magnetic suppression and turbulent dispersion, but coordinated by self-gravity into hierarchical fragmentation. Benefits from feedback-driven structure; costs imposed by suppressed formation efficiency. d≈0.68, f(d)≈1.06, σ=0.9 → χ≈0.35.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAGNETIC FIELD ORGANIZATION (ROPE) — Benefits from the constraint as a coordination mechanism. Magnetic pressure and flux freezing create order in gas dynamics, enabling hierarchical fragmentation and channeling energy dissipation. The barrier enables rather than extracts from the magnetic structure's function. d≈0.12, f(d)≈0.08, σ=0.9 → χ≈0.03.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: OBSERVATIONAL ASTRONOMY CONSENSUS (SCAFFOLD) — Modern interferometry and infrared surveys are systematically mapping magnetic morphologies and turbulent power spectra, building alternative interpretive frameworks that may bypass the traditional star formation efficiency puzzle. The 'barrier' framing may be temporary as observational sophistication increases. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.16. Sunset logic: improved observational resolution and dust polarimetry will clarify the causal chain (suppression vs. coordination).
constraint_indexing:constraint_classification(star_formation_barrier_g0253, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STAR FORMATION EFFICIENCY PARADIGM (PITON) — The 'global star formation efficiency ~ 5%' rule persists in theoretical models and observational analyses despite accumulated evidence that the concept conflates multiple distinct processes (core formation, protostellar accretion, feedback). Theater_ratio=0.68: much analysis focuses on fitting the efficiency puzzle rather than resolving its physical basis. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMIT VIEW (MOUNTAIN) — From a civilizational vantage, some suppression of star formation may be inherent to the physics of self-gravitating gas in magnetic fields: pressure support, dissipation, and feedback impose fundamental constraints on conversion efficiency. However, the claim that this view represents a natural law requires ε≤0.25 and suppression≤0.05. The measured values (ε=0.38, suppression=0.62) reveal this as a false summit: the barrier is more contingent on magnetic topology and turbulent state than on immutable laws.
constraint_indexing:constraint_classification(star_formation_barrier_g0253, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(star_formation_barrier_g0253_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(star_formation_barrier_g0253, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(star_formation_barrier_g0253, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(star_formation_barrier_g0253, TR),
    TR >= 0.70.

:- end_tests(star_formation_barrier_g0253_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high, trending upward. The constraint diverts gas from efficient lower-mass star formation to less efficient high-mass pathways. However, the extraction is not maximal (ε ≤ 0.46) because the barrier is mediated by physical processes (magnetic fields, turbulence) that also serve coordination functions. Some gas does form stars; the constraint is suppression, not prohibition. The upward trend reflects increasing observational evidence that the bias is real and persistent, not an artifact of older detection limits. Suppression (0.62): High. Multiple barriers operate: magnetic pressure opposes collapse, turbulent dispersion disrupts fragmentation, radiation pressure from high-mass stars quenches lower-mass accretion, and the ionizing radiation field suppresses new core formation. However, suppression is not total (≤0.60 would be snare range) because some processes (self-gravity, ambipolar diffusion) do drive collapse. Theater ratio (0.68): High. The field expends substantial effort on efficiency measurements and model fitting without definitively resolving what the constraint IS. Is it magnetic suppression? Turbulent disruption? Radiation feedback? Observational selection? The ~5% efficiency value persists in the literature partly because alternative formulations have not fully displaced it, not because the value is itself clearly explained.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays five distinct perspectival classifications from a single set of base properties. The bulk gas perceives a snare (extraction without compensation). The star-forming population perceives tangled rope (mixed benefits and costs). Magnetic fields perceive a rope (coordination enablement). The observational community perceives a scaffold (temporary barrier being systematically resolved). The efficiency paradigm itself perceives a piton (performative ritual maintained by inertia). The analytical observer risks a false mountain (naturalizing a contingent state). The perspectival range reveals that the barrier is fundamentally ambiguous: it is simultaneously a physical coordination mechanism and an asymmetric extraction device, depending on which structural position one occupies.
 *
 * DIRECTIONALITY LOGIC:
 *   Bulk molecular gas: Victim + trapped → d≈0.92, f(d)≈1.38. Extracted maximally; cannot exit. Magnetic field: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08. Net beneficiary; has freedom to organize energy dissipation pathways. Star-forming population: Victim/beneficiary + constrained → d≈0.68, f(d)≈1.06. Significant extraction (constrained by suppression) but also coordination benefits (hierarchical fragmentation). Observational community: Neither strongly + mobile → d≈0.35, f(d)≈0.35. Moderate; agents can shift measurement frameworks and exit the efficiency paradigm if better alternatives emerge. Efficiency paradigm: Institutional + constrained → d≈0.60, f(d)≈0.85. Moderate extraction from theoretical and observational effort (defending the concept), but constrained by accumulating evidence that the concept is conceptually muddled. Analytical observer: d≈0.72, f(d)≈1.15. Risks naturalizing constraint; false summit detector should flag.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY MEMBER: The Brick star formation barrier is downstream of more fundamental constraints in magnetic field structure and turbulent cascade physics. The mandatrophy is resolved by recognizing that the barrier exhibits all six types because it conflates multiple distinct physical processes. (1) Magnetic pressure support (coordination) — enables ordered fragmentation. (2) Magnetic suppression (extraction) — prevents bulk gas collapse. (3) Turbulent cascade (coordination) — dissipates energy hierarchically. (4) Turbulent disruption (extraction) — disperses forming cores. (5) Radiation feedback (extraction) — quenches accretion. (6) Observational incompleteness (theater). The 'barrier' is not a single constraint but a presheaf over multiple structural positions. Different theoretical frameworks emphasize different subsets: numerical simulations stress (1) and (2); turbulent theories stress (3) and (4); radiation-hydrodynamic models stress (5); efficiency-based analyses stress (6). The constraint achieves clarity not by unifying these into a single type but by decomposing into separate stories for each mechanism and linking them via network dependencies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magnetic_suppression_strength,
    'What is the fundamental division between magnetic pressure support (coordination) versus magnetic suppression of star formation (extraction)?',
    'MHD simulations with varying field strengths and turbulent states; observational correlation between measured magnetic energy density and observed star formation rate in comparable clouds',
    'If suppression dominates: snare classification confirmed for bulk gas. If coordination dominates: rope or scaffold classifications preferred; barrier becomes functional rather than pathological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magnetic_suppression_strength, empirical, 'Degree to which magnetic fields suppress vs. coordinate star formation').

omega_variable(
    fragmentation_scale_bifurcation,
    'Does the observed fragmentation scale bias (preferential high-mass star formation) reflect a physical barrier or an observational selection artifact (high-mass protostars are brighter and more easily detected)?',
    'Volumetric complete surveys using submillimeter continuum; statistical modeling of survey sensitivity vs. mass distribution; comparison of extinction-corrected protostellar mass functions across clouds with varying observational depth',
    'If physical barrier: tangled_rope classification stands. If artifact: bulk of gas forms stars across all masses; constraint is observational (Piton) rather than physical (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fragmentation_scale_bifurcation, empirical, 'Whether fragmentation bias reflects physics or observational selection').

omega_variable(
    turbulent_energy_cascade_timescale,
    'Does turbulent energy dissipation timescale allow gas to reach gravitationally unstable density before turbulent dispersion disrupts collapse?',
    'Direct measurements of turbulent velocity dispersion evolution via line broadening; numerical simulations of collapse with realistic turbulent forcing; correlation between measured turbulent decay time and observed core formation rate',
    'If dissipation is fast: gas cannot reach critical density; barrier is fundamental (Mountain tendencies). If dissipation is slow: sufficient time for collapse; barrier is contingent on magnetic field geometry (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(turbulent_energy_cascade_timescale, empirical, 'Timescale matching of turbulent dissipation vs. gravitational collapse').

omega_variable(
    high_mass_preferential_formation,
    'What physical mechanism privileges high-mass star formation in the Brick relative to lower-mass star formation?',
    'Observations of core mass function at different evolutionary stages; modeling of competitive accretion in dense filaments; spectral index measurements of magnetic field at scales where fragmentation occurs',
    'If magnetic field topology: institutional beneficiary (magnetic organization) confirmed. If turbulent cascade: coordination function confirmed (Rope aspects). If observational: Piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_mass_preferential_formation, empirical, 'Physical basis for high-mass star formation preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(star_formation_barrier_g0253, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfb_g0253_tr_t0, star_formation_barrier_g0253, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sfb_g0253_tr_t3, star_formation_barrier_g0253, theater_ratio, 3, 0.58).
narrative_ontology:measurement(sfb_g0253_tr_t6, star_formation_barrier_g0253, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(sfb_g0253_be_t0, star_formation_barrier_g0253, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sfb_g0253_be_t3, star_formation_barrier_g0253, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(sfb_g0253_be_t6, star_formation_barrier_g0253, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(star_formation_barrier_g0253, resource_allocation).
narrative_ontology:affects_constraint(star_formation_barrier_g0253, magnetic_field_fragmentation).
narrative_ontology:affects_constraint(star_formation_barrier_g0253, turbulent_cascade_core_formation).
narrative_ontology:affects_constraint(star_formation_barrier_g0253, radiation_feedback_suppression).
narrative_ontology:affects_constraint(star_formation_barrier_g0253, observational_mass_function_bias).

% DUAL FORMULATION NOTE:
% The Brick barrier decomposes into four structurally distinct constraints: (1) magnetic_field_fragmentation (ε≈0.12, Mountain/Rope) — flux freezing enables hierarchical structure; (2) turbulent_cascade_core_formation (ε≈0.28, Rope/Tangled Rope) — energy dissipation coordinates fragmentation but disrupts collapse; (3) radiation_feedback_suppression (ε≈0.45, Tangled Rope/Snare) — high-mass star radiation quenches lower-mass accretion; (4) observational_mass_function_bias (ε≈0.32, Piton) — detection limits bias toward bright high-mass sources. The aggregated barrier story (ε=0.38) represents the joint effect. This story treats the barrier as a single tangled rope; decomposition into constraint family members enables clearer analysis of which mechanism dominates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(star_formation_barrier_g0253, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
