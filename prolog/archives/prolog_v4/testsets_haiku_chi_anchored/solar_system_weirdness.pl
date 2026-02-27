% ============================================================================
% CONSTRAINT STORY: solar_system_weirdness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solar_system_weirdness, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: solar_system_weirdness
 *   human_readable: The Solar System Configuration Anomaly
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   The solar system's orbital architecture presents a persistent anomaly:
 *   planets are distributed in ways that classical in situ formation models
 *   struggle to explain. Jupiter is too massive and too close; the
 *   terrestrial planets span a narrower range than theory predicts; the
 *   asteroid belt shows compositional gradients and orbital resonances
 *   suggesting past dynamical instability. For forty years, astronomers
 *   observed these anomalies while the institutional consensus defended in
 *   situ formation, treating migration theories as speculative add-ons. The
 *   constraint operates as a tangled hybrid: the anomaly does coordinate
 *   legitimate research (migration models enable new predictions), but
 *   institutional suppression (textbook defense of obsolete models, career
 *   risk for challenging consensus, pedagogical inertia) extracts a cost from
 *   scientific progress. The constraint's theater_ratio (0.68) reflects
 *   significant performative work: defending the in situ model requires
 *   rhetorical effort that outpaces empirical justification. The migration
 *   theory community experiences this as a snare when defending classical
 *   orthodoxy; as a rope when articulating new models; as a scaffold when
 *   observing that JWST and future surveys will resolve the question within a
 *   generation.
 *
 * KEY AGENTS:
 *   - Classical Planetary Formation Theory: Primary victim (powerless/trapped) — contradicted by observations but defended through institutional inertia; cannot exit without abandoning pedagogical consensus
 *   - Planetary Migration Theory Advocates: Primary beneficiary (institutional/arbitrage) — capture research funding, citations, and institutional prestige by solving anomalies; experience the constraint as productive coordination
 *   - Observational Astronomers: Secondary victim (moderate/constrained) — must fund expensive surveys to test between models; benefit from methodological refinement but face resource barriers
 *   - Space Mission Planners: Organized agents (organized/constrained) — see configuration anomaly as a tractable problem with finite sunset; JWST and future direct imaging will resolve within 10-15 years
 *   - Solar System Pedagogy: Institutional actor (institutional/arbitrage) — maintains performative defense of in situ model in textbooks and curricula despite empirical weakness; persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional suppression as inherent scientific caution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solar_system_weirdness, 0.38).
domain_priors:suppression_score(solar_system_weirdness, 0.52).
domain_priors:theater_ratio(solar_system_weirdness, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solar_system_weirdness, extractiveness, 0.38).
narrative_ontology:constraint_metric(solar_system_weirdness, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(solar_system_weirdness, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(solar_system_weirdness, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(solar_system_weirdness, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solar_system_weirdness, tangled_rope).
narrative_ontology:human_readable(solar_system_weirdness, "The Solar System Configuration Anomaly").
narrative_ontology:topic_domain(solar_system_weirdness, "technological/scientific").

domain_priors:requires_active_enforcement(solar_system_weirdness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(solar_system_weirdness, planetary_migration_theory_advocates).
narrative_ontology:constraint_beneficiary(solar_system_weirdness, exoplanet_observation_programs).
narrative_ontology:constraint_victim(solar_system_weirdness, classical_solar_system_models).
narrative_ontology:constraint_victim(solar_system_weirdness, planetary_formation_consensus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLASSICAL PLANETARY FORMATION THEORY (SNARE) — Trapped within observational anomalies that contradict the in situ formation model. Cannot exit without abandoning decades of pedagogical consensus. Suppressed by institutional inertia: textbooks, curricula, and career incentives for defending the model rather than overturning it. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(solar_system_weirdness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLANETARY MIGRATION THEORY ADVOCATES (ROPE) — Institutional beneficiaries of the anomaly. Migration models (Grand Tack, Nice model) solve observational puzzles and attract funding, citations, and institutional prestige. Experience the constraint as coordination: explaining the weirdness enables new research programs. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Negative extraction = net beneficiary.
constraint_indexing:constraint_classification(solar_system_weirdness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: OBSERVATIONAL ASTRONOMERS (TANGLED ROPE) — Constrained by instrumentation limits and resource barriers but also benefit from exoplanet discoveries and methodological refinement. Must fund expensive surveys to test between models. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(solar_system_weirdness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SPACE MISSION PLANNERS (SCAFFOLD) — Organized agents (NASA, ESA, JWST collaboration) see the configuration anomaly as a tractable problem with a sunset: higher-resolution exoplanet observations and direct imaging surveys will definitively resolve migration vs in situ questions within 10-15 years. Current theoretical theater (competing models) will yield to observational clarity. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.16. Low effective extraction because agency and exit path are clear.
constraint_indexing:constraint_classification(solar_system_weirdness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOLAR SYSTEM PEDAGOGY (PITON) — Institutional inertia in astronomy education. Textbooks and curricula present the in situ model as settled fact despite decades of anomalies and competing theories. Theater ratio 0.68: significant portion of pedagogical activity is performative (defending model coherence) rather than functional (explaining actual observations). The system persists through institutional habit, not empirical strength. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(solar_system_weirdness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, there is no escaping the observational data: the solar system's orbital configuration and composition distribution represent physical facts independent of our theories. Any model must conform to these observations. However, this perspective risks naturalizing what is actually a contingent institutional arrangement (the suppression of migration theories for decades). The mountain classification here is a false summit: the structural data (ε=0.38, suppression=0.52, theater=0.68) reveals institutional constraints, not natural laws.
constraint_indexing:constraint_classification(solar_system_weirdness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solar_system_weirdness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(solar_system_weirdness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solar_system_weirdness, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(solar_system_weirdness, TR),
    TR >= 0.70.

:- end_tests(solar_system_weirdness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The anomaly enables migration theory research and funding, creating a career asymmetry favoring those who embrace new models over those defending classical orthodoxy. But extraction is not severe — the classical theory is not being actively suppressed by powerful actors, merely defended through institutional habit. The 40-year progression from 0.18 to 0.38 reflects increasing tension: as exoplanet observations accumulate, the cost of defending in situ against the evidence rises. Suppression (0.52): Moderate-high. Barriers include textbook pedagogy that presents classical theory as settled, career risk for junior researchers challenging consensus, publication bias favoring novel models over null results, and the intellectual lock-in from decades of educational investment in in situ frameworks. But suppression is not absolute — migration theories are published, funded, and widely discussed. Theater ratio (0.68): High and rising. The defense of classical in situ theory requires increasing rhetorical sophistication as anomalies accumulate: invoking local uniqueness of our system, emphasizing differences between Solar System and exoplanet populations, defending the model as 'good enough' despite empirical problems. This performative work has grown from 0.35 to 0.68 over the measurement interval. Actual predictive function has stagnated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows perspectival divergence across power levels. The powerless perspective (classical theory) sees a snare — trapped by contradictory observations and institutional defense of indefensible positions. The institutional perspective (migration advocates) sees coordination (rope) — solving real problems and explaining observations. The organized perspective (mission planners) sees a temporary problem with a known solution (scaffold) — higher-resolution observations will resolve within a generation. The analytical observer risks seeing natural law (mountain) — the solar system's configuration is just how it is — but the structural data reveals this as institutional suppression masquerading as inevitable fact. The perspectival gap is the gap between defending an outdated consensus and articulating a coherent new framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical planetary theory: Victim + trapped → d≈0.92, f(d)≈1.38. Trapped by contradictory observations; no exit without abandoning institutional identity. Migration theory advocates: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Capture research prestige and funding; can exit into new domains easily. Observational astronomers: Victim + constrained → d≈0.68, f(d)≈1.05. Must invest in expensive surveys; constrained by resource barriers but benefit from methodological development. Space mission planners: Organized + constrained → d≈0.35, f(d)≈0.35. Have agency and see a clear exit path (JWST observations); constraint is tactical, not structural. Solar system pedagogy: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification from theater_ratio gate, not from high chi. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is false summit; engine's natural law detector should flag this.
 *
 * MANDATROPHY ANALYSIS:
 *   The solar system weirdness resolves mandatrophy by revealing how institutional consensus can suppress empirical anomalies without active coercion. The constraint is genuinely hybrid (tangled_rope): migration theories do solve real problems (coordination function), but institutional defense of classical theory imposes costs on those who challenge consensus (asymmetric extraction). The mandatrophy resolution lies in recognizing that the 'natural law' perspective (mountain) is actually the most extractive: it naturalizes institutional suppression as inevitable scientific caution. The analytical observer's mountain classification is a false summit. The true structure is tangled_rope: coordination (explaining anomalies) layered with extraction (defending outdated orthodoxy through institutional inertia). This constraint demonstrates that institutional suppression can operate without explicit victims or beneficiaries — the cost is distributed across the epistemic commons (slower scientific progress, wasted pedagogical effort) while the benefit accrues to those defending institutional status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    in_situ_vs_migration_criterion,
    'What observational or dynamical criterion definitively distinguishes in situ formation from planetary migration?',
    'Direct imaging of exoplanet systems at formation-relevant ages; detection of migrant planet signatures (e.g., eccentric orbits, retrograde inclinations, compositional gradients); high-resolution radial velocity surveys for architecture statistics',
    'If in situ dominant: classical theory vindicated, migration theory becomes piton. If migration widespread: classical theory loses epistemic standing entirely. Current evidence suggests mixed processes, but institutional suppression may prevent acknowledgment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(in_situ_vs_migration_criterion, empirical, 'Whether in situ or migration processes dominate planetary architecture').

omega_variable(
    grand_tack_timing_resolution,
    'Can orbital resonance signatures and isotopic gradients definitively constrain the timing and sequence of Jupiter-Saturn migration?',
    'Precise N-body simulations with constraints from asteroid belt structure, isotopic anomalies (e.g., molybdenum 100Mo), and meteoritic evidence; comparison with exoplanet system statistics',
    'If timing is resolvable: Grand Tack and Nice models become testable rather than interpretive. If ambiguous: institutional actors can continue defending classical in situ model by casting migration as underconstrained speculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grand_tack_timing_resolution, empirical, 'Whether Grand Tack migration timing can be resolved observationally').

omega_variable(
    exoplanet_architecture_statistics,
    'Do exoplanet orbital architectures show statistical features inconsistent with in situ formation and requiring migration?',
    'Large surveys of exoplanet orbital parameters (eccentricity, inclination, period ratios); correlation with host star properties; machine learning classification of architecture types and comparison to formation models',
    'If exoplanet statistics strongly favor migration: institutional pressure on classical theory becomes irresistible. If statistics are ambiguous: classical theory can persist by invoking local uniqueness. Current evidence leans toward migration prevalence, but survey bias and selection effects create interpretive flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exoplanet_architecture_statistics, empirical, 'Whether exoplanet statistics require planetary migration mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solar_system_weirdness, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(solwrd_tr_t0, solar_system_weirdness, theater_ratio, 0, 0.35).
narrative_ontology:measurement(solwrd_tr_t20, solar_system_weirdness, theater_ratio, 20, 0.52).
narrative_ontology:measurement(solwrd_tr_t40, solar_system_weirdness, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(solwrd_be_t0, solar_system_weirdness, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(solwrd_be_t20, solar_system_weirdness, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(solwrd_be_t40, solar_system_weirdness, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solar_system_weirdness, information_standard).
narrative_ontology:affects_constraint(solar_system_weirdness, exoplanet_formation_consensus).
narrative_ontology:affects_constraint(solar_system_weirdness, giant_planet_migration_models).

% DUAL FORMULATION NOTE:
% The solar system configuration anomaly is upstream of exoplanet formation models: the observed architecture of our system constrains theories that predict exoplanet distributions. These three constraints form a family where the upstream (solar system weirdness, ε=0.38) shapes the downstream constraints through empirical data flow and institutional pressure to maintain theoretical coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
