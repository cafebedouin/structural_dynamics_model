% ============================================================================
% CONSTRAINT STORY: exoplanet_habitability_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exoplanet_habitability_arbitrage, []).

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
 *   constraint_id: exoplanet_habitability_arbitrage
 *   human_readable: Exoplanetary Habitability Arbitrage Strategy
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   The exoplanetary habitability search has undergone a strategic pivot from
 *   seeking direct Earth-analog planets to identifying statistical anomalies
 *   relative to solar-system norms. The classical approach — habitable zone
 *   calculations indexing Earth's parametric space (stellar flux,
 *   temperature, water equilibrium) — yielded negligible results despite
 *   decades of observation. The emerging strategy reframes the problem:
 *   instead of 'find planets like Earth,' identify planets with unusual
 *   configurations (mass + orbital + metallicity interactions) that deviate
 *   from the solar system's observed statistical profile, under the
 *   hypothesis that genuine extraterrestrial habitability may depend on
 *   non-obvious state combinations untested in our solar system. This
 *   constraint exhibits the full structure of arbitrage extraction: the
 *   transition serves genuine methodological value (reframing an ill-posed
 *   search problem), yet simultaneously extracts institutional position,
 *   publication advantage, and research direction priority from programs
 *   invested in the older paradigm. The theater ratio (0.62) reflects that
 *   institutional language around 'habitable zones' persists in funding
 *   justifications while actual research methodology has migrated toward
 *   anomaly detection; the old framework functions as performative
 *   justification for work that has operationally moved beyond it.
 *
 * KEY AGENTS:
 *   - Statistical Anomaly Research Groups: Primary beneficiary (organized/constrained) — capture methodological innovation and first-mover advantage in anomaly-detection publication space
 *   - Earth-Twin Search Programs: Primary victim (powerless/trapped) — locked into obsolete methodology; cannot pivot without losing credibility
 *   - Competing Exoplanet Research Teams: Secondary victim (moderate/constrained) — constrained by existing expertise and publication record alignment
 *   - Major Observatory Networks and Space Agencies: Institutional beneficiary (institutional/arbitrage) — experience genuine coordination benefit through observational asset repurposing
 *   - Terrestrial Analog Legacy Programs: Transitional actor (organized/mobile) — provide scaffolding for anomaly detection models but face phaseout
 *   - Habitable Zone Institutional Framework: Performative system (institutional/arbitrage) — maintains institutional inertia while actual research methodology evolves
 *   - Analytical Observer: Epistemological lens (analytical/analytical) — identifies both genuine methodological innovation and extractive institutional dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_habitability_arbitrage, 0.38).
domain_priors:suppression_score(exoplanet_habitability_arbitrage, 0.48).
domain_priors:theater_ratio(exoplanet_habitability_arbitrage, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, extractiveness, 0.38).
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_habitability_arbitrage, tangled_rope).
narrative_ontology:human_readable(exoplanet_habitability_arbitrage, "Exoplanetary Habitability Arbitrage Strategy").
narrative_ontology:topic_domain(exoplanet_habitability_arbitrage, "technological/scientific").

domain_priors:requires_active_enforcement(exoplanet_habitability_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_habitability_arbitrage, statistical_anomaly_research_groups).
narrative_ontology:constraint_beneficiary(exoplanet_habitability_arbitrage, observational_astronomy_institutions).
narrative_ontology:constraint_victim(exoplanet_habitability_arbitrage, earth_twin_discovery_programs).
narrative_ontology:constraint_victim(exoplanet_habitability_arbitrage, terrestrial_analog_search_paradigm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARTH-TWIN SEARCH PROGRAMS (SNARE) — Locked into obsolete methodology. Programs funded to find direct terrestrial analogs cannot pivot to anomaly-based arbitrage without losing institutional credibility and funding justification. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING EXOPLANET RESEARCH TEAMS (SNARE) — Face high barriers to switching research focus. Existing publication records, methodology expertise, and institutional alignment with mainstream earth-analog paradigm create switching costs. Teams that pioneered earth-twin search cannot easily claim credit for anomaly-discovery strategy. d≈0.78, f(d)≈1.08, σ=1.2 → χ≈0.49.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STATISTICAL ANOMALY RESEARCH COALITION (TANGLED ROPE) — Organized actors (computational astrophysics groups, machine learning researchers, Bayesian analysis specialists) see both coordination benefit and extractive advantage. The anomaly-based strategy solves the real problem: earth-twin search had negligible success (false coordination frame). Anomalies offer genuine methodological innovation AND first-mover advantage in a new publication paradigm. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.19. Low extraction because the coalition has agency and captures real innovation value, not just institutional rents.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MAJOR OBSERVATORY NETWORKS (ROPE) — Institutional actors (NASA, ESA, observatory consortia) benefit from paradigm shift without core extraction. The anomaly strategy allows observational assets (Kepler, TESS, JWST spectroscopy) to be repurposed toward higher-discovery-yield targets. This is genuine coordination: reframing the search problem to match observable data actually improves information yield. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Institutional beneficiaries see net coordination value.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TERRESTRIAL ANALOG LEGACY PROGRAMS (SCAFFOLD) — Existing earth-analog programs (Mars analogs, exoplanet climate models indexed to Earth's parametric space) function as temporary scaffolding. They provide baseline training data and validation sets for anomaly detection models, but their primary mission becomes subordinate. As anomaly detection matures (5-10 year horizon), earth-analog programs phase into data-provider roles or dissolve. χ≈0.15 reflects that the old paradigm provides real but diminishing value; suppression is declining as the field collectively acknowledges that direct earth-twin search was a false frame.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: HABITABLE ZONE INSTITUTIONAL FRAMEWORK (PITON) — The classical habitable zone model (stellar flux, water equilibrium temperature) persists despite decreasing predictive power. It was a rational frame 20 years ago when exoplanet detection was new, but the framework now functions primarily as institutional justification for ongoing work. Many funding justifications still reference 'habitable zone' although the field has effectively moved to statistical anomaly detection. theater_ratio=0.62 reflects this degradation: institutional language persists but drives diminishing fraction of actual research methodology. Maintenance is through inertia and pedagogical convention, not because the model works.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the arbitrage is genuine: the earth-twin paradigm was implicitly assuming a single high-probability zone in parameter space, when empirical exoplanet data (and solar system comparison) reveals that habitability depends on non-obvious state combinations (planetary mass + stellar metallicity interactions, tidal heating, atmospheric circulation models, magnetic field dynamics). The shift from 'find earth-twins' to 'find statistical anomalies relative to solar-system-normal' is both methodologically honest AND extractive: it fragments the search space in ways that advantage computationally sophisticated teams over observation-limited programs. d≈0.55, f(d)≈0.73, σ=1.2 → χ≈0.34.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanet_habitability_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(exoplanet_habitability_arbitrage, TR),
    TR >= 0.70.

:- end_tests(exoplanet_habitability_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low initially (0.18), rising to 0.38 over 10-year interval. The paradigm shift genuinely solves a real problem (earth-twin search had ~zero discovery rate), but the transition simultaneously extracts position and priority from competing programs. The low initial value reflects that the strategy began as a methodological correction, not an extraction play. The increase reflects mounting evidence that anomaly detection is driving publication volume and observational priority regardless of whether it accelerates genuine habitability discovery. Suppression (0.48): Moderate. Earth-twin programs cannot easily exit (high institutional switching costs, funding commitments, expertise sunk). But suppression is not total — some programs have pivoted successfully, and the transition is occurring openly in the literature (not coercively enforced). Theater ratio (0.62): Moderate-high. The classical habitable zone framework persists in institutional language and grant justifications despite declining explanatory power in actual research. Funding agencies still reference 'habitable zone' targets while allocating observation time to anomaly detection; this gap indicates performative maintenance of outdated framing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extraction across institutional tiers. Earth-twin programs see a snare — they are trapped in methodology that produces zero results, yet cannot exit without losing credibility. Competing teams see extraction (snare) — methodological standards have shifted, making their existing expertise less valuable. The anomaly coalition sees genuine innovation (tangled rope) — they are solving a real problem AND capturing first-mover advantage; both are legitimate. Major observatories see pure coordination benefit (rope) — the reframing allows higher-yield observational strategy. Legacy terrestrial analog programs see temporary support with a sunset clause (scaffold) — they provide baseline data but face institutional subordination as anomaly detection matures. The institutional habitability framework sees its own degradation (piton) — the classical framework persists through inertia, not function. The analytical observer sees both methodological honesty and extractive arbitrage (tangled rope) — the pivot is epistemologically justified AND exploits institutional asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Earth-twin programs: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction with no exit path. Competing teams: Victim + constrained → d≈0.78, f(d)≈1.08. High extraction; some exit possible through methodological transition but at significant cost. Anomaly coalition: Beneficiary + constrained → d≈0.42, f(d)≈0.42. Moderate position; coalition has agency but operates within observational and computational constraints. Major observatories: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries with high mobility. Legacy programs: Mixed status + mobile → d≈0.50, f(d)≈0.65. Transitional actors; high mobility enables phaseout. Habitability framework: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Framework maintains institutional position despite declining functional role. Analytical observer: d≈0.55, f(d)≈0.73. Observer experiences the constraint asymmetrically — must balance recognition of genuine methodological innovation against institutional extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the arbitrage is BOTH genuine methodological progress AND extractive institutional dynamics. The earth-twin paradigm was epistemologically incorrect — it assumed habitability could be parametrized in terms of single-variable ranges (habitable zone), when empirical exoplanet data and comparative solar-system analysis suggest habitability depends on multi-variable interactions (mass-metallicity coupling, tidal effects, magnetic shielding). The shift to anomaly detection is methodologically honest. Simultaneously, the transition extracts institutional position from programs committed to the old frame. These are not contradictory — the extraction arises from the institutional lag between epistemological updating and career commitments. The tangled rope classification (not snare, not pure rope) captures this: the strategy provides genuine coordination benefit (solves ill-posed problem) AND asymmetric extraction (advantages organized, computationally sophisticated teams over observation-limited programs). No single type captures the full picture; the presheaf of perspectives reveals the structure. Earth-twin programs experience it as snare (genuine trap with no exit). Anomaly advocates experience it as rope (coordination that advances shared knowledge). Observational institutions experience it as scaffold with sunset (temporary reorientation toward higher-yield targets). The constraint is mandatropic resolved: all six types have legitimate readings, and their coexistence reveals an unstable equilibrium in the scientific methodology landscape.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solar_system_weirdness_threshold,
    'What constitutes ''anomaly'' relative to solar system statistical norms? Is the solar system''s composition typical, or does the definition of anomaly depend on which solar-system properties we treat as reference baseline?',
    'Comparative exoplanet population statistics; identification of which solar-system parameters (planetary mass distribution, orbital spacing, stellar metallicity, atmospheric composition ratios) are statistically typical vs outliers in the full exoplanet catalog',
    'If solar system is typical: anomaly detection is identification of genuinely rare configurations with distinctive properties. If solar system is statistical outlier: anomaly detection becomes circular — anomalies are ''things different from us,'' which biases discovery toward planets unlike Earth regardless of habitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(solar_system_weirdness_threshold, empirical, 'Whether solar system baseline is typical or definitionally anomalous').

omega_variable(
    habitability_multimodal_structure,
    'Does habitability have a single dominant ''attractor'' in parameter space (classical habitable zone assumption), or multiple stable configurations (tidal heating + atmospheric recycling, stellar radiation + magnetic shielding, subsurface chemistry)? If multimodal, is anomaly detection discovering genuinely inhabitable niches or just parameter-space fragmentation?',
    'Theoretical modeling of habitability under diverse planetary conditions; experimental constraints on chemical/biological thresholds; statistical analysis of exoplanet properties vs detection likelihood to identify discovery bias',
    'Single attractor: anomaly detection is high-fidelity. Multiple attractors: anomaly detection is methodologically sound but may yield high false-positive rate for genuine habitability (discovering weird planets, not necessarily inhabited ones). This determines whether the strategy accelerates genuine life detection or just publication productivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(habitability_multimodal_structure, empirical, 'Whether habitability parameter space is unimodal or multimodal').

omega_variable(
    observational_selection_bias_in_anomaly_framing,
    'Does the shift from earth-twin to anomaly-detection selection methodology change which planets are detectable, and does this change inadvertently bias toward planets with detectable anomalies (e.g., strong spectroscopic signatures, high transit timing variations) rather than planets with true habitability signatures?',
    'Meta-analysis of exoplanet discovery bias between earth-analog programs vs anomaly-detection publications; simulation of which planetary types would be detected under each paradigm; comparison of discovery statistics pre- and post-paradigm shift (2024-2030)',
    'If detection bias is equivalent: anomaly detection is a genuine methodological innovation. If anomaly-biased: the strategy may achieve high publication count while missing actual habitable worlds. The extraction becomes more subtle — not resource capture but epistemic arbitrage (controlling which discoveries count as ''scientific success'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_selection_bias_in_anomaly_framing, empirical, 'Whether anomaly framing changes observational selection bias').

omega_variable(
    timeline_to_exoplanet_biosignature_detection,
    'Can the anomaly-detection strategy accelerate identification of planets with detectable biosignatures (atmospheric oxygen, phosphine, methane disequilibrium)? Or does the pivot from ''habitable'' to ''anomalous'' delay genuine biosignature discovery by fragmenting observational focus?',
    'Projected timeline for JWST/future spectrograph biosignature detection under earth-twin vs anomaly-detection strategies; modeling of which exoplanet subsets can yield biosignature measurements with planned instruments',
    'If anomaly detection accelerates biosignature discovery: the arbitrage is justified by genuine scientific gain, and extraction is incidental coordination cost. If it delays: the strategy is extractive rent-seeking (institutional prestige capture) disguised as methodological progress.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(timeline_to_exoplanet_biosignature_detection, empirical, 'Whether anomaly strategy accelerates or delays biosignature detection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_habitability_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exoplanet_hab_tr_t0, exoplanet_habitability_arbitrage, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exoplanet_hab_tr_t5, exoplanet_habitability_arbitrage, theater_ratio, 5, 0.5).
narrative_ontology:measurement(exoplanet_hab_tr_t10, exoplanet_habitability_arbitrage, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(exoplanet_hab_be_t0, exoplanet_habitability_arbitrage, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(exoplanet_hab_be_t5, exoplanet_habitability_arbitrage, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(exoplanet_hab_be_t10, exoplanet_habitability_arbitrage, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_habitability_arbitrage, information_standard).
narrative_ontology:affects_constraint(exoplanet_habitability_arbitrage, habitable_zone_parameter_space).
narrative_ontology:affects_constraint(exoplanet_habitability_arbitrage, exoplanet_atmospheric_spectroscopy_priority).
narrative_ontology:affects_constraint(exoplanet_habitability_arbitrage, biosignature_detection_feasibility).

% DUAL FORMULATION NOTE:
% The habitability arbitrage strategy is downstream of the recognition that classical earth-twin search was poorly calibrated. It also constrains the feasibility of biosignature detection programs by fragmenting observational focus. These constraints form a family: classical habitability framing → recognition of parametric inadequacy → shift to anomaly detection. Each story in the family has distinct ε values reflecting different empirical status and extraction characteristics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exoplanet_habitability_arbitrage, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
