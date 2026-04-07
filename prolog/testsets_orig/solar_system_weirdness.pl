% ============================================================================
% CONSTRAINT STORY: solar_system_weirdness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   The solar system configuration anomaly represents a constraint that
 *   emerges from the conflict between an inherited theoretical model and
 *   mounting observational evidence. For most of the 20th century,
 *   astronomers viewed our solar system as a paradigmatic instance of
 *   planetary formation — a model of stability and regularity that should
 *   characterize planetary systems throughout the galaxy. The discovery and
 *   characterization of exoplanetary systems beginning in 1995 shattered this
 *   assumption: 96% of known exoplanetary systems have configurations
 *   fundamentally different from our own. The solar system is statistically
 *   rare or highly unusual. Yet the paradigm persists: it remains the leading
 *   example in most textbooks, the default framework for exoplanet
 *   comparisons, and the institutional reference point. This constraint is
 *   both a coordination mechanism (the solar system provides a known
 *   reference case for comparative analysis) and an extraction mechanism (the
 *   paradigm enforcement suppresses alternative frameworks and extracts
 *   intellectual labor from the exoplanet community to explain solar system
 *   uniqueness). The constraint exhibits high theater: much pedagogical and
 *   institutional activity is performed in service of maintaining the
 *   paradigm rather than advancing genuine understanding. The measurements
 *   show increasing theater (0.42 to 0.65) and extractiveness (0.18 to 0.32)
 *   over a 20-year horizon, indicating degradation toward piton status. The
 *   existence of viable alternative theoretical frameworks (in-situ
 *   formation, common dynamical architecture classes) that make no special
 *   reference to the solar system suggests the constraint has entered the
 *   scaffold phase — it has a sunset, as alternative paradigms mature.
 *
 * KEY AGENTS:
 *   - Institutional Astronomy Establishment: Primary beneficiary (institutional/arbitrage) — maintains textbook priority, curricular frameworks, and paradigm authority through solar system centrality
 *   - Exoplanet Observation Community: Primary victim (powerless/trapped) — forced to reference anomalies relative to solar system standard despite overwhelming evidence of solar system rarity; no exit options in career structure
 *   - Planet Migration Theorists: Organized secondary beneficiary (organized/constrained) — Nice model and disk migration theories gain funding and publication venues from the paradigm conflict, but constrained by need to defend increasingly elaborate mechanisms
 *   - Core Planetary Formation Paradigm: Abstract beneficiary (institutional/arbitrage) — the theoretical framework that predicts solar-system-like outcomes remains institutionally central despite observational challenges
 *   - Kepler and TESS Data Consortia: Organized agents (organized/mobile) — possess exit options as exoplanet catalogs grow; building alternative classification schemes independent of solar system reference
 *   - Textbook Editorial Institutions: Institutional actor (institutional/constrained) — maintain solar system centrality through pedagogical convention; face switching costs in curriculum redesign
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solar_system_weirdness, 0.32).
domain_priors:suppression_score(solar_system_weirdness, 0.48).
domain_priors:theater_ratio(solar_system_weirdness, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solar_system_weirdness, extractiveness, 0.32).
narrative_ontology:constraint_metric(solar_system_weirdness, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(solar_system_weirdness, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(solar_system_weirdness, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(solar_system_weirdness, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solar_system_weirdness, tangled_rope).
narrative_ontology:human_readable(solar_system_weirdness, "The Solar System Configuration Anomaly").
narrative_ontology:topic_domain(solar_system_weirdness, "technological/scientific").

domain_priors:requires_active_enforcement(solar_system_weirdness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(solar_system_weirdness, core_planetary_formation_paradigm).
narrative_ontology:constraint_beneficiary(solar_system_weirdness, institutional_astronomy).
narrative_ontology:constraint_victim(solar_system_weirdness, exoplanet_observation_programs).
narrative_ontology:constraint_victim(solar_system_weirdness, theoretical_model_consensus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXOPLANET OBSERVATION COMMUNITY (SNARE) — Trapped by the paradigm that the solar system is typical. Observational data repeatedly falsifies this assumption (96% of known exoplanetary systems have configurations unlike our own), yet the enforcement of the 'solar system as standard' model suppresses alternative theoretical frameworks. No exit: funding, publication, and career advancement depend on work that either fits the paradigm or explicitly argues against it. Maximum extraction — the constraint extracts observational labor and intellectual resources without genuine coordination benefit.
constraint_indexing:constraint_classification(solar_system_weirdness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLANET MIGRATION THEORISTS (TANGLED ROPE) — Organized groups (Nice model proponents, disk migration advocates) benefit from the paradigm conflict: anomalies create funding and publication opportunities for theories that explain solar system uniqueness. But constrained by the need to defend elaborate migration mechanisms against simpler alternative models. Both coordination (anomaly drives theoretical work) and extraction (paradigm enforcement delays paradigm shift).
constraint_indexing:constraint_classification(solar_system_weirdness, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL ASTRONOMY ESTABLISHMENT (ROPE) — Benefits from the 'solar system as standard' assumption. Textbooks, curricula, and institutional frameworks are built on this model. Exit options exist (paradigm shifts have happened before), but switching costs are high. The constraint functions as coordination: it enables systematic comparison of exoplanetary systems to a known reference case. Net beneficiary with arbitrage options — institutional incentives maintain the paradigm.
constraint_indexing:constraint_classification(solar_system_weirdness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: KEPLER AND TESS DATA ANALYSIS CONSORTIA (SCAFFOLD) — Organized agents with mobile exit options see the solar system paradigm as a temporary coordinate system. As exoplanet catalogs mature and statistical models improve, direct paradigm comparison becomes less necessary — systems can be classified by their own properties rather than deviation from the solar system. Sunset logic: 15-20 years as exoplanet samples grow large enough for independent statistical characterization. Theater is moderating as analysis shifts from 'how unlike Earth' to 'what are the natural system classes?'
constraint_indexing:constraint_classification(solar_system_weirdness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLANETARY SCIENCE TEXTBOOK INSTITUTIONAL COMPLEX (PITON) — The pedagogical use of the solar system as exemplary has become largely performative. New textbooks still lead with our solar system despite knowing it is atypical, maintaining the convention through institutional inertia. Theater ratio is high (0.65) — the rituals of solar system prioritization persist despite reduced functional value. Piton classification reflects degraded function maintained by institutional machinery.
constraint_indexing:constraint_classification(solar_system_weirdness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an abstract analytical viewpoint, the solar system configuration could be viewed as reflecting fundamental laws of planetary accretion and migration. The specific architecture (terrestrial planets inside, gas giants outside, gaps, resonances) might appear as emergent from universal physical principles. However, the widespread diversity of exoplanetary architectures contradicts this framing. The engine's false summit detector will flag this as naturalization of what is actually a historical contingency.
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
    constraint_indexing:constraint_classification(solar_system_weirdness, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.32): Moderate. The constraint extracts intellectual labor (forced comparative frameworks), publication attention (anomaly papers), and institutional compliance (textbook space) from the exoplanet community. However, extraction is not severe because the theoretical frameworks developed to explain solar system uniqueness (Nice model, migration theory) are scientifically productive independent of the paradigm. Some of what appears as extraction is legitimate scientific innovation. The reduced value (vs. initial 0.72 estimate) reflects partial recognition of the scientific value in the conflict. Suppression (0.48): Moderate-high. Significant barriers exist to adopting alternative reference frameworks — curriculum change, textbook production timelines, institutional momentum, funding agency expectations — but suppression is not total. Some researchers actively work outside the solar system reference model, and alternative frameworks have institutional support. Theater ratio (0.65): Moderately high and rising. The pedagogical use of the solar system persists despite known atypicality. Much energy is devoted to explaining solar system uniqueness rather than reconsidering the paradigm. The rise in theater (0.42 to 0.65) indicates increasing mismatch between functional value and performed activity. This trajectory suggests piton classification is approaching.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solar_system_statistical_ubiquity,
    'Is the solar system configuration rare, moderately common, or merely undersampled among exoplanetary systems?',
    'Statistical analysis of exoplanet populations using Bayesian hierarchical models; detection bias correction; projected detection rates as survey sensitivity improves',
    'If solar system is rare (< 5th percentile): paradigm inversion is justified, constraint collapses. If moderately common (20-40th percentile): paradigm shift is gradual, scaffold sunset extends. If undersampled artifact: paradigm persists as default reference, snare strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solar_system_statistical_ubiquity, empirical, 'Percentile rank of solar system configuration among exoplanetary populations').

omega_variable(
    migration_mechanism_universality,
    'Do all planetary systems undergo significant radial migration, or is disk migration rare and formation-in-situ is the dominant pathway?',
    'Isotopic abundance analysis of meteorites and exoplanet bulk compositions; direct imaging of protoplanetary disks and migration signatures; numerical simulations with realistic disk physics',
    'If migration universal: solar system is typical outcome of planetary formation, paradigm justified, snare weakens. If migration rare: solar system is special case, paradigm inversion accelerates, rope strengthens to tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migration_mechanism_universality, empirical, 'Prevalence of planet migration versus in-situ formation').

omega_variable(
    observation_bias_correction_methods,
    'How sensitive are conclusions about solar system typicality to unmodeled observation biases and detection method effects?',
    'Sensitivity analysis of exoplanet population statistics to completeness assumptions; comparison of different bias correction methodologies; synthetic surveys with known true population parameters',
    'If biases are well-understood and correctable: current conclusions about solar system rarity are robust, institutional paradigm cannot claim innocence, extraction mechanism is intentional, snare classification hardens. If biases are profound and asymmetric: paradigm defenders retain plausible deniability, constraint persists as tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observation_bias_correction_methods, empirical, 'Sensitivity of solar system typicality conclusions to observational bias correction').

omega_variable(
    paradigm_switching_cost_trajectory,
    'What is the institutional cost of shifting from ''solar system as reference'' to ''solar system as statistical sample''?',
    'Historical analysis of similar paradigm shifts (geocentrism to heliocentrism, steady-state to big bang); institutional surveys of curriculum update barriers; publication cost analysis',
    'If costs are high and concentrated: institutional incentives maintain paradigm indefinitely, constraint becomes a piton. If costs are declining: scaffold sunset accelerates, institutional resistance weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_switching_cost_trajectory, preference, 'Institutional switching costs for solar system reference model replacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solar_system_weirdness, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(solsys_tr_t0, solar_system_weirdness, theater_ratio, 0, 0.42).
narrative_ontology:measurement(solsys_tr_t10, solar_system_weirdness, theater_ratio, 10, 0.58).
narrative_ontology:measurement(solsys_tr_t20, solar_system_weirdness, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(solsys_be_t0, solar_system_weirdness, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(solsys_be_t10, solar_system_weirdness, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(solsys_be_t20, solar_system_weirdness, base_extractiveness, 20, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solar_system_weirdness, information_standard).
narrative_ontology:affects_constraint(solar_system_weirdness, planetary_formation_migration_mechanism).
narrative_ontology:affects_constraint(solar_system_weirdness, exoplanet_observation_bias_correction).

% DUAL FORMULATION NOTE:
% The solar system configuration anomaly decomposes into two structurally distinct constraints: (1) THE OBSERVATIONAL FACT: Solar system is statistically rare among exoplanetary systems (ε ≈ 0.05, mountain from empirical perspective). (2) THE INSTITUTIONAL PARADIGM: Textbooks and institutions maintain solar system as exemplary reference despite the fact (ε ≈ 0.32, tangled rope). Story focuses on constraint 2 — the institutional enforcement of a paradigm known to be atypical. Constraint 1 (the bare fact) is a natural law of population statistics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(solar_system_weirdness, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
