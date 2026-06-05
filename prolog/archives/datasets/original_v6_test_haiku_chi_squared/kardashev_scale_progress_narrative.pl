% ============================================================================
% CONSTRAINT STORY: kardashev_scale_progress_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kardashev_scale_progress_narrative, []).

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
 *   constraint_id: kardashev_scale_progress_narrative
 *   human_readable: The Kardashev Scale as a Narrative of Civilizational Progress
 *   domain: social/technological/cosmology
 *
 * SUMMARY:
 *   The Kardashev scale, introduced by Nikolai Kardashev in 1964 as a SETI
 *   framework for categorizing detectable civilizations by energy usage, has
 *   been popularized and institutionalized as a teleological narrative of
 *   civilizational progress. The original technical context — distinguishing
 *   civilizations by their command of stellar, galactic, and universal energy
 *   resources — has been transmuted into a moral narrative where energy
 *   consumption becomes synonymous with advancement. This constraint operates
 *   as a tangled rope: it coordinates technological discourse and provides
 *   shared metrics for discussing civilization-scale problems, while
 *   simultaneously suppressing alternative progress frameworks (resilience,
 *   knowledge density, ecosystem health) and justifying energy-intensive
 *   extraction as moral imperative. The theater ratio (0.65) reflects the
 *   increasing performative content: energy consumption is discussed as
 *   inevitable technological destiny and civilizational necessity, while the
 *   contingent institutional choices enabling energy maximization (fossil
 *   fuel subsidies, energy-dense industrial production, infrastructure
 *   lock-in) remain naturalized and invisible. The extractiveness has risen
 *   from 0.18 (1963, pure technical heuristic) to 0.52 (2024,
 *   institutionalized justification for ecological extraction), revealing how
 *   a neutral metric becomes an enforced narrative.
 *
 * KEY AGENTS:
 *   - Energy-Intensive Industrial Sectors: Primary beneficiaries (institutional/arbitrage) — leverage Kardashev narrative to justify expansion and subsidies
 *   - Technological Optimists and Transhumanists: Secondary beneficiaries (powerful/mobile) — use scale to position human enhancement and space expansion as moral progress
 *   - Ecological Systems and Future Generations: Primary victims (powerless/trapped) — forced to absorb costs of energy maximization framed as civilizational necessity
 *   - Climate Scientists and Alternative Progress Theorists: Secondary victims (organized/constrained) — constrained by dominant narrative; also benefit from shared language for discussing scale
 *   - Degrowth and Ecological Economics Movements: Emerging agents (organized/mobile) — building alternative frameworks with sunset logic for Kardashev dominance
 *   - State Energy Planning Institutions: Performative maintainers (institutional/constrained) — use Kardashev-adjacent metrics for policy theater while remaining locked into fossil infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kardashev_scale_progress_narrative, 0.52).
domain_priors:suppression_score(kardashev_scale_progress_narrative, 0.58).
domain_priors:theater_ratio(kardashev_scale_progress_narrative, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, extractiveness, 0.52).
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kardashev_scale_progress_narrative, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kardashev_scale_progress_narrative, tangled_rope).
narrative_ontology:human_readable(kardashev_scale_progress_narrative, "The Kardashev Scale as a Narrative of Civilizational Progress").
narrative_ontology:topic_domain(kardashev_scale_progress_narrative, "social/technological/cosmology").

domain_priors:requires_active_enforcement(kardashev_scale_progress_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, energy_intensive_industrial_sectors).
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, technological_optimists).
narrative_ontology:constraint_beneficiary(kardashev_scale_progress_narrative, centralized_energy_producers).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, ecological_systems).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, climate_stability).
narrative_ontology:constraint_victim(kardashev_scale_progress_narrative, alternative_progress_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECOLOGICAL SYSTEMS / FUTURE GENERATIONS (SNARE) — Cannot exit the constraint; bears full cost of energy-centric progress narrative justifying ecological extraction. Powerless to reframe progress criteria. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88. The narrative locks in energy maximization as moral progress, suppressing alternatives.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE ADVOCATES / ALTERNATIVE PROGRESS THEORISTS (TANGLED ROPE) — Constrained by institutional dominance of energy-scale metrics but also benefit from shared infrastructure for measuring systemic scale and complexity. Possess some organizing capacity. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.68. Mixed: the narrative suppresses alternative frameworks but also creates common language for discussing civilization-scale problems.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ENERGY-INTENSIVE SECTORS / TECHNOLOGICAL OPTIMISTS (ROPE) — Primary beneficiaries. The Kardashev narrative legitimizes energy consumption as civilizational imperative. Experiences constraint as coordination: shared metric for comparing technology stacks and expansion strategies. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; the constraint coordinates industrial alignment.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEGROWTH / ECOLOGICAL ECONOMICS (SCAFFOLD) — Emerging counternarrative treating the Kardashev constraint as temporary institutional dominance. See alternative progress metrics (wellbeing, resilience, biodiversity, knowledge density per unit energy) as maturing. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31. Moderate extraction because the movement has agency and sees a sunset: post-industrial metrics are building legitimacy through lived communities and policy experiments.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE REGULATORY / ENERGY PLANNING (PITON) — Persists in using Kardashev-adjacent metrics (energy per capita, power generation capacity) for national competitiveness and security. Theater ratio = 0.65: Much state energy policy performs technological mastery and civilizational competition theater while actual governance remains constrained by legacy infrastructure and geopolitics. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39. The constraint's performative function dominates its coordination function.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From thermodynamics and physics, energy is a necessary condition for information processing and work. Any civilization must manage energy flows. However, the structural data (ε=0.52, suppression=0.58, theater=0.65) reveals this perspective as a false summit: the constraint is not about energy's physical necessity but about the NARRATIVE that energy consumption = progress. The mountain classification would naturalize a contingent institutional choice.
constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kardashev_scale_progress_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kardashev_scale_progress_narrative, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kardashev_scale_progress_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kardashev_scale_progress_narrative, TR),
    TR >= 0.70.

:- end_tests(kardashev_scale_progress_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Kardashev narrative justifies energy consumption maximization as moral progress, licensing ecological extraction. But the extraction is not total because: (1) alternative frameworks are emerging with institutional adoption, (2) the narrative itself is contested, and (3) societies retain some capacity to redefine progress. The 0.52 value reflects that the constraint has real structural power but is not identical to physical energy necessity. Suppression (0.58): Moderate-high. Significant barriers to alternative progress frameworks include: institutional inertia in energy-based metrics, economic structures dependent on cheap energy, geopolitical competition incentivizing high-energy capabilities, and the narrative's appeal to technological optimists. But suppression is not total — degrowth movements, circular economy advocates, and indigenous frameworks maintain visibility. Theater ratio (0.65): High and rising. The Kardashev scale is increasingly performative: used to justify energy megaprojects, signal technological sophistication, and naturalize extraction, while the underlying institutional choices (energy subsidies, industrial structure, geopolitical competition) remain unarticulated. The trajectory from 0.15 (1963) to 0.65 (2024) shows how a technical framework becomes a justification machine.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Energy-intensive sectors see legitimate coordination (Rope) — the shared metric enables technological comparison and cooperation. Technological optimists see natural law (Mountain) — energy is physically necessary, and increasing consumption is inevitable progress. Ecological systems and future generations see pure extraction (Snare) — the narrative suppresses alternatives and justifies costs they cannot consent to. Climate advocates see mixed constraint (Tangled Rope) — the scale's descriptive power helps frame problems, but the narrative suppresses solutions. Degrowth movements see temporary dominance (Scaffold) — emerging alternative frameworks with real sunset mechanisms. State regulators see performative theater (Piton) — the scale legitimizes energy megaprojects that persist through bureaucratic inertia despite fiscal and ecological costs. The perspectival gap is not about different factual beliefs but about different structural relationships to the narrative's function: who benefits from treating energy as the ultimate measure of civilization?
 *
 * DIRECTIONALITY LOGIC:
 *   Energy-intensive sectors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. Technological optimists: Powerful + mobile → d≈0.45, f(d)≈0.50. Moderate extraction because they retain choice. Ecological systems: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — cannot exit narrative or reorganize. Climate advocates: Organized + constrained → d≈0.70, f(d)≈1.08. High extraction but with organizing capacity. Degrowth movements: Organized + mobile → d≈0.50, f(d)≈0.65. Moderate extraction because they see alternatives and are building them. State regulators: Victim (to lock-in) + constrained → d≈0.55, f(d)≈0.75. Trapped in infrastructure and geopolitical competition despite rhetorical claims to energy stewardship. The crucial move: treating institutions as agents with directionality reveals how the state is both enforcer and victim of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA HISTORICIZATION. The mandatrophy (Is this coordination or extraction?) is resolved by recognizing that the constraint's function has shifted over 61 years. In 1963, the Kardashev scale was pure coordination — a neutral technical framework (ε≈0.18) enabling SETI hypothesis construction. By 1990, it was becoming hybrid (ε≈0.32) as technological optimists began weaponizing it as justification narrative. By 2024, it functions primarily as extraction-justification (ε≈0.52) while maintaining coordination surface-structure. The theater ratio rising from 0.15 to 0.65 captures this drift. The mandatrophy is resolved not by choosing one type but by tracking the constraint's lifecycle: neutral tool → ideological weapon → institutional lock-in. The scaffold perspective (degrowth movements) suggests a real sunset path, distinguishing this from a pure snare. The analytical observer's mountain classification is revealed as false: physics requires energy management, but physics does not require treating energy maximization as moral progress. That equation is institutional, contestable, and declining in legitimacy among emerging frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    progress_metric_sovereignty,
    'Who has authority to define what counts as civilizational progress, and can that authority shift without a civilization-scale rupture?',
    'Historical analysis of prior progress-metric transitions (agricultural → industrial → informational); ethnographic study of communities successfully operating under alternative progress frameworks; policy experiments with non-energy-based competitiveness metrics',
    'If authority is lock-in (path-dependent institutional inertia): the constraint is a snare, and exit requires civilizational reorganization. If authority is contestable (multiple legitimate frameworks coexist): the constraint is a scaffold, with real sunset mechanisms emerging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progress_metric_sovereignty, conceptual, 'Whether progress metric redefinition is possible without rupture').

omega_variable(
    energy_sufficiency_threshold,
    'Is there a threshold of per-capita energy below which organized civilization becomes impossible, or is this assumption a rationalization for maximization?',
    'Cross-cultural comparison of energy consumption vs. institutional complexity, health, knowledge production, political stability; identification of causal vs. correlative relationships; natural experiments (energy-poor societies with high institutional capacity)',
    'If threshold exists and is high: Kardashev scale captures real physical constraint (partial mountain). If threshold is low and variable: energy maximization is choice, not necessity (pure extraction snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(energy_sufficiency_threshold, empirical, 'Whether energy sufficiency has a civilizational floor or is maximizable').

omega_variable(
    alternative_scale_legitimacy,
    'Can alternative civilizational scales (knowledge density per joule, resilience-index, biodiversity metrics) achieve institutional adoption and predictive power equivalent to energy-based scales?',
    'Implementation of alternative metrics in policy frameworks; correlation with long-term civilizational stability indicators; adoption rates in scientific and policy communities; predictive accuracy for societal outcomes',
    'If yes: scaffold perspective is confirmed; sunset is real. If no: alternative frameworks are aspirational theater (piton); energy scale remains lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_scale_legitimacy, empirical, 'Whether alternative progress scales can achieve institutional legitimacy').

omega_variable(
    seti_selection_bias,
    'Does the Kardashev scale''s original SETI context (identifying detectable civilizations) introduce an observational bias toward high-energy civilizations that actually exist, or is it an accurate hypothesis space for possible civilizations?',
    'Theoretical analysis of SETI detection physics; examination of whether low-energy or non-electromagnetic civilizations would be invisible by design; comparison with other exoplanet detection biases',
    'If observational bias: the scale conflates ''observable'' with ''advanced,'' and the narrative naturalizes a detection artifact. If accurate: energy is genuinely predictive of cosmic presence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seti_selection_bias, empirical, 'Whether Kardashev scale reflects cosmic reality or SETI observation bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kardashev_scale_progress_narrative, 1963, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kardashev_tr_t1963, kardashev_scale_progress_narrative, theater_ratio, 1963, 0.15).
narrative_ontology:measurement(kardashev_tr_t1990, kardashev_scale_progress_narrative, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(kardashev_tr_t2010, kardashev_scale_progress_narrative, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(kardashev_tr_t2024, kardashev_scale_progress_narrative, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(kardashev_be_t1963, kardashev_scale_progress_narrative, base_extractiveness, 1963, 0.18).
narrative_ontology:measurement(kardashev_be_t1990, kardashev_scale_progress_narrative, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(kardashev_be_t2010, kardashev_scale_progress_narrative, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(kardashev_be_t2024, kardashev_scale_progress_narrative, base_extractiveness, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kardashev_scale_progress_narrative, information_standard).
narrative_ontology:affects_constraint(kardashev_scale_progress_narrative, fossil_fuel_subsidies_legitimacy).
narrative_ontology:affects_constraint(kardashev_scale_progress_narrative, geoengineering_acceleration_narrative).
narrative_ontology:affects_constraint(kardashev_scale_progress_narrative, technological_solutionism_lock_in).

% DUAL FORMULATION NOTE:
% The Kardashev scale decomposes into two constraints: (1) the physical necessity of energy management (Mountain, ε≤0.25), and (2) the institutional narrative that energy consumption = progress (Tangled Rope, ε=0.52). The story addresses the narrative constraint downstream of the physical constraint. They share observational overlap but have structurally distinct ε values and classification implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kardashev_scale_progress_narrative, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
