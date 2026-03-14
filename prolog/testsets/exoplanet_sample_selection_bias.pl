% ============================================================================
% CONSTRAINT STORY: exoplanet_sample_selection_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exoplanet_sample_selection_bias, []).

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
 *   constraint_id: exoplanet_sample_selection_bias
 *   human_readable: Exoplanet Sample Selection Bias in Discovery and Characterization
 *   domain: astronomy/exoplanet_science
 *
 * SUMMARY:
 *   Exoplanet sample selection bias represents a structural constraint where
 *   the dominant detection method (transit photometry via Kepler, TESS)
 *   creates a systematically skewed census of the planetary population. The
 *   bias is not random error but a deterministic function of the detection
 *   method's sensitivity to short-orbital-period planets, large planets, and
 *   planets orbiting small stars. This constraint exhibits multiple
 *   classification types depending on structural position: for the actual
 *   planetary population, the bias is a snare (unmeasured, unrepresented,
 *   bearing costs of misunderstanding formation); for institutional survey
 *   leaders, it is coordination (their dominance solves where-to-look
 *   problems); for statistical correction efforts, it is a scaffold with a
 *   sunset (debiasing methods are maturing). The extractiveness value (0.52)
 *   reflects that the transit method's dominance captures career and funding
 *   benefits while also creating genuine scientific value — not pure
 *   extraction but a mixed arrangement where institutional benefits accrue
 *   asymmetrically to transit method developers and leaders. Theater ratio
 *   (0.68) reflects that much of the 'discovery narrative' in exoplanet
 *   astronomy emphasizes headline-grabbing individual planets rather than
 *   population-level statistical understanding.
 *
 * KEY AGENTS:
 *   - True Exoplanet Population: Primary victim (powerless/trapped) — unmeasured, unrepresented, cannot self-correct; bears cost of systematic bias in formation theory
 *   - Transit Survey Leaders (Kepler, TESS collaborations): Primary beneficiary (institutional/arbitrage) — dominant discovery census, funding concentration, sustained institutional priority
 *   - Alternative Detection Communities (RV, direct imaging, timing): Secondary victim (moderate/constrained) — face resource barriers, publication bias against null results, systematic underweighting in population statistics
 *   - Statistical Correction and Debiasing Projects: Organized agents (organized/mobile) — actively building debiasing methods and occurrence rate frameworks; see the problem as solvable with a sunset trajectory
 *   - Journal and Career Reward System: Institutional actor (institutional/arbitrage) — maintains performance hierarchy favoring 'big discoveries' (individual new exoplanet announcements) over population-level methodological advances
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing transit-method selection bias as inherent to observational astronomy rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_sample_selection_bias, 0.52).
domain_priors:suppression_score(exoplanet_sample_selection_bias, 0.58).
domain_priors:theater_ratio(exoplanet_sample_selection_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_sample_selection_bias, extractiveness, 0.52).
narrative_ontology:constraint_metric(exoplanet_sample_selection_bias, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exoplanet_sample_selection_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_sample_selection_bias, tangled_rope).
narrative_ontology:human_readable(exoplanet_sample_selection_bias, "Exoplanet Sample Selection Bias in Discovery and Characterization").
narrative_ontology:topic_domain(exoplanet_sample_selection_bias, "astronomy/exoplanet_science").

domain_priors:requires_active_enforcement(exoplanet_sample_selection_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_sample_selection_bias, detection_method_developers).
narrative_ontology:constraint_beneficiary(exoplanet_sample_selection_bias, institutional_survey_leaders).
narrative_ontology:constraint_victim(exoplanet_sample_selection_bias, actual_planetary_population_knowledge).
narrative_ontology:constraint_victim(exoplanet_sample_selection_bias, overlooked_detection_methods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRUE EXOPLANET POPULATION (SNARE) — Cannot advocate or self-correct. The actual distribution of exoplanet properties (orbital periods, masses, architectures) remains unmeasured. Bias accumulates without feedback mechanism. Maximum extraction: our models of planetary formation are systematically distorted by selection effects we cannot escape without abandoning current detection methods.
constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE DETECTION COMMUNITIES (TANGLED ROPE) — Modest-scale teams using radial velocity, direct imaging, or timing variations face resource constraints and publication bias against negative results. They benefit from the shared exoplanet catalog and collaborative data access, but their methods are systematically underweighted in population statistics. Mixed extraction and coordination — some benefits, significant constraints.
constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSIT SURVEY LEADERS (ROPE) — Experiences the constraint as pure coordination: dominating the discovery census enables follow-up science, community engagement, and continued funding. The selection bias is their selection criterion — instrumental and methodological advantage. Net beneficiary. The constraint solves a real coordination problem: where should we look, and with what priority?
constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATISTICAL CORRECTION INITIATIVES (SCAFFOLD) — Organized projects (Exoplanet Catalog, occurrence rate frameworks, Bayesian hierarchical models) are building debiasing methods with a sunset trajectory. As statistical techniques mature and machine learning improves detection completeness characterization, the hard selection bias can be converted to a knowable, correctable systematic. The scaffold sunset is real: in 10-15 years, direct imaging and other methods may break the transit dominance.
constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLICATION AND PRIORITY SYSTEMS (PITON) — The journal impact hierarchy and career reward structure for 'big discovery' announcements persist through institutional inertia. High-profile exoplanet discoveries (habitable-zone transiting planets) receive disproportionate visibility, reinforcing funding flows to transit methods. The ritual is performative — it maintains a narrative of 'breakthrough discovery' divorced from statistical understanding of the population.
constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some selection bias is inherent to observational astronomy: any detection method (transit, radial velocity, direct imaging, microlensing) is sensitive only to certain planetary property regimes. This perspective naturalizes selection bias as an immutable constraint of observational physics. However, the structural data contradicts this mountain classification — the bias is not inherent to physics but to the current resource allocation and institutional prioritization favoring transit methods.
constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanet_sample_selection_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exoplanet_sample_selection_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exoplanet_sample_selection_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exoplanet_sample_selection_bias, TR),
    TR >= 0.70.

:- end_tests(exoplanet_sample_selection_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The transit method's dominance captures disproportionate funding, publication visibility, and career advancement opportunities. But the extraction is not maximal because (a) the method also produces genuine scientific discoveries enabling follow-up characterization, (b) collaborative data access benefits the entire community, and (c) alternative methods still function and contribute (though underweighted). The 0.38 → 0.52 progression reflects accumulation of institutional lock-in as transit-dominant surveys become the default reference. Suppression (0.58): Moderate-high. Barriers to detecting bias include: (1) the bias is 'invisible' unless explicitly modeled—many astronomers work within the transit census as given; (2) resource asymmetry makes alternative methods difficult to sustain; (3) publication bias against null results (failed searches, non-detections) hides the incompleteness; (4) career risk of challenging dominant methods. Theater ratio (0.68): Moderate-high, trending upward. The 'exoplanet discovery' narrative emphasizes individual headline planets (habitable-zone candidates, Earth analogues) over population statistics. This performative content has increased as social media amplifies discovery announcements. Statistical debiasing papers, while methodologically rigorous, receive less attention than individual-planet discovery papers.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival divergence. Transit survey leaders see pure coordination (Rope) — they solve the legitimate problem of prioritizing sky surveys. Statistical debiasing projects see a temporary problem with a methodological sunset (Scaffold) — as corrective techniques mature, the hard bias becomes a knowable systematic. The publication system sees its own performative ritual (Piton) — individual planet announcements drive engagement and visibility despite low contribution to population-level understanding. Alternative method communities experience mixed benefits and constraints (Tangled Rope) — they benefit from collaborative data but face resource asymmetry. The true planetary population experiences pure extraction (Snare) — its actual properties remain unknown and unmeasured. The analytical observer risks seeing a natural law (Mountain) — selection bias is inherent to observational astronomy — but the structural data reveals this as false naturalization: the bias is contingent on institutional funding allocation and career reward structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position. Transit survey leaders (beneficiaries with arbitrage options) experience low d, negative f(d), and therefore low or negative experienced extraction—the constraint subsidizes them. Alternative method communities (victims with constrained exit) experience moderate-high d and therefore moderate extraction. Statistical correction groups (organized with mobile options) experience lower d because they have alternative pathways (methodological innovation, international collaboration) and are not structurally dependent on a single funding stream. The true planetary population (powerless, trapped) has maximum d and maximum extraction. The publication system (beneficiary with arbitrage) experiences low d. The piton classification for the publication system derives from theater ratio (0.68) exceeding 0.70 threshold trajectory, not from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disambiguating physical inevitability from institutional contingency. All detection methods have selection effects (true physical limit) but only transit-method dominance is institutional contingency. The mountain perspective ('selection bias is inherent') conflates these. The scaffold perspective correctly identifies the institutional problem as solvable: (1) statistical debiasing improves over time, (2) alternative methods mature and diversify, (3) funding allocation can be rebalanced. The snare perspective correctly identifies that the unknown true population bears the cost and has no self-correction mechanism within current institutions. The tangled rope perspective correctly identifies that alternative methods both benefit from shared data access and suffer from resource asymmetry. The mandatrophy is resolved by showing that the constraint is NOT immutable physics but a solvable institutional problem with a known sunset pathway via statistical sophistication and method diversification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_correction_sufficiency,
    'Can statistical debiasing methods adequately correct for transit-method dominance, or do unknown unknowns in detection physics render the correction incomplete?',
    'Comparison of occurrence rates computed via statistical correction against direct surveys using alternative methods (e.g., direct imaging completeness, radial velocity population inference) when they reach comparable sample sizes',
    'If correction is sufficient: scaffold perspective confirmed, selection bias is a temporary institutional problem with a real sunset. If incomplete: the constraint persists as snare-like extraction despite correction efforts because underlying structural asymmetry remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_correction_sufficiency, empirical, 'Whether statistical debiasing adequately corrects for transit-method dominance').

omega_variable(
    alternative_method_breakthrough,
    'Will alternative detection methods (direct imaging, timing variations, astrometry via Gaia) achieve comparable or larger sample sizes in the next 10-15 years, disrupting transit-method dominance?',
    'Tracking of exoplanet discovery rates by method over time; analysis of funding allocation and telescope time distribution across methods; emergence of new major survey capabilities (e.g., ELT, next-generation direct imaging instruments)',
    'If breakthrough occurs: scaffold sunset is real, extraction mechanism loses force. If transit dominance persists: snare classification is correct, the constraint is structurally locked despite correction efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_method_breakthrough, empirical, 'Whether alternative detection methods will disrupt transit dominance').

omega_variable(
    career_incentive_structure,
    'Does the journal impact and career reward system for ''big exoplanet discoveries'' create independent institutional pressure favoring transit-method stories, independent of scientific merit?',
    'Citation analysis and career trajectory analysis: comparison of citation success and career advancement for papers discovering transit planets via major surveys vs papers advancing population-level understanding via debiasing methods; editorial desk-reject rates for low-statistical-significance but methodologically rigorous studies',
    'If independent institutional pressure exists: the piton is not just performative ritual but an active reinforcer of selection bias. If career incentives are method-agnostic: the bias is more purely methodological (mountain-like) than institutionally reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(career_incentive_structure, empirical, 'Whether career reward structures independently reinforce transit-method bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_sample_selection_bias, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exoplanet_tr_t0, exoplanet_sample_selection_bias, theater_ratio, 0, 0.52).
narrative_ontology:measurement(exoplanet_tr_t10, exoplanet_sample_selection_bias, theater_ratio, 10, 0.6).
narrative_ontology:measurement(exoplanet_tr_t20, exoplanet_sample_selection_bias, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(exoplanet_be_t0, exoplanet_sample_selection_bias, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(exoplanet_be_t10, exoplanet_sample_selection_bias, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(exoplanet_be_t20, exoplanet_sample_selection_bias, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_sample_selection_bias, information_standard).
narrative_ontology:affects_constraint(exoplanet_sample_selection_bias, habitable_zone_planet_frequency).
narrative_ontology:affects_constraint(exoplanet_sample_selection_bias, super_earth_formation_theory).
narrative_ontology:affects_constraint(exoplanet_sample_selection_bias, hot_jupiter_prevalence_puzzle).

% DUAL FORMULATION NOTE:
% Exoplanet sample selection bias is upstream of specific planetary population claims. Individual discoveries (habitable-zone candidates, super-Earth formation efficiency, hot Jupiter prevalence) each inherit the selection bias but represent distinct downstream constraints with their own extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
