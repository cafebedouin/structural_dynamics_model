% ============================================================================
% CONSTRAINT STORY: exoplanet_habitability_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: astronomical/technological
 *
 * SUMMARY:
 *   The exoplanetary habitability search has undergone a strategic
 *   reorientation in the past decade: from seeking 'Earth twins' (planets in
 *   habitable zones around Sun-like stars with rocky, temperate atmospheres)
 *   to identifying and prioritizing anomalous systems that violate our solar
 *   system's observed architecture. This constraint represents the
 *   institutional mechanism that enforces the pivot, creating asymmetric
 *   benefits and costs across the research community. On one side, elite
 *   research groups and instrumentation developers benefit from the novelty
 *   premium and flexible target allocation that anomaly-seeking enables. On
 *   the other side, systematic Earth-analog search programs lose funding and
 *   credibility, and the statistical foundation required to interpret
 *   biosignatures reliably is undermined. The constraint exhibits a hybrid
 *   coordination-extraction structure: genuine coordination problems are
 *   solved (resource allocation toward high-impact targets), but the solution
 *   mechanism creates extraction by privileging anomaly-focused coalitions
 *   and suppressing Earth-analog comparison infrastructure.
 *
 * KEY AGENTS:
 *   - Anomaly-Focused Research Groups: Primary beneficiary (organized/constrained) — gain career advancement, publication advantage, and observation-time allocation from the pivot toward exotic targets
 *   - Instrumentation Developers: Primary beneficiary (institutional/arbitrage) — drive investment in higher-resolution spectroscopy and novel detection modalities, justified by anomaly-seeking targets
 *   - Earth-Analog Search Programs: Primary victim (moderate/constrained) — face resource reallocation, reduced publication prestige, and institutional devaluation as anomaly-seeking dominates
 *   - Statistical Epistemic Commons: Secondary victim (powerless/trapped) — the shift undermines foundational baselines needed to interpret biosignatures; unable to exit or organize
 *   - Open-Data Methodological Communities: Secondary actor (analytical/constrained) — building alternative infrastructure (public exoplanet archives, automated anomaly detection, citizen science) that bypasses elite-group definitions
 *   - Space Agencies and Major Telescopes: Institutional actor (institutional/arbitrage) — allocate observation time and funding based on anomaly prioritization, benefiting from the coordination problem solved by the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_habitability_arbitrage, 0.38).
domain_priors:suppression_score(exoplanet_habitability_arbitrage, 0.42).
domain_priors:theater_ratio(exoplanet_habitability_arbitrage, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, extractiveness, 0.38).
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(exoplanet_habitability_arbitrage, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_habitability_arbitrage, tangled_rope).
narrative_ontology:human_readable(exoplanet_habitability_arbitrage, "Exoplanetary Habitability Arbitrage Strategy").
narrative_ontology:topic_domain(exoplanet_habitability_arbitrage, "astronomical/technological").

domain_priors:requires_active_enforcement(exoplanet_habitability_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_habitability_arbitrage, anomaly_focused_research_groups).
narrative_ontology:constraint_beneficiary(exoplanet_habitability_arbitrage, instrumentation_developers).
narrative_ontology:constraint_victim(exoplanet_habitability_arbitrage, earth_analog_search_programs).
narrative_ontology:constraint_victim(exoplanet_habitability_arbitrage, statistical_clarity_of_biosignature_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATISTICAL CLARITY OF BIOSIGNATURE INTERPRETATION (SNARE) — The shift toward anomaly-seeking undermines the foundational statistical baseline required to interpret biosignatures reliably. Without a clear Earth-analog comparison framework, anomalies become unfalsifiable. The epistemic commons cannot exit this framework and bears the full cost of reduced interpretability.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARTH-ANALOG SEARCH PROGRAMS (SNARE) — Funded initiatives focused on identifying true Earth twins (similar orbital period, stellar type, atmospheric indicators) face resource reallocation and credibility erosion as funders and journals pivot toward 'exotic anomaly' narratives. Programs have some institutional momentum but constrained exit — abandoning Earth-analog searches yields no publishable results and risks appearing 'conventional'.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANOMALY-FOCUSED RESEARCH GROUPS (TANGLED ROPE) — Organized coalitions benefit from the shift (career advancement, novelty premium, accessible discovery claim) but also depend on maintaining enough Earth-analog infrastructure to define what counts as 'anomalous'. They enforce this framework by controlling which targets get observation time and funding, creating a hybrid coordination-extraction structure.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTRUMENTATION DEVELOPERS AND SPACE AGENCIES (ROPE) — Perceive the pivot as genuine coordination: anomaly-focused targets drive investment in higher-resolution spectroscopy, better coronagraphs, and novel detection modalities. The constraint solves a real resource-allocation problem (which stars to observe) while enabling new technological capability development.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-DATA METHODOLOGICAL COALITION (SCAFFOLD) — Open-access exoplanet databases (NASA Exoplanet Archive, GAIA archival data) and publicly archived observation records create an alternative verification pathway that bypasses the career-incentive arbitrage. Automated anomaly detection tools and citizen-science projects enable independent researchers to challenge elite-group definitions of 'anomaly' and 'habitability'. This is temporary because statistical literacy and computational tools will eventually commoditize anomaly ranking — sunset estimated at 15-25 years as ML-driven habitability scoring becomes standard.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the constraint appears as an immutable aspect of discovery science: all search processes exploit anomalies relative to some baseline expectation, and that baseline is always contingent on prior observations. The shift from Earth-analog to anomaly-seeking is simply the discovery of our solar system's statistical typicality — a claim about the world, not about human institutions. However, the structural data contradicts this mountain classification: the extraction is institutional (funding reallocation, credibility hierarchies, observation-time gatekeeping), not natural law.
constraint_indexing:constraint_classification(exoplanet_habitability_arbitrage, mountain,
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

:- end_tests(exoplanet_habitability_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The pivot captures real career and funding benefits for anomaly-focused coalitions during the observation-allocation window, but the extraction is not as severe as a pure snare (0.66+) because genuine scientific questions (target prioritization, optimal spectral allocation) are being solved. The initial value of 0.18 reflects that the pivot was justified by empirical findings about solar system atypicality; the increase to 0.38 reflects the institutional entrenchment of anomaly-seeking despite ambiguous evidence. Suppression (0.42): Moderate. Barriers include publication bias toward anomalous claims, citation networks favoring elite groups, and control of observation-time allocation by major institutions. However, suppression is incomplete because open-data archives and citizen-science tools are reducing gatekeeping power. Theater ratio (0.55): Moderate-high. Significant performative content exists in anomaly ranking — novelty narratives, discovery-framing in abstracts, and media engagement with 'exotic' systems. But the underlying science (high-resolution spectroscopy, multi-wavelength characterization) has genuine function. The theater has increased over the interval as the pivot has become institutionalized and career incentives have concentrated on anomaly narratives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival disagreement. Anomaly-focused groups and instrumentalists see coordination (Rope or Tangled Rope with coordination benefits) — the pivot solves a real resource-allocation problem. Earth-analog programs see extraction (Snare) — they are systematically defunded and delegitimized. The statistical commons sees pure extraction (Snare) — baselines for biosignature interpretation are being erased. The open-data coalition sees a temporary problem being solved (Scaffold) — public archives and automated tools will eventually commoditize anomaly rankings and reduce elite-group authority. The civilizational analytical observer risks naturalizing an institutional arrangement as a law of science (Mountain) — but the structural data reveals this as a false summit. The pivot is not justified by overwhelming empirical evidence that solar systems are typically anomalous; rather, the discovery of our own system's statistical typicality is being weaponized to privilege exotic targets and concentrate observation time in elite hands.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from structural position: beneficiaries with arbitrage options (instrumentalists, space agencies) experience low d → negative or low χ; victims with trapped or constrained exit (Earth-analog programs, statistical commons) experience high d → high χ; anomaly-focused organized groups with constrained exit but extraction benefits experience moderate d reflecting hybrid coordination-extraction. The constraint's directionality pattern shows asymmetric institutional power: elite groups enforce the pivot through observation-time gatekeeping and editorial leverage, while distributed communities (open-data projects, replication networks) have emerging countervailing power via public archives. Directionality overrides are not needed because the structural derivation (beneficiary + arbitrage → low d; victim + trapped → high d; organized + enforcement → moderate d) captures the institutional dynamics accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by distinguishing the empirical claim (solar system typicality) from the institutional mechanism (anomaly-seeking pivot). The empirical claim may be sound: if our solar system is indeed in the 5th percentile of typicality, then anomaly-seeking is a justified discovery strategy. But the institutional mechanism (observation-time gatekeeping, publication bias, career-incentive concentration) creates extraction that is orthogonal to the scientific validity of the empirical claim. The constraint is Tangled Rope because both claims are true: (1) anomaly-seeking solves a real coordination problem (target prioritization under resource constraints), and (2) the pivot creates asymmetric extraction (benefiting elite groups, harming distributed research programs). The mandatrophy is resolved by showing that coordination and extraction are not mutually exclusive — institutions can solve real problems while creating extraction. The false summit (Mountain perspective) fails because the pivot is contingent on institutional arrangements (funding allocation, journal gatekeeping, observation-time control), not on natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    solar_system_typicality_threshold,
    'How statistically typical is our solar system''s architecture (terrestrial planets in inner region, ice giants in outer region, habitable ecosphere on third planet) relative to the exoplanet population?',
    'Population synthesis models with observational bias correction; comparison of our system against synthetic populations constrained by detection limits; Bayesian inference of true exoplanet distributions',
    'If highly atypical (1st-5th percentile): anomaly-seeking is justified and prioritizes discovery efficiency. If typical (40th-60th percentile): Earth-analog searches are scientifically defensible, and the pivot is motivated by career incentives rather than empirical findings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solar_system_typicality_threshold, empirical, 'Statistical typicality of solar system architecture').

omega_variable(
    anomaly_biosignature_coupling,
    'Are biosignatures more likely to be found on planets with anomalous orbital/atmospheric properties, or is anomaly-seeking orthogonal to habitability probability?',
    'Theoretical models of biosignature generation under diverse atmospheric chemistries; comparison of biosignature detectability across Earth-analog vs exotic-architecture scenarios; meta-analysis of exoplanet discovery papers for implicit priors',
    'If coupled: anomaly-seeking is a legitimate search strategy. If orthogonal: the pivot is a status-seeking distraction from evidence-driven target prioritization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anomaly_biosignature_coupling, empirical, 'Relationship between orbital anomalies and biosignature detectability').

omega_variable(
    definition_capture_authority,
    'Who has institutional authority to define what counts as ''anomalous'' vs ''typical'' for habitability assessment, and does that authority structure create asymmetric extraction?',
    'Institutional analysis of funding allocation patterns; citation analysis of anomaly definitions in major papers; survey of early-career researcher perception of definition authority; tracking of observation-time allocation by target type',
    'If authority is distributed and transparent: constraint is pure coordination (Rope). If concentrated in elite groups with opaque criteria: constraint is mixed coordination-extraction (Tangled Rope) or pure extraction (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_capture_authority, conceptual, 'Institutional authority over anomaly definitions').

omega_variable(
    statistical_baseline_recovery_feasibility,
    'Can the Earth-analog baseline be recovered from archived observation data and synthetic population models, or has the pivot to anomaly-seeking destroyed the statistical infrastructure needed for future hypothesis testing?',
    'Reconstruction of historical Earth-analog search datasets; comparison with original published criteria; simulation of recovery feasibility under current observation strategy; assessment of data loss vs data preservation across archives',
    'If recoverable: institutional harm is reversible, and the constraint is a temporary misallocation (Scaffold). If destroyed: the constraint has inflicted permanent epistemic damage (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_baseline_recovery_feasibility, empirical, 'Feasibility of recovering Earth-analog baseline from archived data').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_habitability_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exohab_tr_t0, exoplanet_habitability_arbitrage, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exohab_tr_t5, exoplanet_habitability_arbitrage, theater_ratio, 5, 0.42).
narrative_ontology:measurement(exohab_tr_t10, exoplanet_habitability_arbitrage, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(exohab_be_t0, exoplanet_habitability_arbitrage, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(exohab_be_t5, exoplanet_habitability_arbitrage, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(exohab_be_t10, exoplanet_habitability_arbitrage, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_habitability_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(exoplanet_habitability_arbitrage, biosignature_interpretation_reliability).
narrative_ontology:affects_constraint(exoplanet_habitability_arbitrage, exoplanet_sample_selection_bias).
narrative_ontology:affects_constraint(exoplanet_habitability_arbitrage, earth_similarity_index_governance).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the empirical claim about solar system typicality (omega variable: solar_system_typicality_threshold) and upstream of biosignature interpretation reliability. The empirical status of solar system atypicality determines whether the pivot is scientifically justified; the institutional entrenchment of the pivot determines whether extraction occurs. These are structurally distinct constraints with different ε values and should be modeled separately if the empirical claim is disputed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
