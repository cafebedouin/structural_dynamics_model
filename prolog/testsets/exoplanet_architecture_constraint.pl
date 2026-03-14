% ============================================================================
% CONSTRAINT STORY: exoplanet_architecture_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exoplanet_architecture_constraint, []).

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
 *   constraint_id: exoplanet_architecture_constraint
 *   human_readable: Exoplanet Architecture Constraint: Kepler-Type Detection Bias and Target Selection Asymmetry
 *   domain: observational_astronomy/exoplanet_detection
 *
 * SUMMARY:
 *   The exoplanet discovery landscape exhibits a structural constraint
 *   arising from the asymmetric allocation of premium observation time
 *   combined with the path-dependent advantage of early discoveries. Major
 *   observatories (Kepler, TESS, ground-based facilities) command scarce
 *   instrument time and strategic observation priority. Within this
 *   architecture, established research teams capture disproportionate access,
 *   enabling higher discovery rates and early characterization rights.
 *   Underfunded programs and early-career researchers face constrained access
 *   to follow-up observations, reducing their ability to confirm marginal
 *   signals or pursue systematic surveys. The constraint combines genuine
 *   coordination (observation scheduling, target prioritization standards)
 *   with asymmetric extraction (allocation favoring institutional actors).
 *   The theater ratio (0.68) reflects that formal proposal review and
 *   observation allocation processes appear merit-based but are substantially
 *   performative — they ratify pre-determined preferences shaped by prior
 *   resource distribution. Meanwhile, open data catalogs and computational
 *   verification methods are building alternative pathways that could reduce
 *   reliance on scarce instrument time, suggesting a scaffold structure with
 *   potential sunset logic.
 *
 * KEY AGENTS:
 *   - Major Observatory Institution: Primary beneficiary (institutional/arbitrage) — controls premium observation slots and can allocate them strategically; captures first-discovery advantage
 *   - Established Research Team: Secondary beneficiary (institutional/constrained) — has historical proposal success and institutional affiliation; preferential access to observation time
 *   - Underfunded Research Program: Primary victim (powerless/trapped) — lacks resources for competitive proposals; cannot conduct follow-up observations; trapped in unequal resource allocation
 *   - Early-Career Researcher: Secondary victim (moderate/constrained) — faces high proposal rejection rates; limited institutional prestige for priority allocation; constrained by career vulnerability
 *   - Open Exoplanet Catalog Coalition: Organized agents (organized/constrained) — NASA Exoplanet Archive, ESA databases, community platforms providing alternative data pathways; building democratized computational verification methods
 *   - Target Selection Committee: Institutional actor (institutional/arbitrage) — manages observation queue allocation; maintains formal review process; sees own role as increasingly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing resource scarcity as inherent to astronomy rather than as a policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exoplanet_architecture_constraint, 0.38).
domain_priors:suppression_score(exoplanet_architecture_constraint, 0.52).
domain_priors:theater_ratio(exoplanet_architecture_constraint, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanet_architecture_constraint, extractiveness, 0.38).
narrative_ontology:constraint_metric(exoplanet_architecture_constraint, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(exoplanet_architecture_constraint, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exoplanet_architecture_constraint, tangled_rope).
narrative_ontology:human_readable(exoplanet_architecture_constraint, "Exoplanet Architecture Constraint: Kepler-Type Detection Bias and Target Selection Asymmetry").
narrative_ontology:topic_domain(exoplanet_architecture_constraint, "observational_astronomy/exoplanet_detection").

domain_priors:requires_active_enforcement(exoplanet_architecture_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exoplanet_architecture_constraint, high_resource_observatories).
narrative_ontology:constraint_beneficiary(exoplanet_architecture_constraint, established_research_teams).
narrative_ontology:constraint_victim(exoplanet_architecture_constraint, underfunded_programs).
narrative_ontology:constraint_victim(exoplanet_architecture_constraint, early_career_researchers).
narrative_ontology:constraint_victim(exoplanet_architecture_constraint, small_scale_surveys).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERFUNDED PROGRAM (SNARE) — Lacks access to premium observation slots, cannot conduct systematic follow-up of marginal signals, trapped by resource allocation mechanisms. Bears full cost of detection architecture bias without capacity to exit or organize alternative verification pathways. Experiences the constraint as pure extraction: observation time is allocated to high-resource teams; marginal discoveries remain unconfirmed.
constraint_indexing:constraint_classification(exoplanet_architecture_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER TEAM (TANGLED ROPE) — Constrained by limited proposal success rates and reliance on survey data, but benefits from published exoplanet catalogs and collaborative networks. Must work within observation queue hierarchy; significant extraction (unequal access to resources) paired with genuine coordination function (shared survey standards enable follow-up work).
constraint_indexing:constraint_classification(exoplanet_architecture_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR OBSERVATORY (ROPE) — Institutional beneficiary with arbitrage options (can allocate observation time strategically to maximize publication returns). Experiences the constraint as pure coordination: establishing target priority criteria enables efficient use of expensive instrumentation. Net beneficiary of resource allocation architecture — extraction flows toward this agent.
constraint_indexing:constraint_classification(exoplanet_architecture_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN CATALOG COALITION (SCAFFOLD) — Organized agents (NASA Exoplanet Archive, ESA archives, community crowdsourcing platforms) see the architecture constraint as a temporary coordination problem with a sunset clause. Distributed data standards, computational verification methods, and machine-learning-assisted analysis are building alternative pathways that bypass premium observation time bottlenecks. The coalition has agency and sees an exit strategy: democratized data access and computational methods reducing reliance on scarce instrument time.
constraint_indexing:constraint_classification(exoplanet_architecture_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TARGET SELECTION COMMITTEE (PITON) — The formal peer-review process for observation time allocation is substantially performative. Committees assess proposal quality and scientific merit, but the primary allocation mechanism is path-dependent institutional preference and prior success metrics. The process is maintained through institutional inertia despite its reduced functional role — alternative allocation mechanisms (algorithmic fairness in queue management, randomized trials, dedicated small-telescope windows) could replace it. Theater ratio reflects that formal review sessions are rituals that ratify pre-determined allocation patterns.
constraint_indexing:constraint_classification(exoplanet_architecture_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some observational asymmetry appears inherent to astronomy: scarce instrument time must be allocated somehow, and established teams with track records have legitimate advantages in resource competition. This perspective risks naturalizing what is actually a contingent institutional choice — the architecture could be designed for equal access with randomized slots, staggered observation windows, or computational augmentation of limited observations. The engine's false summit detector identifies this as naturalization of a policy choice as a law of nature.
constraint_indexing:constraint_classification(exoplanet_architecture_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanet_architecture_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exoplanet_architecture_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exoplanet_architecture_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(exoplanet_architecture_constraint, TR),
    TR >= 0.70.

:- end_tests(exoplanet_architecture_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from underfunded and early-career researchers through unequal observation access, but the extraction is not maximal because (a) published exoplanet data is openly shared, enabling secondary analysis; (b) some observation programs explicitly reserve time for small-scale surveys; and (c) computational methods are reducing the observation time required for verification. The value reflects genuine resource scarcity (scarce telescope time is real) combined with institutional preference (allocation favors established teams beyond what resource scarcity alone would require). Suppression (0.52): Moderate-high. Barriers to exit include limited alternative observation facilities for many science cases, high cost of proposal preparation competing for limited acceptance rates, and career risk of pursuing marginal signals without institutional backing. However, suppression is not total — some researchers successfully navigate the system, computational methods provide partial workarounds, and open data enables alternative research pathways. Theater ratio (0.68): High. The proposal review process appears merit-based but operates largely as a ratification of institutional preferences. Committee decisions show high correlation with prior institutional success, proposal affiliation, and resource availability. The formal review session is a ritual that confirms patterns established by prior allocation decisions. As computational and open-data methods mature, this performative element becomes more apparent — the review process' function is decreasing even as its form persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent structural experiences despite shared observable reality. The major observatory sees a coordination problem with a reasonable solution (Rope) — allocate premium time to high-quality proposals. The established team sees partial constraint (Tangled Rope) — some resource disadvantages offset by institutional advantages. The early-career researcher sees significant extraction (Tangled Rope or Snare boundary) — unequal access combined with career vulnerability. The underfunded program sees pure extraction (Snare) — no meaningful exit option, all costs, no benefits. The open science coalition sees a temporary problem with an exit route (Scaffold) — distributed data and computational methods are building alternatives. The target selection committee sees its own role degrading (Piton) — formal review persists through inertia despite reduced function. The analytical observer risks seeing an immutable feature (Mountain) — scarce telescope time means unequal allocation is inevitable — but this naturalizes what is actually a policy choice (one could allocate equally via lottery, staggered windows, or computational priority tiers). The perspectival gap reveals that the constraint is not inherent to astronomy but contingent on institutional architecture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from structural position: who benefits and who pays in this constraint. Major observatories with arbitrage options (can direct observation time to maximize outcomes) have d ≈ 0.10-0.20 (beneficiaries with exit capacity). Established teams with constrained access (face high proposal costs but have institutional resources and track record) have d ≈ 0.35-0.45 (partial beneficiaries with limited exit). Early-career researchers with trapped financial status and institutional vulnerability have d ≈ 0.70-0.80 (victims constrained by career risk). Underfunded programs with no resource alternatives have d ≈ 0.85-0.95 (full victims trapped in allocation hierarchy). The open science coalition has d ≈ 0.50-0.60 (organized agents with mixed costs and benefits, but agency to build alternatives). Computed χ values reflect this directionality spread: institutional beneficiaries experience low or negative effective extraction; trapped agents experience high effective extraction. The piton perspective's moderate d reflects institutional arbitrage options despite low functional role.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly documenting both its coordination function (scheduling complex observations, prioritizing high-impact targets) and its extraction mechanism (unequal allocation that favors institutional actors). The tangled rope classification acknowledges both: genuine coordination exists, but paired with asymmetric extraction. The theater ratio (0.68) indicates that the coordination function is weakening relative to the extraction mechanism — proposal review is increasingly performative. The measurements show theater increasing from 0.52 to 0.68 over the interval, indicating institutional inertia (the formal process persists despite declining functional role). The scaffold perspective identifies a real structural feature with sunset logic: open data catalogs and computational methods are genuinely building alternative pathways that bypass the observation time bottleneck. The mountain perspective is flagged as a false summit — the analytical observer's claim that 'scarce telescope time necessitates unequal allocation' naturalizes what is actually a policy choice. The constraint's resolution lies not in proving extraction (which the tangled rope already captures) but in recognizing that the 'inherent scarcity' framing serves institutional interests by making the extraction appear inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_bias_mechanism_identity,
    'Is the measured observational bias primarily driven by architectural resource constraints (scarce telescope time) or by scientific legitimacy differentials (established teams have better targets)?',
    'Counterfactual analysis: simulate observation queue allocation under resource-blind (randomized) vs. merit-ranked assignment; measure discovery rate parity across funding tiers',
    'If architecture-driven: constraint classifies as Tangled Rope (extractive allocation of coordinated resource). If merit-driven: constraint reflects legitimate scientific hierarchies, reducing extraction narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_bias_mechanism_identity, empirical, 'Whether observational bias is architectural or merit-based').

omega_variable(
    computational_verification_sufficiency,
    'Can machine-learning and computational methods reliably verify exoplanet signals without intensive follow-up observation, thereby bypassing the instrument time bottleneck?',
    'False positive rate comparison: ML-verified candidates vs. traditionally confirmed planets; multi-dataset cross-validation of computational methods on archival data',
    'If yes: scaffold sunset is structural (open tools genuinely democratize discovery). If no: computational methods cannot replace instrument time, and the constraint persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(computational_verification_sufficiency, empirical, 'Whether computational methods can replace expensive follow-up observation').

omega_variable(
    institutional_capture_depth,
    'To what extent is the observation architecture controlled by major observatory institutions (captured by their interests) vs. coordinated through genuinely neutral scientific consensus?',
    'Decision audit: trace allocation outcomes for proposals from major-institution vs. independent teams; analyze committee composition and funding flows; measure correlation between committee membership affiliation and proposal success rates',
    'If captured: extraction mechanism is institutional gatekeeping (Snare from underfunded perspective). If neutral: allocation reflects legitimate resource scarcity (Rope from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_depth, empirical, 'Degree of institutional capture in observation allocation').

omega_variable(
    follow_up_bias_irreversibility,
    'Are initial discoveries by major teams sufficiently well-characterized that later independent follow-up would be redundant, or does the first-mover advantage create persistent knowledge asymmetries?',
    'Publication lag analysis: time between initial discovery and subsequent characterization by independent teams; measurement quality comparison between first and subsequent observations',
    'If irreversible: first-mover creates lasting extraction (underfunded teams cannot catch up even with later access). If reversible: delayed access is constraint but not permanent disadvantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(follow_up_bias_irreversibility, empirical, 'Whether first-mover advantages in exoplanet discovery are persistent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exoplanet_architecture_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exo_arch_tr_t0, exoplanet_architecture_constraint, theater_ratio, 0, 0.52).
narrative_ontology:measurement(exo_arch_tr_t5, exoplanet_architecture_constraint, theater_ratio, 5, 0.6).
narrative_ontology:measurement(exo_arch_tr_t10, exoplanet_architecture_constraint, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(exo_arch_be_t0, exoplanet_architecture_constraint, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(exo_arch_be_t5, exoplanet_architecture_constraint, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(exo_arch_be_t10, exoplanet_architecture_constraint, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exoplanet_architecture_constraint, resource_allocation).
narrative_ontology:affects_constraint(exoplanet_architecture_constraint, exoplanet_confirmation_bias).
narrative_ontology:affects_constraint(exoplanet_architecture_constraint, habitable_zone_target_selection).

% DUAL FORMULATION NOTE:
% The exoplanet architecture constraint has upstream dependencies on specific detection methods (Kepler photometry, radial velocity follow-up) which have their own extractiveness profiles, and downstream effects on confirmation bias and target selection strategies. Each stage of the discovery pipeline has its own constraint structure; this story focuses on the resource allocation layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exoplanet_architecture_constraint, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
