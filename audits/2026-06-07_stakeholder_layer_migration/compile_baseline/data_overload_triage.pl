% ============================================================================
% CONSTRAINT STORY: data_overload_triage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_overload_triage, []).

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
 *   constraint_id: data_overload_triage
 *   human_readable: Data Overload Triage in Chinese Mass Surveillance
 *   domain: technology_governance/surveillance_studies/export_control_policy
 *
 * SUMMARY:
 *   China's mass surveillance infrastructure has evolved from targeted
 *   intelligence collection (2000s) into indiscriminate data hoarding
 *   (2010s-present), creating a structural bottleneck where collection
 *   capacity vastly exceeds processing capacity. Public Security Bureaus
 *   deploy millions of cameras, monitor all digital communications, and track
 *   physical movement through ubiquitous sensors, generating petabytes of
 *   data daily. Human analysts cannot process this volume; the original
 *   intelligence function has atrophied into performative total coverage. AI
 *   surveillance vendors (SenseTime, Megvii, Hikvision) market triage systems
 *   as the solution, but the core constraint persists: political mandates
 *   require maintaining the appearance of total awareness regardless of
 *   processing capacity. The theater ratio (0.71) reflects that most
 *   collected data is never analyzed — collection continues because stopping
 *   would signal regime weakness, not because the data serves a functional
 *   intelligence purpose. Export controls on AI accelerators (US CHIPS Act,
 *   Wassenaar Arrangement updates) attempt to exploit this bottleneck by
 *   constraining China's processing capacity, but indigenous chip development
 *   and algorithmic efficiency gains may close the gap within a decade. The
 *   constraint exhibits piton characteristics from the infrastructure's own
 *   perspective (degraded function maintained through inertia), snare
 *   characteristics from the surveilled population's perspective (trapped
 *   with no exit, bearing arbitrary enforcement), and scaffold
 *   characteristics from the export control coalition's perspective
 *   (temporary vulnerability with a sunset).
 *
 * KEY AGENTS:
 *   - Surveilled Population: Primary victim (powerless/trapped) — bears extraction through arbitrary enforcement, false positives, and pervasive monitoring with no exit option within national borders
 *   - Human Analysts: Secondary victim (moderate/constrained) — face impossible workload and moral injury from triage decisions; benefit from job security but bear cognitive overload and career risk
 *   - AI Surveillance Vendors: Primary beneficiary (institutional/arbitrage) — capture revenue from the overload crisis; can exit to commercial markets or export to other authoritarian regimes
 *   - Public Security Bureaus: Mixed actor (institutional/constrained) — benefit from expanded authority and budget but bear extraction of impossible performance standards and political theater requirements
 *   - Mass Surveillance Infrastructure: Institutional actor experiencing its own degradation (institutional/constrained) — original intelligence function atrophied into indiscriminate hoarding maintained through inertia
 *   - Export Control Coalition: Organized actors (organized/mobile) — US, EU, allied democracies implementing AI chip restrictions to exploit the processing bottleneck as a temporary constraint
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent political choice (total coverage mandates) as an information-theoretic limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_overload_triage, 0.68).
domain_priors:suppression_score(data_overload_triage, 0.82).
domain_priors:theater_ratio(data_overload_triage, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_overload_triage, extractiveness, 0.68).
narrative_ontology:constraint_metric(data_overload_triage, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(data_overload_triage, theater_ratio, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_overload_triage, piton).
narrative_ontology:human_readable(data_overload_triage, "Data Overload Triage in Chinese Mass Surveillance").
narrative_ontology:topic_domain(data_overload_triage, "technology_governance/surveillance_studies/export_control_policy").

domain_priors:requires_active_enforcement(data_overload_triage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_overload_triage, ai_surveillance_vendors).
narrative_ontology:constraint_beneficiary(data_overload_triage, public_security_bureaus).
narrative_ontology:constraint_victim(data_overload_triage, surveilled_population).
narrative_ontology:constraint_victim(data_overload_triage, human_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED POPULATION (SNARE) — Trapped within national borders with no exit from pervasive surveillance infrastructure. The data overload does not reduce extraction — it increases false positive rates and arbitrary enforcement. Maximum experienced extraction from a system that collects everything but processes selectively.
constraint_indexing:constraint_classification(data_overload_triage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HUMAN ANALYST (TANGLED ROPE) — Constrained by impossible workload and career dependency on the security apparatus. Benefits from job security in expanding surveillance bureaucracy but bears cognitive overload and moral injury from triage decisions. Mixed coordination (legitimate security work exists) and extraction (impossible standards, arbitrary metrics).
constraint_indexing:constraint_classification(data_overload_triage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: AI SURVEILLANCE VENDOR (ROPE) — Primary beneficiary with arbitrage-level exit options (can pivot to commercial markets, export to other authoritarian regimes). Experiences the constraint as pure coordination: solving the legitimate technical problem of data volume management. Net beneficiary of the overload crisis that creates demand for their product.
constraint_indexing:constraint_classification(data_overload_triage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC SECURITY BUREAU (TANGLED ROPE) — Institutional actor constrained by political mandates for total surveillance coverage and zero-failure expectations. Benefits from expanded budget and authority but bears the extraction of impossible performance standards. The bureau cannot exit the surveillance mandate but experiences both coordination (legitimate security functions) and extraction (political theater requirements).
constraint_indexing:constraint_classification(data_overload_triage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MASS SURVEILLANCE INFRASTRUCTURE (PITON) — The original function (targeted intelligence collection) has atrophied into indiscriminate data hoarding. The infrastructure persists through institutional inertia and political theater — maintaining the appearance of total awareness despite processing bottlenecks. High theater ratio: most collected data is never analyzed, but collection continues because stopping would signal weakness.
constraint_indexing:constraint_classification(data_overload_triage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EXPORT CONTROL COALITION (SCAFFOLD) — Organized actors (US, EU, allied democracies) implementing AI chip export restrictions see the triage bottleneck as a temporary vulnerability with a sunset: if China cannot access cutting-edge AI accelerators, the surveillance system's processing capacity remains constrained. The coalition experiences low extraction because they have agency and see an exit path (either export controls succeed in maintaining the gap, or they fail and the constraint dissolves).
constraint_indexing:constraint_classification(data_overload_triage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint appears as an immutable information-theoretic limit: surveillance systems always generate more data than can be processed; the signal-to-noise problem is inherent to mass collection. This perspective naturalizes the bottleneck as a law of information theory. However, the structural data contradicts this — the bottleneck is a contingent product of political mandates for total coverage combined with resource allocation choices, not an unavoidable natural law.
constraint_indexing:constraint_classification(data_overload_triage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_overload_triage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_overload_triage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_overload_triage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_overload_triage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_overload_triage, TR),
    TR >= 0.70.

:- end_tests(data_overload_triage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts substantially from the surveilled population (arbitrary enforcement, false positives, chilling effects on behavior) and from human analysts (impossible workload, moral injury). AI vendors and Public Security Bureaus benefit, but the net extraction is high because the victims are numerous and trapped while beneficiaries are concentrated and mobile. The value reflects that the overload does not reduce surveillance harm — it increases arbitrary enforcement through selective processing. Suppression (0.82): Very high. The surveilled population faces pervasive monitoring with no exit option within national borders; dissent is detected and punished; alternatives to compliance are systematically suppressed. Human analysts face career dependency and political pressure. The suppression has increased over the interval as surveillance infrastructure expanded and political controls tightened (2013 Xi consolidation, 2017 Social Credit rollout, 2020 COVID tracking normalization). Theater ratio (0.71): High. Most collected data is never analyzed — collection continues for political theater (maintaining appearance of total awareness) rather than functional intelligence. The theater has increased over the interval as collection capacity outpaced processing capacity, but has plateaued since 2019 as AI triage systems provide enough processing to maintain the performance. The plateau reflects that AI adoption has stabilized the theater at a high level rather than reducing it — the systems automate the performance rather than restoring function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — data volume exceeding processing capacity — appears as different constraint types depending on the observer's position. The surveilled population sees pure extraction (Snare) — they are trapped with no exit and bear arbitrary enforcement. Human analysts see mixed coordination and extraction (Tangled Rope) — legitimate security work exists but is overwhelmed by impossible standards and political theater. AI surveillance vendors see pure coordination (Rope) — they are solving a legitimate technical problem and capturing fair reward for their solution. Public Security Bureaus see mixed coordination and extraction (Tangled Rope) — they benefit from expanded authority but bear impossible performance mandates. The mass surveillance infrastructure sees its own degradation (Piton) — the original intelligence function has atrophied into performative data hoarding maintained through institutional inertia. The export control coalition sees a temporary problem with a sunset (Scaffold) — the processing bottleneck is a strategic vulnerability that will either persist (if controls hold) or dissolve (if China closes the gap). The analytical observer risks seeing an immutable natural law (Mountain) — the signal-to-noise problem is inherent to mass collection — but the structural data reveals this as a false summit: the bottleneck is a contingent product of political choices (total coverage mandates, resource allocation to collection over processing), not an unavoidable information-theoretic limit.
 *
 * DIRECTIONALITY LOGIC:
 *   The surveilled population is the primary victim with trapped exit options — they experience maximum effective extraction because they cannot escape the surveillance apparatus and bear the full cost of false positives and arbitrary enforcement. Human analysts are secondary victims with constrained exit options — they face high extraction (impossible workload, moral injury) but also receive some benefit (job security, career advancement within the security apparatus). Their directionality is moderate-high, reflecting mixed costs and benefits. AI surveillance vendors are primary beneficiaries with arbitrage exit options — they capture revenue from the overload crisis and can pivot to commercial markets or export to other regimes if domestic demand shifts. Their directionality is very low (near zero), reflecting that extraction flows toward them rather than away from them. Public Security Bureaus are institutional actors with constrained exit options — they benefit from expanded authority and budget but bear extraction through impossible performance standards and political theater requirements. Their directionality is moderate, reflecting the mixed coordination (legitimate security functions exist) and extraction (political mandates for total coverage regardless of capacity). The export control coalition has mobile exit options — they can maintain or abandon the restrictions based on strategic calculus, and they experience low extraction because they have agency in the constraint's operation. The analytical observer risks naturalizing the bottleneck as an information-theoretic limit when it is actually a product of political mandates for total coverage combined with resource allocation choices.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR PITON DETECTION: The constraint resolves the mandatrophy by showing that the piton classification is perspectival — it is the infrastructure's own view of its degraded function, not the only valid reading. The surveilled population's snare is their structural reality. The analyst's tangled rope is their lived experience. The vendor's rope is their genuine market position. The bureau's tangled rope reflects their institutional bind. The export control coalition's scaffold reflects their strategic intervention. The analytical observer's mountain is a false summit (naturalized political choice). The piton perspective is diagnostic because it reveals that the original function (targeted intelligence collection) has atrophied into performative total coverage — the constraint persists through institutional inertia and political theater rather than functional necessity. The high theater ratio (0.71) and the plateau in measurements since 2019 confirm the piton signature: AI adoption has stabilized the performance at a high level rather than restoring function. The constraint is maintained because stopping collection would signal regime weakness, not because the data serves its claimed intelligence purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_triage_effectiveness,
    'Do AI triage systems actually improve actionable intelligence rates, or do they merely automate bias and increase false positive volume?',
    'Comparative analysis of pre-AI vs post-AI arrest/detention rates, false positive rates, and intelligence actionability metrics across Public Security Bureaus with different AI adoption levels',
    'If effective: the constraint is genuine coordination (Rope from more perspectives) — AI solves a real technical problem. If ineffective or bias-amplifying: the constraint is extraction theater (Piton/Snare from more perspectives) — AI adoption is performance for political superiors, not functional improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_triage_effectiveness, empirical, 'Whether AI triage improves intelligence quality or amplifies bias').

omega_variable(
    export_control_sustainability,
    'Can export controls on AI accelerators maintain a meaningful processing gap, or will indigenous Chinese chip development and algorithmic efficiency gains close the gap within a decade?',
    'Longitudinal tracking of Chinese AI chip capabilities (FLOPS, memory bandwidth, power efficiency) vs surveillance processing requirements; monitoring of algorithmic efficiency improvements in triage models',
    'If export controls hold: scaffold perspective confirmed — the bottleneck is a temporary constraint with a sunset (either controls succeed or fail). If gap closes quickly: the bottleneck was never a real constraint, just a brief window of Western technological advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(export_control_sustainability, empirical, 'Whether export controls can sustain the processing bottleneck').

omega_variable(
    collection_reduction_political_feasibility,
    'Is reducing data collection volume politically feasible for the CCP, or does the surveillance apparatus''s legitimacy depend on maintaining the appearance of total awareness?',
    'Analysis of internal CCP directives, budget allocations, and political discourse around surveillance scope; comparison with historical cases of surveillance reduction in authoritarian regimes',
    'If reduction is feasible: the constraint is a coordination problem (Rope/Tangled Rope) — technical solutions exist but political will is needed. If reduction is politically impossible: the constraint is structural extraction (Snare/Piton) — the theater of total surveillance is the point, not a bug.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collection_reduction_political_feasibility, preference, 'Whether surveillance scope reduction is politically viable').

omega_variable(
    analyst_capacity_natural_limit,
    'Is the human analyst bottleneck a natural cognitive limit, or is it a product of inadequate training, poor tool design, and impossible performance standards?',
    'Comparative analysis of analyst productivity across different surveillance regimes with different training programs, tool interfaces, and performance expectations; cognitive load studies of surveillance work',
    'If natural limit: mountain perspective has merit — some processing bottleneck is inherent to human cognition. If institutional failure: the bottleneck is constructed through poor management and unrealistic mandates, not cognitive limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analyst_capacity_natural_limit, empirical, 'Whether analyst bottleneck is cognitive limit or institutional failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_overload_triage, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triage_theater_2010, data_overload_triage, theater_ratio, 0, 0.45).
narrative_ontology:measurement(triage_theater_2013, data_overload_triage, theater_ratio, 3, 0.58).
narrative_ontology:measurement(triage_theater_2016, data_overload_triage, theater_ratio, 6, 0.67).
narrative_ontology:measurement(triage_theater_2019, data_overload_triage, theater_ratio, 9, 0.71).
narrative_ontology:measurement(triage_theater_2022, data_overload_triage, theater_ratio, 12, 0.71).

% Extraction over time
narrative_ontology:measurement(triage_extract_2010, data_overload_triage, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(triage_extract_2013, data_overload_triage, base_extractiveness, 3, 0.59).
narrative_ontology:measurement(triage_extract_2016, data_overload_triage, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(triage_extract_2019, data_overload_triage, base_extractiveness, 9, 0.68).
narrative_ontology:measurement(triage_extract_2022, data_overload_triage, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(triage_suppress_2010, data_overload_triage, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(triage_suppress_2013, data_overload_triage, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(triage_suppress_2016, data_overload_triage, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(triage_suppress_2019, data_overload_triage, suppression_requirement, 9, 0.82).
narrative_ontology:measurement(triage_suppress_2022, data_overload_triage, suppression_requirement, 12, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_overload_triage, enforcement_mechanism).
narrative_ontology:affects_constraint(data_overload_triage, social_credit_system).
narrative_ontology:affects_constraint(data_overload_triage, uyghur_surveillance_xinjiang).
narrative_ontology:affects_constraint(data_overload_triage, covid_tracking_normalization).

% DUAL FORMULATION NOTE:
% The data overload triage constraint is upstream of specific surveillance applications (Social Credit, Xinjiang monitoring, COVID tracking) but represents a distinct structural bottleneck. The downstream constraints have their own extractiveness values reflecting the specific harms of each application; the triage bottleneck has its own extractiveness reflecting the career incentive asymmetries, resource allocation failures, and political theater requirements that create the processing gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_overload_triage, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
