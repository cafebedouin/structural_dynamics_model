% ============================================================================
% CONSTRAINT STORY: dark_matter_detection_null_results
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dark_matter_detection_null_results, []).

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
 *   constraint_id: dark_matter_detection_null_results
 *   human_readable: Dark Matter Detection Null Results and Publication Bias
 *   domain: particle_physics/experimental_verification
 *
 * SUMMARY:
 *   Dark matter detection has been a 25-year experimental program producing
 *   an escalating sequence of null results. WIMPs (Weakly Interacting Massive
 *   Particles) remain the dominant theoretical hypothesis despite no
 *   confirmed detection signals. The structural tension between the
 *   institutional commitment to WIMP searches and the empirical reality of
 *   repeated non-detections creates a constraint that exhibits tangled rope
 *   characteristics: genuine coordination value (shared detector
 *   infrastructure, cross-experiment methodology standards, collective
 *   parameter space mapping) layered onto asymmetric extraction (publication
 *   bias against null results, career risk for negative-focused researchers,
 *   funding concentration on mainstream WIMP experiments over alternative
 *   theories). The theater ratio (0.68) reflects that detector improvement
 *   cycles have become increasingly performative — each new generation of
 *   instruments claims higher sensitivity but targets progressively finer
 *   parameter space regions with diminishing probability of signal detection.
 *   The rising extractiveness over the interval (0.28 → 0.52) indicates that
 *   the publication bias and career suppression mechanisms have intensified
 *   as null results have accumulated. The constraint's classification depends
 *   critically on perspective: the null result researcher is trapped (snare),
 *   alternative theory communities are constrained within a rigged system
 *   (tangled rope), the WIMP establishment maintains institutional inertia
 *   despite contradictory data (piton), and open data initiatives are
 *   building sunset mechanisms (scaffold).
 *
 * KEY AGENTS:
 *   - Null Result Researchers: Primary victim (powerless/trapped) — specialized expertise in precision non-detection, zero career advancement from publishing negative results
 *   - Field Empirical Integrity: Primary victim (powerless/trapped) — abstract collective bearing full cost of suppressed null results; lacks mechanisms to aggregate constraints
 *   - Dark Matter Theory Establishment: Primary beneficiary (institutional/arbitrage) — maintains WIMP paradigm dominance through framing null results as parameter space constraints rather than falsifications
 *   - Detector Technology Vendors: Secondary beneficiary (institutional/arbitrage) — extract ongoing funding through detector upgrade cycles framed as responding to instrumental limitations
 *   - Alternative Dark Matter Theory Communities: Secondary victim (organized/constrained) — benefit from shared infrastructure but face asymmetric publication bias against null results on their hypotheses
 *   - WIMP Paradigm: Institutional structure (institutional/arbitrage) — persists through career incentives and funding concentration despite empirical degradation (piton)
 *   - Open Dark Matter Data Movement: Organized agents (organized/constrained) — building infrastructure to sunset the publication bias mechanism through open parameter space mapping
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent bias as immutable property of particle physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dark_matter_detection_null_results, 0.52).
domain_priors:suppression_score(dark_matter_detection_null_results, 0.58).
domain_priors:theater_ratio(dark_matter_detection_null_results, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dark_matter_detection_null_results, extractiveness, 0.52).
narrative_ontology:constraint_metric(dark_matter_detection_null_results, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dark_matter_detection_null_results, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dark_matter_detection_null_results, tangled_rope).
narrative_ontology:human_readable(dark_matter_detection_null_results, "Dark Matter Detection Null Results and Publication Bias").
narrative_ontology:topic_domain(dark_matter_detection_null_results, "particle_physics/experimental_verification").

domain_priors:requires_active_enforcement(dark_matter_detection_null_results).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dark_matter_detection_null_results, dark_matter_theory_establishment).
narrative_ontology:constraint_beneficiary(dark_matter_detection_null_results, supersymmetry_research_programs).
narrative_ontology:constraint_beneficiary(dark_matter_detection_null_results, detector_technology_vendors).
narrative_ontology:constraint_victim(dark_matter_detection_null_results, null_result_researchers).
narrative_ontology:constraint_victim(dark_matter_detection_null_results, alternative_dark_matter_theories).
narrative_ontology:constraint_victim(dark_matter_detection_null_results, field_empirical_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NULL RESULT RESEARCHER (SNARE) — Trapped by publication bias and career risk. Investment in precision null results yields no publishable narrative. Career advancement requires positive claims in high-impact journals. Exit is blocked: switching fields requires abandoning decade of specialized expertise; publishing negative results in low-tier venues carries no career weight.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD EMPIRICAL INTEGRITY (SNARE) — Abstract collective good bears the full extraction cost. Non-detection data is systematically underreported relative to marginal positive claims. The field lacks mechanisms to aggregate null results into coherent constraints on parameter space. Structural victims with no exit option and no power to organize.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALTERNATIVE DARK MATTER THEORIES (TANGLED ROPE) — Organized but constrained. Null results on WIMPs and axions could strengthen axion-like particle, sterile neutrino, or primordial black hole models, but funding and attention concentrate on mainstream WIMP searches. Benefit from coordination (shared detector infrastructure, cross-experiment methodology) but face extraction: null results on competitor hypotheses are suppressed; positive hints for alternatives are scrutinized asymmetrically.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DARK MATTER THEORY ESTABLISHMENT (ROPE) — Institutional beneficiary with arbitrage. WIMPs and supersymmetry remain conceptually attractive despite non-detections; null results are integrated as parameter space constraints within the theory's success narrative. High-profile negative results are reframed as engineering challenges (detector sensitivity, background discrimination) rather than theoretical falsifications. Experiences the constraint as coordination: publications aggregate into narrative progress.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DETECTOR TECHNOLOGY VENDORS (ROPE) — Institutional beneficiary with arbitrage. Null results drive continued detector improvement cycles. LUX → XENONnT → DARWIN demand chain is sustained by 'we need better sensitivity' narrative. Non-detections become sales pitches for next-generation instruments. Experience the constraint as pure coordination.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE WIMP PARADIGM (PITON) — Institutionally entrenched but functionally degraded. Two decades of null results have not falsified the WIMP hypothesis — each non-detection is integrated as 'we're looking in the wrong mass range' or 'WIMPs interact more weakly than expected.' The paradigm persists through funding allocation and career incentive structure rather than empirical success. Theater ratio is high: the ritual of improved detector sensitivity continues despite lack of confirming signal. Classification derives from inertial maintenance of a theory framework despite repeated null results.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN DARK MATTER DATA MOVEMENT (SCAFFOLD) — Organized agents (LHC collaborations, direct detection consortia, data aggregation initiatives) are building infrastructure to make null results publicly accessible and directly comparable across experiments. Open parameter space maps, combined sensitivity curves, and public likelihood uploads reduce incentive to bury negative results. Sunset clause: as open-data norms mature and systematic null result aggregation becomes standard, the publication bias mechanism loses extractive force. Estimated sunset: 10-15 years for norms to mature in particle physics.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From a civilizational view, some degree of positive bias toward detection is inherent to particle physics searches: experiments are designed to find signals, and negative results are technically harder to interpret (did we fail to detect, or did the particle not exist in our search space?). This perspective risks naturalizing the bias as immutable. However, the structural data (publication bias, career incentives, funding concentration) reveals this as a contingent institutional arrangement, not a law of nature. The engine's false summit detector should flag this perspective as naturalization.
constraint_indexing:constraint_classification(dark_matter_detection_null_results, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_matter_detection_null_results_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dark_matter_detection_null_results, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_matter_detection_null_results, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dark_matter_detection_null_results, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dark_matter_detection_null_results, TR),
    TR >= 0.70.

:- end_tests(dark_matter_detection_null_results_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint exhibits asymmetric benefit flow: WIMP establishment and detector vendors benefit from continued searches (career advancement, funding, instrument sales); null result researchers and alternative theory communities bear suppression costs. The extractiveness has risen from 0.28 (early 2000s, when WIMP paradigm was still empirically open) to 0.52 (current, with 25 years of null results and paradigm functioning primarily through inertia). Suppression (0.58): Moderate-high. Structural barriers to publishing null results include journal bias toward positive claims, career risk of publishing in low-tier venues, funding agency reluctance to support 'negative' programs, and institutional momentum behind WIMP searches. However, suppression is not total — null results do appear in literature, arxiv preprints bypass some journal bias, and some alternative theory funding exists. Theater ratio (0.68): High and rising. Detector upgrade cycles have become increasingly performative — each generation of XENON, LUX, SuperCDMS improves sensitivity by 1-2 orders of magnitude but targets progressively finer parameter space with lower prior probability of discovery. The theatrical function (demonstrating research momentum, justifying continued funding) has become more salient than the detection function. The rising trajectory reflects that as null results accumulate, the justification for continued searches shifts from 'we might find something' to 'we need better instruments to rule things out' — a shift from discovery narrative to constraint narrative, characteristic of piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The null result researcher trapped in the system sees extraction (snare) — their specialized work yields no career rewards and their data is suppressed. The WIMP establishment sees successful coordination — they are aggregating parameter space constraints into a coherent research narrative. The detector vendors see pure coordination (rope) — they are solving the legitimate problem of achieving higher sensitivity. Alternative theory communities see mixed coordination and extraction (tangled rope) — they benefit from shared infrastructure but face publication bias against their hypotheses. The field's empirical integrity sees pure extraction (snare) — no voice, no exit, bearing all costs. The WIMP paradigm itself sees institutional persistence (piton) — it functions through career incentive structure and funding concentration, not empirical success. The analytical observer risks seeing an immutable law (mountain) — 'detection always lags behind non-detection in particle physics searches' — but the structural data reveals this as naturalization of contingent institutional biases. This perspectival range (snare through mountain) indicates that the base_properties extractiveness (0.52) sits at a critical regime where institutional framing still dominates but empirical constraints are becoming visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position. Null result researchers are victims (high d) with trapped exit (maximum d → high f(d) → maximum experienced extraction). The WIMP establishment are beneficiaries (low d) with arbitrage exit (minimum d → negative f(d) → negative experienced extraction — they benefit). Alternative theory researchers are victims (high d) but with some organizing capacity (constrained exit → moderate d). Detector vendors are beneficiaries (low d) with strong arbitrage (exploit upgrade cycle). The open data movement is organized (moderate d) with constrained exit (they are building the sunset mechanism themselves). The field's empirical integrity is powerless/trapped, a structural victim with no agency. The analytical perspective is observational (canonical d ≈ 0.72 for analytical atom, producing moderate experienced extraction reflecting the observer's external position). The piton classification for the WIMP paradigm derives from the theater gate, not from low extractiveness — the paradigm itself is sustained by inertial institutional mechanisms despite empirical degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR INSTITUTIONAL PARADIGM INERTIA: The constraint resolves the mandatrophy by distinguishing between a hypothesis's scientific status and its institutional status. The WIMP hypothesis is scientifically degraded — 25 years of null results without detection signals — yet institutionally robust because funding, career paths, and equipment cycles are built around WIMP searches. The piton classification is the correct reading: a former rope (genuine coordination on shared dark matter detection infrastructure) that has degraded into theater (detector improvements now justify themselves through increasingly marginal parameter space constraints rather than discovery probability). The tangled rope classification for the overall constraint captures that coordination (shared infrastructure, standard methodologies) genuinely exists alongside extraction (publication bias, career suppression of null result researchers). The analytical observer's mountain is a false summit (naturalizing the bias as immutable), correctly detected by the false summit filter. The open data movement's scaffold perspective is the key forward path: by building infrastructure that makes null results directly comparable and aggregatable, the movement is creating conditions where the extraction mechanism loses force. When parameter space mapping becomes automatic and open, individual null results cease to be career-damaging and instead contribute to a collective constraint picture — the coordination function strengthens and the extraction function weakens, transitioning from tangled rope back toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    null_result_publication_threshold,
    'What publication threshold for null results balances empirical completeness against false positive accumulation?',
    'Statistical analysis of parameter space coverage vs. journal publication rates; comparison of arxiv null results against published claims to measure suppression ratio',
    'If threshold is very low: field becomes saturated with marginal non-detections. If threshold is very high: genuine constraints remain unpublished and extraction continues.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(null_result_publication_threshold, empirical, 'Publication threshold that balances null result inclusion against false positive noise').

omega_variable(
    wimp_parameter_space_exhaustion,
    'Have null results eliminated supersymmetric WIMPs as viable dark matter candidates, or does unmeasured parameter space remain that could harbor detection signals?',
    'Systematic mapping of WIMP parameter space; calculation of remaining unexplored regions; assessment of whether remaining space is theoretically motivated or artifact of search strategy limitations',
    'If parameter space is mostly exhausted: WIMP framework should be retired and alternative theories elevated. Continues tangled rope. If parameter space remains: WIMP research can claim legitimacy of continued searches. Transitions toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wimp_parameter_space_exhaustion, empirical, 'Degree of WIMP parameter space exclusion vs. remaining unmeasured regions').

omega_variable(
    alternative_theory_asymmetric_scrutiny,
    'Are null results on alternative dark matter candidates (axions, sterile neutrinos, primordial black holes) published and funded at rates equivalent to null results on WIMPs?',
    'Bibliometric analysis of publication rates; funding allocation tracking; comparison of journal impact factors for null results on mainstream vs. alternative hypotheses',
    'If asymmetry is severe: extraction is occurring through preferential suppression of competitive theories. If asymmetry is minimal: field is approaching balanced hypothesis testing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_theory_asymmetric_scrutiny, empirical, 'Asymmetry in publication and funding treatment of null results across competing dark matter theories').

omega_variable(
    detector_upgrade_cycle_necessity,
    'Is the detector upgrade cycle (LUX → XENONnT → DARWIN) driven by genuine instrumental limitations or by the need to maintain research momentum despite null results?',
    'Engineering analysis of sensitivity gains in new instruments relative to theoretical reach of existing ones; cost-benefit analysis of each upgrade cycle; assessment of whether gains target specific parameter space vs. general sensitivity improvements',
    'If driven by genuine limitations: detector upgrading is legitimate coordination. If driven by momentum maintenance: scaffold theater ratio increases, snare extractiveness increases, rope classification for vendors becomes unsustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detector_upgrade_cycle_necessity, empirical, 'Whether detector upgrade cycles respond to instrumental limitations or momentum maintenance').

omega_variable(
    dark_matter_ontological_flexibility,
    'Has the dark matter hypothesis become too flexible — capable of absorbing any non-detection without falsification — or does it retain genuine empirical constraints?',
    'Formal assessment of WIMP hypothesis falsifiability; analysis of post-hoc modifications to theoretical predictions following null results; comparison to other 20-year research programs where falsification occurred',
    'If hypothesis is unfalsifiable: WIMP paradigm should be reclassified as piton with higher theater. If constraints remain genuine: framework retains scientific integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_matter_ontological_flexibility, conceptual, 'Falsifiability and empirical constraint structure of dark matter hypothesis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dark_matter_detection_null_results, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_null_tr_t0, dark_matter_detection_null_results, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dm_null_tr_t5, dark_matter_detection_null_results, theater_ratio, 5, 0.54).
narrative_ontology:measurement(dm_null_tr_t10, dark_matter_detection_null_results, theater_ratio, 10, 0.65).
narrative_ontology:measurement(dm_null_tr_t15, dark_matter_detection_null_results, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(dm_null_be_t0, dark_matter_detection_null_results, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dm_null_be_t5, dark_matter_detection_null_results, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(dm_null_be_t10, dark_matter_detection_null_results, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(dm_null_be_t15, dark_matter_detection_null_results, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dark_matter_detection_null_results, information_standard).
narrative_ontology:affects_constraint(dark_matter_detection_null_results, wimp_parameter_space_constraints).
narrative_ontology:affects_constraint(dark_matter_detection_null_results, particle_physics_publication_bias).
narrative_ontology:affects_constraint(dark_matter_detection_null_results, alternative_dark_matter_theories).

% DUAL FORMULATION NOTE:
% Dark matter null results decompose into two structurally distinct constraints: (1) WIMP parameter space constraints (ε ≈ 0.18, mountain from analytical view — genuine scientific exclusion of parameter space), and (2) institutional bias against null result publication (ε ≈ 0.52, tangled rope — the constraint studied here). The first is empirical measurement; the second is institutional extraction layered on top of empirical measurement. Both link to alternative theory constraints which face even higher publication bias (ε ≈ 0.58, snare). This story focuses on the institutional constraint; parameter space exclusion is a separate constraint family member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dark_matter_detection_null_results, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
