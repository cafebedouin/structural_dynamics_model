% ============================================================================
% CONSTRAINT STORY: collapse_mechanism_ambiguity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collapse_mechanism_ambiguity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: collapse_mechanism_ambiguity
 *   human_readable: Collapse Mechanism Ambiguity in System Dynamics Models
 *   domain: system_dynamics/ecological_economics/computational_modeling
 *
 * SUMMARY:
 *   World3 and similar system dynamics models project societal collapse under
 *   business-as-usual scenarios, but the models cannot uniquely identify
 *   whether collapse is driven by physical resource depletion, climate change
 *   impacts on agricultural and industrial productivity, or socio-economic
 *   system dysfunction (inequality, institutional failure, feedback delays).
 *   The pollution sector shows the poorest empirical fit (NRMSD 0.3371),
 *   suggesting either missing climate physics or incorrect functional forms.
 *   The resource sector fits better (NRMSD 0.7571) but still leaves
 *   substantial uncertainty. Death rate and HDI decline timing in projections
 *   could reflect any combination of mechanisms. This ambiguity is claimed as
 *   a mountain — an irreducible property of modeling complex adaptive systems
 *   with multiple interacting nonlinear feedbacks and limited historical
 *   validation data. However, the constraint declares beneficiaries (policy
 *   advocates who can claim any mechanism supports their preferred
 *   intervention; modeling groups who avoid costly validation work),
 *   triggering false summit evaluation. The measurements show slight
 *   extraction accumulation (0.05 → 0.08) as the ambiguity becomes more
 *   consequential for policy, and minimal theater increase (0.03 → 0.05) as
 *   mechanism attribution debates remain substantive rather than
 *   performative.
 *
 * KEY AGENTS:
 *   - Policy Consumers: Powerless/trapped — cannot distinguish mechanisms from model outputs; experience ambiguity as irreducible
 *   - Independent Researchers: Moderate/constrained — face structural barriers (coupled equations, sparse data, confounded observations) that persist despite effort
 *   - Modeling Consortia: Institutional/mobile — have resources for decomposition but still face irreducible ambiguity from system properties
 *   - Policy Advocates with Preferred Interventions: Institutional/arbitrage — benefit from ambiguity allowing multiple mechanism interpretations to support preferred policies
 *   - Established Modeling Groups: Institutional/mobile — benefit from ambiguity reducing pressure for costly empirical validation and model restructuring
 *   - Analytical Observer: Analytical/analytical — sees ambiguity as structural property of complex system modeling with limited validation data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collapse_mechanism_ambiguity, 0.08).
domain_priors:suppression_score(collapse_mechanism_ambiguity, 0.12).
domain_priors:theater_ratio(collapse_mechanism_ambiguity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, extractiveness, 0.08).
narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, resistance, 0.09).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collapse_mechanism_ambiguity, mountain).
narrative_ontology:human_readable(collapse_mechanism_ambiguity, "Collapse Mechanism Ambiguity in System Dynamics Models").
narrative_ontology:topic_domain(collapse_mechanism_ambiguity, "system_dynamics/ecological_economics/computational_modeling").

domain_priors:emerges_naturally(collapse_mechanism_ambiguity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collapse_mechanism_ambiguity, policy_advocates_with_preferred_interventions).
narrative_ontology:constraint_beneficiary(collapse_mechanism_ambiguity, modeling_groups_with_established_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLICY CONSUMER (MOUNTAIN) — Policymakers and public audiences consuming model outputs cannot distinguish collapse mechanisms from the aggregated projections. The ambiguity is experienced as an irreducible feature of complex system modeling — no amount of effort at this position reveals which mechanism dominates.
constraint_indexing:constraint_classification(collapse_mechanism_ambiguity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT RESEARCHER (MOUNTAIN) — Researchers attempting to decompose collapse mechanisms face structural barriers: model equations are coupled, historical data is sparse for counterfactual scenarios, and empirical climate impacts are confounded with economic responses. Even with biographical effort and moderate resources, the ambiguity persists as a property of the system being modeled.
constraint_indexing:constraint_classification(collapse_mechanism_ambiguity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MODELING CONSORTIUM (MOUNTAIN) — Institutional actors with resources for ensemble modeling, sensitivity analysis, and empirical validation still face irreducible ambiguity. The pollution sector's poor fit (NRMSD 0.3371) could reflect missing climate feedbacks, incorrect functional forms, or socio-economic dynamics not captured. Generational effort and institutional resources reduce but do not eliminate the ambiguity — it is a property of modeling complex adaptive systems with limited historical analogs.
constraint_indexing:constraint_classification(collapse_mechanism_ambiguity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the analytical position, collapse mechanism ambiguity is a structural feature of modeling systems with multiple interacting nonlinear feedbacks and limited empirical constraints. The ambiguity is not a failure of current models but a property of the epistemic situation: we have one historical trajectory, multiple plausible mechanisms, and insufficient data to fully decompose their contributions. This is the claimed natural law: complex system models with coupled dynamics and sparse validation data cannot uniquely identify collapse mechanisms.
constraint_indexing:constraint_classification(collapse_mechanism_ambiguity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collapse_mechanism_ambiguity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(collapse_mechanism_ambiguity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collapse_mechanism_ambiguity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, ExtMetricName, E),
    domain_priors:suppression_score(collapse_mechanism_ambiguity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(collapse_mechanism_ambiguity),
    narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(collapse_mechanism_ambiguity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(collapse_mechanism_ambiguity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The ambiguity itself extracts minimally — it is primarily an epistemic limit rather than a rent-seeking mechanism. The slight extraction comes from beneficiaries (policy advocates, modeling groups) who can exploit the ambiguity, but this is a secondary effect. Most agents experience the ambiguity as a genuine constraint on knowledge rather than as extraction. Suppression (0.12): Very low. There are no significant barriers preventing researchers from attempting mechanism decomposition — the ambiguity persists despite these attempts, which is characteristic of a natural limit rather than a suppressed alternative. The slight suppression reflects resource requirements for validation work and some institutional inertia in modeling frameworks. Theater ratio (0.05): Very low. Mechanism attribution debates are substantive — researchers genuinely attempt decomposition through sensitivity analysis, counterfactual scenarios, and empirical validation. The small theater component reflects some performative complexity in model presentations, but the core activity is functional. Accessibility collapse (0.88): Very high. Once the structural properties of complex system modeling are understood (coupled dynamics, sparse validation data, multiple plausible mechanisms), alternative framings collapse — the ambiguity is recognized as inherent rather than resolvable through better modeling alone. Resistance (0.09): Very low. The constraint meets minimal resistance because it is widely recognized as a genuine epistemic challenge rather than an imposed limitation. The slight resistance comes from researchers who believe better data or methods could resolve the ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as mountain, which is unusual and diagnostic. The uniformity reflects that collapse mechanism ambiguity is experienced as an irreducible limit across all structural positions — powerless policy consumers, moderate independent researchers, institutional modeling consortia, and analytical observers all encounter the same epistemic barrier. The perspectival gap is not in classification type but in the confidence of the mountain claim: the analytical observer sees the ambiguity as a deep structural property of complex system modeling; institutional actors see it as a practical limit that might be reduced with better methods; moderate researchers see it as a barrier they cannot overcome with available resources; powerless consumers see it as an opaque feature of model outputs. The false summit detector will evaluate whether the declared beneficiaries and slight extraction accumulation indicate that the mountain is partly constructed rather than fully natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy advocates with preferred interventions and established modeling groups are declared beneficiaries because they gain strategic flexibility from mechanism ambiguity — any mechanism interpretation can be invoked to support preferred policies, and ambiguity reduces pressure for costly validation work. However, these benefits are modest compared to the genuine epistemic constraint the ambiguity represents. The engine derives low directionality values for these beneficiaries (they experience the constraint as coordination or mild extraction rather than as pure extraction). No victims are declared because the ambiguity does not systematically extract from any agent — it is primarily an epistemic limit that all agents face. Policy consumers and independent researchers experience the ambiguity as a barrier to knowledge, but this is the constraint operating as a natural limit rather than as extraction. The measurements show slight extraction accumulation as the ambiguity becomes more consequential for climate policy, but the trajectory remains consistent with a natural limit that becomes more salient rather than with an extractive mechanism intensifying.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing between epistemic limits (genuine mountains) and strategic ambiguity (constructed constraints that benefit identifiable agents). Collapse mechanism ambiguity is claimed as mountain because it arises from structural properties of complex system modeling: multiple interacting nonlinear feedbacks, limited historical validation data, and observational equivalence of different mechanisms. However, the declaration of beneficiaries (policy advocates, modeling groups) and the slight extraction accumulation (0.05 → 0.08) create an irreducible uncertainty: is the ambiguity entirely natural, or is it partly constructed by modeling choices that serve beneficiary interests? The omega variables document this uncertainty and specify what evidence would resolve it: if empirical climate data resolves the ambiguity, the mountain claim weakens; if beneficiaries influence modeling choices to preserve ambiguity, the constraint is a false summit; if counterfactual scenarios produce distinguishable signatures, the ambiguity is temporary (scaffold) rather than permanent (mountain). The mandatrophy is resolved by making the natural-vs-constructed question explicit and measurable rather than assuming the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is collapse mechanism ambiguity an irreducible property of complex system modeling, or is it constructed by modeling choices that could be resolved with better data, alternative model structures, or decomposition methods?',
    'Comparison of mechanism identification success across modeling paradigms (agent-based vs system dynamics vs integrated assessment); analysis of whether empirical climate impact data (when incorporated) resolves the ambiguity or merely shifts it; assessment of whether the ambiguity persists in models with richer validation data (e.g., regional models with more granular observations)',
    'If irreducible: mountain classification confirmed — the ambiguity is a natural limit of modeling complex systems. If constructed: the constraint is tangled_rope or snare — modeling conventions and data choices create the ambiguity, and identifiable groups benefit from leaving it unresolved (policy advocates can claim any mechanism supports their preferred intervention; modeling groups avoid costly validation work).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, empirical, 'Whether mechanism ambiguity is natural law or constructed by modeling choices').

omega_variable(
    beneficiary_influence_on_ambiguity,
    'Do policy advocates and established modeling groups actively benefit from mechanism ambiguity remaining unresolved, and does this benefit influence modeling choices that perpetuate the ambiguity?',
    'Analysis of modeling choice patterns: do groups with preferred policy interventions systematically avoid decomposition methods that would resolve mechanism ambiguity? Do established modeling frameworks resist incorporating empirical climate data that would constrain mechanism attribution? Comparison of mechanism resolution effort in academic vs policy-advocacy contexts.',
    'If beneficiaries influence modeling choices: the mountain classification is a false summit — the ambiguity is partly constructed to serve beneficiary interests. If no influence detected: mountain classification stands — the ambiguity persists despite efforts to resolve it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_influence_on_ambiguity, empirical, 'Whether beneficiaries influence modeling choices to preserve ambiguity').

omega_variable(
    empirical_climate_data_sufficiency,
    'Would incorporating empirical climate impact data (crop yield responses, mortality-temperature relationships, infrastructure damage functions) into World3-style models resolve collapse mechanism ambiguity, or would it merely shift the ambiguity to different parameters?',
    'Empirical test: augment World3 pollution sector with climate impact functions calibrated to observational data; compare mechanism attribution before and after augmentation; assess whether pollution sector fit improves and whether resource vs climate vs socio-economic mechanisms become distinguishable',
    'If climate data resolves ambiguity: the current ambiguity is a data problem, not a structural limit — mountain classification weakens. If ambiguity persists: supports mountain classification — the ambiguity is deeper than missing climate physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_climate_data_sufficiency, empirical, 'Whether empirical climate data resolves mechanism ambiguity').

omega_variable(
    counterfactual_scenario_distinguishability,
    'Can counterfactual scenarios (e.g., resource abundance + climate stress vs resource scarcity + climate stability) produce distinguishable collapse signatures that would allow mechanism identification from future observations?',
    'Scenario analysis: generate ensemble of World3 runs with mechanism-specific perturbations; identify observable signatures that distinguish mechanisms (e.g., death rate vs HDI decline timing, sectoral collapse ordering); assess whether these signatures are robust to parameter uncertainty',
    'If distinguishable: mechanism ambiguity is resolvable in principle — mountain classification weakens to scaffold (temporary ambiguity pending future data). If indistinguishable: supports mountain — the mechanisms produce observationally equivalent collapse trajectories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_scenario_distinguishability, empirical, 'Whether counterfactual scenarios produce distinguishable collapse signatures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collapse_mechanism_ambiguity, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(collapse_mech_theater_1970, collapse_mechanism_ambiguity, theater_ratio, 0, 0.03).
narrative_ontology:measurement(collapse_mech_theater_1985, collapse_mechanism_ambiguity, theater_ratio, 15, 0.04).
narrative_ontology:measurement(collapse_mech_theater_2000, collapse_mechanism_ambiguity, theater_ratio, 30, 0.04).
narrative_ontology:measurement(collapse_mech_theater_2015, collapse_mechanism_ambiguity, theater_ratio, 45, 0.05).
narrative_ontology:measurement(collapse_mech_theater_2024, collapse_mechanism_ambiguity, theater_ratio, 54, 0.05).

% Extraction over time
narrative_ontology:measurement(collapse_mech_extract_1970, collapse_mechanism_ambiguity, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(collapse_mech_extract_1985, collapse_mechanism_ambiguity, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(collapse_mech_extract_2000, collapse_mechanism_ambiguity, base_extractiveness, 30, 0.07).
narrative_ontology:measurement(collapse_mech_extract_2015, collapse_mechanism_ambiguity, base_extractiveness, 45, 0.08).
narrative_ontology:measurement(collapse_mech_extract_2024, collapse_mechanism_ambiguity, base_extractiveness, 54, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(collapse_mech_suppress_1970, collapse_mechanism_ambiguity, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(collapse_mech_suppress_1985, collapse_mechanism_ambiguity, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(collapse_mech_suppress_2000, collapse_mechanism_ambiguity, suppression_requirement, 30, 0.11).
narrative_ontology:measurement(collapse_mech_suppress_2015, collapse_mechanism_ambiguity, suppression_requirement, 45, 0.12).
narrative_ontology:measurement(collapse_mech_suppress_2024, collapse_mechanism_ambiguity, suppression_requirement, 54, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collapse_mechanism_ambiguity, information_standard).

% DUAL FORMULATION NOTE:
% Collapse mechanism ambiguity is downstream of proxy_measurement_validity (tangled_rope) and optimization_artifact_risk (snare). The upstream constraints create conditions where mechanism ambiguity persists: proxy measurements (GDP, pollution indices) may not capture the mechanisms they claim to represent, and optimization artifacts in model calibration may fit historical data without identifying true mechanisms. The ambiguity is a distinct constraint with its own structural properties — it would persist even if upstream constraints were resolved, because it arises from fundamental properties of complex system modeling with limited validation data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
