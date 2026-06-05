% ============================================================================
% CONSTRAINT STORY: paleoclimate_proxy_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paleoclimate_proxy_substitution, []).

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
 *   constraint_id: paleoclimate_proxy_substitution
 *   human_readable: Paleoclimate Proxy Substitution and Data Dominance
 *   domain: paleoclimatology/Earth_science
 *
 * SUMMARY:
 *   Paleoclimate research reconstructs Earth's climate history before
 *   instrumental records (pre-1850s) using proxy data — ice cores, tree
 *   rings, corals, lake sediments, speleothems, and others. Ice cores,
 *   particularly from Antarctica and Greenland, have acquired institutional
 *   dominance in paleoclimate science since the 1970s. This dominance is
 *   justified in part by genuine measurement advantages: ice cores provide
 *   continuous, annually-resolved, multi-parameter data covering 800,000+
 *   years with direct atmospheric composition measurements. However, the
 *   constraint emerges from the gap between ice cores' genuine epistemic
 *   strength and the inflated institutional authority they exercise over
 *   alternative methods. Ice-core-dominant research groups control funding
 *   allocation, publication gatekeeping, and the definition of what counts as
 *   validation. Alternative proxy communities (tree-ring, coral,
 *   lake-sediment researchers) face systematic publication bias unless they
 *   cite ice-core reconstructions as ground truth, structural barriers to
 *   funding parity, and career risk for proposing multi-proxy integration as
 *   an alternative framework. The constraint exhibits mixed coordination and
 *   extraction: genuine coordination exists around ice-core measurement (they
 *   do solve real problems), but extraction emerges through institutional
 *   enforcement of ice-core primacy over methods that might be
 *   epistemologically equivalent or superior for specific questions. Theater
 *   has increased over the interval as ice-core authority has become more
 *   performative — ice cores are cited as validation standard not because
 *   they are always best but because institutional weight makes them appear
 *   so. The multi-proxy synthesis community recognizes the constraint as a
 *   coordination problem with a methodological solution, but faces resource
 *   barriers and citation pressure that sustain ice-core dominance. Rising
 *   suppression over the interval reflects increasing publication barriers
 *   and funding competition that make alternative approaches harder to
 *   pursue.
 *
 * KEY AGENTS:
 *   - Ice-core-dominant research groups: Primary beneficiary (institutional/arbitrage) — control funding, publication priority, citation authority; experience constraint as legitimate coordination
 *   - Alternative proxy communities: Primary victim (powerless/trapped or moderate/constrained) — face publication bias, funding scarcity, requirement to validate against ice cores; bear extraction costs without exit option
 *   - Funding agencies (NSF, DFG, NERC, NSFC): Institutional mediator (institutional/arbitrage) — may be captured by ice-core constituencies or independently assess merit; structural position determines whether they enforce or relax the constraint
 *   - Early-career paleoclimate researchers: Secondary victim (moderate/constrained) — depend on established networks and funding agencies for career advancement; constrained by requirement to work within dominant paradigm
 *   - Multi-proxy synthesis community: Organized agents (organized/constrained) — PAGES 2k, PMIP, data networks recognize integration as solution but lack resources and institutional authority to implement
 *   - Ice-core measurement technology: Institutional infrastructure (institutional/arbitrage) — genuine measurement capability underlies constraint, but epistemological authority has become inflated beyond measurement quality
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangement as immutable consequence of measurement physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paleoclimate_proxy_substitution, 0.52).
domain_priors:suppression_score(paleoclimate_proxy_substitution, 0.58).
domain_priors:theater_ratio(paleoclimate_proxy_substitution, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paleoclimate_proxy_substitution, extractiveness, 0.52).
narrative_ontology:constraint_metric(paleoclimate_proxy_substitution, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(paleoclimate_proxy_substitution, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paleoclimate_proxy_substitution, tangled_rope).
narrative_ontology:human_readable(paleoclimate_proxy_substitution, "Paleoclimate Proxy Substitution and Data Dominance").
narrative_ontology:topic_domain(paleoclimate_proxy_substitution, "paleoclimatology/Earth_science").

domain_priors:requires_active_enforcement(paleoclimate_proxy_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paleoclimate_proxy_substitution, ice_core_dominant_groups).
narrative_ontology:constraint_beneficiary(paleoclimate_proxy_substitution, established_funding_agencies).
narrative_ontology:constraint_victim(paleoclimate_proxy_substitution, alternative_proxy_communities).
narrative_ontology:constraint_victim(paleoclimate_proxy_substitution, paleoclimate_epistemology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE PROXY COMMUNITY (SNARE) — Trapped by publication bias, funding concentration, and the requirement to cite ice-core reconstructions as ground truth even when their own methods (tree rings, corals, lake sediments, speleothems) are theoretically superior or complementary. Cannot exit without abandoning their research agenda. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER PALEOCLIMATE RESEARCHERS (TANGLED ROPE) — Constrained by career dependence on established citation networks and funding agencies. However, benefit from access to institutional data repositories, collaborative networks, and training in standard methods. Significant extraction but also genuine access to research infrastructure — asymmetric but not totalizing.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ICE-CORE-DOMINANT RESEARCH GROUPS (ROPE) — Experience the constraint as legitimate coordination: ice cores provide continuous, annually-resolved, globally-representative climate data. The institutional position allows arbitrage — can leverage funding, publication priority, and data access privileges. Net beneficiary but with genuine coordination function (ice cores do solve a real measurement problem).
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTI-PROXY SYNTHESIS COMMUNITY (TANGLED ROPE) — Organized agents (paleoclimate data synthesis networks, PAGES 2k, PMIP, climate modeling consortia) recognize ice-core dominance as a coordination problem with a methodological solution: integrating multiple proxies with explicit error models and uncertainty propagation. They benefit from institutional coordination (standardized data formats, shared repositories) while bearing extraction costs (funding dependence on established agencies, publication pressure to cite ice cores as validation standard).
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ICE-CORE MEASUREMENT TECHNOLOGY (PITON) — The physical capability of ice cores (continuous records, isotopic measurement, trapped air analysis) is genuine and irreplaceable. However, the epistemic authority granted to ice-core reconstructions has become partially performative — they are cited as the validation standard not because they are always superior but because they carry institutional weight. Theater_ratio reflects the gap between the genuine measurement capability and the inflated epistemological authority. Technology persists through institutional inertia while alternatives that might provide superior or complementary data languish.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, ice cores represent the closest approximation to direct climate measurement available for pre-instrumental periods. Proxy substitution (using imperfect alternatives) is inherently limited — you cannot overcome the fundamental epistemological constraint that all paleoclimate data are indirect inferences. This perspective naturalizes the ice-core dominance as a consequence of measurement physics rather than institutional arrangement. The engine's false summit detector will identify this as naturalization of what is actually a contingent institutional configuration.
constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paleoclimate_proxy_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(paleoclimate_proxy_substitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(paleoclimate_proxy_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(paleoclimate_proxy_substitution, TR),
    TR >= 0.70.

:- end_tests(paleoclimate_proxy_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The ice-core research constituency captures substantial career, funding, and publication advantages during the interval. However, extraction is not total because (a) ice cores do provide genuine measurement value that justifies some preference, and (b) alternative proxy researchers can still publish and advance careers, albeit with barriers. The reduced value from earlier estimates reflects the reality that coordination function is not negligible. Suppression (0.58): Moderate-high. Significant barriers exist to alternative proxy research: publication bias against findings that contradict ice-core reconstructions, funding concentration on ice-core programs, career risk of challenging dominance, lack of access to established data repositories for non-ice-core proxies, and requirement to position alternative work as complementary rather than competitive. But suppression is not total — alternative proxies are published, funded at lower levels, and cited; the constraint is not absolute. Theater ratio (0.64): Moderate-high. Ice-core citations occur partly because of measurement quality (justified), partly because of institutional authority and path dependence (performative), and partly because alternatives' complexity creates uncertainty that makes the simplicity of ice-core narrative appealing (theater masquerading as clarity). The theater has increased over the interval because the dominance has become more institutional and less scientifically grounded as alternative proxy methods have matured. Rising measurements reflect institutional entrenchment: as alternative methods improved, suppression had to increase to maintain dominance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the gap between genuine coordination (ice cores do solve a real measurement problem) and institutional extraction (ice-core dominance is enforced through funding and publication mechanisms that exceed what measurement quality justifies). The ice-core research group experiences Rope — they coordinate scientific understanding and communicate findings. The alternative proxy community experiences Snare — they cannot exit without abandoning their research agenda. Early-career researchers experience Tangled Rope — constrained by institutional dependence but also benefiting from access to infrastructure. The multi-proxy synthesis community experiences Tangled Rope at a higher power level — they have agency and see a methodological solution but lack resources. The ice-core technology itself represents Piton — genuine measurement capability undergirds the constraint, but the epistemic authority has become ritualistic. The analytical observer risks Mountain — naturalizing the dominance as immutable consequence of physics — but the structural data reveals this as false summit: the constraint is institutional, not physical. The perspectival gap reveals how a legitimate coordination mechanism has been captured by institutional arrangements that exceed the justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. Ice-core-dominant groups benefit from funding priority and publication authority — low d (0.10-0.15), resulting in negative χ (net beneficiary position). Alternative proxy researchers bear costs of publication bias and funding scarcity — high d (0.75-0.85) combined with powerless/trapped exit options, resulting in high χ (maximum experienced extraction). Early-career researchers occupy constrained middle ground — moderate d (0.55-0.65) reflecting mixed costs and benefits. Funding agencies occupy institutional position with arbitrage options — low d baseline (0.15), but directionality_override may be warranted if capture analysis shows they actively maintain the constraint rather than neutrally assess merit (would override to d=0.35-0.40, increasing institutional extraction responsibility). The multi-proxy synthesis community has organized exit (they can form consortia and pursue alternative frameworks) — lower d (0.40-0.50) than powerless victims, resulting in moderate χ. The pivot between Rope (beneficiary experience) and Snare (victim experience) occurs at d ≈ 0.50, which is crossed when moving from institutional/arbitrage agents to powerless/trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through perspectival pluralism: ice-core dominance is legitimate Rope from the perspective of ice-core researchers (genuine coordination function), but legitimate Snare from the perspective of alternative proxy communities (institutional extraction). Both perspectives are correct within their structural context. The presheaf over observer positions produces all six types: Snare (powerless victims), Tangled Rope (moderate constrained researchers, organized synthesis community), Rope (institutional beneficiaries), Piton (degraded but persistent ice-core authority), and Mountain (risked false naturalization). No single type is 'correct' — the full classification is the presheaf. The constraint demonstrates that mandatrophy in interdisciplinary science emerges from unequal power positions accessing the same institutional mechanisms. Ice-core dominance is not extracted extraction (it has genuine value), but extracted dominance (the institutional authority exceeds what measurement quality justifies). The resolution is to separate the measurement problem (ice cores do solve genuine problems, Rope justified) from the authority problem (institutional dominance is institutional, Tangled Rope) and address them independently: (a) maintain ice-core research programs because they produce valuable data (Rope), (b) equalize funding and publication access for alternative proxies to enable genuine multi-proxy integration (Tangled Rope mitigation), and (c) explicitly distinguish between 'ice cores are superior for specific measurements' (true, empirical) and 'ice cores are the validation standard for all paleoclimate reconstruction' (false, institutional).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_complementarity_vs_substitution,
    'Are alternative proxies genuine substitutes for ice-core data, or are they fundamentally complementary methods addressing different climate aspects?',
    'Systematic comparison of what each proxy actually measures: ice cores capture atmospheric temperature/composition; tree rings capture growing-season precipitation/temperature; corals capture sea surface temperature and ocean dynamics; lake sediments capture regional precipitation and vegetation change. Clarify whether dominance reflects measurement incomparability (ice cores measure something unique) or institutional preference for a particular method.',
    'If genuinely complementary: constraint reclassifies toward Tangled Rope (all perspectives) because the coordination problem becomes genuine integration of diverse data streams. If substitutable: constraint remains Snare for alternative communities because ice-core dominance reflects institutional preference rather than measurement necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proxy_complementarity_vs_substitution, empirical, 'Whether alternative proxies are substitutes or complements to ice-core data').

omega_variable(
    ice_core_spatial_representativeness,
    'Do Antarctic and Greenland ice cores actually represent global climate as effectively as claimed, or do they capture polar-specific dynamics with uncertain extrapolation to tropical and mid-latitude regions?',
    'Comparison of ice-core-based climate reconstructions with independent regional proxies (tree rings, corals, speleothems) in tropical and mid-latitude regions; assessment of correlation vs divergence; analysis of whether ice-core reconstructions systematically overestimate or underestimate regional climate variability.',
    'If ice cores systematically overestimate global representativeness: extraction magnitude increases (institutional authority is not justified by measurement capability) and alternative proxies gain epistemological ground. If ice cores prove superior for global patterns: ice-core dominance reflects genuine measurement advantage, Rope classification more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ice_core_spatial_representativeness, empirical, 'Whether ice-core data truly represent global climate patterns').

omega_variable(
    funding_concentration_causality,
    'Does funding concentration on ice-core research cause dominance, or does dominance arise from ice cores'' genuine epistemological superiority, with funding following superior science?',
    'Historical analysis of funding allocation decisions: are funding agencies directing resources to ice cores because of pre-existing dominance (institutional path dependence) or because of genuine quality assessment? Comparison of research output quality (citation rates, predictive skill, hypothesis confirmation) across proxy types when controlling for funding level. Track whether alternative proxies become dominant when funding equalizes.',
    'If causality runs funding → dominance: extraction mechanism is institutional (Snare/Tangled Rope). If causality runs quality → dominance → funding: dominance reflects genuine measurement advantage (Rope or Mountain). If bidirectional reinforcement: Tangled Rope persists because genuine coordination value is entangled with institutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_concentration_causality, empirical, 'Whether funding concentration causes or follows ice-core dominance').

omega_variable(
    false_summit_natural_law,
    'Is ice-core dominance a natural consequence of measurement physics (inevitable given current technology), or a contingent institutional arrangement that could be reorganized?',
    'Thought experiment: if funding were equalized and publication bias reversed, could alternative proxies achieve parity or dominance? Are there theoretical or technological barriers to multi-proxy integration, or only institutional ones? Assessment of whether the constraint could be dissolved by changing incentive structures without violating physical laws.',
    'If natural (immutable physics): Mountain classification justified, no FSM trigger. If institutional (mutable arrangement): FSM fires — constraint reclassifies toward Tangled Rope, revealing false summit. This omega determines whether the analytical observer''s naturalization is accurate or defensive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether dominance is natural law or contingent institutional arrangement').

omega_variable(
    alternative_proxy_quality_parity,
    'Do alternative proxies actually match ice-core measurement quality and temporal resolution, or is the quality gap itself a legitimate reason for hierarchical citation?',
    'Standardized benchmarking: compare measurement precision (instrumental calibration error), temporal resolution (annual vs decadal vs centennial), sampling density, and uncertainty quantification across ice cores, tree rings, corals, speleothems, and lake sediments. Assess whether quality differences justify the current hierarchy or whether perceived differences reflect historical familiarity bias.',
    'If ice cores genuinely superior: Rope/Mountain classification justified. If quality parity with different error profiles: constraint reclassifies toward Tangled Rope (genuine coordination problem of integrating multiple sources rather than ranking them). If alternative proxies superior for some climate aspects: institutional hierarchy becomes clearly extractive (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_proxy_quality_parity, empirical, 'Whether ice cores have justified quality superiority').

omega_variable(
    inter_institutional_capture,
    'Are funding agencies (NSF, DFG, NERC, NSFC) captured by ice-core research constituencies, or do they independently assess research merit and happen to fund ice cores at higher rates?',
    'Analysis of program officer backgrounds (do they come from ice-core traditions?), review panel composition (are ice-core researchers overrepresented?), funding decision rationales (do proposals mention ice-core validation as criterion?), and temporal patterns (did funding shifts lag or precede scientific advances?). Track whether funding agencies have explicitly considered multi-proxy integration as a strategic priority.',
    'If captured: directionality override for institutional agency perspective — d should increase toward 0.4-0.5 (from ~0.15 beneficiary baseline) to reflect institutional entrenchment. If independent assessment: d remains at beneficiary baseline, constraining classification to Rope. Capture would upgrade constraint to higher-χ Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_institutional_capture, empirical, 'Whether funding agencies are captured by ice-core constituencies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paleoclimate_proxy_substitution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paleoproxy_tr_t0, paleoclimate_proxy_substitution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(paleoproxy_tr_t10, paleoclimate_proxy_substitution, theater_ratio, 10, 0.55).
narrative_ontology:measurement(paleoproxy_tr_t20, paleoclimate_proxy_substitution, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(paleoproxy_be_t0, paleoclimate_proxy_substitution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(paleoproxy_be_t10, paleoclimate_proxy_substitution, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(paleoproxy_be_t20, paleoclimate_proxy_substitution, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(paleoproxy_su_t0, paleoclimate_proxy_substitution, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(paleoproxy_su_t10, paleoclimate_proxy_substitution, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(paleoproxy_su_t20, paleoclimate_proxy_substitution, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paleoclimate_proxy_substitution, information_standard).
narrative_ontology:affects_constraint(paleoclimate_proxy_substitution, paleoclimate_data_integration).
narrative_ontology:affects_constraint(paleoclimate_proxy_substitution, climate_model_validation_bottleneck).
narrative_ontology:affects_constraint(paleoclimate_proxy_substitution, tropical_paleoclimate_epistemology).

% DUAL FORMULATION NOTE:
% Paleoclimate proxy substitution is the parent constraint affecting multiple downstream domains. The constraint decomposes into at least three structurally distinct stories: (1) ice-core institutional dominance (this file) — ε≈0.52, institutional arrangement, (2) measurement complementarity problem (affects_constraints edge) — whether proxies are substitutable or complementary affects whether dominance is justified, (3) tropical paleoclimate epistemology — ice-core authority is least justified in tropics where alternative proxies (corals, speleothems, lake sediments) have greater relative strength, creating regional variation in extraction intensity. Write separate stories for regional variation if constraint's ε differs substantially across geographical domains. Do NOT merge them; link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paleoclimate_proxy_substitution, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
