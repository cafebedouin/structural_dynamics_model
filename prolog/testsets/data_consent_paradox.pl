% ============================================================================
% CONSTRAINT STORY: data_consent_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_consent_paradox, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: data_consent_paradox
 *   human_readable: Data Consent Paradox in AI-Guided Healthcare Precision Medicine
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The data consent paradox in AI-guided healthcare precision medicine
 *   (AIGHP) creates a structural tension between the technical requirement
 *   for population-scale genomic datasets and the ethical/legal requirement
 *   for meaningful individual consent. AIGHP systems require diverse,
 *   large-scale genomic data to achieve clinical accuracy and avoid
 *   algorithmic bias, but the complexity of genomic data — its inferential
 *   power, re-identification risks, and unbounded secondary uses — makes
 *   truly informed consent increasingly impossible as the technology
 *   advances. This constraint exhibits tangled rope structure from the
 *   analytical perspective: genuine coordination need (population health
 *   improvement through better models) coexists with structural extraction
 *   (consent theater legitimizes data collection while obscuring power
 *   asymmetries and profit concentration). The constraint's theater ratio
 *   (0.48) reflects that consent mechanisms have become increasingly
 *   performative as genomic data uses have grown more complex: consent forms
 *   lengthen, legal language proliferates, but actual subject comprehension
 *   declines. The suppression trajectory (0.45 → 0.62 over 16 years) tracks
 *   the intensification of structural barriers to meaningful refusal:
 *   treatment access increasingly tied to data sharing, insurance coverage
 *   conditioned on genomic testing participation, clinical trial enrollment
 *   requiring broad data use permissions. The constraint is downstream of
 *   scientific_viability_uncertainty (the mountain constraint establishing
 *   that AIGHP requires population-scale data) but adds the consent layer
 *   that transforms a technical requirement into an extractive mechanism.
 *
 * KEY AGENTS:
 *   - Coerced Data Subject: Primary victim (powerless/trapped) — faces treatment access conditioned on genomic data sharing; immediate survival needs override privacy autonomy
 *   - Informed Refuser: Secondary victim (moderate/constrained) — has capacity to refuse but faces real costs: exclusion from cutting-edge care, contribution to dataset bias
 *   - Underrepresented Population: Collective victim (moderate/constrained) — dual bind: participation enables exploitation, non-participation perpetuates algorithmic bias harming descendants
 *   - AIGHP Developer: Primary beneficiary (institutional/arbitrage) — experiences constraint as pure coordination; benefits from data access; can arbitrage across jurisdictions
 *   - Healthcare Systems: Secondary beneficiary (institutional/constrained) — revenue increasingly tied to data partnerships; institutional pressure to maximize consent rates
 *   - Future Patients: Diffuse beneficiary (powerless/trapped at present, but beneficiaries in future state) — will benefit from improved models if data collection succeeds, but have no voice in present consent decisions
 *   - Privacy Rights Coalition: Organized agents (organized/mobile) — building alternative pathways via privacy-preserving ML; see constraint as temporary with technical sunset
 *   - Hospital Ethics Board: Institutional mediator (institutional/constrained) — balances patient autonomy against research imperatives; faces revenue pressure from data partnerships
 *   - Data Protection Authority: Regulatory actor (institutional/mobile) — enforces consent frameworks; experiences both coordination (consent enables trust) and extraction (consent theater obscures power asymmetries)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_consent_paradox, 0.58).
domain_priors:suppression_score(data_consent_paradox, 0.62).
domain_priors:theater_ratio(data_consent_paradox, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_consent_paradox, extractiveness, 0.58).
narrative_ontology:constraint_metric(data_consent_paradox, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(data_consent_paradox, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_consent_paradox, tangled_rope).
narrative_ontology:human_readable(data_consent_paradox, "Data Consent Paradox in AI-Guided Healthcare Precision Medicine").
narrative_ontology:topic_domain(data_consent_paradox, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:requires_active_enforcement(data_consent_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_consent_paradox, aighp_developers_requiring_data).
narrative_ontology:constraint_beneficiary(data_consent_paradox, healthcare_systems_adopting_aighp).
narrative_ontology:constraint_beneficiary(data_consent_paradox, future_patients_benefiting_from_improved_models).
narrative_ontology:constraint_victim(data_consent_paradox, data_subjects_privacy_autonomy).
narrative_ontology:constraint_victim(data_consent_paradox, underrepresented_populations_excluded_by_consent_barriers).
narrative_ontology:constraint_victim(data_consent_paradox, current_patients_facing_consent_pressure).
narrative_ontology:constraint_vindicates(data_consent_paradox, population_scale_necessity_doctrine).
narrative_ontology:constraint_vindicates(data_consent_paradox, consent_as_barrier_to_progress_framing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED DATA SUBJECT (SNARE) — Patient facing treatment access conditioned on genomic data sharing consent. Cannot exit healthcare system; immediate survival needs override privacy concerns. Experiences maximum extraction: privacy autonomy traded under duress for medical care that should be unconditional. The coordination story (better future medicine) is cover for present extraction.
constraint_indexing:constraint_classification(data_consent_paradox, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMED REFUSER (TANGLED ROPE) — Patient with resources to understand consent forms and capacity to refuse. Faces real coordination problem: withholding data may harm future patients including their own descendants. But also faces extraction: consent theater obscures how data will actually be used, who profits, and what secondary uses emerge. Can exit but at cost of excluding self from cutting-edge care and contributing to dataset bias.
constraint_indexing:constraint_classification(data_consent_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNDERREPRESENTED POPULATION (TANGLED ROPE) — Communities historically excluded from genomic research face dual bind: participation enables exploitation (extraction), but non-participation perpetuates algorithmic bias that harms their descendants (coordination failure). Constrained exit: can refuse but refusal entrenches health disparities. Genuine coordination need exists alongside structural extraction.
constraint_indexing:constraint_classification(data_consent_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: AIGHP DEVELOPER (ROPE) — Experiences constraint as pure coordination: population-scale data is genuinely necessary for model accuracy and fairness. Consent mechanisms enable legitimate data collection. Developer benefits from data access, can arbitrage across jurisdictions with different consent regimes, and experiences negligible extraction. The constraint solves their coordination problem.
constraint_indexing:constraint_classification(data_consent_paradox, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups (EFF, patient rights organizations, data protection authorities) see current consent regime as temporary coordination failure with sunset logic: federated learning, differential privacy, synthetic data generation, and homomorphic encryption are building technical pathways that enable model training without centralized genomic data collection. Estimated sunset: 10-15 years as privacy-preserving ML matures.
constraint_indexing:constraint_classification(data_consent_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: HOSPITAL ETHICS BOARD (TANGLED ROPE) — Institutional actor mediating between patient autonomy and research imperatives. Faces genuine coordination problem: must balance individual consent rights against population health benefits. But also faces extraction: hospital revenue increasingly tied to data partnerships with AIGHP developers, creating institutional pressure to maximize consent rates. Constrained exit: cannot fully exit data economy without losing competitive position.
constraint_indexing:constraint_classification(data_consent_paradox, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DATA PROTECTION AUTHORITY (TANGLED ROPE) — Regulatory body enforcing GDPR/equivalent frameworks. Experiences both coordination (consent protects autonomy, enables trust) and extraction (consent theater allows data collection under legal cover while meaningful understanding remains impossible for complex genomic uses). Mobile exit: can shift enforcement priorities and reinterpret consent standards, but faces institutional pressure from healthcare and tech sectors.
constraint_indexing:constraint_classification(data_consent_paradox, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, genuine coordination problem exists: population-scale genomic data genuinely improves medical outcomes, and some consent mechanism is necessary. But extraction is also structural: consent has become theater that legitimizes data collection while obscuring power asymmetries, secondary uses, and profit distribution. The constraint coordinates data flow while extracting autonomy and concentrating benefits.
constraint_indexing:constraint_classification(data_consent_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_consent_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_consent_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_consent_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_consent_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(data_consent_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts privacy autonomy from data subjects while concentrating benefits (improved models, commercial value, research prestige) among AIGHP developers and healthcare systems. Extraction has increased over the interval (0.38 → 0.58) as genomic data uses have expanded beyond original consent scopes and as treatment access has become more tightly coupled to data sharing. The value reflects that extraction is substantial but not total: some subjects retain meaningful choice, some data uses genuinely serve population health, and privacy-preserving alternatives are emerging. Suppression (0.62): Moderate-high. Significant barriers to meaningful refusal include treatment access conditionality, insurance coverage requirements, clinical trial participation tied to broad data permissions, and information asymmetry (subjects cannot evaluate complex genomic inference risks). Suppression has intensified over the interval (0.45 → 0.62) as AIGHP adoption has grown and as healthcare systems have integrated genomic data collection into standard care pathways. But suppression is not total: some jurisdictions maintain strong consent protections, some subjects can refuse without losing care access, and organized advocacy has created exit pathways. Theater ratio (0.48): Moderate. Consent mechanisms are substantially performative: forms have grown longer and more complex while subject comprehension has declined; legal language obscures actual data uses and profit distribution; consent theater legitimizes collection while providing minimal autonomy protection. Theater has increased over the interval (0.32 → 0.48) as genomic data uses have outpaced consent form evolution and as secondary uses have proliferated beyond original scopes. But theater is not total: some consent processes involve genuine education and choice, some subjects do understand and refuse, and regulatory enforcement does constrain some abuses.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates tangled rope structure from most perspectives but exhibits significant perspectival variation in experienced extraction. The coerced data subject sees snare: consent is cover for extraction under duress, coordination story is false. The informed refuser sees tangled rope: genuine coordination need exists (better future medicine) but extraction is also real (consent theater, profit concentration). The underrepresented population sees tangled rope with dual bind: participation enables exploitation, non-participation perpetuates bias. The AIGHP developer sees rope: constraint solves legitimate coordination problem (data access for model training), extraction is negligible from their position. The privacy rights coalition sees scaffold: current consent regime is temporary, privacy-preserving ML will provide sunset. The hospital ethics board sees tangled rope: must balance autonomy against research imperatives and revenue pressure. The data protection authority sees tangled rope: consent both protects autonomy and legitimizes extraction. The analytical observer sees tangled rope: genuine coordination coexists with structural extraction, and the constraint's classification depends on which observable dominates (consent comprehension rates vs model accuracy gains vs profit concentration vs algorithmic bias reduction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Coerced data subjects (powerless/trapped, victim) experience maximum extraction: d approaches 1.0, yielding high chi. They bear privacy loss under duress with minimal benefit. Informed refusers (moderate/constrained, victim) experience substantial extraction but retain some agency: d ≈ 0.65-0.75, yielding moderate-high chi. They can exit but at cost. Underrepresented populations (moderate/constrained, victim) face dual extraction: privacy loss plus algorithmic bias from non-participation: d ≈ 0.70, yielding moderate-high chi. AIGHP developers (institutional/arbitrage, beneficiary) experience negligible or negative extraction: d approaches 0.0, yielding low or negative chi (subsidy). They benefit from data access and can arbitrage across jurisdictions. Healthcare systems (institutional/constrained, beneficiary) experience low extraction: d ≈ 0.20-0.30, yielding low chi. They benefit from data partnerships but face some regulatory constraint. Hospital ethics boards (institutional/constrained, mixed) experience moderate extraction: d ≈ 0.45-0.55, yielding moderate chi. They mediate between autonomy and revenue pressures. Data protection authorities (institutional/mobile, mixed) experience low-moderate extraction: d ≈ 0.35-0.45, yielding low-moderate chi. They enforce consent but face institutional pressure. Privacy rights coalition (organized/mobile, beneficiary of alternative pathway) experiences low extraction: d ≈ 0.25, yielding low chi. They are building exit routes via privacy-preserving ML.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled rope is the structurally accurate classification when both coordination and extraction are irreducible. The coordination function is genuine: population-scale genomic data does improve AIGHP accuracy and reduce algorithmic bias (this is the mountain constraint scientific_viability_uncertainty that this constraint is downstream of). But extraction is also structural: consent has become theater that legitimizes data collection while obscuring power asymmetries, secondary uses, and profit distribution. The constraint cannot be reduced to pure rope (the coerced data subject's experience is real extraction, not coordination) or pure snare (the future patient benefit and algorithmic bias reduction are real coordination, not cover). The perspectival gaps are not measurement error but structural features: agents at different positions in the data flow experience different ratios of coordination to extraction. The scaffold perspective (privacy rights coalition) identifies a potential sunset via technical alternatives, but the sunset is not yet realized — current extraction persists. The constraint's mandatrophy is resolved by accepting that coordination and extraction coexist, that both are measurable, and that the classification depends on the observer's structural position and the observable used to evaluate the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaningful_consent_threshold,
    'At what complexity threshold does genomic data consent become structurally impossible rather than merely difficult — when secondary uses, algorithmic inference, and re-identification risks exceed any individual''s capacity to evaluate?',
    'Empirical studies of consent comprehension across education levels and consent form complexity; legal doctrine evolution on what constitutes ''informed'' consent for genomic data with unbounded future uses',
    'If threshold already exceeded: current consent is pure theater (extraction). If threshold not yet reached: consent remains meaningful coordination mechanism (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_consent_threshold, empirical, 'Threshold where genomic consent becomes structurally impossible').

omega_variable(
    privacy_preserving_ml_timeline,
    'Will federated learning, differential privacy, and synthetic genomic data generation mature sufficiently to enable AIGHP training without centralized real genomic data collection within 15 years?',
    'Technical benchmarking of privacy-preserving ML methods against centralized training; adoption rates in clinical settings; regulatory acceptance of synthetic data for model validation',
    'If yes: scaffold perspective confirmed, constraint has genuine sunset. If no: privacy-preserving ML is aspirational cover story, and extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_preserving_ml_timeline, empirical, 'Whether privacy-preserving ML provides viable alternative pathway').

omega_variable(
    consent_conditionality_prevalence,
    'What proportion of genomic data collection occurs under explicit or implicit conditionality — treatment access, insurance coverage, clinical trial participation, or employment tied to consent?',
    'Survey of consent practices across healthcare systems; legal analysis of consent form language; patient testimony on perceived coercion; insurance policy analysis',
    'If high (>40%): constraint is snare from many more perspectives (consent is cover for coercion). If low (<15%): constraint remains tangled rope (genuine choice exists for most subjects).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_conditionality_prevalence, empirical, 'Prevalence of coercive conditionality in genomic consent').

omega_variable(
    algorithmic_bias_consent_tradeoff,
    'Does the consent barrier actually cause algorithmic bias (by excluding populations with higher refusal rates), or is bias primarily driven by historical data gaps that consent-based collection cannot remedy?',
    'Longitudinal analysis of dataset diversity before and after consent regime implementation; comparison of bias metrics in consent-based vs legacy datasets; causal modeling of refusal rate disparities',
    'If consent causes bias: the coordination story is real (participation genuinely helps underrepresented groups). If bias is upstream: the coordination story is cover (consent theater doesn''t solve the problem it claims to address).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_consent_tradeoff, empirical, 'Whether consent barriers cause or merely reveal algorithmic bias').

omega_variable(
    secondary_use_scope_creep,
    'How frequently do genomic datasets consented for specific research purposes get repurposed for secondary uses beyond original consent scope — and how often do subjects learn of or object to such uses?',
    'Audit of data use agreements vs actual usage; tracking of consent form amendments; analysis of data breach and misuse litigation; subject notification rates for secondary uses',
    'If high repurposing with low notification: consent is theater masking extraction (snare from more perspectives). If low repurposing or high notification: consent provides meaningful boundary (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_use_scope_creep, empirical, 'Prevalence and transparency of secondary genomic data uses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_consent_paradox, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consent_theater_2010, data_consent_paradox, theater_ratio, 0, 0.32).
narrative_ontology:measurement(consent_theater_2013, data_consent_paradox, theater_ratio, 3, 0.38).
narrative_ontology:measurement(consent_theater_2016, data_consent_paradox, theater_ratio, 6, 0.42).
narrative_ontology:measurement(consent_theater_2019, data_consent_paradox, theater_ratio, 9, 0.45).
narrative_ontology:measurement(consent_theater_2022, data_consent_paradox, theater_ratio, 12, 0.47).
narrative_ontology:measurement(consent_theater_2026, data_consent_paradox, theater_ratio, 16, 0.48).

% Extraction over time
narrative_ontology:measurement(consent_extract_2010, data_consent_paradox, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(consent_extract_2013, data_consent_paradox, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(consent_extract_2016, data_consent_paradox, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(consent_extract_2019, data_consent_paradox, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(consent_extract_2022, data_consent_paradox, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(consent_extract_2026, data_consent_paradox, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(consent_suppress_2010, data_consent_paradox, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(consent_suppress_2016, data_consent_paradox, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(consent_suppress_2022, data_consent_paradox, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(consent_suppress_2026, data_consent_paradox, suppression_requirement, 16, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_consent_paradox, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of scientific_viability_uncertainty (the mountain establishing that AIGHP requires population-scale data). The upstream constraint has negligible extractiveness (it is a technical requirement, not a policy choice). This constraint adds the consent layer that transforms the technical requirement into an extractive mechanism. The two constraints have different epsilon values and different structural positions: scientific_viability_uncertainty is mountain from all perspectives (immutable technical requirement), while data_consent_paradox is tangled_rope from the analytical perspective (coordination coexists with extraction in the consent regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_consent_paradox, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
