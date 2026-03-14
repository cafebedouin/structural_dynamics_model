% ============================================================================
% CONSTRAINT STORY: health_surveillance_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_health_surveillance_infrastructure, []).

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
 *   constraint_id: health_surveillance_infrastructure
 *   human_readable: Health Surveillance Infrastructure: Coordination and Asymmetric Extraction
 *   domain: public_health/digital_infrastructure/governance
 *
 * SUMMARY:
 *   Health surveillance infrastructure coordinates disease prevention,
 *   outbreak response, and population health monitoring while simultaneously
 *   extracting intimate personal data for commercial and governmental
 *   purposes. This constraint exhibits the full range of DR classification
 *   types: health data subjects experience snare-level extraction (mandatory
 *   participation, no real exit), public health authorities experience
 *   rope-level coordination (surveillance enables legitimate disease
 *   control), pharmaceutical companies experience tangled rope (benefits from
 *   data while contributing to commercial extraction), privacy advocates see
 *   a temporary scaffold (privacy-preserving alternatives are emerging),
 *   legacy systems persist as pitons (institutional inertia), and the
 *   analytical observer risks naturalizing contingent surveillance
 *   architecture as an immutable requirement of epidemiology. The
 *   constraint's extractiveness has increased from 0.32 to 0.58 over the
 *   interval as secondary use of health data for commercial profiling,
 *   predictive policing, and insurance discrimination has accelerated faster
 *   than regulatory constraints. Theater ratio increased from 0.35 to 0.48 as
 *   data security practices became more performative (encryption that does
 *   not prevent authorized access) and consent frameworks became more
 *   theatrical (privacy notices that do not provide meaningful choice).
 *
 * KEY AGENTS:
 *   - Health Data Subjects: Primary victims (powerless/trapped) — citizens whose medical information is extracted without meaningful consent or control; no option to access healthcare while preserving privacy
 *   - Public Health Authorities: Primary beneficiary (institutional/arbitrage) — coordinate disease surveillance and outbreak response; experience surveillance as pure coordination with institutional flexibility
 *   - Pharmaceutical Industry: Secondary beneficiary (powerful/arbitrage) — access health data for drug efficacy, market segmentation, and pricing strategy; powerful agent with global arbitrage options
 *   - Patient Advocacy Community: Mixed (moderate/constrained) — benefit from aggregate health surveillance for disease pattern identification but face extraction through data profiling and discriminatory outcomes
 *   - Privacy Rights Coalition: Organized agents (organized/constrained) — building privacy-preserving alternatives (differential privacy, federated learning) with sunset logic for centralized surveillance
 *   - Legacy EHR Systems: Institutional actor (institutional/mobile) — persist through inertia; vulnerability to breaches and data access abuse is normalized through theater of security
 *   - Medical Privacy Commons: Primary victim (powerless/trapped) — abstract collective good; no mechanism to represent shared interest in data protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(health_surveillance_infrastructure, 0.58).
domain_priors:suppression_score(health_surveillance_infrastructure, 0.62).
domain_priors:theater_ratio(health_surveillance_infrastructure, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(health_surveillance_infrastructure, extractiveness, 0.58).
narrative_ontology:constraint_metric(health_surveillance_infrastructure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(health_surveillance_infrastructure, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(health_surveillance_infrastructure, tangled_rope).
narrative_ontology:human_readable(health_surveillance_infrastructure, "Health Surveillance Infrastructure: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(health_surveillance_infrastructure, "public_health/digital_infrastructure/governance").

domain_priors:requires_active_enforcement(health_surveillance_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(health_surveillance_infrastructure, public_health_authorities).
narrative_ontology:constraint_beneficiary(health_surveillance_infrastructure, epidemiological_research).
narrative_ontology:constraint_beneficiary(health_surveillance_infrastructure, pharmaceutical_industry).
narrative_ontology:constraint_victim(health_surveillance_infrastructure, health_data_subjects).
narrative_ontology:constraint_victim(health_surveillance_infrastructure, medical_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEALTH DATA SUBJECT (SNARE) — Citizens cannot opt out of health surveillance without forfeiting access to medical care. Data contribution is structurally mandatory; exit options are illusory (formal opt-out does not prevent de facto tracking). Extraction is maximal: intimate health information flows to authorities and third parties with minimal consent or transparency.
constraint_indexing:constraint_classification(health_surveillance_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT ADVOCACY COMMUNITY (TANGLED ROPE) — Constrained by information asymmetry and regulatory barriers, but also benefits from aggregate health surveillance data for identifying disease patterns and advocating for treatment access. Genuine coordination function (disease monitoring) paired with asymmetric extraction (data used for profiling, insurance discrimination, pharmaceutical pricing).
constraint_indexing:constraint_classification(health_surveillance_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences health surveillance as pure coordination: epidemic detection, outbreak response, disease elimination campaigns all depend on integrated data flow. Authority has institutional flexibility and can arbitrage between surveillance programs. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(health_surveillance_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (TANGLED ROPE) — Benefits from surveillance data for drug efficacy tracking, pharmacovigilance, and market segmentation while contributing to public health coordination. Powerful agent with arbitrage options (can access data through regulatory channels, negotiate with authorities, operate across jurisdictions). Asymmetric extraction flows toward this actor through commercial use of health data.
constraint_indexing:constraint_classification(health_surveillance_infrastructure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY RIGHTS COALITION (SCAFFOLD) — Organized movement building alternative trust frameworks: differential privacy techniques, federated learning, data minimization protocols, and consent-respecting alternatives to centralized surveillance. Sees health surveillance as a temporary institutional arrangement with technological sunset: privacy-preserving epidemiology is maturing and will enable equivalent public health function with drastically reduced extraction.
constraint_indexing:constraint_classification(health_surveillance_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY EHR SYSTEMS (PITON) — Traditional centralized medical records infrastructure persists through institutional inertia and switching costs despite known vulnerabilities and suboptimal disease surveillance function. The system is maintained because alternatives haven't fully migrated yet, not because it provides superior coordination. Theater ratio high due to performative data security (encryption that doesn't prevent authorized access for profit-seeking) and illusion of individual control through privacy policies.
constraint_indexing:constraint_classification(health_surveillance_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, disease surveillance may appear as an immutable requirement of public health: you cannot eliminate plague without tracking plague. However, this naturalizes the specific institutional form (centralized surveillance with unrestricted access) rather than the coordination requirement (disease tracking). The constraint is contingent on architecture choice, not a law of nature.
constraint_indexing:constraint_classification(health_surveillance_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(health_surveillance_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(health_surveillance_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(health_surveillance_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(health_surveillance_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(health_surveillance_infrastructure, TR),
    TR >= 0.70.

:- end_tests(health_surveillance_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Health surveillance has genuine coordination function (disease detection, outbreak response) but is paired with accelerating secondary use for commercial profiling, insurance discrimination, and targeted marketing. The value reflects the significant and growing extractive overlay on legitimate public health coordination. Suppression (0.62): Moderate-high. Structural barriers include regulatory requirements for data contribution, technical locks preventing meaningful exit (opting out of surveillance requires forfeiting care access), and information asymmetry about data use. Legal frameworks (consent doctrine) are theater — they grant formal choice while providing no substantive alternatives. Theater ratio (0.48): Moderate. Data security practices are substantially performative — encryption and access controls exist but do not prevent authorized third-party access for profit-seeking. Consent frameworks are theater (privacy policies that do not provide meaningful choice). However, the epidemiological function itself (disease tracking, pattern detection) has real coordination content, so theater is not maximal. The trajectory shows increasing theater as performative security replaces actual privacy safeguards.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival disagreement. The powerless data subject sees a snare (mandatory participation, extraction with no exit). The institutional authority sees a rope (pure coordination). The pharmaceutical company sees a tangled rope (mixed benefit and contribution). The privacy coalition sees a scaffold with sunset (technology emerging to replace centralized surveillance). The analytical observer risks a false summit (naturalizing surveillance as inherent to epidemiology). The gap reveals that surveillance architecture is a contingent choice, not a law of nature — the coordination requirement (disease tracking) is real, but the institutional form (centralized, unrestricted access) is imposed, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply by agent and power level. Health data subjects (powerless/trapped) experience d ≈ 0.95 — they are structurally the full target of extraction. Public health authorities (institutional/arbitrage) experience d ≈ 0.05 — they are net beneficiaries with structural flexibility. Pharmaceutical companies (powerful/arbitrage) experience d ≈ 0.30 — they benefit from data access but contribute to genuine coordination, creating a middle position. Constrained moderate agents (patient advocates) experience d ≈ 0.70 — they bear significant extraction but retain some agency and secondary benefits. The directionality spread reveals that this is not a uniform constraint but a structured hierarchy: powerless agents experience maximal extraction, powerful agents extract value while contributing coordination, institutional authorities coordinate and benefit. The asymmetry is not accidental but structural to the surveillance design.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through architectural decomposition. The constraint conflates two structurally distinct claims: (1) Disease surveillance is necessary coordination (true, legitimate coordination requirement). (2) Centralized surveillance with unrestricted secondary access is necessary to achieve disease surveillance (false, privacy-preserving alternatives exist). The first claim is mountain-like (inherent requirement). The second is snare-like (imposed institutional choice). The tangled rope classification reflects the current state where both claims are fused in practice, but the decomposition enables exit: privacy-preserving epidemiology (federated learning, differential privacy) achieves equivalent disease detection without centralized extraction. The scaffold perspective captures this: as alternative technologies mature, centralized surveillance will cease to be coordination and will reveal itself as pure extraction. The false summit detector identifies the mountain classification as naturalization of architectural choice rather than genuine natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_preserving_technology_maturity,
    'Can privacy-preserving epidemiology (differential privacy, federated learning, homomorphic encryption) achieve equivalent public health outcomes to centralized surveillance?',
    'Real-world deployment comparison: disease detection rates, outbreak response times, and epidemiological accuracy comparing privacy-preserving vs centralized systems in equivalent populations over 5+ year periods',
    'If achievable: scaffold sunset is structural; privacy-preserving alternatives will make centralized extraction obsolete. If not: centralized surveillance is a necessary coordination mechanism, extractive asymmetry becomes justifiable (Rope reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_preserving_technology_maturity, empirical, 'Whether privacy-preserving technologies enable equivalent epidemiological function').

omega_variable(
    secondary_use_boundary,
    'What is the boundary between legitimate secondary use of health data for public health and extractive commercial/governmental profiling?',
    'Analysis of data access patterns: frequency and justification for pharmaceutical industry access; correlation between surveillance data access and discriminatory outcomes (insurance denial, targeted marketing, predictive policing); comparison with jurisdictions that restrict secondary use',
    'If secondary use is unavoidable: extraction is intrinsic to data infrastructure (snare classification strengthens). If secondary use is contingent policy choice: extraction is imposed by architecture design, not coordination necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secondary_use_boundary, empirical, 'Boundary between legitimate secondary use and extractive profiling').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is measured suppression (0.62) structural (legal barriers, technical enforcement, institutional gatekeeping) or internalized (subjects believe surveillance is necessary, have normalized loss of privacy, trust authority)?',
    'Post-exit surveys and behavioral analysis: when subjects are given genuinely functional opt-out (privacy-preserving alternatives that don''t compromise care access), what proportion maintain engagement? If suppression persists despite exit, it is internalized.',
    'If structural: removing legal barriers enables exit. If internalized: subjects carry suppression with them; constraint persists even after formal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized components of suppression').

omega_variable(
    coalition_power_threshold,
    'At what proportion of population participating in privacy-rights alternatives does the centralized surveillance system''s coordination function degrade?',
    'Measurement of disease detection effectiveness as proportion of population in privacy-preserving systems increases; outbreak response latency; coverage gaps in surveillance',
    'If threshold < 20% participation in alternatives: centralized system is fragile; organizing coordination migration is feasible. If threshold > 60%: system is resilient; coalition power is insufficient to force transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_power_threshold, empirical, 'Coalition power threshold for surveillance system degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(health_surveillance_infrastructure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(healthsurv_tr_t0, health_surveillance_infrastructure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(healthsurv_tr_t7, health_surveillance_infrastructure, theater_ratio, 7, 0.41).
narrative_ontology:measurement(healthsurv_tr_t15, health_surveillance_infrastructure, theater_ratio, 15, 0.48).
narrative_ontology:measurement(healthsurv_tr_t20, health_surveillance_infrastructure, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(healthsurv_be_t0, health_surveillance_infrastructure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(healthsurv_be_t7, health_surveillance_infrastructure, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(healthsurv_be_t15, health_surveillance_infrastructure, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(healthsurv_be_t20, health_surveillance_infrastructure, base_extractiveness, 20, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(health_surveillance_infrastructure, global_infrastructure).
narrative_ontology:affects_constraint(health_surveillance_infrastructure, medical_data_commodification).
narrative_ontology:affects_constraint(health_surveillance_infrastructure, insurance_discrimination_pricing).
narrative_ontology:affects_constraint(health_surveillance_infrastructure, predictive_policing_health_profiling).

% DUAL FORMULATION NOTE:
% Health surveillance infrastructure decomposes into multiple structurally distinct constraints: (1) disease_coordination_necessity (ε ≈ 0.15, Mountain/Rope) — epidemiological coordination requirement. (2) centralized_surveillance_architecture (ε ≈ 0.68, Snare) — imposed institutional choice to achieve coordination via unrestricted access. (3) secondary_use_extraction (ε ≈ 0.72, Snare) — commercial and governmental profiling enabled by surveillance architecture. This story represents the fused state; decomposition enables clarity on which mechanisms are necessary and which are imposed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(health_surveillance_infrastructure, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
