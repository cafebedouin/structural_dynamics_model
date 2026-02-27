% ============================================================================
% CONSTRAINT STORY: personalized_nutritional_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personalized_nutritional_arbitrage, []).

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
 *   constraint_id: personalized_nutritional_arbitrage
 *   human_readable: Personalized Nutritional Arbitrage
 *   domain: economic/technological/healthcare
 *
 * SUMMARY:
 *   Personalized nutritional arbitrage describes the system where companies
 *   like Zoe, Viome, and similar platforms use consumer microbiome and
 *   continuous glucose monitoring data to sell individualized dietary
 *   recommendations. The constraint exhibits tension between legitimate
 *   coordination (solving the genuine problem that metabolic heterogeneity
 *   makes one-size-fits-all nutrition advice suboptimal) and extractive
 *   mechanisms (proprietary algorithm lock-in, data capture, vendor
 *   dependency, pricing barriers to low-income users, knowledge capture from
 *   the public health commons). The structure reveals all six constraint
 *   types from different perspectives: low-income consumers experience pure
 *   extraction (snare); health-conscious consumers experience mixed
 *   coordination-extraction (tangled rope); platform companies experience
 *   coordination (rope); public health systems experience extraction (snare);
 *   medical researchers experience mixed effects (tangled rope); traditional
 *   nutritional science persists through institutional inertia (piton); and
 *   the analytical observer risks naturalizing proprietary platforms as
 *   inevitable consequences of metabolic biology (false summit mountain). The
 *   extractiveness has increased from 0.28 to 0.52 over six years as
 *   platforms have matured and locked in users; theater ratio has
 *   simultaneously increased from 0.35 to 0.58 as marketing claims about
 *   personalization have outpaced evidence of causality.
 *
 * KEY AGENTS:
 *   - Platform Companies (Zoe, Viome, others): Primary beneficiaries (institutional/arbitrage) — capture subscription revenue, build proprietary datasets, establish algorithmic lock-in, arbitrage into pharmaceutical partnerships and insurance data sales
 *   - Low-Income Consumers: Primary victims (powerless/trapped) — cannot afford biomarker testing or subscriptions; trapped in generic dietary guidance while algorithms optimize for wealthy users
 *   - Public Health Systems: Secondary victims (powerless/trapped) — locked out of proprietary algorithms and individual-level data; unable to tailor population-level interventions to actual heterogeneity
 *   - Health-Conscious Consumers: Mixed position (moderate/constrained) — gain genuine benefit from personalized guidance but experience vendor lock-in, lack data portability, face opaque algorithmic decision-making
 *   - Medical Research Community: Mixed position (moderate/constrained) — gain access to large-scale biomarker datasets but face licensing fees and proprietary data restrictions
 *   - Traditional Nutrition Science: Institutional actor (institutional/arbitrage) — maintains generic guidelines through inertia despite evidence of individual heterogeneity; sees own authority gradually eroded but retains insurance coverage and government endorsement
 *   - Open Health Data Initiative: Organized actors (organized/constrained) — building alternative pathways through data portability regulations (HIPAA right of access, GDPR), open biomarker standards, and federated learning models
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing proprietary platforms as inevitable consequence of metabolic biology rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personalized_nutritional_arbitrage, 0.52).
domain_priors:suppression_score(personalized_nutritional_arbitrage, 0.48).
domain_priors:theater_ratio(personalized_nutritional_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personalized_nutritional_arbitrage, extractiveness, 0.52).
narrative_ontology:constraint_metric(personalized_nutritional_arbitrage, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(personalized_nutritional_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personalized_nutritional_arbitrage, tangled_rope).
narrative_ontology:human_readable(personalized_nutritional_arbitrage, "Personalized Nutritional Arbitrage").
narrative_ontology:topic_domain(personalized_nutritional_arbitrage, "economic/technological/healthcare").

domain_priors:requires_active_enforcement(personalized_nutritional_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personalized_nutritional_arbitrage, nutrition_platform_companies).
narrative_ontology:constraint_beneficiary(personalized_nutritional_arbitrage, data_aggregators).
narrative_ontology:constraint_beneficiary(personalized_nutritional_arbitrage, subscription_ecosystem).
narrative_ontology:constraint_victim(personalized_nutritional_arbitrage, health_data_commons).
narrative_ontology:constraint_victim(personalized_nutritional_arbitrage, low_income_consumers).
narrative_ontology:constraint_victim(personalized_nutritional_arbitrage, public_health_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME CONSUMER (SNARE) — Cannot afford continuous biomarker monitoring (microbiome tests $150-300, blood glucose monitors $50-200). Trapped in generic dietary guidance while proprietary algorithms optimize for paying users. Extraction maximized: bears cost of data exclusivity without access to personalized benefit.
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEMS (SNARE) — Cannot access the proprietary algorithms and individual-level data that predict metabolic response. Trapped funding population-level interventions with generic guidelines while private platforms optimize for high-margin individual users. Extraction: loss of epidemiological commons and inability to tailor public health policy to actual population heterogeneity.
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTH-CONSCIOUS CONSUMER (TANGLED ROPE) — Pays subscription fees ($30-50/month) for personalized guidance. Receives genuine benefit (tailored dietary adjustments, reduced bloating, stable energy) but experiences moderate extraction: vendor lock-in prevents data portability, algorithm opacity prevents independent verification, platform can alter advice based on business partnerships with food brands. Both coordination (helpful guidance) and extraction (data capture, algorithmic control) present.
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM COMPANIES (ROPE) — Experience the constraint as coordination problem: aggregating user biomarker data, predicting metabolic response, and distributing personalized recommendations is genuinely useful. Net beneficiary: capture subscription revenue, build proprietary datasets, establish algorithmic advantage. Low experienced extraction because they control the enforcement mechanism and can exit to adjacent markets (pharmaceutical partnerships, insurance data sales).
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MEDICAL RESEARCH COMMUNITY (TANGLED ROPE) — Gains access to large-scale metabolic datasets (Zoe has 30,000+ users with paired genotype/microbiome/glucose data). Extraction: platform companies restrict research access, set licensing fees, require data use agreements that benefit platform commercial interests. Both coordination (data enables discovery) and extraction (proprietary control, revenue extraction from researchers) present.
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL NUTRITION SCIENCE (PITON) — Generic dietary guidelines (Mediterranean diet, low-glycemic index) persist despite evidence that individual metabolic response varies 10x. The guidelines are maintained through institutional inertia (government health agencies, medical school curricula, insurance coverage) despite being substantially superseded by personalized approaches. Theater ratio: 0.72 — nutrition science conducts ritual randomized controlled trials on average effects while real heterogeneity is now visible in individual biomarker data.
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN HEALTH DATA INITIATIVE (SCAFFOLD) — Organized actors (patient organizations, research nonprofits, regulators) are building alternative pathways: open-source biomarker standards (MITRE FHIR for health data, standardized microbiome taxonomies), data portability regulations (HIPAA right of access, emerging digital rights frameworks), and federated learning models that preserve privacy while enabling research. Sees the proprietary lockdown as temporary. Sunset logic: as data becomes portable and algorithms commoditize, the extraction mechanism loses force. Estimated sunset: 7-10 years as GDPR-style regulations and open science norms mature.
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, individual metabolic heterogeneity is a fundamental biological fact: people's blood glucose responses to identical meals vary by 25-30% due to microbiome composition, genetics, and host factors. Personalized nutrition algorithms cannot be eliminated because the biology demands it. However, this perspective risks naturalizing the INSTITUTIONAL ARRANGEMENT (proprietary platforms) as if it were the BIOLOGICAL FACT (metabolic heterogeneity). The false summit occurs when 'we need personalization' is conflated with 'proprietary algorithmic platforms are inevitable.'
constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personalized_nutritional_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personalized_nutritional_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personalized_nutritional_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personalized_nutritional_arbitrage, TR),
    TR >= 0.70.

:- end_tests(personalized_nutritional_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The initial value (0.28) reflected genuine coordination gain from matching individuals to metabolic phenotypes. The current value (0.52) reflects accumulation of extractive mechanisms: (1) vendor lock-in through data silos and non-portable proprietary formats; (2) pricing barriers ($30-50/month subscriptions) that exclude low-income users from personalized benefit; (3) repeated testing for microbiome (claimed necessity every 6-12 months) that generates recurring revenue but provides limited additional information if baseline stability is high; (4) algorithmic opacity that prevents independent verification of recommendations; (5) capture of the health data commons — companies aggregate anonymized datasets that could feed population-level public health intervention but withhold this from public health systems. Suppression (0.48): Moderate. Barriers to exit include: (1) data lock-in (users cannot export microbiome/glucose data in interoperable formats); (2) switching costs (learning new platform, re-purchasing baseline tests); (3) informational asymmetry (consumers cannot verify algorithmic claims); (4) no transparent alternatives (public health systems offer generic guidelines only). Some suppression is eroding as regulatory frameworks (GDPR right of access, emerging digital rights) create legal right to data export. Theater ratio (0.58): Moderate-high and rising. The original (0.35) reflected that personalized recommendations were novel and provided measurable benefit. Current (0.58) reflects: (1) marketing claims about 'precision medicine' and 'genetic personalization' that exceed causal evidence (much benefit may be from selection bias — health-conscious users make multiple lifestyle changes simultaneously); (2) ritualistic testing and retesting; (3) adoption of scientific language ('metabolic phenotype', 'dysbiosis') to legitimize commercial product. The constraint was plausibly Rope at inception; it has degraded toward Tangled Rope as extraction mechanisms accumulated.
 *
 * PERSPECTIVAL GAP:
 *   Platform companies see the constraint as coordination (Rope): aggregating biomarker data and predicting response is genuinely valuable, solves a real problem (metabolic heterogeneity), produces measurable benefit. The experienced extraction is low because they control the system and can exit to adjacent markets. Low-income consumers see the constraint as pure extraction (Snare): they receive no personalized benefit, are locked out by pricing, and cannot exit (no public alternative exists). Public health systems see extraction (Snare): they are locked out of the proprietary algorithms and cannot design population interventions. Health-conscious consumers see mixed coordination-extraction (Tangled Rope): real benefit (personalized guidance improves their health) but real extraction (vendor lock-in, data capture, opaque algorithms, pricing power). Medical researchers see mixed effects (Tangled Rope): access to large datasets enables discovery but licensing fees and proprietary restrictions limit research scope. The open health data coalition sees a temporary problem being solved (Scaffold): data portability regulations and open standards are building alternative pathways, sunset timeline 7-10 years. The analytical observer risks seeing natural law (Mountain): 'metabolic heterogeneity requires personalization, therefore proprietary platforms are inevitable' — but this conflates the biological fact with the institutional arrangement, producing a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and structural position. Platform companies benefit from proprietary lock-in and command premium pricing; they have institutional power and arbitrage options (pivot to pharma data, insurance partnerships). They experience low d (beneficiary with exit) → negative χ. Low-income consumers are victims with trapped exit (no alternative personalization available); they experience high d (victim + trapped) → high χ. Health-conscious consumers are both partial beneficiaries (genuine guidance) and partial victims (lock-in); they experience moderate d consistent with constrained exit and ambiguous beneficiary status. Public health systems are victims with trapped exit (cannot build alternative algorithms without access data); they experience high d. Medical researchers are mixed — they gain access to data (beneficiary signal) but face licensing restrictions (victim signal); constrained exit produces moderate d. The platform companies' institutional power ordinarily would suppress d (canonical: institutional → 0.00 → f(d) ≈ -0.12), but here they are net beneficiaries of the constraint, not enforcers of it. The derivation chain produces appropriate d values: beneficiaries with arbitrage exit get low d → negative χ, victims with trapped exit get high d → high χ, mixed agents with constrained exit get moderate d.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint currently classifies as Tangled Rope (ε=0.52, χ estimated at 0.54-0.62 depending on perspective power level and exit options). The classification is stable IF the constraint genuinely provides both coordination function (personalized guidance for metabolic heterogeneity) AND asymmetric extraction (vendor lock-in, data capture, pricing barriers). However, mandatrophy remains unresolved because the omega variables (algorithmic necessity, microbiome stability, data portability enforceability) are empirically ambiguous. Scenario 1: If algorithmic personalization is NOT strictly necessary (transparent rules capture 80% of benefit) and microbiome is stable (one baseline test sufficient), then much of the apparent coordination is theater, and the true structure is Snare with a thin veneer of coordination. Scenario 2: If data portability is enforced (GDPR-style regulations mature), then the extraction mechanism dissolves within 7-10 years and the constraint reclassifies toward Scaffold (temporary). Scenario 3: If neither algorithmic necessity nor data portability materializes, the constraint remains Tangled Rope or reclassifies toward Snare as extraction accumulates. The mandatrophy is not resolved because the team has not yet conducted the RCTs and regulatory feasibility studies needed to answer the omegas. Recommend: prioritize empirical omega resolution before finalizing classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_personalization_necessity,
    'Is algorithmic personalization strictly necessary for effective metabolic response prediction, or can simple rules-of-thumb (fermented foods for dysbiosis, low FODMAP for irritable bowel, etc.) capture 80% of the benefit with transparent, nonproprietary guidance?',
    'Randomized controlled trial: proprietary personalized algorithms vs. transparent algorithmic decision trees vs. standard generic guidelines, measured by glycemic control, symptom relief, adherence, cost-effectiveness',
    'If transparent rules capture 80%+ of benefit: much of the extraction is rent-seeking on genuine but marginal algorithmic advantage. Constraint reclassifies toward Rope or Scaffold. If personalization requires proprietary ML: extraction is structurally harder to eliminate, constraint remains Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_personalization_necessity, empirical, 'Whether algorithmic personalization is necessary or transparent rules suffice').

omega_variable(
    microbiome_stability_and_portability,
    'How stable is an individual''s microbiome profile across 6-12 months? Can a single baseline test provide actionable personalization for a year, or must users pay for repeated testing as the platform claims?',
    'Longitudinal microbiome profiling: same-person sampling at monthly intervals over 12 months; variance decomposition (biological change vs measurement noise)',
    'If stable (>70% similarity over 12 months): one baseline test is sufficient, repeated testing is extraction. Constraint reclassifies toward Snare. If unstable: repeated testing has genuine value, extraction mechanism becomes less clear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(microbiome_stability_and_portability, empirical, 'Microbiome stability determining retest necessity').

omega_variable(
    data_portability_enforceability,
    'Can data portability regulations (GDPR right of access, emerging digital rights legislation) be enforced such that consumers can export their microbiome and glucose data in standardized formats compatible with open-source analysis tools?',
    'Regulatory feasibility analysis; testing of data export functionality in existing platforms; assessment of interoperability of exported data with open-source tools; legal analysis of enforcement mechanisms',
    'If enforceable: scaffold sunset mechanism is real, proprietary lock-in dissolves, extraction drops. Constraint reclassifies toward Scaffold or Rope. If unenforceable: proprietary platforms maintain control, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_portability_enforceability, empirical, 'Data portability regulation enforceability').

omega_variable(
    population_health_vs_individual_optimization,
    'Can personalized nutrition platforms be required to share anonymized aggregate data with public health systems for population-level intervention design without compromising proprietary algorithm advantage?',
    'Federated learning pilot: platform companies contribute aggregate statistics (microbiome prevalence, glucose response distribution) to public health agencies while preserving individual user privacy and algorithm secrecy',
    'If yes: public health systems access epidemiological commons, constraint toward Tangled Rope with clear extraction boundaries. If no: public health systems remain locked out, extraction mechanism stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(population_health_vs_individual_optimization, empirical, 'Whether anonymized data sharing is compatible with proprietary algorithms').

omega_variable(
    metabolic_phenotype_causality,
    'Do personalized diet changes *cause* improved metabolic health (measured by continuous glucose monitor data, sustained microbiome changes, clinical outcomes like HbA1c) or merely correlate with improved outcomes driven by selection bias (people who buy Zoe are already health-conscious and make multiple lifestyle changes)?',
    'Randomized controlled trial: personalized Zoe recommendations vs. matched generic diet advice in otherwise identical cohorts; blinded measurement of clinical outcomes over 12 months',
    'If causal: the platforms are providing genuine health value, extraction is more justifiable. If selection bias: much of the perceived benefit is theater, constraint reclassifies toward Snare (selling hope to health-conscious wealthy users).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metabolic_phenotype_causality, empirical, 'Causal efficacy of personalized diet recommendations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personalized_nutritional_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pna_tr_t0, personalized_nutritional_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pna_tr_t3, personalized_nutritional_arbitrage, theater_ratio, 3, 0.48).
narrative_ontology:measurement(pna_tr_t6, personalized_nutritional_arbitrage, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(pna_be_t0, personalized_nutritional_arbitrage, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pna_be_t3, personalized_nutritional_arbitrage, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(pna_be_t6, personalized_nutritional_arbitrage, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personalized_nutritional_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(personalized_nutritional_arbitrage, health_data_commons_enclosure).
narrative_ontology:affects_constraint(personalized_nutritional_arbitrage, microbiome_research_asymmetry).
narrative_ontology:affects_constraint(personalized_nutritional_arbitrage, metabolic_phenotype_individualization).

% DUAL FORMULATION NOTE:
% Personalized nutritional arbitrage decomposes into three structurally distinct constraints: (1) health_data_commons_enclosure (ε≈0.65, Snare) — proprietary capture of biomarker datasets that could inform population-level public health; (2) microbiome_research_asymmetry (ε≈0.48, Tangled Rope) — platform access to large datasets vs. researcher licensing restrictions; (3) metabolic_phenotype_individualization (ε≈0.35, Rope) — the genuine coordination problem of matching individuals to response phenotypes. The current story focuses on the consumer-facing arbitrage system; the three decomposed stories model the structural levels separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
