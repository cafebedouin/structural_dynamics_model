% ============================================================================
% CONSTRAINT STORY: genetic_predisposition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_predisposition, []).

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
 *   constraint_id: genetic_predisposition
 *   human_readable: Socio-Economic Response to Genetic Predisposition
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   The socio-economic response to genetic predisposition creates a
 *   structural constraint distinct from the biological fact of heritable
 *   disease risk (which would be a Mountain). This constraint models how
 *   societies institutionalize genetic information into gatekeeping systems:
 *   insurers using genetic data for risk stratification and coverage denial,
 *   employers using genetic screening for hiring discrimination, healthcare
 *   systems using genetic tests to ration access, and reproductive systems
 *   using genetic information to enforce normative trait selection. The
 *   constraint exhibits all six types depending on perspective: pure
 *   extraction (snare) for high-risk individuals trapped by knowledge; mixed
 *   coordination and extraction (tangled rope) for populations dependent on
 *   genetic medicine but vulnerable to discrimination; coordination (rope)
 *   for the testing industry and analytical observers; degraded ritual
 *   (piton) for medical essentialism persisting despite modest predictive
 *   power; and temporary scaffolding (scaffold) for anti-discrimination
 *   frameworks with explicit sunset clauses. The extractiveness has risen
 *   from 0.35 to 0.58 over the interval as genetic databases have expanded,
 *   risk stratification has become more granular, and gatekeeping
 *   institutions have learned to weaponize genetic information more
 *   effectively. Theater ratio has similarly increased from 0.42 to 0.64 as
 *   medical essentialism has decoupled from actual predictive power — genetic
 *   tests are marketed as deterministic despite environmental dominance and
 *   low penetrance for most conditions.
 *
 * KEY AGENTS:
 *   - Genetically High-Risk Individuals: Primary victims (powerless/trapped) — face binary choice between knowledge and social/economic penalties; cannot escape the constraint
 *   - Low-Income Populations: Secondary victims (moderate/constrained) — dependent on genetic screening for disease prevention but vulnerable to discrimination; limited exit options
 *   - Genetic Testing Industry: Primary beneficiary (institutional/arbitrage) — expands market through medicalization of genetic risk; experiences constraint as coordination
 *   - Insurance and Employment Gatekeepers: Primary beneficiary (organized/mobile) — use genetic information for actuarial stratification and risk selection; maintain information asymmetry
 *   - High-Income Earners: Secondary beneficiary (powerful/arbitrage) — can afford genetic testing, expert interpretation, and insurance alternatives; can exit constraint through wealth
 *   - Medical Essentialism Narrative: Institutional actor (institutional/arbitrage) — maintains gatekeeping function through performative genetic determinism
 *   - Anti-Discrimination Coalitions: Organized agents (organized/constrained) — civil rights, disability justice, reproductive autonomy movements building regulatory barriers with sunset clauses
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing predatory gatekeeping as inevitable genomic medicine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_predisposition, 0.58).
domain_priors:suppression_score(genetic_predisposition, 0.68).
domain_priors:theater_ratio(genetic_predisposition, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_predisposition, extractiveness, 0.58).
narrative_ontology:constraint_metric(genetic_predisposition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(genetic_predisposition, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_predisposition, snare).
narrative_ontology:human_readable(genetic_predisposition, "Socio-Economic Response to Genetic Predisposition").
narrative_ontology:topic_domain(genetic_predisposition, "technological/social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_predisposition, genetic_testing_industry).
narrative_ontology:constraint_beneficiary(genetic_predisposition, insurance_gatekeepers).
narrative_ontology:constraint_beneficiary(genetic_predisposition, high_income_earners).
narrative_ontology:constraint_victim(genetic_predisposition, genetically_high_risk_individuals).
narrative_ontology:constraint_victim(genetic_predisposition, low_income_populations).
narrative_ontology:constraint_victim(genetic_predisposition, reproductive_autonomy_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH-RISK INDIVIDUAL (SNARE) — Individual with genetic predisposition (BRCA1, familial hypercholesterolemia, sickle cell trait) faces binary choice: accept social/economic penalties (insurance denial, employment discrimination, reproductive stigma) or remain ignorant of health status. Knowledge itself becomes a trap. Cannot escape the constraint without abandoning biological identity.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW-INCOME POPULATION (TANGLED ROPE) — Constrained by healthcare access and genetic literacy barriers, but also dependent on genetic screening for disease prevention. Experiences both coordination (preventive medicine) and extraction (risk stratification enabling insurance/employment discrimination). Moderate power through collective health advocacy.
constraint_indexing:constraint_classification(genetic_predisposition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GENETIC TESTING INDUSTRY (ROPE) — Benefits from expanding genetic knowledge and market expansion. Experiences constraint as coordination: testing standardization, medical guideline formation, and data sharing solve collective action problems in genomic medicine. Net beneficiary with arbitrage options.
constraint_indexing:constraint_classification(genetic_predisposition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE AND EMPLOYMENT GATEKEEPERS (SNARE) — Institutional extraction mechanism using genetic information for actuarial stratification and risk selection. High coercion (genetic discrimination) with minimal coordination benefit. Organized actors (insurers, employers) maintain information asymmetry advantage. Mobile exit options only available to privileged actors.
constraint_indexing:constraint_classification(genetic_predisposition, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL ESSENTIALISM NARRATIVE (PITON) — The framing that genetic predisposition 'determines' health outcomes is largely performative. Penetrance and expressivity vary enormously; lifestyle, environment, and medical intervention often override genetic risk. The theater ratio reflects continued institutional reliance on genetic determinism despite overwhelming counterevidence. Theater maintains gatekeeping function without delivering promised predictive power.
constraint_indexing:constraint_classification(genetic_predisposition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GENETIC PRIVACY AND ANTI-DISCRIMINATION FRAMEWORKS (SCAFFOLD) — Regulatory sunset mechanisms (genetic non-discrimination laws, GDPR genetic data protections, NIH genome privacy standards) are building temporary barriers to gatekeeping while alternative verification methods mature. Sunset mechanism: as polygenic risk scores and environmental interventions mature, genetic discrimination loses actuarial justification. High suppression is tolerated because the framework explicitly includes exiting logic.
constraint_indexing:constraint_classification(genetic_predisposition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — From a universal/analytical perspective, genomic information generates coordination benefits: disease prevention, pharmacogenomics, population health infrastructure. The constraint appears as a coordination mechanism (how to distribute health information efficiently) rather than extraction. This perspective risks naturalizing the predatory gatekeeping as inevitable.
constraint_indexing:constraint_classification(genetic_predisposition, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_predisposition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_predisposition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_predisposition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genetic_predisposition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genetic_predisposition, TR),
    TR >= 0.70.

:- end_tests(genetic_predisposition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantial value from high-risk individuals through insurance denial, employment discrimination, and reproductive stigma. The trajectory from 0.35 to 0.58 reflects that genetic gatekeeping has become increasingly sophisticated — early genetic testing was crude and high-theater; modern applications are more targeted and have lower theater, enabling more effective extraction. However, extractiveness is not at snare maximum (0.75+) because: (1) some high-risk individuals benefit from early intervention enabled by genetic knowledge, (2) wealth-based exit options remain available to privileged populations, and (3) anti-discrimination frameworks have created some friction. Suppression (0.68): High. Multiple suppression mechanisms operate: information asymmetry (testing industry controls interpretation), regulatory capture (insurance industry influences genetic guidelines), knowledge barriers (genetic literacy is highly skewed), coordination failure (no collective action by high-risk individuals to resist discrimination), and structural coercion (no alternative to genetic testing for disease prevention in many contexts). Theater ratio (0.64): Moderate-high. Medical essentialism — the framing that genetic predisposition 'determines' disease — is substantially performative. Penetrance is low for most genetic risk variants (5-15% lifetime risk for BRCA1 carriers, for example); environmental factors usually dominate; polygenic risk scores have poor clinical accuracy. Yet genetic testing is marketed as deterministic, and genetic data is used for gatekeeping despite weak predictive power. The theater has increased because testing has become more granular (can now identify hundreds of low-penetrance variants) while clinical utility has not increased proportionally.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The high-risk individual sees pure extraction (snare) — genetic knowledge becomes a trap from which there is no exit. The low-income population sees mixed coordination and extraction (tangled rope) — genetic medicine enables disease prevention but simultaneously enables discrimination. The testing industry sees coordination (rope) — they are solving the legitimate problem of identifying heritable disease risks. Insurance gatekeepers see profitable risk stratification (snare, but experienced as rope from their perspective). Anti-discrimination frameworks see a temporary problem (scaffold) — genetic privacy laws and non-discrimination protections are building exit pathways while alternative risk assessment methods mature. The piton perspective reveals that medical essentialism has decoupled from function — genetic determinism is performatively maintained despite modest actual predictive power. The analytical observer risks seeing genomic information as a neutral coordination good (rope) and missing the predatory gatekeeping enabled by information asymmetry. The perspectival gap is driven by differential exit options: wealth enables escape from genetic discrimination; poverty locks individuals into the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agents' structural positions in the extraction flow. High-risk individuals have d ≈ 0.95 (full targets, trapped exit) producing high f(d) ≈ 1.42, maximizing experienced extraction (snare). Low-income populations have d ≈ 0.70 (partial targets, constrained exit) producing f(d) ≈ 1.05, moderate-high extraction (tangled rope). The genetic testing industry has d ≈ 0.05 (beneficiary, arbitrage exit) producing f(d) ≈ -0.12, negative/minimal extraction (rope). Insurance gatekeepers have d ≈ 0.35 (mixed beneficiary-target, mobile exit conditional on wealth) producing f(d) ≈ 0.35, moderate-high extraction (snare/rope boundary). The analytical observer has d ≈ 0.72 (observer, analytical exit) producing f(d) ≈ 1.15, moderate extraction, but risks misclassifying coordination as natural law. Directionality overrides are not needed; the structural data produces appropriate d values for most agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (χ ≈ 0.72 > 0.70): This constraint resolves mandatrophy by identifying the structural asymmetry that converts potential coordination into extraction. Genetic information itself (the biological data) has genuine coordination value: identifying disease risk, enabling preventive medicine, matching patients to effective treatments. But the socio-economic system built on genetic information is predatory: insurance companies use genetic data to deny coverage, employers use genetic information to avoid hiring liability, and populations are pressured into genetic testing under threat of future discrimination. The mandatrophy is resolved by separating the information (coordination good) from the institution (extraction mechanism). Alternative institutional designs would enable genetic coordination without extraction: public insurance (eliminates insurance gatekeeping), employment protections (prevents hiring discrimination), genetic literacy (reduces information asymmetry), collective bargaining (organizes high-risk individuals), and anti-eugenic reproductive ethics (prevents selection-based discrimination). The current constraint persists because institutional actors profit from extractive gatekeeping and have successfully naturalized genetic essentialism as inevitable — a false summit. The scaffold perspective and anti-discrimination frameworks provide evidence that alternative designs are possible, making the current extraction contingent rather than natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_penetrance_determination,
    'What threshold of penetrance and environmental modifiability determines when genetic information should be treated as predictive vs performative?',
    'Longitudinal cohort studies tracking genetic-risk individuals with and without preventive intervention; correlation analysis between genetic markers and actual health outcomes controlling for environmental factors',
    'If low penetrance dominates: genetic tests are largely theater (piton classification strengthens). If high penetrance: coordination benefits are real (rope classification strengthens). Current evidence shows mixed penetrance by condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_penetrance_determination, empirical, 'Determination of genetic penetrance and environmental modifiability thresholds').

omega_variable(
    discrimination_suppression_enforcement,
    'How effectively do genetic non-discrimination laws actually prevent insurance and employment discrimination against high-risk individuals?',
    'Audit studies with fictitious high-genetic-risk applicants; comparison of insurance denial rates before/after GINA passage; employee litigation analysis showing successful vs dismissed discrimination claims',
    'If enforcement is effective: suppression drops, classification shifts toward rope/scaffold. If enforcement is weak: suppression persists, snare classification confirmed. Current evidence suggests significant gaps in enforcement and regulatory scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discrimination_suppression_enforcement, empirical, 'Effectiveness of genetic non-discrimination law enforcement').

omega_variable(
    alternative_risk_stratification_sufficiency,
    'Do environmental health scores, lifestyle monitoring, and molecular phenotypes provide equally effective risk stratification without genetic information?',
    'Prospective studies comparing predictive power of genetic-only vs environmental-only vs integrated models; insurance and employment outcome studies using alternative stratification methods',
    'If alternative methods are sufficient: genetic information is optional (enables scaffold/piton sunset). If genetic is uniquely powerful: constraint becomes harder to escape (snare persists). Current evidence: modest incremental value of genetics beyond environment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_risk_stratification_sufficiency, empirical, 'Whether alternative risk stratification methods provide equivalent predictive power').

omega_variable(
    reproductive_autonomy_versus_health_duty,
    'Is genetic screening for reproductive decision-making a neutral information tool (rope) or an extraction mechanism enabling eugenic stigma (snare)?',
    'Qualitative research on reproductive decision-making post-genetic counseling; tracking of stigma, coercion, and autonomous choice; cross-cultural comparison of reproductive selection pressures and outcomes',
    'If autonomous choice dominates: screening is rope/coordination. If structural coercion dominates: screening is snare/extraction. Classification depends on whether the constraint enables freedom or restricts it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reproductive_autonomy_versus_health_duty, conceptual, 'Whether genetic reproductive screening enables autonomy or enforces eugenic extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_predisposition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genpred_tr_t0, genetic_predisposition, theater_ratio, 0, 0.42).
narrative_ontology:measurement(genpred_tr_t5, genetic_predisposition, theater_ratio, 5, 0.53).
narrative_ontology:measurement(genpred_tr_t10, genetic_predisposition, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(genpred_be_t0, genetic_predisposition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(genpred_be_t5, genetic_predisposition, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(genpred_be_t10, genetic_predisposition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_predisposition, resource_allocation).
narrative_ontology:affects_constraint(genetic_predisposition, insurance_risk_stratification).
narrative_ontology:affects_constraint(genetic_predisposition, employment_discrimination_mechanisms).
narrative_ontology:affects_constraint(genetic_predisposition, reproductive_coercion_systems).

% DUAL FORMULATION NOTE:
% Genetic predisposition is itself a Mountain (immutable biological fact). This constraint story addresses the socio-economic system built on genetic information. Upstream constraint: genetic_biology (Mountain, ε ≈ 0.05). Downstream constraints: insurance stratification, employment screening, reproductive selection pressures. The three downstream constraints form a constraint family — each is a distinct extraction mechanism using genetic data, but they share structural dependence on genetic information asymmetry and medical essentialism narratives. Affects relationships establish that genetic discrimination in insurance enables employment discrimination (insurability affects employability) and both enable reproductive coercion (health gatekeeping affects reproductive autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genetic_predisposition, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
