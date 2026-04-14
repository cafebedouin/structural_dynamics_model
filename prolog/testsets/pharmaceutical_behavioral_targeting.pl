% ============================================================================
% CONSTRAINT STORY: pharmaceutical_behavioral_targeting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_behavioral_targeting, []).

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
 *   constraint_id: pharmaceutical_behavioral_targeting
 *   human_readable: Pharmaceutical Behavioral Targeting and Prescription Influence
 *   domain: healthcare/pharmaceutical_marketing
 *
 * SUMMARY:
 *   Pharmaceutical behavioral targeting represents a structural extraction
 *   mechanism that operates through multiple coupled channels: algorithmic ad
 *   delivery targeting prescribing physicians, patient behavioral profiling
 *   integrated into healthcare delivery systems, and data brokerage that
 *   monetizes health information. Unlike traditional pharmaceutical marketing
 *   (sales representatives, journal advertising, sponsored conferences),
 *   behavioral targeting operates at scale through opaque algorithmic systems
 *   that patients and physicians cannot audit or exit. The constraint
 *   combines pure extraction (snare) with some coordination functions
 *   (information efficiency for payers and manufacturers) and regulatory
 *   theater (rules that existed before algorithmic systems evolved). The
 *   extractiveness has accelerated over the interval (0.35 → 0.68 over 10
 *   years) as behavioral data sources have integrated into electronic health
 *   records and advertising platforms have achieved fine-grained physician
 *   and patient profiling. The theater_ratio has also increased (0.30 → 0.58)
 *   as regulatory compliance becomes more performative relative to actual
 *   oversight capacity.
 *
 * KEY AGENTS:
 *   - Patients: Primary victims (powerless/trapped) — cannot detect or audit behavioral targeting; healthcare is not optional; exit requires abandoning access to care system
 *   - Prescribing Physicians: Secondary victims (moderate/constrained) — face professional targeting through CME, speaker programs, algorithmic ad placement in EHRs; career dependence constrains exit
 *   - Pharmaceutical Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture marketing efficiency gains, higher prescription rates for on-patent drugs regardless of clinical evidence; high exit optionality across platforms and regulatory jurisdictions
 *   - Healthcare Payers / PBMs: Complex position (powerful/mobile) — benefit from some targeting efficiency but bear costs from preferential prescribing of expensive drugs; tangled rope from cost control vs efficiency tension
 *   - Healthcare Data Brokers: Secondary beneficiaries (institutional/arbitrage) — monetize patient and physician behavioral data; pure intermediation role
 *   - Regulatory Agencies (FDA/FTC): Institutional actors (institutional/arbitrage) — enforcement capacity degraded relative to technological evolution; piton classification from regulatory theater
 *   - Physician Advocacy Groups: Organized agents (organized/constrained) — partly captured by pharmaceutical relationships; have some agency (guidelines, lobbying) but face structural asymmetry
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees pure extraction mechanism disguised as healthcare optimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_behavioral_targeting, 0.68).
domain_priors:suppression_score(pharmaceutical_behavioral_targeting, 0.72).
domain_priors:theater_ratio(pharmaceutical_behavioral_targeting, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_behavioral_targeting, extractiveness, 0.68).
narrative_ontology:constraint_metric(pharmaceutical_behavioral_targeting, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(pharmaceutical_behavioral_targeting, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_behavioral_targeting, snare).
narrative_ontology:human_readable(pharmaceutical_behavioral_targeting, "Pharmaceutical Behavioral Targeting and Prescription Influence").
narrative_ontology:topic_domain(pharmaceutical_behavioral_targeting, "healthcare/pharmaceutical_marketing").

domain_priors:requires_active_enforcement(pharmaceutical_behavioral_targeting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_behavioral_targeting, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_behavioral_targeting, digital_marketing_platforms).
narrative_ontology:constraint_beneficiary(pharmaceutical_behavioral_targeting, healthcare_data_brokers).
narrative_ontology:constraint_victim(pharmaceutical_behavioral_targeting, prescribing_physicians).
narrative_ontology:constraint_victim(pharmaceutical_behavioral_targeting, patient_autonomy).
narrative_ontology:constraint_victim(pharmaceutical_behavioral_targeting, healthcare_cost_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT EPISTEMIC CLOSURE (SNARE) — Patients cannot escape pharmaceutical targeting because it operates through mechanisms they cannot detect or audit: algorithmic ad personalization, physician-directed marketing that influences prescribing decisions, and behavioral data collection integrated into healthcare delivery itself. The patient is trapped in the extraction without visibility into how their behavioral data drives treatment recommendations. Exit requires abandoning access to healthcare systems, which is not viable.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRESCRIBING PHYSICIAN (SNARE) — Physicians face professional targeting through continuing medical education (CME), sponsored research, speaker programs, and now algorithmic ad campaigns deployed through healthcare portals and electronic health records (EHR) systems. They experience suppression through information asymmetry: pharmaceutical companies have better data on drug efficacy and safety than many practicing physicians do. Exit is constrained (career dependence on CME, research collaboration, patient demand shaped by direct-to-consumer marketing) but not fully trapped. High experienced extraction from this perspective.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences the targeting system as coordination: identifying physicians likely to prescribe, matching patients to drugs through behavioral inference, and automating the personalization of marketing. Net beneficiary with high exit optionality (can shift to different platforms, adjust strategy, leverage regulatory arbitrage across jurisdictions). The manufacturer sees the constraint as solving a market efficiency problem — connecting the right drug to the right customer. From this position, it is coordination with asymmetric information advantage.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEALTHCARE PAYER (TANGLED ROPE) — Insurance companies and pharmacy benefit managers (PBMs) benefit from behavioral targeting (it helps them route patients to formulary drugs) but also bear costs (higher premiums when expensive on-patent drugs are preferentially prescribed; moral hazard from targeting unhealthy populations into high-cost treatment paths). They have some exit options (formulary tiers, prior authorization, negotiating rebates) but are locked into the behavioral data ecosystem to compete. Mixed extraction and coordination — the constraint serves both functions.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PHYSICIAN ADVOCACY GROUPS (TANGLED ROPE) — Medical societies and physician organizations have some agency (can establish prescribing guidelines, push for transparency in pharma relationships, lobby for regulation) but are partly captured by pharmaceutical relationships (research funding, conference sponsorship, CME underwriting). The constraint includes coordination functions (establishing medical standards) alongside extraction (being leveraged as proxies for pharma influence). Organized agents see both dimensions — they have agency but face structural asymmetry in resources and information.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY OVERSIGHT (PITON) — FDA and FTC regulation of pharmaceutical marketing is substantially degraded and performative. Regulations exist (restrictions on direct-to-consumer advertising, physician relationship transparency rules like Physician Payments Sunshine Act) but enforcement is weak and the rules were written before algorithmic behavioral targeting existed. The regulatory system maintains theater (compliance documentation, transparency reporting) while the extraction mechanisms (behavioral data, EHR-integrated targeting, algorithmic CME placement) evolve beyond the framework's reach. Piton classification derives from high theater_ratio and atrophied functional capacity.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, pharmaceutical behavioral targeting is a pure extraction mechanism disguised as healthcare optimization. Patients cannot opt out (healthcare is mandatory for livability). Physicians cannot opt out (professional credentialing and income depend on participating in the system). The beneficiary (pharmaceutical manufacturers) has perfect information advantage and active enforcement through data collection integrated into healthcare infrastructure itself. Analytical perspective sees the constraint as a snare with maximal suppression and no genuine coordination function — the 'efficiency' framing is false naturalization.
constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_behavioral_targeting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_behavioral_targeting, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_behavioral_targeting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_behavioral_targeting, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_behavioral_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Pharmaceutical manufacturers extract significant value through behavioral targeting by achieving higher prescription rates for on-patent (high-cost) drugs and by consolidating price power through demand generation and physician influence. The value extraction is not maximal (0.90+) because some targeting genuinely improves treatment matching, but the dominant function is capture of prescribing decisions independent of clinical necessity. Suppression (0.72): Very high. Patients cannot detect algorithmic influence on treatment recommendations embedded in healthcare delivery. Physicians face information asymmetry (manufacturers have better efficacy/safety data than many practitioners). Data flows are opaque — behavioral profiles are built without consent frameworks that provide real transparency. Regulatory rules (Sunshine Act, DTC advertising restrictions) create theater but do not constrain algorithmic targeting. Exit is blocked by structural dependence on healthcare systems. Theater ratio (0.58): Moderate-high and increasing. FDA/FTC oversight exists (compliance documentation, transparency reporting) but rules predate algorithmic systems and enforcement is weak. The therapy itself (behavioral targeting optimization) involves high theater — the framing as 'healthcare efficiency' masks extraction. The constraint has accelerated extraction (from 0.35 to 0.68) as data integration deepened and theater increased (from 0.30 to 0.58) as regulatory response lagged.
 *
 * PERSPECTIVAL GAP:
 *   Pharmaceutical manufacturers perceive rope (coordination) because from their position the system solves a real problem (matching products to customers efficiently). Physicians perceive tangled rope (mixed coordination and professional capture) because they experience both coordination benefit (better information for prescribing) and extraction (bias toward expensive drugs, career dependence on pharmaceutical relationships). Patients perceive snare (pure extraction) because they have no agency in the system and cannot audit how behavioral profiles influence treatment. The analytical observer perceives snare with high confidence because from a civilizational perspective the extraction mechanism is clear: patients are trapped in healthcare dependence, physicians are professionally captured, and the system funnels them toward high-cost medications independent of clinical evidence. The gap between beneficiary perception (rope/coordination) and victim perception (snare/extraction) is the core diagnostic of this constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness chi is computed from base extractiveness (ε = 0.68), their directionality d (derived from structural position), and scope modifier (σ = 1.0 for national scope). Pharmaceutical manufacturers are beneficiaries with high exit optionality (arbitrage) — their d ≈ 0.10, giving f(d) ≈ -0.05, so chi ≈ -0.03 (negative extraction, they benefit). Prescribing physicians are constrained targets — their d ≈ 0.70, giving f(d) ≈ 1.00, so chi ≈ 0.68 (experience full base extraction). Patients are trapped — their d ≈ 0.95, giving f(d) ≈ 1.40, so chi ≈ 0.95 (experience amplified extraction beyond base). The snare classification depends on both high base extraction ε and high d for trapped agents producing high chi — the numerical structure enforces the snare type because suppression is high and victims are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint resolves the mandatrophy (false natural law risk) by distinguishing between coordination and extraction through structural analysis. The 'healthcare optimization' framing naturalizes pharmaceutical behavioral targeting as inevitable efficiency. But the structural data reveals contingency: the system benefits pharmaceutical manufacturers, extraction targets patients and physicians, suppression is high, and victims are trapped with no exit options. The mandatrophy is resolved by showing that the apparent coordination (efficiency) is asymmetric extraction. A true coordination mechanism would benefit all parties (rope) or explicitly trade off benefits and costs with transparent negotiation (tangled rope with agency). Here, transparency is minimal, exit is blocked, and asymmetry favors manufacturers. The snare classification is therefore correct and the natural law framing is false. The mandatrophy is further resolved by the regulatory piton perspective — rules exist (theater) but do not constrain the mechanism, indicating that the 'oversight solves the problem' narrative is also false. The constraint requires structural change, not just regulatory tinkering.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_targeting_detection_gap,
    'Can patients and physicians detect and audit how their behavioral data influences prescription decisions?',
    'Transparency testing: request algorithmic impact assessments from EHR vendors and pharmaceutical data brokers; track correlation between behavioral profiles and prescription recommendations; longitudinal audit of prescribing patterns before/after behavioral data access',
    'If detection gap persists: suppression is structural and constrains both patients and physicians. If detection becomes routine: some suppression mechanism weakens and exit options improve (constrained rather than trapped). The snare classification depends on this gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_targeting_detection_gap, empirical, 'Whether patients and physicians can detect algorithmic prescription influence').

omega_variable(
    physician_autonomy_substitution,
    'Does behavioral targeting of physicians constitute evidence-based information sharing or evidence-obscuring extraction?',
    'Comparison of pharma-targeted vs non-targeted physicians: prescribing variance, outcomes, adherence to guideline-recommended drugs vs on-patent alternatives; analysis of CME content sponsored vs unsponored; pharmacovigilance data for targeting-driven prescribing errors',
    'If targeted physicians prescribe evidence-based drugs at equivalent rates: the coordination function is real and the rope/tangled_rope classifications gain weight. If targeted physicians show systematic bias toward expensive/on-patent drugs regardless of evidence: the snare classification is confirmed and extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_autonomy_substitution, empirical, 'Whether pharmaceutical targeting drives evidence-based prescribing or evidence-obscuring extraction').

omega_variable(
    regulatory_capture_depth,
    'Is FDA/FTC inability to regulate behavioral targeting a resource constraint or a structural capture of regulatory agencies by pharmaceutical industry?',
    'Historical analysis of regulatory rule-making timelines vs technological development; audit of FDA/FTC advisory board composition and pharmaceutical funding patterns; comparative analysis of behavioral targeting enforcement across different regulatory jurisdictions (EU vs US)',
    'If resource constraint: piton classification is accurate (degraded but potentially remediable). If structural capture: the regulatory framework is itself a snare mechanism (apparent oversight masks actual facilitation). The constraint would extend upward to include the regulatory agency as a controlled actor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Whether regulatory degradation is resource-constrained or structurally captured').

omega_variable(
    data_broker_intermediation_necessity,
    'Is the involvement of healthcare data brokers a necessary coordination layer or pure extractive intermediation?',
    'Analysis of data flow: do data brokers enable genuine coordination between payers, providers, and patients, or merely multiply extraction points without coordination benefit? Comparison of healthcare outcomes in jurisdictions with different data broker regulations',
    'If necessary coordination: beneficiaries may include patients (through better matched treatments) and the constraint classification shifts toward tangled rope from more perspectives. If pure intermediation: the data broker layer is pure extraction rent and suppression is higher than measured (hidden via intermediation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_broker_intermediation_necessity, empirical, 'Whether healthcare data brokers provide coordination or pure extraction').

omega_variable(
    direct_to_consumer_demand_generation,
    'How much of patient behavioral targeting operates through direct-to-consumer (DTC) advertising driving patient demand vs through physician targeting driving supply?',
    'Decompose behavioral targeting into patient-side (DTC ads, social media targeting) and physician-side (CME, EHR-integrated ads, peer influence); measure relative extraction contribution from each pathway',
    'If patient-side dominates: constraint involves identity_locked patient agent (patient identity/health narrative is captured by pharmaceutical framing). If physician-side dominates: constraint is primarily professional capture. The decomposition may indicate need for separate constraint stories per pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_to_consumer_demand_generation, empirical, 'Relative contribution of patient-side vs physician-side behavioral targeting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_behavioral_targeting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_behav_tr_t0, pharmaceutical_behavioral_targeting, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pharma_behav_tr_t5, pharmaceutical_behavioral_targeting, theater_ratio, 5, 0.45).
narrative_ontology:measurement(pharma_behav_tr_t10, pharmaceutical_behavioral_targeting, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pharma_behav_tr_t3, pharmaceutical_behavioral_targeting, theater_ratio, 3, 0.38).
narrative_ontology:measurement(pharma_behav_tr_t8, pharmaceutical_behavioral_targeting, theater_ratio, 8, 0.53).

% Extraction over time
narrative_ontology:measurement(pharma_behav_be_t0, pharmaceutical_behavioral_targeting, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pharma_behav_be_t5, pharmaceutical_behavioral_targeting, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(pharma_behav_be_t10, pharmaceutical_behavioral_targeting, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(pharma_behav_be_t2, pharmaceutical_behavioral_targeting, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(pharma_behav_be_t7, pharmaceutical_behavioral_targeting, base_extractiveness, 7, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_behavioral_targeting, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_behavioral_targeting, drug_pricing_monopoly).
narrative_ontology:affects_constraint(pharmaceutical_behavioral_targeting, direct_to_consumer_pharmaceutical_advertising).
narrative_ontology:affects_constraint(pharmaceutical_behavioral_targeting, physician_professional_capture).

% DUAL FORMULATION NOTE:
% Pharmaceutical behavioral targeting decomposes into patient-side (DTC advertising, social media targeting, health data profiling) and physician-side (CME targeting, EHR-integrated ads, professional relationship capture) constraints. This story addresses the integrated system; decomposition into separate stories may be warranted if the two pathways show significantly different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_behavioral_targeting, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
