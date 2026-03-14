% ============================================================================
% CONSTRAINT STORY: amyloid_targeting_drug_approval
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amyloid_targeting_drug_approval, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: amyloid_targeting_drug_approval
 *   human_readable: Amyloid-Targeting Drug Approval Bottleneck
 *   domain: pharmaceutical/neurodegeneration/regulatory
 *
 * SUMMARY:
 *   The accelerated drug approval pathway for amyloid-targeting therapies in
 *   Alzheimer's disease creates a structural tension between early access for
 *   patients with progressive neurodegenerative disease and the epistemic and
 *   safety requirements for validating clinical benefit. The constraint
 *   exhibits the full spectrum of DR classifications depending on observer
 *   position. The pharmaceutical beneficiary experiences this as coordination
 *   (Rope) — a mechanism for getting therapies to patients. Early-stage
 *   patient advocates experience mixed coordination and extraction (Tangled
 *   Rope) — access comes with suppressed safety monitoring. Later-stage
 *   patients experience pure extraction (Snare) — they are trapped by disease
 *   progression while earlier cohorts receive treatments that may be
 *   clinically unproven. The regulatory system itself is degraded
 *   institutional theater (Piton) — biomarker endpoints substitute for
 *   clinical outcomes, post-market surveillance is slow and weak, and the
 *   system persists through inertia despite weak clinical evidence.
 *   Real-world evidence coalitions see this as a temporary coordination
 *   problem with sunset (Scaffold) — alternative verification pathways
 *   (adaptive trials, decentralized monitoring) will eventually replace
 *   accelerated approval. The analytical observer risks naturalizing this as
 *   an immutable feature of neurodegenerative disease research (Mountain),
 *   but the structural data reveals it as a contingent alignment of
 *   regulatory incentives, industry timelines, and patient hope.
 *
 * KEY AGENTS:
 *   - Later-Stage Alzheimer's Patients: Primary victim (powerless/trapped) — cannot exit constraint; must accept drugs with uncertain clinical benefit or progress untreated; disease timeline creates time-lock that prevents later access
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) — capture revenue and market position during extended approval periods; can exit via regulatory arbitrage across jurisdictions
 *   - Early-Stage Patient Advocates: Secondary beneficiary (moderate/constrained) — benefit from accelerated access but trapped by pharmaceutical industry dependence; must suppress safety concerns to maintain funding
 *   - Real-World Evidence Coalition: Organized actors (organized/mobile) — academic centers, patient registries building alternative verification pathways with sunset logic
 *   - Regulatory Approval System: Institutional actor (institutional/arbitrage) — maintains performative biomarker verification while post-market safety monitoring remains weak
 *   - Patient Safety Monitoring Systems: Victim (institutional/trapped) — adverse event detection lags months to years behind disease progression; system constraints prevent timely signal detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amyloid_targeting_drug_approval, 0.52).
domain_priors:suppression_score(amyloid_targeting_drug_approval, 0.68).
domain_priors:theater_ratio(amyloid_targeting_drug_approval, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amyloid_targeting_drug_approval, extractiveness, 0.52).
narrative_ontology:constraint_metric(amyloid_targeting_drug_approval, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(amyloid_targeting_drug_approval, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amyloid_targeting_drug_approval, tangled_rope).
narrative_ontology:human_readable(amyloid_targeting_drug_approval, "Amyloid-Targeting Drug Approval Bottleneck").
narrative_ontology:topic_domain(amyloid_targeting_drug_approval, "pharmaceutical/neurodegeneration/regulatory").

domain_priors:requires_active_enforcement(amyloid_targeting_drug_approval).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amyloid_targeting_drug_approval, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(amyloid_targeting_drug_approval, early_stage_patient_advocates).
narrative_ontology:constraint_victim(amyloid_targeting_drug_approval, later_stage_patients).
narrative_ontology:constraint_victim(amyloid_targeting_drug_approval, patient_safety_monitoring_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATER-STAGE PATIENT (SNARE) — Trapped by disease progression timeline and regulatory sequencing. Cannot exit the constraint; must either accept amyloid-targeting drugs with uncertain safety profiles or progress without treatment. Bears full extraction cost: early-access cohorts receive therapies while later-stage patients wait through approval cycles, by which time they are often ineligible due to disease severity thresholds. No alternatives and no agency.
constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PHARMACEUTICAL COMPANY (ROPE) — Net beneficiary (institutional/arbitrage). Experiences constraint as pure coordination: rapid approval pathways, accelerated review programs, and expanded access programs serve legitimate role of getting drugs to patients while maintaining safety oversight. Can exit via alternative regulatory jurisdictions if domestic approval slows. Extraction runs toward this agent.
constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY-STAGE ADVOCATE (TANGLED ROPE) — Mixed position. Benefits from accelerated access programs that enable treatment at optimal disease stage; but also trapped by the constraint structure: advocates must endorse expanded access to gain access, which suppresses safety monitoring and creates extraction for later-stage cohorts. Moderate power, constrained by funding and visibility dependence on pharmaceutical industry partnerships.
constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REAL-WORLD EVIDENCE COALITION (SCAFFOLD) — Organized actors (academic centers, patient registries, decentralized trial networks) see accelerated approval as temporary coordination problem with sunset: post-marketing surveillance infrastructure, real-world evidence databases, and adaptive trial designs are creating alternative verification pathways. Organized agents have mobility and agency. Sunset logic: as real-world evidence systems mature, the regulatory gap between accelerated and traditional approval narrows.
constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPROVAL SYSTEM (PITON) — The accelerated approval pathway itself is degraded institutional theater. Biomarker endpoints (amyloid PET reduction, phosphorylated tau) have weak correlation with clinical outcomes; yet approval relies heavily on these proxy metrics. Post-market safety monitoring is performative: adverse event reporting systems are slow, causal attribution is difficult, and withdrawal from market is rare even when safety concerns emerge. The system persists through inertia despite declining functional verification of clinical benefit.
constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some approval lag is inherent to neurodegenerative disease research: cognitive endpoints are hard to measure, patient heterogeneity is extreme, and the gap between biomarker changes and clinical outcomes is a structural feature of AD biology, not regulatory choice. This perspective naturalizes the approval bottleneck as an immutable property of the disease domain. However, the structural data contradicts this classification — the engine will flag this as a false summit, revealing that 'inherent disease complexity' naturalizes what is actually a contingent alignment of incentives between regulators, industry, and patient advocates.
constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amyloid_targeting_drug_approval_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amyloid_targeting_drug_approval, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(amyloid_targeting_drug_approval, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(amyloid_targeting_drug_approval, TR),
    TR >= 0.70.

:- end_tests(amyloid_targeting_drug_approval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint extracts from later-stage patients through three mechanisms: (1) disease stage eligibility criteria that screen in responders and screen out those most desperate, (2) approval timeline that advances early-stage cohorts while later-stage patients wait, and (3) biomarker theater that substitutes for clinical outcome verification, allowing drugs with uncertain clinical benefit to be approved and marketed. The rising trajectory (0.28 → 0.52) reflects increasing reliance on biomarker proxies and expanding patient access programs that depend on reduced post-market surveillance. Suppression (0.68): High. Barriers to exit include: disease progression timeline (patients cannot wait indefinitely), cognitive decline (patients cannot participate in standard trials once disease advances), regulatory stage-eligibility criteria that lock in early-stage preference, and information asymmetry (biomarker changes are visible to regulators while clinical outcomes are diffuse and long-term). Theater ratio (0.64): Moderate-high and rising. The system uses biomarker endpoints (amyloid PET reduction, tau pathology changes) as proxy validation while the actual clinical endpoint — cognitive and functional decline — remains weakly correlated. Post-market safety monitoring is performative: adverse event reporting relies on healthcare provider vigilance in patients often seen by multiple specialists; causal attribution is difficult; withdrawal from market is rare even when safety signals emerge. The rising trajectory (0.42 → 0.64) reflects increasing substitution of biomarker theater for clinical verification as more drugs enter the space.
 *
 * PERSPECTIVAL GAP:
 *   The pharmaceutical company sees this as pure coordination (Rope) — a mechanism enabling therapy delivery. Early-stage advocates see mixed coordination and extraction (Tangled Rope) — access comes with embedded asymmetry. Later-stage patients see pure extraction (Snare) — the system extracts their disease progression uncertainty as the price of early-stage access. The regulatory system sees its own ritual as degraded (Piton) — biomarker endpoints are weak proxies; post-market safety is theater; the system persists because alternatives haven't yet matured. Real-world evidence coalitions see temporary problem with exit path (Scaffold) — adaptive trials and decentralized monitoring will eventually provide verification without the delay. The analytical observer risks seeing natural law (Mountain) — 'neurodegenerative diseases are inherently hard to measure' — but this naturalizes what is actually a regulatory choice: to prioritize speed and industry incentives over verification precision, justified by disease urgency.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical companies (beneficiary/institutional/arbitrage) experience low d and negative effective extraction — the system benefits them through expanded access programs, reduced trial requirements, and rapid market entry. Later-stage patients (victim/powerless/trapped) experience high d and maximum effective extraction — they have no alternatives, no exit options, and cannot organize (individually scattered across healthcare systems). Early-stage advocates (moderate/constrained) experience intermediate d because they benefit from accelerated access (lowering d toward beneficiary) but also bear extraction risk through suppressed safety data (raising d toward victim). The deriv chain produces different χ values for each agent despite identical base extractiveness, reflecting their different structural positions relative to the approval flow. Real-world evidence organizations (organized/mobile) experience reduced d because they have exit options (can build parallel verification systems) and organizational capacity (can mobilize resources), placing them near the rope/scaffold boundary rather than snare extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR TANGLED ROPE: This constraint demonstrates why tangled rope differs from both rope and snare. A pure rope (like a consensus safety standard) would coordinate without extracting — all participants benefit from the standard. A pure snare (like predatory lending) would extract without coordinating — the beneficiary gains only at others' expense. The amyloid pathway is tangled rope because: (1) it genuinely coordinates therapy delivery to early-stage patients who would benefit (coordination function is real), but (2) it does this through mechanisms (accelerated approval, biomarker theater, weak post-market surveillance) that systematically extract from later-stage patients and safety monitoring systems (asymmetric extraction is real), and (3) both functions are required for the constraint to exist. Remove the coordination function and you lose the apparent legitimacy; remove the extraction and you lose the mechanism that enables speed. Tangled rope is the only type that captures this hybrid logic. The mandatrophy is resolved by recognizing that the classification must reflect BOTH functions or it's false. A benign 'accelerated access pathway' framing (rope) ignores the systematic harm to later-stage cohorts. A purely malicious 'patient exploitation' framing (snare) ignores that early-stage patients genuinely benefit from speed. Tangled rope holds both truths simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amyloid_biomarker_sufficiency,
    'Do amyloid biomarker changes (PET reduction, tau pathology decline) constitute adequate evidence of clinical benefit, or are they proxy metrics masking persistent cognitive decline?',
    'Longitudinal comparison of amyloid reduction magnitude vs clinical cognitive slope; meta-analysis of post-market cognitive outcomes in patients treated under accelerated approval vs those in standard trial cohorts; autopsy data on amyloid clearance vs neurodegeneration pathology',
    'If biomarkers are sufficient: the accelerated pathway provides genuine early treatment benefit (rope/scaffold classification upheld). If biomarkers decouple from clinical outcomes: the constraint is extractive (snare/tangled_rope classification strengthened), using patient hope and biomarker theater to justify expanded access while clinical benefit remains unproven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amyloid_biomarker_sufficiency, empirical, 'Whether amyloid biomarkers predict clinical cognitive outcomes').

omega_variable(
    safety_monitoring_detection_lag,
    'What is the median time between drug administration and adverse event detection in post-market surveillance systems for amyloid-targeting monoclonals? How does this lag compare to the disease progression window?',
    'Analysis of FDA adverse event database (FAERS) signal detection timelines; comparison to known pharmacokinetics and disease progression rates; audit of post-market safety commitments vs actual monitoring intensity',
    'If detection lag < 6 months and disease window > 5 years: safety system is functional. If detection lag > 18 months: safety monitoring is theater, adverse events are suppressed during window when early adopters still benefit, and later-stage patients bear discovery cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safety_monitoring_detection_lag, empirical, 'Time lag in post-market adverse event detection').

omega_variable(
    patient_stage_selection_bias,
    'Are early-stage patients preferentially enrolled in accelerated approval programs while later-stage patients are excluded, creating artificial efficacy signal through selection rather than drug effect?',
    'Comparison of baseline disease severity, cognitive reserve, and comorbidity profiles in accelerated approval cohorts vs traditional trial arms; disease stage distribution in expanded access programs vs patient registry populations seeking treatment',
    'If strong selection bias exists: the apparent efficacy is artifact of patient stratification, not drug benefit. The snare classification is confirmed — later-stage patients are trapped by stage-based eligibility criteria that screen in responders and screen out those most desperate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_stage_selection_bias, empirical, 'Disease stage selection in accelerated approval cohorts').

omega_variable(
    regulatory_arbitrage_incentives,
    'Do international regulatory differences (EMA vs FDA vs China approval standards) create incentives for pharmaceutical companies to pursue approval in the most lenient jurisdiction first, creating expectation effect in more conservative jurisdictions?',
    'Timeline analysis of regulatory applications across jurisdictions; correlation between approval in one jurisdiction and subsequent approval decisions in others; evidence of patient expectation pressure following earlier approval elsewhere',
    'If arbitrage incentives are strong: the constraint is institutional (arbitrage exit for beneficiary is real), but the extraction mechanism targets patients in conservative-regulation jurisdictions who wait longer. Tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_incentives, empirical, 'Regulatory arbitrage effects on approval sequencing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amyloid_targeting_drug_approval, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amyloid_tr_t0, amyloid_targeting_drug_approval, theater_ratio, 0, 0.42).
narrative_ontology:measurement(amyloid_tr_t3, amyloid_targeting_drug_approval, theater_ratio, 3, 0.51).
narrative_ontology:measurement(amyloid_tr_t6, amyloid_targeting_drug_approval, theater_ratio, 6, 0.61).
narrative_ontology:measurement(amyloid_tr_t9, amyloid_targeting_drug_approval, theater_ratio, 9, 0.64).

% Extraction over time
narrative_ontology:measurement(amyloid_be_t0, amyloid_targeting_drug_approval, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(amyloid_be_t3, amyloid_targeting_drug_approval, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(amyloid_be_t6, amyloid_targeting_drug_approval, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(amyloid_be_t9, amyloid_targeting_drug_approval, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amyloid_targeting_drug_approval, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(amyloid_targeting_drug_approval, 0.12).
narrative_ontology:affects_constraint(amyloid_targeting_drug_approval, biomarker_proxy_validation).
narrative_ontology:affects_constraint(amyloid_targeting_drug_approval, post_market_safety_monitoring).
narrative_ontology:affects_constraint(amyloid_targeting_drug_approval, neurodegenerative_clinical_trial_design).

% DUAL FORMULATION NOTE:
% The amyloid targeting drug approval constraint is upstream of specific biomarker proxy sufficiency claims (does amyloid reduction predict cognitive outcomes?) and post-market safety monitoring infrastructure. Each downstream constraint has its own ε reflecting empirical status of the specific claim; the approval bottleneck has its own ε reflecting the career and regulatory incentive asymmetry between early-stage and later-stage patient cohorts. Decomposed as constraint family: approval_bottleneck (this story, ε=0.52) → biomarker_proxy_validation (ε=0.68, snare of therapeutic optimism) → post_market_surveillance (ε=0.45, tangled rope of liability vs patient access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amyloid_targeting_drug_approval, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
