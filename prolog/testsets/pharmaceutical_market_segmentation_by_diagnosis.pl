% ============================================================================
% CONSTRAINT STORY: pharmaceutical_market_segmentation_by_diagnosis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_market_segmentation_by_diagnosis, []).

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
 *   constraint_id: pharmaceutical_market_segmentation_by_diagnosis
 *   human_readable: Pharmaceutical Market Segmentation By Diagnosis
 *   domain: pharmaceutical_policy/healthcare_economics
 *
 * SUMMARY:
 *   Pharmaceutical market segmentation by diagnosis creates a structural
 *   hybrid between genuine clinical coordination and extractive rent-seeking.
 *   Diagnostic segmentation enables legitimate precision medicine — matching
 *   treatments to disease subtypes based on biomarkers, genetic profiles, and
 *   clinical phenotypes — but simultaneously enables pharmaceutical
 *   manufacturers to narrow markets, restrict prescriber discretion, and
 *   extract rents from patients and payers unable to access treatment outside
 *   formal diagnostic boundaries. The constraint operates through diagnostic
 *   classification systems (ICD-10, CPT codes, FDA-approved indications) that
 *   simultaneously serve clinical coordination functions and market
 *   gatekeeping functions. Over the interval 0-20 (approximately 2004-2024),
 *   extractiveness has increased from 0.35 to 0.58 as pharmaceutical
 *   manufacturers have moved from broad therapeutic categories to narrow
 *   biomarker-defined populations, concentrating pricing power in smaller,
 *   wealthier, more diagnosable patient groups. Theater ratio has increased
 *   from 0.42 to 0.64 as diagnostic classification has become increasingly
 *   performative — diagnostic codes now function primarily as billing/access
 *   proxies rather than clinical communication devices. The constraint
 *   exhibits all six classification types depending on observer position,
 *   with the greatest tension between the manufacturer's rope experience
 *   (genuine coordination enabling precision medicine) and the trapped
 *   patient's snare experience (formal diagnostic exclusion from access).
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — capture market definition power, pricing discretion, and competitive moat through proprietary diagnostic segmentation
 *   - Patients Without Formal Diagnosis: Primary victim (powerless/trapped) — cannot access segmented medications; symptomatic individuals excluded by diagnostic gatekeeping
 *   - Healthcare System Payers: Secondary beneficiary and victim (institutional/constrained) — benefit from clear cost allocation and evidence-based coverage criteria; face extraction through highest-cost medications concentrated in narrow populations and formulary management overhead
 *   - Prescribers: Secondary victim (moderate/constrained) — experience therapeutic restriction, prior authorization burden, but benefit from structured clinical decision support and evidence-based pathways
 *   - Diagnostic Test Manufacturers: Secondary beneficiary (institutional/mobile) — profit from requirement to validate diagnosis before medication access; have some exit optionality through test repositioning
 *   - Population-Level Health Systems: Diffuse victim (powerless/trapped) — systematic treatment gap in populations whose disease presentations diverge from marketed diagnostic criteria; intergenerational extraction
 *   - Academic Medicine and Diagnostic Innovation: Organized agent (organized/mobile) — pushing for interoperable biomarker standards and real-world evidence platforms as alternatives to proprietary segmentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_market_segmentation_by_diagnosis, 0.58).
domain_priors:suppression_score(pharmaceutical_market_segmentation_by_diagnosis, 0.68).
domain_priors:theater_ratio(pharmaceutical_market_segmentation_by_diagnosis, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_market_segmentation_by_diagnosis, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_market_segmentation_by_diagnosis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pharmaceutical_market_segmentation_by_diagnosis, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_market_segmentation_by_diagnosis, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_market_segmentation_by_diagnosis, "Pharmaceutical Market Segmentation By Diagnosis").
narrative_ontology:topic_domain(pharmaceutical_market_segmentation_by_diagnosis, "pharmaceutical_policy/healthcare_economics").

domain_priors:requires_active_enforcement(pharmaceutical_market_segmentation_by_diagnosis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_market_segmentation_by_diagnosis, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_market_segmentation_by_diagnosis, specialty_pharmacy_networks).
narrative_ontology:constraint_beneficiary(pharmaceutical_market_segmentation_by_diagnosis, formulary_managers).
narrative_ontology:constraint_victim(pharmaceutical_market_segmentation_by_diagnosis, patients_without_diagnosis_validation).
narrative_ontology:constraint_victim(pharmaceutical_market_segmentation_by_diagnosis, prescribers_with_limited_alternatives).
narrative_ontology:constraint_victim(pharmaceutical_market_segmentation_by_diagnosis, healthcare_systems_bearing_costs).
narrative_ontology:constraint_victim(pharmaceutical_market_segmentation_by_diagnosis, diagnostic_outsider_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDIAGNOSED PATIENT (SNARE) — Patient with symptoms matching a marketed diagnosis but lacking formal diagnostic validation. Trapped by inability to access treatment without diagnosis code; bears full extraction cost while pharmaceutical benefit constraints remain opaque. No exit option: cannot obtain medication without institutional validation; cannot challenge the segmentation framework without medical credentials.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POPULATIONS WITHOUT MARKET-RECOGNIZED DIAGNOSES (SNARE) — Geographic, ethnic, or socioeconomic populations whose disease presentations or diagnostic practices diverge from marketed diagnostic criteria. Extraction flow is structural and intergenerational: market segmentation based on profitable diagnoses systematically excludes populations whose disease burden doesn't align with pharmaceutical market incentives. No exit; systematic under-treatment.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PRESCRIBER (TANGLED ROPE) — Physician benefits from diagnostic segmentation (structured, evidence-based treatment pathways; clinical decision support; biomarker-driven precision medicine) but faces extraction through restricted formularies, prior authorization requirements, and therapeutic substitution mandates. Constrained by employment, patient population, and institutional formulary decisions; limited alternatives for off-label or emerging diagnoses. Mixed coordination-extraction hybrid.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURER (ROPE) — Primary beneficiary. Experiences market segmentation as a coordination mechanism that enables precision targeting, market protection, and pricing power. Diagnostic segmentation aligns revenue with clinical efficacy claims. Arbitrage exit available: product repositioning, indication expansion, new market entry. Net positive: constraint subsidizes innovation incentives and market definition.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HEALTHCARE SYSTEM PAYER (TANGLED ROPE) — Benefits from diagnostic segmentation (clear cost allocation, evidence-based coverage criteria, reduced off-label prescribing) but faces extraction through highest-cost medications concentrated in narrow diagnostic populations, prior authorization burden, and formulary management overhead. Constrained by regulatory requirements, patient advocacy pressure, and network lock-in. Active enforcement required to maintain segmentation boundaries.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN BIOMARKER & DIAGNOSTIC ALLIANCE (SCAFFOLD) — Patient advocacy groups, academic medicine, and regulatory reformers pushing for decoupling pharmaceutical access from proprietary diagnostic segmentation. See diagnostic segmentation as a temporary coordination solution being replaced by interoperable biomarker platforms, real-world evidence databases, and algorithmic treatment matching. Sunset mechanism: as diagnostic standards commoditize and biomarker databases open, the pharmaceutical manufacturer's monopoly on diagnostic definition decays. Estimated sunset: 15-25 years for mature interoperable diagnostics platforms.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY DIAGNOSTIC CLASSIFICATION (PITON) — ICD-10, CPT, and DSM-5 diagnostic codes were originally designed for billing transparency and clinical communication. Their current role as gatekeepers for pharmaceutical access is largely performative — the codes have become proxies for market segmentation rather than descriptors of clinical reality. Theater ratio high because diagnostic classification performs clinical legitimacy (evidence-based medicine appearance) while enabling rent extraction (market segmentation). Regulatory inertia: alternatives exist (continuous biomarker scores, real-world phenotypes) but institutional lock-in maintains the code-based system.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE UNIVERSALITY) — Risks naturalizing market segmentation as inherent to disease classification. 'Disease categories exist; market reflects disease categories; this is just how medicine works.' But the analytical engine detects this as a false summit: the mountain claim fails accessibility_collapse and resistance gates. Diagnostic segmentation is contingent on specific regulatory, economic, and institutional choices, not natural law. The appearance of immutability derives from institutional entrenchment, not structural necessity.
constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_market_segmentation_by_diagnosis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_market_segmentation_by_diagnosis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_market_segmentation_by_diagnosis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_market_segmentation_by_diagnosis, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_market_segmentation_by_diagnosis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. The constraint combines genuine precision medicine (legitimate coordination: 0.30-0.35 of the value) with market segmentation rent-seeking (extraction: 0.20-0.28 of the value). The increasing trend reflects pharmaceutical industry movement toward narrower, more profitable populations (e.g., ultra-rare disease definitions, biomarker-selected cohorts) that concentrate pricing power. Suppression (0.68): High. Multiple layers: (1) Regulatory gate: formal diagnosis required for insurance coverage (structural barrier). (2) Epistemic gate: diagnosis requires specialist evaluation, biomarker testing, clinical access (information asymmetry and expertise barrier). (3) Economic gate: biomarker tests often require manufacturer sponsorship; uninsured populations cannot access testing. (4) Institutional gate: prescriber constraint through formulary restrictions and prior authorization. (5) Advocacy gate: patient organizations organized around marketed diagnoses marginalize populations without diagnostic labels. Theater ratio (0.64): Moderately high and increasing. Diagnostic classification performs clinical legitimacy ('evidence-based,' 'precision medicine') while enabling rent extraction. The performance intensifies as diagnostic categories proliferate — increasingly arcane diagnostic criteria serve legitimation function without clinical communication content.
 *
 * PERSPECTIVAL GAP:
 *   Maximized perspectival divergence between manufacturer and trapped patient. The manufacturer's rope-class experience (genuine precision medicine coordination with net benefit) contrasts sharply with the powerless patient's snare experience (pure extraction with no exit). This gap is the core diagnostic signal for the constraint's hybrid nature: the same mechanism produces coordination value for those with power and extraction cost for those without. The gap also reveals that 'precision medicine' framing serves a legitimation function — the constraint genuinely provides precision medicine value to privileged populations (those with access to diagnosis, specialists, biomarker testing) while extracting from populations who bear disease burden but lack diagnostic validation. The payer's tangled rope experience sits in between: they benefit from the structure (evidence basis for coverage decisions) but bear significant extraction costs (highest-cost medications concentrated in narrow populations).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows beneficiary/victim structure: Pharmaceutical manufacturers benefit from market segmentation (arbitrage exit, institutional power) → low d (~0.15) → negative chi (subsidized). Trapped patients bear extraction cost with no exit (trapped exit, powerless status) → high d (~0.95) → high chi (maximum experienced extraction). Payers benefit from structured coverage criteria but face extraction through concentrated pricing (constrained exit, institutional status) → moderate d (~0.60) → moderate chi. Prescribers experience mixed coordination and extraction (constrained exit, moderate status) → moderate-high d (~0.65) → moderate-high chi. The organized diagnostic innovation community has mobile exit options (investing in alternative platforms) → lower d (~0.35) → lower chi relative to trapped populations.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by showing that both 'genuine coordination' and 'extractive rent-seeking' framings are correct — they apply to different populations and time horizons. For patients with formal diagnoses and access to specialists/biomarkers (typically high-income, well-insured populations in developed countries), the constraint functions as genuine precision medicine (Rope: 0.25-0.30 of the coordination value is real). For populations without diagnostic validation or specialist access (typically lower-income, rural, global south populations, and populations whose disease presentations diverge from marketed criteria), the constraint functions as pure extraction (Snare: 0.30-0.35 of the extraction cost is real). The mandatrophy resolves by recognizing that this is not a single constraint with ambiguous classification but a **constraint family with different ε values for different populations**. (1) Diagnostic segmentation for high-income, formally-diagnosed populations: ε ≈ 0.25, Rope (genuine precision medicine coordination). (2) Diagnostic gatekeeping for populations without formal diagnosis or specialist access: ε ≈ 0.72, Snare (pure extraction). (3) Diagnostic segmentation as payer administrative tool: ε ≈ 0.45, Tangled Rope (mixed coordination and extraction). These are structurally distinct constraints sharing institutional infrastructure (diagnostic codes) but producing different extraction patterns depending on agent position. The aggregate story (ε = 0.58, Tangled Rope) captures the population-weighted average but masks the extreme variation: the constraint is Rope for rich, diagnosable populations and Snare for poor, undiagnosed populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_criteria_capture,
    'To what degree have pharmaceutical market incentives shaped the diagnostic criteria themselves (DSM-5, ICD-10 revision, biomarker thresholds)?',
    'Historical analysis of diagnostic criteria revision timelines against pharmaceutical product launches; funding source analysis for diagnostic standardization committees; correlation between diagnostic expansion and drug approvals in the same disease area',
    'If high capture: the market segmentation is not responsive to prior diagnostic categories but constitutive of them — extraction is deeper and less reversible. If low: diagnostic categories are largely independent, and market segmentation is applied post-hoc to pre-existing categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_criteria_capture, empirical, 'Degree of pharmaceutical industry influence on diagnostic criteria development').

omega_variable(
    off_label_prescribing_prevalence,
    'What fraction of pharmaceutical prescribing occurs outside the formally segmented diagnostic categories, and what are the cost and clinical outcome differences?',
    'Claims database analysis of off-label prevalence by indication; real-world evidence studies comparing off-label vs on-label efficacy and safety; cost analysis of off-label vs segmented pathways',
    'If off-label prevalence high and outcomes equivalent: market segmentation is artificial constraint (Snare strengthened). If off-label rare or worse outcomes: segmentation reflects clinical reality (Tangled Rope strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(off_label_prescribing_prevalence, empirical, 'Prevalence and outcomes of off-label pharmaceutical use outside formal segmentation').

omega_variable(
    diagnostic_expansion_as_market_strategy,
    'Are diagnostic criteria systematically expanded (diagnostic drift) following pharmaceutical product approvals and market saturation?',
    'Time series analysis of diagnostic prevalence estimates before and after drug approvals; trend analysis of DSM/ICD diagnostic threshold changes correlated with pharmaceutical product lifecycles',
    'If systematic: pharmaceutical manufacturers are using diagnostic expansion as extraction strategy (Snare extraction deepens). If random: diagnostic expansion responds to clinical evidence independently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_expansion_as_market_strategy, empirical, 'Whether diagnostic criteria expand systematically to follow pharmaceutical market opportunities').

omega_variable(
    real_world_evidence_platform_viability,
    'Are open, interoperable real-world evidence platforms technically and economically viable as alternatives to proprietary diagnostic segmentation?',
    'Feasibility studies of federated learning approaches for treatment matching; cost-benefit analysis of interoperable biomarker platforms vs current diagnostic segmentation infrastructure; proof-of-concept real-world evidence systems',
    'If viable: scaffold sunset mechanism is real (15-25 year pathway). If not viable: pharmaceutical market segmentation persists as lowest-cost coordination mechanism (Tangled Rope becomes permanent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_world_evidence_platform_viability, empirical, 'Technical and economic viability of alternative treatment-matching infrastructure').

omega_variable(
    suppression_mechanism_composition,
    'What fraction of suppression (0.68) is structural (regulatory/economic barriers) vs. epistemic (diagnostic expertise requirements, information asymmetry)?',
    'Policy experiment: reduce regulatory friction for off-label access without diagnostic validation; measure uptake and outcomes. Separate measurement of information vs. institutional barriers.',
    'If structural dominant: suppression declines with regulatory reform. If epistemic dominant: suppression persists even with regulatory change because actors lack access to alternative treatment information.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of suppression between structural and epistemic barriers').

omega_variable(
    intersectional_diagnostic_exclusion,
    'Are diagnostic segmentation patterns correlated with patient demographic characteristics (race, income, geography), independent of disease prevalence?',
    'Adjusted prevalence ratio analysis: compare diagnosed prevalence in segmented vs unsegmented demographics, controlling for true disease prevalence from population studies; analyze pharmaceutical marketing spend by diagnostic category and demographic',
    'If strong correlation: market segmentation functions as a mechanism of health inequity extraction (additional snare layer targeting specific populations). If weak: segmentation is diagnosis-neutral.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersectional_diagnostic_exclusion, empirical, 'Correlation between pharmaceutical market segmentation and patient demographic exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_market_segmentation_by_diagnosis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharmseg_tr_t0, pharmaceutical_market_segmentation_by_diagnosis, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pharmseg_tr_t10, pharmaceutical_market_segmentation_by_diagnosis, theater_ratio, 10, 0.55).
narrative_ontology:measurement(pharmseg_tr_t20, pharmaceutical_market_segmentation_by_diagnosis, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(pharmseg_be_t0, pharmaceutical_market_segmentation_by_diagnosis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pharmseg_be_t10, pharmaceutical_market_segmentation_by_diagnosis, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pharmseg_be_t20, pharmaceutical_market_segmentation_by_diagnosis, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_market_segmentation_by_diagnosis, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_market_segmentation_by_diagnosis, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(pharmaceutical_market_segmentation_by_diagnosis, biomarker_test_availability).
narrative_ontology:affects_constraint(pharmaceutical_market_segmentation_by_diagnosis, specialty_pharmacy_network_gatekeeping).

% DUAL FORMULATION NOTE:
% This story represents the aggregate structure of pharmaceutical market segmentation. Three decomposed constraints capture the constraint family: (1) diagnostic_segmentation_high_income_populations (ε ≈ 0.25, Rope) — genuine precision medicine coordination in well-resourced settings; (2) diagnostic_gatekeeping_undiagnosed_populations (ε ≈ 0.72, Snare) — pure extraction gatekeeping for populations without diagnostic validation; (3) diagnostic_administration_payer_systems (ε ≈ 0.45, Tangled Rope) — payer administrative coordination with embedded extraction. These three stories should be linked via network.affects_constraints showing the population-specific differentiation of the aggregate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_market_segmentation_by_diagnosis, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
