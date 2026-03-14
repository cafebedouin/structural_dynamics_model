% ============================================================================
% CONSTRAINT STORY: healthcare_equity_access_disparity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_healthcare_equity_access_disparity, []).

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
 *   constraint_id: healthcare_equity_access_disparity
 *   human_readable: Healthcare Equity Access Disparity
 *   domain: healthcare/access/equity
 *
 * SUMMARY:
 *   Healthcare equity access disparity represents a structural constraint
 *   where genuine coordination functions (risk pooling through insurance,
 *   capital concentration for specialized care infrastructure, knowledge
 *   coordination across providers) operate alongside asymmetric extraction
 *   (cost barriers, access rationing, profit maximization). The constraint
 *   affects different populations with radically different structural
 *   relationships: uninsured populations trapped by material barriers;
 *   underinsured populations constrained by high cost-sharing; providers
 *   enriched through market power; payers arbitraging regulatory gaps;
 *   reformers organized to build alternative pathways. The base
 *   extractiveness has increased from 0.42 to 0.58 over the measurement
 *   interval (20 years), reflecting rising medical costs, increasing
 *   uninsured rates, and widening out-of-pocket burden. Theater ratio has
 *   risen from 0.55 to 0.65, indicating that administrative overhead (prior
 *   authorization, claims adjudication, coding verification) has grown as a
 *   proportion of system activity without corresponding efficiency gains.
 *   This is a classic tangled rope: coordination is real (insurance does
 *   solve the collective action problem of pooling health risk; specialized
 *   care requires concentration of capital and knowledge), but the extraction
 *   mechanism is also real and growing.
 *
 * KEY AGENTS:
 *   - Uninsured populations: Primary victim (powerless/trapped) — structurally excluded from care access; bears maximum extraction cost through preventive care deprivation and worse health outcomes
 *   - Underinsured low-income patients: Secondary victim (moderate/constrained) — constrained by high deductibles and cost barriers; experience genuine coordination alongside asymmetric extraction
 *   - Private insurance corporations: Primary beneficiary (institutional/arbitrage) — arbitrage regulatory gaps, shift costs to patients, benefit from risk segmentation; experience constraint as pure coordination
 *   - Specialty care providers in concentrated markets: Secondary beneficiary (powerful/mobile) — coordinate specialized knowledge and capital; extract through monopolistic pricing in rare disease and tertiary care
 *   - Health equity reform coalition: Organized actors (organized/constrained) — community health centers, Medicaid advocates, universal healthcare movements building alternative pathways with sunset logic
 *   - Fee-for-service billing system: Institutional actor (institutional/arbitrage) — maintains performative administrative theater; persists through inertia rather than function
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — sees genuine hybrid coordination-extraction; classifies constraint as tangled rope structurally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(healthcare_equity_access_disparity, 0.58).
domain_priors:suppression_score(healthcare_equity_access_disparity, 0.72).
domain_priors:theater_ratio(healthcare_equity_access_disparity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(healthcare_equity_access_disparity, extractiveness, 0.58).
narrative_ontology:constraint_metric(healthcare_equity_access_disparity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(healthcare_equity_access_disparity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(healthcare_equity_access_disparity, tangled_rope).
narrative_ontology:human_readable(healthcare_equity_access_disparity, "Healthcare Equity Access Disparity").
narrative_ontology:topic_domain(healthcare_equity_access_disparity, "healthcare/access/equity").

domain_priors:requires_active_enforcement(healthcare_equity_access_disparity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(healthcare_equity_access_disparity, high_income_insured_patients).
narrative_ontology:constraint_beneficiary(healthcare_equity_access_disparity, private_insurance_corporations).
narrative_ontology:constraint_beneficiary(healthcare_equity_access_disparity, specialty_care_providers).
narrative_ontology:constraint_beneficiary(healthcare_equity_access_disparity, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(healthcare_equity_access_disparity, uninsured_populations).
narrative_ontology:constraint_victim(healthcare_equity_access_disparity, underinsured_low_income_patients).
narrative_ontology:constraint_victim(healthcare_equity_access_disparity, rural_communities).
narrative_ontology:constraint_victim(healthcare_equity_access_disparity, healthcare_system_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT (SNARE) — Structurally trapped by inability to pay for care, absence of insurance safety net, and geographic isolation from specialty services. No realistic exit from the constraint. Maximum extraction: bears full cost burden while unable to access preventive care, resulting in worse health outcomes and deeper poverty. The suppression mechanism is material and complete — no alternatives exist within the patient's economic reach.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERINSURED LOW-INCOME PATIENT (TANGLED ROPE) — Constrained by high deductibles, limited network coverage, and cost of co-payments. Experiences genuine coordination: insurance enables access to emergency care and some routine services that would be impossible without the system. However, the coordination is paired with asymmetric extraction: high out-of-pocket costs, delayed care due to cost barriers, and exclusion from specialized treatment options. Cost of exit (loss of insurance) makes constrained mobility real but expensive.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIVATE INSURANCE CORPORATION (ROPE) — Experiences the constraint as a coordination mechanism with substantial arbitrage options. The insurance system genuinely solves the collective action problem of pooling health risk. Net beneficiary with full exit capacity: can arbitrage regulatory gaps, shift costs to patients, adjust underwriting to exclude high-risk groups, or relocate operations. The constraint is pure coordination from this perspective — extraction runs toward this agent.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SPECIALTY CARE PROVIDER IN CONCENTRATED MARKET (TANGLED ROPE) — Coordinates specialized knowledge and high-tech care delivery (genuine function), while also extracting through monopolistic pricing in concentrated markets where competition is limited. Mobile with respect to geographic location but locked into the specialty care role. Moderate extraction experienced as reimbursement pressures from payers, but substantial power over pricing and access for rare services. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTH EQUITY REFORM COALITION (SCAFFOLD) — Organized agents (community health centers, Medicaid expansion advocates, universal healthcare movements) see the disparity as a temporary structural problem with identifiable sunset mechanisms: Medicaid expansion, community health center growth, telehealth infrastructure, and price regulation all represent pathways to reduced extraction. The suppression of alternative systems is real but eroding. Constrained by political economy but with agency and exit pathways visible. Sunset logic applies: as these mechanisms mature, the disparity constraint's extraction mechanism loses force.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEE-FOR-SERVICE BILLING SYSTEM (PITON) — The billing and reimbursement infrastructure is substantially performative: much administrative overhead (prior authorizations, coding verification, insurance adjudication) is theater that extracts time and resources without improving care coordination. The system persists through institutional inertia despite recognized dysfunction. High theater ratio reflects that the billing apparatus consumes 25-30% of healthcare spending while adding minimal value. The system is maintained by legacy incentives and entrenched workflows, not because it solves coordination problems effectively.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systems perspective, the healthcare constraint exhibits genuine coordination functions (risk pooling, specialized knowledge concentration, capital investment incentives for research) paired with real asymmetric extraction (cost barriers, access rationing, profit maximization). The constraint cannot be classified as pure coordination or pure extraction — it is structurally hybrid. The driver of asymmetry is that the coordination function (risk pooling) could theoretically operate with low extraction, but market mechanisms and regulatory gaps create conditions where extraction is maximized alongside coordination.
constraint_indexing:constraint_classification(healthcare_equity_access_disparity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(healthcare_equity_access_disparity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(healthcare_equity_access_disparity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(healthcare_equity_access_disparity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(healthcare_equity_access_disparity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(healthcare_equity_access_disparity, TR),
    TR >= 0.70.

:- end_tests(healthcare_equity_access_disparity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint has grown more extractive over the measurement interval. Initial extractiveness (0.42) reflected baseline cost barriers and access disparity; current level (0.58) reflects rising medical inflation, increasing uninsured rates (peak uninsured rate ~16% nationally, 2010), and widening out-of-pocket burden for underinsured populations. The growth trajectory indicates accumulation of extraction alongside genuine coordination function. Suppression (0.72): High. Multiple independent barriers prevent exit and access: (1) Material barriers — inability to pay for uninsured and underinsured; (2) Geographic barriers — specialty care concentration in urban areas, provider shortage in rural regions; (3) Informational barriers — medical complexity and epistemic closure preventing navigation of system; (4) Institutional barriers — administrative complexity of insurance authorization and claims; (5) Regulatory capture — policy barriers preventing alternative systems. The suppression is both structural and, for some populations, partially internalized. Theater ratio (0.65): Moderately high. Administrative overhead (prior authorization, claims processing, coding verification, insurance adjudication) consumes 25-30% of healthcare spending while adding minimal value to care coordination. This is theater: performative infrastructure that extracts time and resources without solving the underlying coordination problem of matching patients with appropriate care.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the beneficiaries (insurance corporations, specialty providers, high-income insured patients) who experience the system as pure coordination enabling access to care at the margin, and the victims (uninsured and underinsured populations) who experience barriers that block access entirely. The beneficiary perspective naturalizes the system as the only way to achieve care coordination; the victim perspective reveals the system as a barrier masquerading as coordination. The analytical observer detects both: coordination IS achieved through risk pooling and specialization, AND extraction IS real through cost barriers and access rationing. The resolution is that both are true — the constraint is genuinely hybrid (tangled rope), not purely one or the other. The perspectival gap reveals the distribution of benefits and harms, not a disagreement about what the system does.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation for each perspective follows the structural relationship declaration. Insurance corporations are declared as beneficiaries with arbitrage exit: the pipeline derives d ≈ 0.15 (low d for beneficiaries with escape options), producing low effective extraction χ. Uninsured populations are declared as victims with trapped exit: the pipeline derives d ≈ 0.95 (high d for victims without escape options), producing high effective extraction χ. The underinsured populations are victims but with constrained (not trapped) exit: d ≈ 0.65, producing moderate χ. Specialty providers in concentrated markets are beneficiaries with mobile exit but operating in markets with artificial scarcity: baseline d would be ~0.25 (beneficiary + mobile), but the concentrated market structure supports directionality override to d ≈ 0.45 to reflect that their apparent mobility is constrained by market structure and scope limitations. No overrides are declared in the current JSON because the automatic derivation from beneficiary/victim + exit produces accurate relative directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE RESOLUTION: This constraint passes all three tangled rope gates: (1) Beneficiaries are declared (insurance corporations, specialty providers, high-income patients). (2) Victims are declared (uninsured populations, underinsured low-income patients). (3) Requires active enforcement is true (insurance regulation, reimbursement rules, licensing of providers all require active maintenance). The mandate for this constraint is to coordinate health risk pooling and specialized care delivery while distributing access equitably. The mandatrophy surfaces the fundamental tension: the coordination function (risk pooling, specialization) requires concentration of capital and selective enrollment, which creates extraction mechanisms (cost barriers, access rationing). The tangled rope classification captures this hybrid: the constraint is neither pure coordination (because extraction is real and structural) nor pure extraction (because coordination benefit is real). The mandatrophy is resolved by recognizing that efficient extraction is embedded in the coordination function — the cost of access is not separable from the benefit of care coordination. The constraint cannot be 'fixed' by removing extraction while preserving coordination; it can only be managed by (a) accepting higher-cost coordination models (like universal systems with lower selective enrollment), (b) redistributing extraction burden more equitably, or (c) building alternative coordination pathways (health equity reform coalition's scaffold approach). The current analysis assumes path (b) is the implicit mandate: keep the coordination function, reduce the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_threshold,
    'What level of cost-sharing and access restriction is necessary for healthcare coordination (risk pooling, care delivery incentives) versus what level constitutes extractive overhead?',
    'Comparative analysis of healthcare outcomes and cost-access correlation across OECD nations with different cost-sharing models; identification of threshold where patient cost barriers meaningfully reduce health outcomes without corresponding system efficiency gains',
    'If threshold is high: disparity is legitimate coordination cost (more perspectives shift toward Rope). If threshold is low: most current cost-sharing is extractive (more perspectives shift toward Snare). Classification sensitivity directly proportional to empirical threshold location.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_threshold, empirical, 'Necessary cost-sharing threshold for healthcare coordination').

omega_variable(
    insurance_risk_segmentation_mechanism,
    'Is health insurance risk segmentation (excluding high-risk populations, high deductibles for chronic disease) a necessary market mechanism for sustainability or an extractive exclusion strategy?',
    'Analysis of insurance profitability with and without risk segmentation; comparison of outcomes in systems with risk equalization pools versus competitive risk selection; identification of whether risk segmentation reduces overall system sustainability or merely concentrates profit',
    'If necessary: suppression metrics should be lower (constraints from beneficiaries'' perspective shift toward coordination). If extractive: suppression remains high and institutional perspectives confirm high chi. Classification of insurance corporation perspective (currently Rope) may shift to Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insurance_risk_segmentation_mechanism, empirical, 'Whether insurance risk segmentation is necessary or extractive').

omega_variable(
    specialty_care_monopoly_dynamics,
    'Do specialty care provider monopolies in concentrated markets (rare disease treatment, tertiary care) reflect genuine scarcity-driven concentration or rent-extraction through artificial supply limitation?',
    'Analysis of specialty care pricing variance across regions; identification of relationship between provider concentration and patient outcomes versus pricing; study of cases where new providers entered concentrated markets and whether pricing normalized',
    'If genuine scarcity: specialty provider perspective remains Tangled Rope with moderate extraction justified by coordination value. If artificial limitation: perspective shifts to Snare or high-extraction Tangled Rope, and organized actors gain leverage for antitrust intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialty_care_monopoly_dynamics, empirical, 'Whether specialty care concentration is scarcity-driven or rent-extractive').

omega_variable(
    telehealth_sunset_mechanism,
    'Does telehealth infrastructure genuinely reduce access barriers at scale, or does it primarily serve mobile populations while leaving trapped populations further isolated?',
    'Longitudinal analysis of telehealth adoption and health equity trends post-COVID; identification of whether telehealth expansion correlates with reduced access disparity or merely increased disparity in telehealth access itself',
    'If scalable: scaffold perspective''s sunset mechanism is credible and extraction timeline shortens (measurements should show theater_ratio declining). If limited: scaffold is aspirational rather than structural, and trapped populations face compounding barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(telehealth_sunset_mechanism, empirical, 'Whether telehealth reduces access barriers at scale').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.72) entirely structural (material barriers to access, inability to pay) or partially internalized (patients have internalized health system framing of deservingness, believe barriers are natural, have epistemic isolation from alternative systems)?',
    'Post-barrier removal trajectory: if suppression persists after material barriers are removed (e.g., Medicaid expansion, free clinic availability), reclassify as partially internalized. Interview-based identification of whether trapped populations perceive barriers as changeable.',
    'If internalized: effective suppression is higher than structural metrics suggest because populations carry suppression constraints with them. The identity_locked exit option becomes more salient for understanding behavioral responses. If structural only: current suppression metrics are accurate and exit depends on barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural versus internalized suppression mechanism in healthcare access barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(healthcare_equity_access_disparity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcequity_tr_t0, healthcare_equity_access_disparity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hcequity_tr_t10, healthcare_equity_access_disparity, theater_ratio, 10, 0.62).
narrative_ontology:measurement(hcequity_tr_t20, healthcare_equity_access_disparity, theater_ratio, 20, 0.65).
narrative_ontology:measurement(hcequity_tr_t5, healthcare_equity_access_disparity, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(hcequity_be_t0, healthcare_equity_access_disparity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hcequity_be_t10, healthcare_equity_access_disparity, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(hcequity_be_t20, healthcare_equity_access_disparity, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hcequity_be_t5, healthcare_equity_access_disparity, base_extractiveness, 5, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(healthcare_equity_access_disparity, resource_allocation).
narrative_ontology:boltzmann_floor_override(healthcare_equity_access_disparity, 0.18).
narrative_ontology:affects_constraint(healthcare_equity_access_disparity, medical_bankruptcy_debt_trap).
narrative_ontology:affects_constraint(healthcare_equity_access_disparity, racial_health_outcome_disparity).
narrative_ontology:affects_constraint(healthcare_equity_access_disparity, rural_provider_shortage).

% DUAL FORMULATION NOTE:
% Healthcare equity access disparity is upstream of several constraint stories that inherit its structural properties. Medical bankruptcy represents the personal-level extraction mechanism driven by disparity. Racial health outcome disparity represents the empirical measurement of disparity's health consequences. Rural provider shortage represents a geographic manifestation of disparity dynamics. All three are influenced by the resource allocation mechanisms and cost barriers defined in this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(healthcare_equity_access_disparity, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
