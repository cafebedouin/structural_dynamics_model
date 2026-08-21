% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint represents the 'livelihood security' reading of
 *   performance legitimacy, where a state's right to rule is primarily
 *   grounded in its ability to deliver tangible improvements in citizens'
 *   daily lives, such as employment, healthcare, education, and elderly care.
 *   This reading prioritizes social welfare and consumption support, often
 *   requiring the state to direct resources away from other development
 *   goals. The claimed type is 'rope' reflecting the state's
 *   self-justification as a coordinator of welfare, but the structural data
 *   (beneficiaries/victims) and metrics suggest a more complex, potentially
 *   extractive dynamic.
 *
 * KEY AGENTS:
 *   - state_planning_agencies: Agenda setter (institutional/constrained) — directs policy and resource allocation.
 *   - citizens_receiving_services: Primary beneficiary (moderate/constrained) — receives direct welfare benefits.
 *   - social_service_providers: Secondary beneficiary (organized/constrained) — implements welfare programs.
 *   - heavy_industry_sectors: Primary payer (powerful/constrained) — bears costs of diverted investment.
 *   - local_government_officials: Secondary payer (organized/constrained) — manages local resource diversion.
 *   - international_observers: Analytical observer (analytical/analytical) — assesses policy outcomes and trade-offs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.45).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.6).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'e2715e12-90ec-4417-bc95-99d2cbfa56c7').
narrative_ontology:cs_kernel_codification('e2715e12-90ec-4417-bc95-99d2cbfa56c7', formalized).
narrative_ontology:cs_authority_grounding('e2715e12-90ec-4417-bc95-99d2cbfa56c7', practice).
narrative_ontology:cs_interpretation_layer_present('e2715e12-90ec-4417-bc95-99d2cbfa56c7').
narrative_ontology:cs_reading_relation('e2715e12-90ec-4417-bc95-99d2cbfa56c7', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('e2715e12-90ec-4417-bc95-99d2cbfa56c7', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2715e12-90ec-4417-bc95-99d2cbfa56c7', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('e2715e12-90ec-4417-bc95-99d2cbfa56c7', foundational, social_stability_through_welfare).
narrative_ontology:cs_axiom_status(social_stability_through_welfare, holdable).
narrative_ontology:cs_axiom_grounding('e2715e12-90ec-4417-bc95-99d2cbfa56c7', social_stability_through_welfare, instrumental).
narrative_ontology:cs_axiom('e2715e12-90ec-4417-bc95-99d2cbfa56c7', foundational, state_responsibility_for_basic_needs).
narrative_ontology:cs_axiom_status(state_responsibility_for_basic_needs, holdable).
narrative_ontology:cs_axiom_grounding('e2715e12-90ec-4417-bc95-99d2cbfa56c7', state_responsibility_for_basic_needs, deontological).
narrative_ontology:cs_reference_frame('e2715e12-90ec-4417-bc95-99d2cbfa56c7', social_contract_of_welfare_provision).
narrative_ontology:cs_drift_state('e2715e12-90ec-4417-bc95-99d2cbfa56c7', contemporary_economic_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2715e12-90ec-4417-bc95-99d2cbfa56c7', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, citizens_receiving_services).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, social_service_providers).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, heavy_industry_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for formulating and implementing policies that prioritize social welfare, employment, healthcare, education, and elderly care. They allocate state resources and direct economic activity to achieve these livelihood security goals, often at the expense of other development priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, state_planning_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from improved access to essential services and social safety nets. Their support for the state's legitimacy is contingent on the continued delivery of these tangible benefits. Exit options are limited as the state is often the sole or primary provider.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, citizens_receiving_services, beneficiary,
    moderate, biographical, constrained, local).

% Organizations and individuals involved in delivering healthcare, education, elderly care, and other social services. They receive state funding and support, aligning their operations with the state's livelihood security agenda. Their growth and stability are tied to this policy.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, social_service_providers, beneficiary,
    organized, biographical, constrained, national).

% Capital-intensive industries whose investment and expansion plans may be deprioritized or taxed to fund social welfare programs. They bear the cost of diverted state investment and regulatory burdens, potentially impacting their long-term growth prospects.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, heavy_industry_sectors, payer,
    powerful, generational, constrained, national).

% Officials at sub-national levels who might see their budgets for infrastructure projects or local economic development diverted to central social welfare initiatives. They are tasked with implementing central policies, even if it means sacrificing local priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_officials, payer,
    organized, biographical, constrained, regional).

% Analysts and organizations (e.g., UN, World Bank, NGOs) who assess the state's development model, evaluating its effectiveness in delivering livelihood security and its trade-offs with other development goals like economic growth or environmental sustainability.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state resources and societal efforts towards ensuring basic welfare, social stability, and citizen satisfaction by delivering tangible improvements in daily life, thereby securing the state's legitimacy.
% TRANSFER_FUNCTION: Transfers state resources (e.g., tax revenues, profits from state-owned enterprises) from sectors like heavy industry and local infrastructure development towards social welfare programs, public services, and consumption support for households.
% ABSENT_VOICES: Advocates for rapid, capital-intensive industrialization, or those prioritizing environmental sustainability over immediate consumption, would argue that this reading sacrifices long-term development or ecological health for short-term social gains.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the state would lose its primary source of legitimacy, leading to widespread social unrest, a collapse of public services, and a fundamental reordering of political and economic priorities as citizens demand basic needs be met.
% FOUNDING_PROBLEM: Widespread poverty, significant social inequality, and a historical lack of basic public services, leading to a crisis of state legitimacy and potential social instability.
% FOUNDING_PROBLEM_CORROBORATION: While the state itself asserts the problem is live, independent social surveys, reports from international development organizations, and academic analyses of social welfare indicators corroborate the ongoing need for livelihood security, even if the state's specific approach is debated.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).
:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) is moderate, reflecting that while genuine benefits are delivered, the state's centralized control and resource allocation mechanisms may involve inefficiencies or implicit costs. Suppression (0.60) is present as the state actively enforces its development priorities, limiting alternative approaches or dissent from affected sectors. The theater ratio (0.20) is relatively low, indicating a genuine focus on tangible delivery, though some performative aspects of 'success' reporting may exist. Accessibility collapse (0.50) is moderate, as the state provides essential services but may limit private alternatives. Resistance (0.40) is also moderate, with beneficiaries showing low resistance, but victimized sectors potentially pushing back against resource diversion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of citizens receiving services, this constraint operates as a beneficial 'rope' that coordinates essential welfare provisions. However, from the perspective of heavy industry sectors or local governments whose resources are diverted, it functions more like a 'tangled_rope' or even a 'snare', as it extracts resources to fund the social agenda. The state's own claim of 'rope' reflects its desired image, while the structural data and metrics capture the underlying asymmetric transfers.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies are clear beneficiaries and agenda setters, directing the flow of resources. Citizens and social service providers are direct beneficiaries of the welfare provisions. Heavy industry and local government officials are payers, bearing the costs of resource reallocation. Their exit options are constrained by the state's dominant role in the economy and governance.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification helps prevent mislabeling genuine welfare coordination as pure extraction, while simultaneously highlighting the extractive elements inherent in state-led resource reallocation. If the founding problem (poverty, inequality) were to become 'dead' but the state continued to prioritize livelihood security at the expense of other sectors, it would signal mandatrophy, potentially shifting the classification towards a 'piton' or 'snare' if the benefits became purely theatrical or the extraction intensified without justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    livelihood_security_vs_state_control,
    'Is the prioritization of livelihood security genuinely driven by citizen welfare, or is it primarily a mechanism for consolidating state control and legitimacy?',
    'Analysis of policy implementation transparency, citizen participation in policy design, and the presence of independent oversight mechanisms. If policies are opaque and participation is minimal, it suggests a control-oriented agenda.',
    'If primarily a mechanism for state control, the effective extractiveness and suppression would be higher, pushing the classification closer to a ''snare'' by revealing the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_security_vs_state_control, conceptual, 'Ambiguity between genuine welfare provision and state control.').

omega_variable(
    measurement_of_tangible_improvements,
    'How are ''tangible improvements in daily life'' objectively measured and verified, and are these metrics susceptible to manipulation or selective reporting?',
    'Independent, third-party audits of social welfare indicators, citizen satisfaction surveys conducted by non-state actors, and comparative analysis with international benchmarks.',
    'If metrics are unreliable or manipulated, the actual benefits to citizens could be lower than claimed, increasing the effective extractiveness and theater ratio, potentially shifting the classification towards ''piton'' or ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_tangible_improvements, empirical, 'Reliability of welfare improvement metrics.').

omega_variable(
    trade_off_justification,
    'Is the sacrifice of capital-intensive industrial expansion and local infrastructure spending genuinely necessary and justified for achieving livelihood security, or are these trade-offs inefficient?',
    'Comprehensive economic modeling comparing different development pathways, cost-benefit analysis of social welfare programs versus industrial investment, and evaluation of long-term economic sustainability.',
    'If trade-offs are found to be inefficient or unnecessary, the extraction from victim sectors would be less justified, increasing the perceived extractiveness and potentially reclassifying towards a ''snare'' for those sectors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trade_off_justification, empirical, 'Efficiency and justification of development trade-offs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__livelihood_security_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__livelihood_security_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(perf_tr_t50, performance_legitimacy__livelihood_security_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__livelihood_security_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__livelihood_security_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(perf_be_t50, performance_legitimacy__livelihood_security_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__livelihood_security_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__livelihood_security_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(perf_su_t50, performance_legitimacy__livelihood_security_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
