% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Substance Control: Legalization Reading (Individual Liberty & Externality Capture)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'legalization_reading' of the
 *   'substance_control_kernel'. It frames substance use as an individual
 *   liberty issue, where state intervention is limited to preventing
 *   third-party harm and capturing externality costs through regulation and
 *   taxation. This reading aims to dismantle the extractive and suppressive
 *   mechanisms of prohibition, replacing them with a regulated market and
 *   public health approach, while acknowledging new forms of extraction
 *   (taxes, unmitigated externalities) and suppression (for unlicensed
 *   activity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.3).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Substance Control: Legalization Reading (Individual Liberty & Externality Capture)").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '54059da7-fe86-4e01-8b31-552468d3f675').
narrative_ontology:cs_kernel_codification('54059da7-fe86-4e01-8b31-552468d3f675', formalized).
narrative_ontology:cs_authority_grounding('54059da7-fe86-4e01-8b31-552468d3f675', practice).
narrative_ontology:cs_interpretation_layer_present('54059da7-fe86-4e01-8b31-552468d3f675').
narrative_ontology:cs_reading_relation('54059da7-fe86-4e01-8b31-552468d3f675', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('54059da7-fe86-4e01-8b31-552468d3f675', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('54059da7-fe86-4e01-8b31-552468d3f675', foundational, individual_autonomy_over_paternalism).
narrative_ontology:cs_axiom_status(individual_autonomy_over_paternalism, holdable).
narrative_ontology:cs_axiom_grounding('54059da7-fe86-4e01-8b31-552468d3f675', individual_autonomy_over_paternalism, deontological).
narrative_ontology:cs_axiom('54059da7-fe86-4e01-8b31-552468d3f675', foundational, state_limited_to_harm_prevention_and_externality_capture).
narrative_ontology:cs_axiom_status(state_limited_to_harm_prevention_and_externality_capture, holdable).
narrative_ontology:cs_axiom_grounding('54059da7-fe86-4e01-8b31-552468d3f675', state_limited_to_harm_prevention_and_externality_capture, conventional).
narrative_ontology:cs_reference_frame('54059da7-fe86-4e01-8b31-552468d3f675', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('54059da7-fe86-4e01-8b31-552468d3f675', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('54059da7-fe86-4e01-8b31-552468d3f675', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_treasury).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, criminal_justice_system).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, third_parties_affected_by_externalities).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, unlicensed_dealers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, individual_autonomy_principle).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, economic_efficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances and avoid criminalization, affirming individual liberty. They pay taxes on legal products and may still face social stigma or health risks, but without state-imposed criminal penalties for use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, substance_users, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, substance_users, payer).

% Operates legally, generating profits from the sale of regulated substances. They pay taxes and adhere to regulatory standards, but benefit from market access and legitimacy that was previously denied.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, legal_substance_industry, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, legal_substance_industry, payer).

% Collects significant tax revenue from legal substance sales, which can be used to fund public services, including harm mitigation or public health initiatives. Sets and enforces regulations to prevent third-party harm.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_treasury, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_treasury, beneficiary).

% Bear the unmitigated costs of externalities such as impaired driving incidents, public intoxication, or secondhand exposure, even if some costs are captured by the state. Their ability to avoid these harms is limited by the prevalence of substance use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, third_parties_affected_by_externalities, payer,
    powerless, immediate, constrained, local).

% Shift focus from criminalization to public health education, treatment, and harm reduction. They advocate for policies that minimize health risks and manage externalities, often funded by substance tax revenue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, public_health_agencies, observer).

% Experiences a reduced burden from prosecuting minor substance possession and use offenses, allowing resources to be reallocated to more serious crimes. Still enforces laws against impaired driving, underage sales, and unlicensed distribution.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, criminal_justice_system, beneficiary,
    institutional, biographical, constrained, national).

% Are pushed out of the market by legal competition and face continued criminal penalties for operating outside the regulated framework. Their economic activity is suppressed by the legal market.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, unlicensed_dealers, excluded,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, unlicensed_dealers, payer).

% Object to the legalization framework on moral or social order grounds, arguing it normalizes harmful behavior. Their policy preferences are not reflected in this reading's framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a regulated legal market for substances, ensuring product quality and safety, generating tax revenue, and shifting enforcement focus to public safety and harm prevention rather than individual use.
% TRANSFER_FUNCTION: Transfers tax revenue from legal substance sales to the state treasury; shifts some externality costs (e.g., healthcare, public safety) from the general public to substance users and the industry via taxation; transfers enforcement resources from prosecuting users to regulating the market and preventing third-party harm.
% ABSENT_VOICES: Prohibition advocates are excluded from the core framing, as their moral and social order arguments are superseded by individual liberty. Unlicensed dealers are also excluded, as their economic model is criminalized by the new legal framework.
% DISAPPEARANCE_RATIONALE: If this legalization framework vanished overnight, the legal substance market would collapse, tax revenues would disappear, and the black market would likely re-emerge to fill the void, leading to a return of many problems associated with prohibition (e.g., lack of quality control, increased criminalization of users).
% FOUNDING_PROBLEM: The failures of substance prohibition, including the creation of vast black markets, criminalization of individuals for personal use, lack of product safety, and the inability of the state to capture revenue or effectively manage public health outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Economic analyses of black markets, public health data on overdose rates under prohibition, criminal justice statistics on arrests for substance offenses, and comparative studies from jurisdictions that have implemented legalization policies all corroborate the problems this reading seeks to address.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the shift from criminal penalties to taxation and regulatory costs, as well as the uncaptured externality costs borne by third parties. Suppression (0.30) is significantly lower than under prohibition, as individual use is decriminalized, but remains for unlicensed sales, impaired conduct, and underage access. Theater ratio (0.10) is low, as the framework is designed for functional regulation and revenue generation, not performative moral enforcement. Accessibility collapse (0.35) is reduced due to legal access, but still present for those unable to afford legal products or who are underage. Resistance (0.25) is lower from users but persists from those impacted by externalities and from prohibition advocates.
 *
 * PERSPECTIVAL GAP:
 *   Substance users and the legal industry perceive this as a liberation from the harms of prohibition, while third parties affected by externalities may see it as an inadequate response to public safety concerns. The state views it as a pragmatic solution for revenue generation and harm management, whereas prohibition advocates see it as a moral failing.
 *
 * DIRECTIONALITY LOGIC:
 *   Substance users and the legal industry are primary beneficiaries, gaining liberty and market access, respectively. The state treasury and criminal justice system also benefit from revenue and reduced burdens. Third parties affected by externalities and unlicensed dealers are victims, bearing unmitigated costs or facing continued criminalization. Public health agencies act as agenda-setters and observers, guiding policy within the new framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This legalization reading directly addresses the mandatrophy of prohibition, where the original mandate (protecting social order) led to unintended consequences (black markets, mass incarceration) that became the primary function. By reframing substance use as a liberty issue with externality management, it attempts to align the constraint's function with its stated purpose, preventing the accumulation of extraction and theatrical enforcement seen in prohibition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_capture_completeness,
    'Is the state''s intervention truly capturing all externality costs, or are significant costs still borne by third parties without compensation?',
    'Comprehensive economic and social impact assessments, including healthcare costs, public safety expenditures, and quality-of-life metrics, compared against tax revenues and mitigation program funding.',
    'If externality capture is incomplete, the effective extraction from third parties is higher than estimated, potentially shifting the constraint closer to a Snare for those groups. If complete, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capture_completeness, empirical, 'Assesses the effectiveness of externality cost capture by the state.').

omega_variable(
    black_market_persistence,
    'Does legalization genuinely reduce the black market, or does it merely shift its focus (e.g., to underage sales, illicit production, or cheaper unregulated products)?',
    'Longitudinal studies of black market activity, pricing, and product availability in legalized jurisdictions, compared to pre-legalization data and prohibition jurisdictions.',
    'If the black market persists significantly, the suppression metric is underestimated, and the coordination benefit of a regulated market is diminished, potentially increasing overall societal harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Evaluates the actual impact of legalization on black market activity.').

omega_variable(
    liberty_vs_profit_framing,
    'Is the ''individual liberty'' framing primarily a genuine commitment to autonomy, or does it serve as a rhetorical cover for the legal substance industry''s profit motives?',
    'Analysis of lobbying efforts, campaign contributions, and public messaging from the legal substance industry, alongside legislative outcomes regarding taxation, advertising, and public health regulations.',
    'If primarily a cover, the constraint''s extractiveness from consumers (via pricing and marketing) and the political influence of the industry are higher than acknowledged, potentially reclassifying aspects of the system as a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(liberty_vs_profit_framing, conceptual, 'Examines the underlying motivations behind the individual liberty framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t5, substance_control_kernel__legalization_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(subs_tr_t15, substance_control_kernel__legalization_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t5, substance_control_kernel__legalization_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(subs_be_t15, substance_control_kernel__legalization_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(subs_su_t5, substance_control_kernel__legalization_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(subs_su_t15, substance_control_kernel__legalization_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_kernel', which also includes prohibition and harm reduction readings. Each reading represents a distinct structural claim about substance control policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
