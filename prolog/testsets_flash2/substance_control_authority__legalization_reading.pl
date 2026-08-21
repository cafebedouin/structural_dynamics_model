% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority for Legalized Drug Markets with Quality and Access Controls
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to establish and manage
 *   legal markets for previously illicit substances, focusing on quality
 *   control, access restrictions, and taxation. It is a 'legalization
 *   reading' of the broader 'substance_control_authority' kernel. The core
 *   premise is that regulated commerce is superior to both prohibition and
 *   unregulated supply in minimizing harms and generating public benefit.
 *   This reading aims to eliminate illegal markets by providing a safer,
 *   legal alternative, while protecting third parties through market
 *   regulation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.15).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.2).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority for Legalized Drug Markets with Quality and Access Controls").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'd7fada24-0fed-4d82-a147-2624aae1e5e8').
narrative_ontology:cs_kernel_codification('d7fada24-0fed-4d82-a147-2624aae1e5e8', formalized).
narrative_ontology:cs_authority_grounding('d7fada24-0fed-4d82-a147-2624aae1e5e8', expertise).
narrative_ontology:cs_interpretation_layer_present('d7fada24-0fed-4d82-a147-2624aae1e5e8').
narrative_ontology:cs_reading_relation('d7fada24-0fed-4d82-a147-2624aae1e5e8', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('d7fada24-0fed-4d82-a147-2624aae1e5e8', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('d7fada24-0fed-4d82-a147-2624aae1e5e8', foundational, regulated_commerce_minimizes_harms).
narrative_ontology:cs_axiom_status(regulated_commerce_minimizes_harms, holdable).
narrative_ontology:cs_axiom_grounding('d7fada24-0fed-4d82-a147-2624aae1e5e8', regulated_commerce_minimizes_harms, empirically_contingent).
narrative_ontology:cs_axiom('d7fada24-0fed-4d82-a147-2624aae1e5e8', foundational, state_has_right_to_control_commerce_for_public_good).
narrative_ontology:cs_axiom_status(state_has_right_to_control_commerce_for_public_good, holdable).
narrative_ontology:cs_axiom_grounding('d7fada24-0fed-4d82-a147-2624aae1e5e8', state_has_right_to_control_commerce_for_public_good, conventional).
narrative_ontology:cs_reference_frame('d7fada24-0fed-4d82-a147-2624aae1e5e8', rational_public_health_governance).
narrative_ontology:cs_drift_state('d7fada24-0fed-4d82-a147-2624aae1e5e8', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d7fada24-0fed-4d82-a147-2624aae1e5e8', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_consumers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_producers_distributors).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, tax_authorities).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, illegal_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, law_enforcement).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, public_health_approach_to_substance_use).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, market_regulation_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing and implementing regulatory frameworks, setting quality standards, managing public education campaigns, and monitoring health outcomes. They benefit from reduced harms associated with unregulated markets and increased capacity for public health interventions.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Gain access to tested, regulated products with clear labeling, reducing risks of adulteration and overdose. They exit the victim sets of both criminal markets and unregulated supply. They are subject to age and quantity restrictions.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulated_consumers, beneficiary,
    moderate, biographical, mobile, local).

% Operate legally within a regulated market, paying taxes and adhering to quality controls. They benefit from legal protection, access to banking, and stable supply chains, but are subject to strict licensing and operational requirements.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_producers_distributors, beneficiary,
    organized, biographical, constrained, national).

% Collect tax revenue from legal sales, which can be earmarked for public health, education, or general funds. They benefit from a new, stable revenue stream that was previously unavailable from illegal markets.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Lose market share and profitability as legal alternatives emerge. They face increased enforcement pressure and reduced demand for their unregulated products. Their business model is directly undermined by legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, illegal_market_actors, payer,
    powerful, immediate, trapped, regional).

% Shifts focus from prosecuting users and small-scale dealers to targeting large-scale illegal trafficking and ensuring compliance with legal market regulations. They benefit from reduced street crime and clearer enforcement priorities, but may face resistance to changing established practices.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, law_enforcement, beneficiary).

% Monitor the impact of legalization on crime rates, public order, and youth access. They advocate for robust regulatory controls and public education to mitigate potential negative consequences.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_safety_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a regulated market for substances, coordinating production, distribution, and consumption under state oversight to ensure product safety, control access, and generate tax revenue, replacing chaotic and harmful illegal markets.
% TRANSFER_FUNCTION: Transfers control and revenue from illegal market actors to licensed businesses and the state (via taxes), while transferring safer products and regulated access to consumers.
% ABSENT_VOICES: Advocates for complete deregulation (anarcho-capitalist reading) would argue against any state control, while strict prohibitionists would argue against any legal market. Both are excluded from the policy-making process that establishes a regulated legal market.
% DISAPPEARANCE_RATIONALE: If state authority to regulate legal drug markets vanished, the market would immediately revert to either prohibition (if enforcement capacity remained) or an unregulated free-for-all, leading to a rapid increase in harms, criminal activity, and public health crises. The entire infrastructure of legal production, distribution, and taxation would collapse.
% FOUNDING_PROBLEM: The harms of both drug prohibition (criminalization, black markets, violence, lack of quality control) and completely unregulated markets (public health crises, exploitation) are severe and widespread.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, criminal justice statistics, and economic analyses from numerous jurisdictions (e.g., cannabis legalization in various states/countries) corroborate the ongoing problems of both prohibition and unregulated supply, supporting the need for a regulated legal market approach.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the primary goal is public health and safety, not revenue maximization, and the 'extraction' from illegal market actors is a feature, not a bug. Suppression (0.20) is moderate, focused on preventing illegal market activity and ensuring compliance within the legal market, rather than suppressing use itself. Theater ratio is low (0.05) as the regulatory functions are genuine and directly address public health and safety concerns. Accessibility collapse is high (0.70) because the legal market aims to collapse the accessibility of illegal, unregulated alternatives. Resistance (0.10) is low from the perspective of those within the legal framework, though illegal market actors will resist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health and regulated consumers, this is a clear Rope, solving a collective action problem with net benefits. From the perspective of illegal market actors, it is a Snare that actively dismantles their operations. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies, regulated consumers, licensed producers, and tax authorities are beneficiaries, gaining from reduced harms, safer products, legal commerce, and new revenue. Illegal market actors are the primary victims, as their business model is directly undermined. Law enforcement shifts from suppressing users to regulating markets and targeting serious illegal trafficking, making them both agenda-setters and beneficiaries of a more rational system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_volume_impact,
    'Does the legalization of drug markets lead to a significant and sustained increase in overall substance use volume, and if so, does this outweigh the benefits of reduced harms from unregulated supply?',
    'Longitudinal epidemiological studies and public health data from jurisdictions that have implemented legalization, comparing pre- and post-legalization use rates and associated health outcomes.',
    'If a significant increase in use volume leads to new public health burdens that outweigh the benefits of reduced harms, the overall ''beneficiary'' status of regulated consumers could be challenged, potentially increasing the perceived extractiveness of the regulatory system (as it would be seen as enabling greater harm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_impact, empirical, 'Uncertainty regarding the net public health impact of increased use volume versus reduced harms from unregulated markets.').

omega_variable(
    illegal_market_persistence,
    'To what extent do illegal markets persist and adapt after legalization, particularly for specific substances or demographics, and does this undermine the ''elimination of illegal markets'' goal?',
    'Ongoing law enforcement intelligence, market analysis, and sociological studies tracking the size and activity of illegal markets post-legalization.',
    'If illegal markets persist significantly, the ''suppression'' metric for this constraint might be underestimated, and the ''victim'' status of illegal market actors might be less complete than assumed, indicating a less effective transition from illicit to legal supply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illegal_market_persistence, empirical, 'Uncertainty about the complete displacement of illegal markets by legal regulation.').

omega_variable(
    regulatory_capture_risk,
    'Is there a significant risk of regulatory capture by large licensed producers, leading to policies that favor industry profits over public health goals or small business access?',
    'Analysis of lobbying expenditures, campaign contributions, and legislative outcomes in legalized jurisdictions, alongside studies of market concentration and barriers to entry for new businesses.',
    'If regulatory capture is substantial, the ''beneficiary'' status of licensed producers would be amplified, and the ''agenda_setter'' role of public health agencies could be compromised, potentially shifting the constraint towards a Tangled Rope or Snare for smaller businesses and public health outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_risk, conceptual, 'Risk of industry influence distorting public health-oriented regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(subs_tr_t5, substance_control_authority__legalization_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__legalization_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(subs_tr_t15, substance_control_authority__legalization_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__legalization_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__legalization_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__legalization_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__legalization_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
