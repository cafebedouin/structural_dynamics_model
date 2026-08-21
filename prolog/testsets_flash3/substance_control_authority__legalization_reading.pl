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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to regulate drug markets
 *   as legal commerce, focusing on quality control, access, and taxation. It
 *   is a specific reading of the broader 'substance_control_authority'
 *   kernel, contrasting with prohibition and harm reduction approaches. This
 *   reading posits a system where illicit markets are largely displaced by
 *   regulated ones, and public health is managed through market controls
 *   rather than criminalization. The metrics reflect a relatively
 *   low-extraction, low-suppression system, as its primary function is
 *   coordination and public benefit, with extraction mainly through taxation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.25).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.15).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, 'deca39f2-d784-4e5a-a7bb-21082767ec4a').
narrative_ontology:cs_kernel_codification('deca39f2-d784-4e5a-a7bb-21082767ec4a', formalized).
narrative_ontology:cs_authority_grounding('deca39f2-d784-4e5a-a7bb-21082767ec4a', lineage).
narrative_ontology:cs_interpretation_layer_present('deca39f2-d784-4e5a-a7bb-21082767ec4a').
narrative_ontology:cs_reading_relation('deca39f2-d784-4e5a-a7bb-21082767ec4a', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('deca39f2-d784-4e5a-a7bb-21082767ec4a', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('deca39f2-d784-4e5a-a7bb-21082767ec4a', foundational, individual_autonomy_over_substance_use).
narrative_ontology:cs_axiom_status(individual_autonomy_over_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('deca39f2-d784-4e5a-a7bb-21082767ec4a', individual_autonomy_over_substance_use, deontological).
narrative_ontology:cs_axiom('deca39f2-d784-4e5a-a7bb-21082767ec4a', foundational, market_regulation_as_public_health_tool).
narrative_ontology:cs_axiom_status(market_regulation_as_public_health_tool, holdable).
narrative_ontology:cs_axiom_grounding('deca39f2-d784-4e5a-a7bb-21082767ec4a', market_regulation_as_public_health_tool, instrumental).
narrative_ontology:cs_reference_frame('deca39f2-d784-4e5a-a7bb-21082767ec4a', liberal_democratic_market_governance).
narrative_ontology:cs_drift_state('deca39f2-d784-4e5a-a7bb-21082767ec4a', contemporary_global_policy_shifts, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('deca39f2-d784-4e5a-a7bb-21082767ec4a', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_regulatory_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, legal_drug_producers_distributors).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, adult_consumers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, illegal_drug_markets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, criminal_justice_system).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, public_health_approach_to_substance_use).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, individual_autonomy_principle).
narrative_ontology:constraint_vindicates(substance_control_authority__legalization_reading, market_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces regulations for legal drug production, distribution, and sale, including quality control, taxation, and access restrictions. Benefits from increased tax revenue and reduced criminal justice costs, but bears the burden of developing and maintaining a new regulatory infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Operate within a regulated market, gaining legal access to consumers and protection from illicit competition. They benefit from clear legal frameworks and the ability to build legitimate businesses, but must comply with strict quality and marketing standards.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, legal_drug_producers_distributors, beneficiary,
    organized, biographical, mobile, national).

% Gain access to regulated, quality-controlled substances, reducing health risks associated with adulterated products and avoiding criminal penalties for use. They benefit from consumer protection and personal liberty, but may face higher prices due to taxation and regulation.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adult_consumers, beneficiary,
    moderate, biographical, mobile, local).

% Are directly undermined and eventually eliminated by the establishment of legal, regulated alternatives. They bear the cost of losing market share and facing continued law enforcement pressure as their illicit activities become less viable.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, illegal_drug_markets, payer,
    powerless, immediate, trapped, global).

% Monitor the public health outcomes of legalization, including rates of use, addiction, and related harms. They advocate for policies that balance access with harm prevention, and their analysis informs ongoing regulatory adjustments.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% Experiences a reduction in drug-related arrests, prosecutions, and incarcerations, freeing up resources for other areas of law enforcement. Benefits from a more efficient and less burdened system, but may need to adapt to new types of regulatory enforcement.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, criminal_justice_system, beneficiary,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production, distribution, and consumption of substances by establishing a legal framework that ensures product quality, controls access, and generates tax revenue, replacing unregulated illicit markets with a transparent system.
% TRANSFER_FUNCTION: Transfers control over substance markets from criminal organizations to state regulatory bodies and legal businesses. It also transfers tax revenue from legal sales to public coffers, and health/safety assurances to consumers.
% ABSENT_VOICES: Those who advocate for absolute prohibition would object, arguing that any legal market normalizes drug use and increases societal harm. They are often excluded from policy discussions once a legalization framework is adopted, or their concerns are reframed as public health challenges within the new system.
% DISAPPEARANCE_RATIONALE: If state authority to regulate legal drug markets vanished, the market would immediately revert to an unregulated, illicit state. Criminal organizations would reassert control, product quality would plummet, public health risks would skyrocket, and the state would lose significant tax revenue and regulatory control. The entire social and economic infrastructure built around legal commerce would collapse.
% FOUNDING_PROBLEM: The prohibition of substances created vast, violent black markets, fueled organized crime, led to mass incarceration, and exposed consumers to unregulated, dangerous products, while failing to eliminate substance use.
% FOUNDING_PROBLEM_CORROBORATION: International commissions on drug policy, public health organizations, and economic analyses from outside the benefiting parties consistently corroborate that the problems of prohibition (black markets, crime, health risks) remain live and are exacerbated by the absence of legal regulation.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.25) because the system aims to provide a public good (safe access, reduced crime) and generate revenue through taxation, rather than extracting rents from participants. Suppression is low (0.15) as it primarily targets illegal markets, not consumers or legal businesses. Theater ratio is minimal (0.05) because the regulatory functions are genuine and directly contribute to public health and safety goals. Accessibility collapse is high (0.70) for illicit alternatives, as legal markets provide a superior, safer option. Resistance is low (0.10) from the general public and legal actors, though some resistance persists from residual illicit markets.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state regulatory agencies and legal businesses, this is a highly functional Rope, providing order and opportunity. From the perspective of former participants in illegal markets, it is a Snare that eliminates their livelihood. Adult consumers largely experience it as a beneficial Rope. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory agencies and legal producers/distributors are clear beneficiaries, gaining new roles, revenue, and legitimacy. Adult consumers benefit from safer products and reduced legal risk. The criminal justice system benefits from reduced caseloads. Illegal drug markets are the primary target, facing displacement and elimination. Public health advocates act as observers, ensuring the system meets its stated goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to resolve the mandatrophy of prohibition, where the original mandate (eliminating drug use and crime) failed, but the system persisted through coercive enforcement. The legalization reading aims to create a new, functional mandate focused on public health and safety through regulation, preventing the accumulation of extraction seen in prohibition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    use_volume_impact,
    'Does the legalization of drug markets lead to a significant increase in overall substance use volume and associated public health harms, offsetting the benefits of regulation?',
    'Longitudinal epidemiological studies comparing use rates and health outcomes in jurisdictions before and after legalization, and against control jurisdictions.',
    'If use volume and harms significantly increase, the ''legalization_reading'' might be reclassified as a Tangled Rope or Snare from a public health perspective, as the coordination function (safe access) would be overshadowed by negative externalities. If harms are contained or reduced, its Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_volume_impact, empirical, 'Uncertainty regarding the net public health impact of increased access under legalization.').

omega_variable(
    regulatory_capture_risk,
    'Is the state regulatory authority susceptible to capture by legal drug producers and distributors, leading to regulations that favor industry profits over public health and safety?',
    'Analysis of lobbying expenditures, campaign contributions, and revolving-door employment between industry and regulatory bodies, combined with policy outcome analysis (e.g., lax marketing rules, weak product standards).',
    'If significant regulatory capture is detected, the constraint''s extractiveness would be re-evaluated upwards, and its classification could shift towards a Tangled Rope or Snare, as the ''beneficiary'' role of legal producers would become more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_risk, empirical, 'Risk of industry influence distorting the public health mandate of legal drug regulation.').

omega_variable(
    prohibition_vs_legalization_framing,
    'Is the ''legalization_reading'' fundamentally incompatible with the ''prohibition_reading'' within a single legal framework, or can elements of both coexist (e.g., legal cannabis but prohibited opioids)?',
    'Legal and policy analysis of hybrid regulatory models in different jurisdictions, and conceptual analysis of the underlying normative principles (e.g., individual liberty vs. state paternalism).',
    'If fundamentally incompatible, the ''legalization_reading'' forecloses the ''prohibition_reading'' as a viable policy for the same substance. If elements can coexist, the relationship is more ''coexists_with'' or ''influences'', allowing for a more nuanced policy landscape.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_vs_legalization_framing, conceptual, 'Conceptual compatibility between legalization and prohibition frameworks.').


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
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(subs_be_t5, substance_control_authority__legalization_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__legalization_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(subs_be_t15, substance_control_authority__legalization_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(subs_su_t5, substance_control_authority__legalization_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__legalization_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(subs_su_t15, substance_control_authority__legalization_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_authority' kernel, focusing on market legalization. It is structurally distinct from the 'prohibition_reading' (criminalization) and 'harm_reduction_reading' (public health interventions), which are modeled as separate constraints due to differing ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
