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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to regulate drug markets
 *   as legal commerce, with a focus on quality and access controls. It is a
 *   'legalization_reading' of the broader 'substance_control_authority'
 *   kernel, contrasting with prohibitionist and harm-reduction-only
 *   approaches. The goal is to displace illegal markets, reduce associated
 *   crime, and improve public health outcomes through regulated supply and
 *   education. The claimed type is 'rope' as it aims to be a stable,
 *   beneficial coordination mechanism, despite inherent regulatory costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.35).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.2).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '840995ac-9ca3-4c5b-889c-9fe061f7b5e1').
narrative_ontology:cs_kernel_codification('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', formalized).
narrative_ontology:cs_authority_grounding('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', practice).
narrative_ontology:cs_interpretation_layer_present('840995ac-9ca3-4c5b-889c-9fe061f7b5e1').
narrative_ontology:cs_reading_relation('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', foundational, individual_autonomy_over_body).
narrative_ontology:cs_axiom_status(individual_autonomy_over_body, holdable).
narrative_ontology:cs_axiom_grounding('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', individual_autonomy_over_body, deontological).
narrative_ontology:cs_axiom('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', foundational, public_health_through_market_regulation).
narrative_ontology:cs_axiom_status(public_health_through_market_regulation, holdable).
narrative_ontology:cs_axiom_grounding('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', public_health_through_market_regulation, instrumental).
narrative_ontology:cs_reference_frame('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', regulated_market_public_health_framework).
narrative_ontology:cs_drift_state('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', contemporary_policy_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('840995ac-9ca3-4c5b-889c-9fe061f7b5e1', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_regulatory_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, legal_drug_producers_distributors).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, consumers_of_substances).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, third_parties_affected_by_crime).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, former_illegal_market_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces regulations for legal substance markets, including licensing, quality control, taxation, and access restrictions. Benefits from tax revenue and increased public health control.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Operate within the legal framework, benefiting from market access, stability, and the elimination of illegal competition. Subject to regulatory compliance costs and taxes.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, legal_drug_producers_distributors, beneficiary,
    powerful, biographical, arbitrage, national).

% Gain access to regulated, quality-controlled products, reducing health risks associated with unregulated markets. Avoid criminalization for use/possession. May face higher prices due to taxes/regulation.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, consumers_of_substances, beneficiary,
    moderate, biographical, mobile, local).

% Lose their market share and revenue as legal alternatives emerge. Face the choice of exiting the industry, transitioning to legal operations (if possible), or attempting to persist in a shrinking black market.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, former_illegal_market_operators, payer,
    powerless, immediate, trapped, local).

% Monitor the public health outcomes of legalization, advocating for effective access controls, public education, and treatment services. May raise concerns about potential increases in substance use.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% Experiences a reduction in drug-related arrests and prosecutions, potentially leading to shifts in resource allocation and a re-evaluation of its role in public safety. Bears the cost of adapting to new legal frameworks.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, criminal_justice_system, payer,
    institutional, generational, constrained, national).

% Benefit from a reduction in drug-related crime and associated social disorder that often accompanies illegal markets. Their safety and quality of life improve.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, third_parties_affected_by_crime, beneficiary,
    powerless, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, state_regulatory_agencies).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legal, regulated market for substances, ensuring product quality, controlling access, generating tax revenue, and displacing illegal markets and their associated crime and public health harms.
% TRANSFER_FUNCTION: Transfers control, revenue, and risk from illegal market operators to state regulatory bodies and legal businesses. Shifts public health management from criminalization to public health interventions and market regulation. Transfers tax revenue to public services.
% ABSENT_VOICES: Hardline prohibitionists would object, arguing that any legalization undermines public morality and inevitably leads to increased societal harm. Some public health groups might argue for stricter controls or a more cautious approach to prevent increased use.
% DISAPPEARANCE_RATIONALE: If the authority to regulate legal substance markets vanished, the system would revert to either unregulated illegal markets or a prohibitionist regime, both of which would lead to significant social and public health disruption, crime, and loss of tax revenue. The entire economic and social infrastructure built around legal commerce would collapse.
% FOUNDING_PROBLEM: The widespread harms associated with illegal drug markets, including crime, violence, lack of product quality control, public health crises (e.g., overdose deaths), and the social and economic costs of criminalizing substance users.
% FOUNDING_PROBLEM_CORROBORATION: Numerous international commissions on drug policy, public health organizations (e.g., WHO), economic studies on the costs of prohibition, and former law enforcement officials corroborate the persistence and severity of these problems under prohibitionist regimes, supporting the need for alternative approaches like legalization.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.35) is moderate, reflecting necessary taxes and regulatory overhead, but significantly lower than the 'extraction' of illegal markets. Suppression (0.20) is low, as criminal penalties for users are removed, replaced by regulatory compliance for producers. Theater ratio (0.10) is low, indicating a genuine and functional regulatory apparatus. Accessibility collapse (0.40) reflects the shift from illegal to legal access, while resistance (0.50) comes from displaced illegal operators and some public health concerns about increased use. The metrics reflect a system that, while not cost-free, is designed to be a net benefit for society compared to alternatives.
 *
 * PERSPECTIVAL GAP:
 *   While the state and legal industry view this as a beneficial coordination, former illegal market operators experience it as a complete loss of their livelihood. Public health advocates may see it as a necessary step but remain vigilant about potential negative consequences like increased use, highlighting a tension between economic and public health goals.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory agencies are the agenda-setters and primary beneficiaries, gaining control and tax revenue. Legal producers and consumers are also beneficiaries, gaining market stability and safe access, respectively. Former illegal market operators are clear victims, losing their illicit revenue streams. The criminal justice system is a payer, losing its traditional enforcement role but potentially gaining efficiency. Third parties benefit from reduced crime.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    increased_use_volume_net_impact,
    'Will the potential increase in substance use volume, a delta of legalization, result in a net negative public health outcome that outweighs the benefits of reduced crime and regulated supply?',
    'Longitudinal epidemiological studies tracking substance use rates, addiction rates, and public health indicators in jurisdictions that have implemented legalization, compared to control groups.',
    'If a significant net negative impact is demonstrated, it could lead to calls for stricter access controls, higher taxation, or a re-evaluation of the legalization model, potentially shifting the constraint''s effective extractiveness (e.g., through higher taxes to fund treatment) or suppression (e.g., through stricter marketing rules).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(increased_use_volume_net_impact, empirical, 'Assessing the overall public health impact of increased substance use under legalization.').

omega_variable(
    illegal_market_persistence,
    'To what extent will illegal markets for substances truly be eliminated, or will they adapt and persist by targeting specific niches (e.g., lower prices, specific products, underage access)?',
    'Ongoing market analysis, law enforcement intelligence, and sociological studies tracking the size and activity of illegal markets post-legalization.',
    'If illegal markets persist significantly, the constraint''s effectiveness in achieving its coordination function (displacing illicit trade) would be undermined, potentially increasing the effective suppression required to manage the remaining black market and reducing the perceived benefits for consumers and public safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illegal_market_persistence, empirical, 'The degree to which legalization successfully eliminates illegal substance markets.').

omega_variable(
    regulatory_control_vs_new_extraction,
    'Does the shift from criminal suppression to regulatory control effectively eliminate extraction, or does it create new forms of extraction or control by legal monopolies or over-burdensome compliance costs?',
    'Economic analysis of market concentration, profit margins of legal producers, and the burden of compliance costs on small businesses, compared to the public health benefits and tax revenues.',
    'If new forms of extraction or control by legal entities are significant, the constraint''s effective extractiveness could be higher than intended, potentially shifting its classification towards a ''tangled_rope'' for some stakeholders (e.g., small producers, consumers facing high prices).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_control_vs_new_extraction, conceptual, 'Evaluating whether regulatory control introduces new forms of extraction.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''legalization_reading'' of the ''substance_control_authority'' kernel. What would change structurally if a sibling reading (e.g., ''prohibition_reading'' or ''harm_reduction_reading'') were adopted?',
    'Conceptual analysis of the core premises and policy implications of each reading.',
    'Adopting the ''prohibition_reading'' would drastically increase suppression and extractiveness for users, re-establishing criminal justice as the primary mechanism. Adopting the ''harm_reduction_reading'' would shift focus to public health interventions, potentially without full market legalization, altering the beneficiary/victim sets and the nature of state intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Clarifying the structural differences between this reading and its siblings within the ''substance_control_authority'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__legalization_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__legalization_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.23).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__legalization_reading, suppression_requirement, 16, 0.21).
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
