% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Prohibition as Moral Transgression
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition reading' of the broader
 *   'substance_control_kernel.' It frames substance use as a moral
 *   transgression requiring state punishment to protect social order. This
 *   reading leads to high extraction from users and marginalized communities,
 *   and high suppression through active enforcement. The enforcement
 *   apparatus and associated industries become primary beneficiaries, while
 *   black market violence emerges as a significant externality. This story
 *   focuses solely on the structural implications of this specific reading,
 *   without incorporating elements of alternative readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Prohibition as Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '5a6d648d-a218-49c6-98e3-2fd381c79937').
narrative_ontology:cs_kernel_codification('5a6d648d-a218-49c6-98e3-2fd381c79937', formalized).
narrative_ontology:cs_authority_grounding('5a6d648d-a218-49c6-98e3-2fd381c79937', extraction).
narrative_ontology:cs_interpretation_layer_present('5a6d648d-a218-49c6-98e3-2fd381c79937').
narrative_ontology:cs_reading_relation('5a6d648d-a218-49c6-98e3-2fd381c79937', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('5a6d648d-a218-49c6-98e3-2fd381c79937', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('5a6d648d-a218-49c6-98e3-2fd381c79937', foundational, substance_use_is_moral_failing).
narrative_ontology:cs_axiom_status(substance_use_is_moral_failing, holdable).
narrative_ontology:cs_axiom_grounding('5a6d648d-a218-49c6-98e3-2fd381c79937', substance_use_is_moral_failing, deontological).
narrative_ontology:cs_axiom('5a6d648d-a218-49c6-98e3-2fd381c79937', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('5a6d648d-a218-49c6-98e3-2fd381c79937', state_punishment_protects_social_order, instrumental).
narrative_ontology:cs_reference_frame('5a6d648d-a218-49c6-98e3-2fd381c79937', moral_order_through_punishment).
narrative_ontology:cs_drift_state('5a6d648d-a218-49c6-98e3-2fd381c79937', contemporary_public_health_advocacy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5a6d648d-a218-49c6-98e3-2fd381c79937', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, political_actors_campaigning_on_order).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, black_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, black_market_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary enforcers of prohibition laws, benefiting from increased budgets, expanded powers, and public support for 'tough on crime' stances. Their mandate is to identify, arrest, and prosecute individuals involved in substance use or distribution.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of criminalization, including arrest, incarceration, fines, and social stigma. Their health needs are often secondary to their legal status, and access to safe supply or treatment is severely restricted.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by prohibition enforcement, leading to family separation, economic instability, and erosion of trust in public institutions. The social order justification often masks underlying racial or class biases in enforcement.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Benefits directly from increased incarceration rates driven by prohibition, securing lucrative government contracts and lobbying for policies that maintain high inmate populations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_industry, beneficiary,
    organized, biographical, arbitrage, national).

% Gain political capital by advocating for strict prohibition, appealing to a segment of the electorate concerned with 'social order' and 'moral values.' They benefit from the perceived success of enforcement, regardless of public health outcomes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, political_actors_campaigning_on_order, beneficiary,
    powerful, immediate, mobile, national).

% Benefit from the inflated prices and lack of regulation in an illegal market, but also bear the costs of violence, interdiction, and legal penalties associated with illicit trade. They are a direct consequence of the prohibition framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_actors, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__prohibition_reading, black_market_actors, payer).

% Advocate for evidence-based public health approaches to substance use, but their voices are often marginalized in policy debates dominated by criminal justice frameworks. They would argue for decriminalization and treatment-on-demand.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state resources (police, courts, prisons) to enforce a moral boundary around substance use, aiming to deter use and maintain a perceived social order. It also coordinates the illicit market by creating a high-risk, high-reward environment for black market actors.
% TRANSFER_FUNCTION: Transfers freedom, economic opportunity, and health from substance users and marginalized communities to the state's enforcement apparatus and associated industries (e.g., private prisons). It also transfers wealth from users to black market actors due to inflated prices.
% ABSENT_VOICES: Public health advocates, medical professionals, and individuals with lived experience of substance use are often excluded from policy-making, replaced by law enforcement and political figures. They would argue for a health-first approach, decriminalization, and safe supply.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the criminal justice system would be dramatically reshaped, black markets would collapse, and public health systems would face an immediate need to scale up treatment and harm reduction services. Social norms around substance use would undergo rapid re-evaluation.
% FOUNDING_PROBLEM: The perceived moral decay and social disorder associated with certain substances, leading to a belief that state intervention through criminalization was necessary to protect public morality and safety.
% FOUNDING_PROBLEM_CORROBORATION: Political actors and some segments of the public attest the problem is live, citing ongoing social harms. Public health experts and civil liberties advocates, from outside the benefiting parties, attest the founding problem is misdiagnosed or exacerbated by prohibition itself, arguing that the 'solution' creates more problems than it solves.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the severe penalties, fines, and loss of liberty imposed on substance users, coupled with the economic burden on marginalized communities. Suppression is extremely high (0.92) as the state actively criminalizes and punishes use, with extensive surveillance and policing. Theater ratio is moderate (0.45) because while the stated goal is social order and public safety, a significant portion of enforcement activity maintains the illicit market's profitability for black market actors and expands the carceral state, rather than genuinely reducing harm or use.
 *
 * PERSPECTIVAL GAP:
 *   The state's perspective (as embodied by law enforcement and political actors) frames prohibition as a necessary defense of social order, justifying high suppression and extraction. From the perspective of substance users and marginalized communities, it is a system of oppression and punishment that exacerbates harm. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and private prisons are clear beneficiaries (d near 0.0) due to expanded budgets and profits. Substance users and marginalized communities are direct targets (d near 1.0) bearing the brunt of criminalization. Black market actors are complex: they benefit from inflated prices but are also targets of enforcement, placing their d in the constrained middle. Political actors benefit from campaigning on 'order' (d near 0.0). Public health advocates are excluded, their perspective actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_vs_health_framing,
    'Is substance use fundamentally a moral transgression or a public health condition?',
    'Shifting societal consensus, evidenced by legislative changes, public opinion polls, and medical community consensus statements. If the framing shifts to health, the entire structural basis of this constraint would be challenged.',
    'If re-framed as a health issue, the constraint would likely reclassify from Snare to a Scaffold (transitional support for health services) or Rope (coordination of health interventions), with significantly lower extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_vs_health_framing, conceptual, 'Ambiguity in the fundamental nature of substance use.').

omega_variable(
    efficacy_of_punishment,
    'Does state punishment effectively deter substance use and protect social order, or does it exacerbate related harms?',
    'Empirical studies comparing outcomes in jurisdictions with prohibition vs. harm reduction/legalization policies (e.g., rates of overdose, violent crime, treatment access).',
    'If punishment is shown to be ineffective or counterproductive, the justification for high suppression and extraction collapses, leading to reclassification towards a Piton (ineffective, inertial) or a different type of Scaffold (transitional to a new policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_punishment, empirical, 'Empirical efficacy of criminalization as a deterrent.').

omega_variable(
    black_market_as_externality_or_feature,
    'Is the black market a regrettable externality of prohibition, or an inherent feature that benefits certain actors within the prohibition framework?',
    'Economic analysis of illicit market dynamics, including profit structures, supply chains, and the role of enforcement in maintaining market conditions. Examination of lobbying efforts by enforcement-adjacent industries.',
    'If the black market is a feature, it strengthens the Snare classification by revealing a deeper, systemic extractive function. If it''s a regrettable externality, it points to a failure of the constraint''s stated purpose, potentially shifting towards Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_as_externality_or_feature, conceptual, 'Role of the black market in the prohibition framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(subs_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(subs_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.84).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(subs_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
