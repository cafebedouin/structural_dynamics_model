% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibitionist Reading of Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition_reading' of the
 *   'substance_control_legitimacy' kernel. It asserts that substance use is
 *   inherently harmful and that state authority derives from a moral duty to
 *   prevent this harm through criminalization. This reading leads to high
 *   extractiveness and suppression, primarily targeting substance users and
 *   marginalized communities, while benefiting carceral systems and law
 *   enforcement. The structural delta for this reading is that users enter
 *   the victim set via criminalization, leading to high carceral
 *   extractiveness and significant black market violence externalities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibitionist Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, 'e2432062-d9d1-495f-8c6b-5b32a379d92a').
narrative_ontology:cs_kernel_codification('e2432062-d9d1-495f-8c6b-5b32a379d92a', formalized).
narrative_ontology:cs_authority_grounding('e2432062-d9d1-495f-8c6b-5b32a379d92a', lineage).
narrative_ontology:cs_interpretation_layer_present('e2432062-d9d1-495f-8c6b-5b32a379d92a').
narrative_ontology:cs_reading_relation('e2432062-d9d1-495f-8c6b-5b32a379d92a', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('e2432062-d9d1-495f-8c6b-5b32a379d92a', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('e2432062-d9d1-495f-8c6b-5b32a379d92a', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('e2432062-d9d1-495f-8c6b-5b32a379d92a', substance_use_inherently_harmful, deontological).
narrative_ontology:cs_axiom('e2432062-d9d1-495f-8c6b-5b32a379d92a', foundational, state_moral_duty_to_criminalize).
narrative_ontology:cs_axiom_status(state_moral_duty_to_criminalize, holdable).
narrative_ontology:cs_axiom_grounding('e2432062-d9d1-495f-8c6b-5b32a379d92a', state_moral_duty_to_criminalize, deontological).
narrative_ontology:cs_reference_frame('e2432062-d9d1-495f-8c6b-5b32a379d92a', moral_prohibition_framework).
narrative_ontology:cs_drift_state('e2432062-d9d1-495f-8c6b-5b32a379d92a', contemporary_public_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e2432062-d9d1-495f-8c6b-5b32a379d92a', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, carceral_system_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, private_prison_corporations).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, black_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, black_market_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and maintain criminalization laws based on the moral duty to prevent harm from substance use. They control the legal framework and resource allocation for enforcement.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Actively enforce prohibition laws, leading to arrests and seizures. They benefit from increased budgets, asset forfeiture, and expanded powers justified by the 'war on drugs'.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, beneficiary).

% Benefit from the high incarceration rates driven by prohibition, leading to stable demand for prison beds and related services. This includes both public and private entities.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, carceral_system_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Face criminal charges, incarceration, social stigma, and barriers to employment and housing. Their health issues are treated as criminal offenses rather than public health concerns, making exit from the cycle of use and criminalization extremely difficult.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by prohibition enforcement, leading to family separation, community destabilization, and intergenerational poverty. The criminalization framework reinforces existing social inequalities.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Profit from the artificially high prices and lack of regulation in the illicit drug trade created by prohibition. They also bear the risks of violence and criminal prosecution inherent in this market.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_actors, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, black_market_actors, payer).

% Argue for a public health approach to substance use, emphasizing treatment, prevention, and harm reduction over criminalization. Their policy proposals are often marginalized or rejected within the prohibitionist framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, carceral_system_operators).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state resources and legal frameworks to enforce a uniform moral stance against substance use, aiming to deter use and protect public morality.
% TRANSFER_FUNCTION: Transfers resources (taxpayer money, law enforcement budgets) to carceral systems and law enforcement, and transfers freedom, economic opportunity, and health from substance users and marginalized communities to the state and its enforcement apparatus.
% ABSENT_VOICES: Public health advocates, civil liberties organizations, and many substance users themselves are largely excluded from policy-making, advocating for approaches that prioritize health and autonomy over criminalization.
% DISAPPEARANCE_RATIONALE: If the prohibitionist framework vanished overnight, the criminal justice system would be dramatically reshaped, incarceration rates would plummet, black markets would collapse or transform, and public health systems would need to rapidly scale up to address substance use as a health issue. Society would need to fundamentally rethink its approach to drugs and crime.
% FOUNDING_PROBLEM: The perceived moral decay and social harms associated with substance use, particularly in the early 20th century, leading to a belief that criminalization was the only effective means of control and protection.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of prohibition (e.g., some law enforcement officials, moral conservative groups) argue the problem of inherent harm is still live. Public health experts, economists, and civil rights organizations (outside the benefiting parties) widely attest that the founding problem has been exacerbated, not solved, by criminalization, leading to a 'dead' status for the original problem and the creation of new harms.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the massive transfer of wealth and freedom from individuals and communities to the carceral state. Suppression (0.92) is extremely high, relying on active enforcement, criminal penalties, and the suppression of alternative public health approaches. The theater ratio (0.45) is moderate but rising, as the stated goal of 'harm prevention' increasingly masks the actual function of maintaining carceral infrastructure and illicit markets. Resistance (0.78) is also high, reflecting ongoing advocacy for reform and decriminalization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and carceral beneficiaries, this is a legitimate exercise of moral authority to protect society. From the perspective of substance users and marginalized communities, it is a highly extractive and suppressive system that creates more harm than it prevents. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and law enforcement act as agenda-setters and beneficiaries, directing resources and enforcing the prohibition. Carceral system operators and private prison corporations are clear beneficiaries of the resulting incarceration rates. Substance users and marginalized communities are the primary victims, bearing the brunt of criminalization. Black market actors are complex: they benefit from the illicit market created by prohibition but also face its inherent violence and legal risks. Public health advocates are excluded, their alternative framings suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_harm_vs_contextual_harm,
    'Is substance use inherently harmful, or is harm primarily a consequence of criminalization and social context?',
    'Comparative analysis of health and social outcomes in jurisdictions with different regulatory regimes (prohibition vs. harm reduction vs. legalization).',
    'If harm is primarily contextual, the foundational axiom of this reading is undermined, shifting the constraint''s justification from moral duty to a policy choice with identifiable victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_harm_vs_contextual_harm, empirical, 'Whether the harm from substance use is intrinsic or policy-induced.').

omega_variable(
    moral_duty_vs_state_interest,
    'Does state authority genuinely derive from a moral duty to prevent harm through criminalization, or from a more instrumental interest in social control and resource allocation?',
    'Historical analysis of policy motivations, legislative debates, and the distribution of benefits and harms from prohibition.',
    'If the grounding is instrumental, the constraint''s claimed legitimacy (deontological moral duty) is a cover for a more extractive political economy, reclassifying it as a clearer snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_duty_vs_state_interest, conceptual, 'The true grounding of state authority in substance control.').

omega_variable(
    black_market_externality_cost,
    'What is the full societal cost of the black market violence and corruption generated by prohibition, and how does it compare to the harms prohibition aims to prevent?',
    'Comprehensive economic and social impact assessments, including crime statistics, public health data, and international comparisons.',
    'If the externality costs outweigh the prevented harms, the constraint''s net effect is negative, further undermining its claimed coordination function and reinforcing its snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_externality_cost, empirical, 'The net societal impact of prohibition-induced black markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_legitimacy__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1985, substance_control_legitimacy__prohibition_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(subs_tr_t2000, substance_control_legitimacy__prohibition_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(subs_tr_t2010, substance_control_legitimacy__prohibition_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(subs_tr_t2024, substance_control_legitimacy__prohibition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(subs_be_t1985, substance_control_legitimacy__prohibition_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(subs_be_t2000, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(subs_be_t2010, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(subs_be_t2024, substance_control_legitimacy__prohibition_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1985, substance_control_legitimacy__prohibition_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(subs_su_t2010, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(subs_su_t2024, substance_control_legitimacy__prohibition_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, carceral_system_funding).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, police_power_expansion).

% DUAL FORMULATION NOTE:
% This constraint is the 'prohibition_reading' of the 'substance_control_legitimacy' kernel, which also includes 'harm_reduction_reading' and 'legalization_reading'. Each reading represents a distinct structural claim about state authority and substance use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
