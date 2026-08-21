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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Substance Use as Moral Transgression (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition reading' of the substance
 *   control kernel, where substance use is framed as a moral transgression
 *   requiring state punishment to protect social order. This reading leads to
 *   high extractiveness and suppression, as users are criminalized and an
 *   enforcement apparatus becomes a primary beneficiary, alongside black
 *   market organizations. The state exercises coercive authority rather than
 *   service provision. The claimed type is 'snare' due to the clear victims
 *   and high extraction, despite the moral coordination narrative.
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
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use as Moral Transgression (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'c60900dc-93da-4f26-91e3-aa1c3b1c76cd').
narrative_ontology:cs_kernel_codification('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', formalized).
narrative_ontology:cs_authority_grounding('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', extraction).
narrative_ontology:cs_interpretation_layer_present('c60900dc-93da-4f26-91e3-aa1c3b1c76cd').
narrative_ontology:cs_reading_relation('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', substance_control_kernel__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', foundational, substance_use_is_moral_failing).
narrative_ontology:cs_axiom_status(substance_use_is_moral_failing, holdable).
narrative_ontology:cs_axiom_grounding('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', substance_use_is_moral_failing, deontological).
narrative_ontology:cs_axiom('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', state_punishment_protects_social_order, instrumental).
narrative_ontology:cs_reference_frame('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', punitive_moral_order_framework).
narrative_ontology:cs_drift_state('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', contemporary_public_health_advocacy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c60900dc-93da-4f26-91e3-aa1c3b1c76cd', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, black_market_organizations).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, public_health_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary enforcers of prohibition laws, benefiting from increased budgets, expanded powers, and asset forfeiture. Their mandate is to suppress substance use through arrest and incarceration, framing it as a moral and criminal issue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefits directly from the high incarceration rates driven by prohibition policies, receiving contracts for housing prisoners. They lobby for stricter sentencing and expanded criminalization.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_industry, beneficiary,
    organized, biographical, mobile, national).

% Thrive in the illicit market created by prohibition, profiting from inflated prices and lack of regulation. They benefit from the state's suppression of legal alternatives, which eliminates competition and drives users to their networks.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_organizations, beneficiary,
    organized, biographical, arbitrage, global).

% Criminalized for their substance use, facing arrest, incarceration, and social stigma. They bear the direct costs of punishment, limited access to healthcare, and increased vulnerability to exploitation by the black market.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by prohibition enforcement, leading to family separation, economic instability, and erosion of trust in public institutions. The moral framing of substance use often reinforces existing social biases against these communities.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Marginalized in policy discussions, with their focus on treatment and harm reduction often clashing with the punitive approach. Their funding and capacity are often diverted or under-resourced compared to law enforcement.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_services, excluded,
    moderate, biographical, constrained, national).

% Critique prohibition policies as infringements on individual rights and drivers of mass incarceration. They advocate for policy reform based on principles of liberty and justice.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social behavior by deterring substance use through criminal penalties, thereby maintaining a perceived moral and social order.
% TRANSFER_FUNCTION: Transfers individual liberty and economic resources (through fines, asset forfeiture, and incarceration costs) from substance users and taxpayers to law enforcement, the private prison industry, and black market organizations.
% ABSENT_VOICES: Public health experts and substance users themselves are largely excluded from policy-making, their perspectives on treatment, harm reduction, and lived experience often overridden by the moral and punitive framework.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the criminal justice system would undergo massive restructuring, private prisons would lose their primary revenue source, black markets would collapse, and public health services would need to rapidly scale up to address substance use as a health issue. Society would need to re-evaluate its approach to individual autonomy and state intervention.
% FOUNDING_PROBLEM: The perceived moral decay and social disorder associated with substance use, particularly among specific demographic groups, leading to calls for state intervention to enforce public morality and protect societal norms.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of prohibition, including some religious organizations and conservative political groups, continue to attest that substance use is a moral failing that threatens social order. However, public health organizations and civil liberties advocates strongly contest this, arguing the 'problem' is a social construct used to justify punitive measures, and that the actual harms are exacerbated by prohibition itself.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.85) because the policy directly criminalizes individuals for a health behavior, leading to loss of liberty, economic penalties, and social marginalization. Suppression is extremely high (0.92) as the state actively enforces prohibition through arrests, incarceration, and asset forfeiture, with limited legal alternatives for users. The theater ratio is moderate (0.45), reflecting that while there's a genuine stated goal of protecting social order, a significant portion of enforcement activity serves to maintain the extractive system and the black market it enables. Accessibility collapse is high (0.70) as legal and safe alternatives are severely restricted, pushing users into illicit channels. Resistance is also high (0.75) from affected communities and advocacy groups, indicating the constraint is actively contested.
 *
 * PERSPECTIVAL GAP:
 *   Law enforcement and private prison industries perceive this as a necessary framework for public safety and moral order, justifying their roles and benefits. Substance users and marginalized communities experience it as a highly extractive and suppressive system that causes immense harm, disproportionately targeting them. Public health services view it as counterproductive to health outcomes. The engine's classification as a snare reflects the latter perspectives, diverging from the 'moral order' narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and private prisons are clear beneficiaries, gaining resources and power from the punitive approach (low directionality). Black market organizations also benefit significantly from the illicit market created by prohibition (low directionality). Substance users and marginalized communities are direct targets, bearing the brunt of criminalization and social harm (high directionality). Public health services are excluded, their efforts often undermined by the punitive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a snare prevents mislabeling this as a legitimate coordination mechanism. While it claims to coordinate social order, its high extractiveness, suppression, and identifiable victims, coupled with the emergence of black market beneficiaries, reveal its true function as a system of extraction. The persistence of the 'founding problem' is contested, suggesting the mandate may have atrophied into a cover for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_vs_health_framing,
    'Is substance use primarily a moral transgression or a public health issue?',
    'Longitudinal studies comparing societal outcomes (crime rates, public health metrics, economic impact) in jurisdictions that adopt prohibition vs. harm reduction/legalization approaches.',
    'If primarily a health issue, the prohibition framework is fundamentally misaligned, leading to reclassification towards a snare or piton, as its stated coordination function (moral order) is a cover for extraction and harm. If genuinely a moral transgression, the prohibition reading might be re-evaluated as a tangled rope, assuming a genuine coordination function with asymmetric costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_vs_health_framing, conceptual, 'Ambiguity in the fundamental framing of substance use.').

omega_variable(
    black_market_dependency,
    'To what extent does the persistence of the prohibition framework depend on the vested interests of the black market it creates?',
    'Analysis of lobbying efforts, political donations, and violence patterns by illicit organizations in response to proposed decriminalization or legalization measures.',
    'If black market interests are a significant driver of prohibition''s persistence, it strengthens the ''snare'' classification by highlighting an additional, often hidden, beneficiary group actively working to maintain the extractive status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_dependency, empirical, 'Role of black market interests in maintaining prohibition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, incarceration) or internalized (stigma, fear of criminalization)?',
    'Post-decriminalization trajectory: if substance use rates and associated harms persist after legal barriers are removed, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them after legal changes, making exit harder even if structural barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for substance users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_kernel__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1980, substance_control_kernel__prohibition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(subs_tr_t1990, substance_control_kernel__prohibition_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__prohibition_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(subs_tr_t2010, substance_control_kernel__prohibition_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(subs_tr_t2024, substance_control_kernel__prohibition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_kernel__prohibition_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(subs_be_t1980, substance_control_kernel__prohibition_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(subs_be_t1990, substance_control_kernel__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__prohibition_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(subs_be_t2010, substance_control_kernel__prohibition_reading, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement(subs_be_t2024, substance_control_kernel__prohibition_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_kernel__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1980, substance_control_kernel__prohibition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(subs_su_t1990, substance_control_kernel__prohibition_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__prohibition_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(subs_su_t2010, substance_control_kernel__prohibition_reading, suppression_requirement, 2010, 0.93).
narrative_ontology:measurement(subs_su_t2024, substance_control_kernel__prohibition_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, mass_incarceration_system).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, racial_disparity_in_justice).

% DUAL FORMULATION NOTE:
% This is one reading of the 'substance_control_kernel'. Its high extractiveness and punitive approach contrast sharply with the harm reduction and legalization readings, which would yield lower extractiveness and different beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
