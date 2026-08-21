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
 *   high extraction from users and disproportionately impacted communities,
 *   while empowering state enforcement and creating lucrative black markets.
 *   The structural delta from other readings is significant: users become a
 *   criminal victim set, the enforcement apparatus becomes a primary
 *   beneficiary, and black market violence emerges as a secondary
 *   externality, with the state exercising coercive rather than
 *   service-provision authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.9).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use as Moral Transgression (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, '087d1327-8573-459a-9acb-b5d28f0c4e66').
narrative_ontology:cs_kernel_codification('087d1327-8573-459a-9acb-b5d28f0c4e66', formalized).
narrative_ontology:cs_authority_grounding('087d1327-8573-459a-9acb-b5d28f0c4e66', extraction).
narrative_ontology:cs_interpretation_layer_present('087d1327-8573-459a-9acb-b5d28f0c4e66').
narrative_ontology:cs_reading_relation('087d1327-8573-459a-9acb-b5d28f0c4e66', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('087d1327-8573-459a-9acb-b5d28f0c4e66', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('087d1327-8573-459a-9acb-b5d28f0c4e66', foundational, substance_use_is_moral_transgression).
narrative_ontology:cs_axiom_status(substance_use_is_moral_transgression, holdable).
narrative_ontology:cs_axiom_grounding('087d1327-8573-459a-9acb-b5d28f0c4e66', substance_use_is_moral_transgression, deontological).
narrative_ontology:cs_axiom('087d1327-8573-459a-9acb-b5d28f0c4e66', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('087d1327-8573-459a-9acb-b5d28f0c4e66', state_punishment_protects_social_order, instrumental).
narrative_ontology:cs_reference_frame('087d1327-8573-459a-9acb-b5d28f0c4e66', moral_order_through_punishment).
narrative_ontology:cs_drift_state('087d1327-8573-459a-9acb-b5d28f0c4e66', contemporary_public_health_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('087d1327-8573-459a-9acb-b5d28f0c4e66', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, state_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, moral_conservatives).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, black_market_actors).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, black_market_victims).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, disproportionately_impacted_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces drug prohibition laws, receiving increased budgets, personnel, and power. Benefits from the expanded scope of criminal justice and maintains a narrative of protecting social order.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Face criminalization, incarceration, social stigma, and barriers to employment and housing. Bear the direct costs of state punishment and often struggle to access health services due to fear of legal repercussions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, national).

% Advocate for and benefit from policies that align with their moral frameworks, seeing prohibition as a vindication of social values and a means to maintain perceived social order. Their political influence is enhanced by the policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, moral_conservatives, beneficiary,
    organized, generational, mobile, national).

% Profit immensely from the illegality of substances, which drives up prices and eliminates legal competition. They operate outside state regulation, often using violence to enforce their market control.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_actors, beneficiary,
    powerful, biographical, arbitrage, regional).

% Suffer from the violence, exploitation, and lack of recourse inherent in unregulated black markets. Often caught between law enforcement and criminal organizations, with no safe avenues for help.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_victims, payer,
    powerless, immediate, trapped, local).

% Benefits from increased incarceration rates driven by drug offenses, securing lucrative government contracts and lobbying for policies that maintain high prison populations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Are systematically excluded from policy-making and resource allocation, as their public health-oriented approach directly contradicts the punitive framework of prohibition. Their expertise is sidelined.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Their evidence-based recommendations for treating substance use as a health issue are often ignored or actively opposed by the prohibition framework, limiting their ability to implement effective interventions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_officials, excluded,
    institutional, biographical, constrained, national).

% Experience higher rates of policing, arrests, and incarceration due to prohibition, leading to family separation, economic instability, and erosion of trust in public institutions. Their identity is often criminalized by association.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, disproportionately_impacted_communities, payer,
    powerless, generational, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, state_enforcement_apparatus).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate social behavior by establishing a moral norm against substance use, aiming to prevent perceived social decay and maintain public order through state coercion.
% TRANSFER_FUNCTION: Transfers significant financial resources (taxpayer money, seized assets) to the state enforcement apparatus and private prison industry. It transfers freedom, well-being, and social capital from substance users and affected communities to the state and those who profit from the black market.
% ABSENT_VOICES: Substance users, public health experts, harm reduction advocates, and communities disproportionately impacted by enforcement are largely excluded. They would argue for health-centered approaches, decriminalization, and an end to punitive policies.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the criminal justice system would undergo a massive restructuring, black markets would collapse, and public health-oriented approaches to substance use would likely emerge as the primary framework. The economic and social landscape would fundamentally shift.
% FOUNDING_PROBLEM: The policy was built to address perceived moral decay, social disorder, and public safety threats associated with substance use, often framed as a 'war on drugs' to protect society.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (some law enforcement, moral conservatives) claim the founding problem is still live, citing ongoing social harms. Opponents (public health organizations, civil liberties groups, economists) argue that the policy itself exacerbates many of the problems it claims to solve, and that the original problem has either shifted or is better addressed by alternative means. Independent academic research and international comparisons often support the latter view.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very high (0.85) due to the severe penalties, asset forfeiture, and social costs imposed on users and communities. Suppression is also very high (0.90) as the state actively criminalizes and punishes, with limited legal alternatives for users. Theater ratio is high (0.60) because while the stated goal is social order, the policy often generates more disorder (e.g., black market violence, mass incarceration) than it prevents, with much enforcement activity serving to maintain the punitive framework itself rather than genuinely reducing harm. Accessibility collapse is moderate (0.70) as legal alternatives are suppressed, but illicit markets provide access, albeit with high risks. Resistance is high (0.75) from users, civil liberties advocates, and those harmed by the policy.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of prohibition (e.g., moral conservatives, some law enforcement) perceive the constraint as a necessary 'rope' for social order, with minimal extraction. However, from the perspective of substance users and affected communities, it operates as a severe 'snare' that extracts freedom, health, and resources while creating immense harm. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state enforcement apparatus, private prison industry, and moral conservatives are clear beneficiaries, gaining power, profit, and moral vindication. Black market actors also benefit significantly from the artificially inflated value of illicit substances. Substance users, black market victims, and disproportionately impacted communities are the primary targets, bearing the brunt of criminalization, violence, and social costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_vs_empirical_grounding,
    'Is the prohibition policy primarily grounded in moral claims about substance use, or in empirically verifiable evidence of its effectiveness in protecting social order?',
    'Comprehensive, independent meta-analysis of the social and public health outcomes of prohibition versus alternative policies (e.g., decriminalization, legalization, harm reduction) across multiple jurisdictions.',
    'If primarily moral, the policy''s persistence is a preference-based choice, less susceptible to empirical challenge. If claimed as empirical but evidence contradicts, it strengthens the ''theater'' and ''snare'' classifications, indicating a policy maintained despite its stated instrumental failures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_vs_empirical_grounding, empirical, 'Distinguishing the policy''s normative vs. instrumental justifications.').

omega_variable(
    efficacy_of_punishment_vs_harm_creation,
    'Does state punishment for substance use effectively reduce overall societal harm and substance use rates, or does it primarily create new harms (e.g., black markets, disease transmission, mass incarceration) that outweigh any benefits?',
    'Longitudinal studies comparing public health, crime, and social welfare indicators in jurisdictions with prohibition versus those with harm reduction or legalization policies, controlling for confounding factors.',
    'If punishment creates more harm, the constraint''s ''extractiveness'' and ''suppression'' are amplified by its counterproductive nature, reinforcing a ''snare'' classification. If it genuinely reduces harm, it would support a ''tangled_rope'' or even ''rope'' classification, though this is highly contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_punishment_vs_harm_creation, empirical, 'Assessing the net societal impact of punitive drug policies.').

omega_variable(
    black_market_integration_ambiguity,
    'To what extent is the black market an ''unintended consequence'' versus an ''integrated feature'' of the prohibition framework, serving to concentrate profits and maintain the policy''s political economy?',
    'Analysis of lobbying efforts by enforcement agencies and private prison industries, and historical studies of policy evolution, to determine if the black market''s existence is tacitly or actively leveraged to justify continued prohibition and its associated budgets.',
    'If integrated, the black market''s harms are part of the constraint''s core extractive function, not an externality, further solidifying a ''snare'' classification and increasing the effective extraction for victims. If truly unintended, it''s a severe policy failure, but not necessarily part of the constraint''s direct extractive mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_integration_ambiguity, conceptual, 'Role of black markets in the political economy of prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_kernel__prohibition_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(subs_tr_t1980, substance_control_kernel__prohibition_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(subs_tr_t1990, substance_control_kernel__prohibition_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__prohibition_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(subs_tr_t2010, substance_control_kernel__prohibition_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(subs_tr_t2020, substance_control_kernel__prohibition_reading, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_kernel__prohibition_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(subs_be_t1980, substance_control_kernel__prohibition_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(subs_be_t1990, substance_control_kernel__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__prohibition_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(subs_be_t2010, substance_control_kernel__prohibition_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(subs_be_t2020, substance_control_kernel__prohibition_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_kernel__prohibition_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(subs_su_t1980, substance_control_kernel__prohibition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(subs_su_t1990, substance_control_kernel__prohibition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__prohibition_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(subs_su_t2010, substance_control_kernel__prohibition_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(subs_su_t2020, substance_control_kernel__prohibition_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
