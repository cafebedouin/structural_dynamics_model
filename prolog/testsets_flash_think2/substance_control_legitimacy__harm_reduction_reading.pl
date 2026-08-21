% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of state
 *   authority over substance use. It frames substance use as a public health
 *   issue, justifying state intervention to minimize harm through
 *   medicalization and treatment, rather than criminalization. However, this
 *   approach still involves significant state control, mandates, and active
 *   suppression of illicit markets, leading to moderate extraction from some
 *   users and black market actors. This is one reading of the
 *   'substance_control_legitimacy' kernel, distinct from prohibitionist or
 *   legalization readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.6).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.7).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Approach to Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '25f9cdfb-999e-4838-b1c2-204809ddd999').
narrative_ontology:cs_kernel_codification('25f9cdfb-999e-4838-b1c2-204809ddd999', formalized).
narrative_ontology:cs_authority_grounding('25f9cdfb-999e-4838-b1c2-204809ddd999', lineage).
narrative_ontology:cs_interpretation_layer_present('25f9cdfb-999e-4838-b1c2-204809ddd999').
narrative_ontology:cs_reading_relation('25f9cdfb-999e-4838-b1c2-204809ddd999', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('25f9cdfb-999e-4838-b1c2-204809ddd999', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('25f9cdfb-999e-4838-b1c2-204809ddd999', foundational, public_health_first).
narrative_ontology:cs_axiom_status(public_health_first, holdable).
narrative_ontology:cs_axiom_grounding('25f9cdfb-999e-4838-b1c2-204809ddd999', public_health_first, empirically_contingent).
narrative_ontology:cs_axiom('25f9cdfb-999e-4838-b1c2-204809ddd999', foundational, state_duty_to_minimize_harm).
narrative_ontology:cs_axiom_status(state_duty_to_minimize_harm, holdable).
narrative_ontology:cs_axiom_grounding('25f9cdfb-999e-4838-b1c2-204809ddd999', state_duty_to_minimize_harm, deontological).
narrative_ontology:cs_reference_frame('25f9cdfb-999e-4838-b1c2-204809ddd999', public_health_governance_framework).
narrative_ontology:cs_drift_state('25f9cdfb-999e-4838-b1c2-204809ddd999', contemporary_policy_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('25f9cdfb-999e-4838-b1c2-204809ddd999', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, some_substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users_mandated_treatment).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, substance_users_medicalized).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, public_health_paradigm).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, evidence_based_policy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities define and implement policies that medicalize substance use, establish treatment mandates, and manage public health interventions. They benefit from increased legitimacy and resources directed towards their mandate, but are constrained by political pressures and the persistence of illicit markets.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Organizations and professionals offering substance use treatment, counseling, and harm reduction services. They receive funding and legitimacy from the state's public health approach, expanding their operational scope and influence. Their services are central to the constraint's function.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, mobile, regional).

% Individuals who benefit from access to medical treatment, clean supplies, and reduced criminalization risks under this framework. While still subject to state oversight, their health outcomes and safety are prioritized over punitive measures. Their choices are often limited to state-sanctioned pathways.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users_medicalized, beneficiary,
    powerless, immediate, constrained, local).

% Individuals who are compelled into treatment or other interventions as a condition of avoiding criminal penalties or maintaining social benefits. They bear the costs of reduced autonomy and potential stigma, even if the intent is therapeutic. Their identity may become fused with their 'patient' status.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users_mandated_treatment, payer,
    powerless, immediate, identity_locked, local).

% Individuals and networks involved in the illicit production and distribution of substances. While the harm reduction approach aims to reduce their market, it does not eliminate them, and they face ongoing enforcement pressure. They are actively excluded from the legitimate economy and bear the costs of criminalization.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_actors, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, black_market_actors, excluded).

% Groups and individuals who believe substance use should be primarily addressed through criminalization and punitive measures. They are excluded from the policy-making process under this harm reduction framework, though they continue to exert political pressure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, analytical, national).

% Groups and individuals who advocate for full adult autonomy over substance use, with minimal state intervention beyond preventing third-party harm. They are excluded from the policy-making process under this harm reduction framework, as it still involves significant state control and mandates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a coherent, public health-led response to substance use, aiming to reduce overdose deaths, disease transmission, and social harms by medicalizing users and providing services, rather than relying solely on criminal justice.
% TRANSFER_FUNCTION: Transfers resources from general taxation to public health infrastructure and treatment services. It also transfers a degree of autonomy from substance users to public health authorities, often through mandated treatment or supervised consumption, in exchange for reduced criminalization risk.
% ABSENT_VOICES: Advocates for full prohibition (who seek criminalization) and full legalization (who seek complete autonomy) are excluded from the core framing of state authority in this harm reduction model. Black market actors are actively suppressed and excluded from any legitimate discourse.
% DISAPPEARANCE_RATIONALE: If this harm reduction framework vanished overnight, the state would likely revert to either a more punitive prohibitionist stance (re-criminalizing users) or move towards full legalization. Both scenarios would drastically alter public health systems, criminal justice practices, and the social landscape for substance users, leading to a significant reorganization of society's response.
% FOUNDING_PROBLEM: The recognition that punitive criminalization of substance use failed to reduce harm, led to mass incarceration, fueled dangerous black markets, and exacerbated public health crises (e.g., HIV/AIDS, overdose deaths), while also acknowledging that unregulated use carried significant social costs.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations, medical professionals, and international bodies (e.g., WHO, UNODC in some contexts) consistently attest to the ongoing public health problems associated with substance use and the comparative efficacy of harm reduction principles over strict prohibition. This corroboration comes from outside the direct beneficiaries of the current policy framework.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) due to treatment mandates and the costs imposed on users and black market actors, even if the intent is public health. Suppression is high (0.70) because the state actively enforces treatment compliance and suppresses the illicit market, limiting alternatives. Theater ratio is low (0.20) as the public health function is genuine, though some performativity may exist around the 'non-criminalization' aspect while mandates persist. Accessibility collapse is moderate (0.65) as legal alternatives to state-sanctioned treatment are limited, and the black market, though suppressed, remains a persistent alternative to the regulated system. Resistance is moderate (0.55) from both those who desire stricter prohibition and those who advocate for full legalization, as well as from black market actors.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and treatment providers view this as a beneficial, evidence-based approach to a complex social problem, genuinely coordinating care and reducing harm. However, some substance users experience it as a form of paternalistic control and extraction of autonomy, while black market actors experience it as direct suppression. Prohibition and legalization advocates see it as an inadequate or misguided compromise.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and treatment providers are beneficiaries, gaining legitimacy, resources, and control over the domain. Substance users who are medicalized benefit from reduced criminalization and access to services, but those under mandated treatment bear costs of reduced autonomy. Black market actors are targets, facing active suppression. Prohibition and legalization advocates are excluded from this specific policy framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this approach as a pure Rope (pure coordination) by acknowledging the significant extraction and suppression involved, particularly through treatment mandates and the ongoing fight against the black market. It also avoids mislabeling it as a Snare (pure extraction) by recognizing the genuine public health coordination function and the benefits for some users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'To what extent are treatment mandates and illicit market suppression structurally necessary for the public health coordination function, versus being forms of extraction or control?',
    'Comparative analysis of jurisdictions with varying degrees of mandates and market suppression within a harm reduction framework, assessing public health outcomes and user autonomy.',
    'If separable, the mandates and suppression are primarily extractive; if inseparable, they are part of the coordination cost. This would shift the balance of extractiveness and suppression in the base properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Distinguishing genuine coordination costs from extractive elements within harm reduction policies.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal mandates, enforcement against black markets) or internalized (stigma, self-perception of illness leading to compliance)?',
    'Longitudinal studies of individuals post-mandate or post-treatment, observing persistence of compliance or self-regulation in the absence of external enforcement. Qualitative research on user experiences of autonomy and stigma.',
    'If internalized, the effective suppression for substance users is higher than the structural measure suggests, as they carry the suppression with them. This would amplify their effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for substance users.').

omega_variable(
    black_market_persistence_impact,
    'Does the persistence of a black market undermine the public health goals of harm reduction, or does it serve as an unacknowledged ''safety valve'' for those unwilling or unable to engage with state-sanctioned pathways?',
    'Epidemiological studies comparing health outcomes (e.g., overdose rates, disease transmission) in areas with different levels of black market activity and harm reduction program penetration. Policy analysis of unintended consequences.',
    'If it undermines public health, the effectiveness of the coordination function is lower, potentially pushing the constraint towards a Snare. If it acts as a safety valve, the coordination function is more robust, but the suppression of the black market is less justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(black_market_persistence_impact, empirical, 'Role of the black market in the harm reduction framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, drug_criminalization_laws).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, public_health_funding).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'substance_control_legitimacy' kernel, each representing a distinct approach to state authority over substance use. This reading focuses on harm reduction and medicalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
