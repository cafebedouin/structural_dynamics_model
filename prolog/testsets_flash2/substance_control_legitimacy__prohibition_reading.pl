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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibitionist Reading of Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the prohibitionist reading of substance
 *   control, asserting that substance use is inherently harmful and that
 *   state authority derives from a moral duty to prevent this harm through
 *   criminalization. This reading places substance users and marginalized
 *   communities in the victim set due to criminalization, and identifies law
 *   enforcement, private prisons, and illicit traffickers as beneficiaries.
 *   The high extractiveness and suppression reflect the carceral costs and
 *   the violence externality of the black market. The claimed type is 'snare'
 *   because the coordination story (preventing harm) is largely cover for the
 *   extraction and suppression, with identifiable victims and active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.9).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibitionist Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '1d95d11b-8508-4db3-91f7-6daff53da4c5').
narrative_ontology:cs_kernel_codification('1d95d11b-8508-4db3-91f7-6daff53da4c5', formalized).
narrative_ontology:cs_authority_grounding('1d95d11b-8508-4db3-91f7-6daff53da4c5', lineage).
narrative_ontology:cs_interpretation_layer_present('1d95d11b-8508-4db3-91f7-6daff53da4c5').
narrative_ontology:cs_reading_relation('1d95d11b-8508-4db3-91f7-6daff53da4c5', substance_control_legitimacy__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('1d95d11b-8508-4db3-91f7-6daff53da4c5', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('1d95d11b-8508-4db3-91f7-6daff53da4c5', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('1d95d11b-8508-4db3-91f7-6daff53da4c5', substance_use_inherently_harmful, deontological).
narrative_ontology:cs_axiom('1d95d11b-8508-4db3-91f7-6daff53da4c5', foundational, state_moral_duty_to_criminalize_harm).
narrative_ontology:cs_axiom_status(state_moral_duty_to_criminalize_harm, holdable).
narrative_ontology:cs_axiom_grounding('1d95d11b-8508-4db3-91f7-6daff53da4c5', state_moral_duty_to_criminalize_harm, deontological).
narrative_ontology:cs_reference_frame('1d95d11b-8508-4db3-91f7-6daff53da4c5', moral_paternalism_criminal_justice_framework).
narrative_ontology:cs_drift_state('1d95d11b-8508-4db3-91f7-6daff53da4c5', contemporary_public_health_paradigm, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1d95d11b-8508-4db3-91f7-6daff53da4c5', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, private_prison_corporations).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, illicit_drug_traffickers).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, public_health_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce drug laws, leading to arrests and incarceration. Their budgets and institutional power are often tied to the scale of drug-related crime and enforcement efforts. They benefit from the expanded scope of their operations and funding.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Profit from increased incarceration rates driven by drug offenses. They lobby for stricter sentencing and benefit directly from contracts to house inmates, creating a financial incentive for the persistence of prohibition.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, private_prison_corporations, beneficiary,
    organized, biographical, arbitrage, national).

% Operate in the black market created by prohibition, where the illegality of substances drives up prices and profit margins. They benefit from the lack of legal competition and the high risk premium associated with their trade.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, illicit_drug_traffickers, beneficiary,
    organized, immediate, mobile, global).

% Face criminal charges, incarceration, social stigma, and health risks due to the unregulated nature of illicit substances. Their access to support and treatment is often hindered by fear of legal repercussions. They bear the direct costs of criminalization.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by drug enforcement, leading to higher rates of arrest, incarceration, and family disruption. The social and economic fabric of these communities is eroded by the criminalization approach, with intergenerational impacts.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Struggle to implement evidence-based harm reduction and treatment programs due to legal restrictions and the criminal justice focus. They bear the cost of an ineffective public health strategy and the moral burden of preventable suffering.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_advocates, payer,
    moderate, generational, constrained, national).

% Are actively marginalized from policy discussions and funding due to the prohibitionist framework. Their proposals for safe consumption sites, needle exchanges, and opioid replacement therapies are often blocked or criminalized, despite evidence of effectiveness.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal efforts to eliminate substance use by establishing a clear legal boundary and enforcing it through the criminal justice system, thereby signaling moral disapproval and deterring use.
% TRANSFER_FUNCTION: Transfers resources (taxpayer money, human capital) from public health and social services to law enforcement and carceral systems. It also transfers wealth from substance users and communities to illicit markets and private prison operators.
% ABSENT_VOICES: Substance users themselves, particularly those from marginalized communities, are largely absent from policy-making, their experiences and needs often framed through a criminal lens. Harm reduction advocates are systematically excluded from the core policy conversation.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the illicit drug market would collapse, carceral populations would drastically decrease, and public health resources would be reallocated to treatment and prevention. Society would have to rapidly establish new regulatory frameworks for substance production and distribution, and address the social harms accumulated under prohibition.
% FOUNDING_PROBLEM: The perceived moral decay and social disorder associated with widespread substance use, particularly in the early 20th century, leading to a belief that criminalization was the only effective means of control and moral uplift.
% FOUNDING_PROBLEM_CORROBORATION: Prohibitionist proponents (e.g., some law enforcement, moral conservative groups) argue the problem of inherent harm and moral duty remains live. Public health experts, economists, and civil liberties advocates (outside the benefiting parties) widely attest that the founding problem has either been exacerbated by prohibition or that the approach itself is counterproductive, leading to a 'dead' or 'transformed' problem.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the immense social and economic costs imposed on individuals and communities through incarceration, legal fees, and the violence of the black market. Suppression is very high (0.90) as the state actively criminalizes and punishes substance use, with severe penalties and limited alternatives. Theater ratio (0.40) is moderate, reflecting that while some harm prevention efforts exist, a significant portion of enforcement is performative, maintaining the prohibitionist stance despite evidence of its ineffectiveness in reducing overall harm. The slight dip in extractiveness and theater ratio at the end of the interval reflects growing public and policy resistance, but the core structure remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of law enforcement and private prison operators, the constraint is a necessary mechanism for public safety and order, justifying their roles and funding. From the perspective of substance users and marginalized communities, it is a deeply extractive and suppressive system that causes more harm than it prevents. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and private prisons are clear beneficiaries, gaining resources and profits from the enforcement of prohibition. Illicit drug traffickers also benefit from the high-profit black market created by prohibition. Substance users and marginalized communities are direct targets, bearing the brunt of criminalization and its associated harms. Public health advocates are also targets, as their efforts are undermined by the prohibitionist framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as extraction by clearly identifying the beneficiaries of the prohibitionist framework (law enforcement, private prisons, traffickers) and the victims (users, communities). The persistence of the constraint, despite its contested effectiveness in achieving its stated harm-prevention goals, indicates a strong extractive function that benefits specific actors, rather than a genuine coordination problem. The 'contested' status of the founding problem further supports this, as the original mandate is no longer universally accepted as live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_efficacy_vs_harm,
    'Does criminalizing substance use effectively reduce overall societal harm, or does it create greater harms (e.g., black market violence, public health crises, mass incarceration)?',
    'Comparative analysis of public health and safety outcomes in jurisdictions with different substance control policies (prohibition vs. harm reduction vs. legalization).',
    'If prohibition is shown to create greater harms, the ''inherently harmful'' axiom is empirically challenged, weakening the moral grounding of the constraint and supporting reclassification towards a more extractive type (e.g., Snare). If it is shown to be effective, it would strengthen the claimed moral duty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_efficacy_vs_harm, empirical, 'Empirical validity of the prohibitionist claim to reduce harm.').

omega_variable(
    state_moral_duty_scope,
    'Is the state''s moral duty to prevent harm so expansive that it justifies criminalizing self-regarding acts, or is it limited to preventing harm to others?',
    'Philosophical and legal analysis of the scope of state paternalism and individual autonomy, informed by evolving societal norms and human rights frameworks.',
    'If the duty is deemed limited, the ''moral duty to prevent harm through criminalization'' axiom is conceptually challenged, undermining the legitimacy of the prohibitionist framework. If it is deemed expansive, the framework''s moral grounding is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_moral_duty_scope, conceptual, 'Conceptual scope of state''s moral duty regarding individual substance use.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, incarceration) or internalized (stigma, fear, self-blame among users)?',
    'Post-decriminalization studies: if suppression persists (e.g., through social stigma or self-stigma) after legal barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making ''exit'' less meaningful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for substance users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.86).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement(subs_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
