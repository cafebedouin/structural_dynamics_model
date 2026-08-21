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
 *   control legitimacy, asserting that substance use is inherently harmful
 *   and that state authority derives from a moral duty to prevent this harm
 *   through criminalization. This reading leads to high extractiveness from
 *   substance users and marginalized communities, and high suppression
 *   through carceral systems. It also creates a black market that benefits
 *   illicit cartels. The structural delta from other readings is significant:
 *   users enter the victim set via criminalization, and carceral
 *   extractiveness is a primary feature.
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
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibitionist Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '17cf66e3-9226-431b-91a4-f480cd1e64f9').
narrative_ontology:cs_kernel_codification('17cf66e3-9226-431b-91a4-f480cd1e64f9', formalized).
narrative_ontology:cs_authority_grounding('17cf66e3-9226-431b-91a4-f480cd1e64f9', lineage).
narrative_ontology:cs_interpretation_layer_present('17cf66e3-9226-431b-91a4-f480cd1e64f9').
narrative_ontology:cs_reading_relation('17cf66e3-9226-431b-91a4-f480cd1e64f9', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('17cf66e3-9226-431b-91a4-f480cd1e64f9', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('17cf66e3-9226-431b-91a4-f480cd1e64f9', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('17cf66e3-9226-431b-91a4-f480cd1e64f9', substance_use_inherently_harmful, deontological).
narrative_ontology:cs_axiom('17cf66e3-9226-431b-91a4-f480cd1e64f9', foundational, state_moral_duty_to_criminalize_harm).
narrative_ontology:cs_axiom_status(state_moral_duty_to_criminalize_harm, holdable).
narrative_ontology:cs_axiom_grounding('17cf66e3-9226-431b-91a4-f480cd1e64f9', state_moral_duty_to_criminalize_harm, deontological).
narrative_ontology:cs_reference_frame('17cf66e3-9226-431b-91a4-f480cd1e64f9', moral_prohibition_framework).
narrative_ontology:cs_drift_state('17cf66e3-9226-431b-91a4-f480cd1e64f9', contemporary_public_health_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('17cf66e3-9226-431b-91a4-f480cd1e64f9', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, private_prison_corporations).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, illicit_drug_cartels).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce drug prohibition laws, leading to arrests, seizures, and incarceration. Their budgets and operational scope often expand with the perceived threat of substance use, creating an incentive to maintain the prohibition framework. They justify their actions as protecting public safety and morality.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Profit from the incarceration of individuals convicted of drug-related offenses. They lobby for stricter sentencing and expanded carceral capacity, directly benefiting from the prohibitionist approach to substance control. Their business model is directly tied to high incarceration rates.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, private_prison_corporations, beneficiary,
    organized, biographical, mobile, national).

% Operate in the black market created by prohibition, where they face reduced competition and can charge inflated prices due to the illegality and risk involved. They benefit from the lack of legal alternatives and the high demand for prohibited substances, often using violence to maintain control.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, illicit_drug_cartels, beneficiary,
    organized, biographical, arbitrage, global).

% Face criminal penalties, social stigma, and health risks due to the unregulated nature of illicit substances. They are often caught in a cycle of addiction and incarceration, with limited access to legal, safe, or therapeutic alternatives. Their identity is often criminalized by the system.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by drug enforcement, leading to higher rates of arrest, incarceration, and family disruption. The criminalization of substance use exacerbates existing social and economic inequalities, creating a cycle of disadvantage that is difficult to escape. Their community identity becomes intertwined with the criminal justice system.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Bear the financial burden of law enforcement, judicial processes, and incarceration related to drug prohibition. They fund a system that, from this reading, is inefficient and creates negative externalities, but have limited direct control over policy changes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Advocate for public health-centered approaches to substance use, focusing on treatment, prevention, and harm reduction. Their perspectives are often marginalized or actively suppressed within a prohibition-driven policy framework, as their proposals challenge the moral and legal foundations of criminalization.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal efforts to prevent harm from substance use by establishing a clear legal boundary against all illicit substances, thereby signaling moral disapproval and deterring use.
% TRANSFER_FUNCTION: Transfers resources (taxpayer money, individual liberty, public safety) to law enforcement and carceral systems, while transferring profits and power to illicit drug markets. It transfers the burden of harm from the state's moral duty to the individual user and affected communities.
% ABSENT_VOICES: Public health advocates, civil liberties organizations, and many substance users themselves are excluded from the policy-making process, where they would argue for decriminalization, treatment-on-demand, and a focus on social determinants of health rather than criminalization.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, illicit markets would collapse, law enforcement priorities would shift dramatically, and carceral populations would plummet. Public health systems would face an immediate need to scale up treatment and harm reduction services, and the social landscape around substance use would fundamentally reorganize.
% FOUNDING_PROBLEM: The perceived moral decay and societal harms associated with widespread substance use, particularly during periods like the temperance movement and later crack epidemics, leading to a belief that state intervention through criminalization was necessary to protect public morality and health.
% FOUNDING_PROBLEM_CORROBORATION: Prohibition advocates and some law enforcement officials attest the problem is still live, citing ongoing social costs and public safety concerns. Public health experts, economists, and civil rights groups, from outside the benefiting parties, attest that the founding problem has either changed in nature or that prohibition itself exacerbates many of the harms it purports to prevent, rendering the original solution obsolete or counterproductive.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) due to the direct financial costs of incarceration, the loss of liberty, and the economic burden on communities. Suppression is extremely high (0.92) as the state actively criminalizes and punishes substance use, with severe penalties and limited alternatives. The theater ratio (0.45) reflects that while some public safety goals are genuinely pursued, a significant portion of enforcement activity serves to maintain the carceral-industrial complex and the moralistic framing, rather than effectively reducing overall harm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of law enforcement and private prisons, this is a necessary and legitimate constraint for public safety and order. From the perspective of substance users and marginalized communities, it is a highly extractive and suppressive snare that perpetuates harm and inequality. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and private prison corporations are clear beneficiaries, gaining resources and power from the prohibition framework. Illicit drug cartels also benefit significantly from the black market created by prohibition. Substance users and marginalized communities are the primary victims, bearing the brunt of criminalization, incarceration, and associated social harms. Taxpayers are also victims, funding an expensive and often ineffective system. Public health advocates are excluded, as their alternative framings are incompatible with the prohibitionist stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_efficacy_vs_harm,
    'Does criminalization effectively reduce overall societal harm from substance use, or does it create greater harms (e.g., black markets, violence, public health crises)?',
    'Comparative analysis of public health and safety outcomes in jurisdictions with different substance control policies (prohibition vs. harm reduction vs. legalization).',
    'If criminalization is shown to create greater harms, the ''inherently harmful'' axiom would be empirically challenged, potentially shifting the constraint''s legitimacy and classification towards a snare or piton, as its stated coordination function (harm prevention) would be undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_efficacy_vs_harm, empirical, 'Empirical validity of prohibition''s harm reduction claims.').

omega_variable(
    moral_duty_vs_autonomy,
    'Is the state''s moral duty to prevent self-harm through criminalization a legitimate basis for authority, or does individual autonomy over one''s body and choices take precedence?',
    'Philosophical and legal debate on the limits of state paternalism and the scope of individual rights, potentially informed by evolving societal values and human rights jurisprudence.',
    'If individual autonomy is prioritized, the ''moral duty'' axiom would be conceptually overridden, fundamentally challenging the prohibitionist reading''s authority grounding and potentially reclassifying it as a snare (pure extraction without legitimate coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_duty_vs_autonomy, conceptual, 'Conceptual grounding of state authority in preventing self-harm.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, incarceration) or internalized (stigma, fear, self-blame among users)?',
    'Post-decriminalization/legalization suppression trajectory: if suppression persists after legal barriers are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making recovery harder even if laws change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in substance use criminalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(subs_tr_t10, substance_control_legitimacy__prohibition_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__prohibition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(subs_be_t10, substance_control_legitimacy__prohibition_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__prohibition_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(subs_be_t30, substance_control_legitimacy__prohibition_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__prohibition_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(subs_be_t50, substance_control_legitimacy__prohibition_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(subs_su_t10, substance_control_legitimacy__prohibition_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.92).


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
