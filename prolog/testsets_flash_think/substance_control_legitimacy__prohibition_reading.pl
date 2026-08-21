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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Substance Prohibition Regime (Prohibition Reading)
 *   domain: Public Health Policy / Criminal Justice / Political Economy
 *
 * SUMMARY:
 *   This constraint instantiates the 'prohibition_reading' of the
 *   'substance_control_legitimacy' kernel. It posits that substance use is
 *   inherently harmful and that state authority derives from a moral duty to
 *   prevent this harm through criminalization. This reading leads to users
 *   entering the victim set via criminalization, high carceral
 *   extractiveness, and the externality of black market violence. The metrics
 *   reflect a system with high extraction and suppression, operating as a
 *   snare, despite its proponents claiming a moral imperative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Substance Prohibition Regime (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "Public Health Policy / Criminal Justice / Political Economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '1839bf09-17e9-4402-b8b2-aa5e68e8456a').
narrative_ontology:cs_kernel_codification('1839bf09-17e9-4402-b8b2-aa5e68e8456a', formalized).
narrative_ontology:cs_authority_grounding('1839bf09-17e9-4402-b8b2-aa5e68e8456a', lineage).
narrative_ontology:cs_interpretation_layer_present('1839bf09-17e9-4402-b8b2-aa5e68e8456a').
narrative_ontology:cs_reading_relation('1839bf09-17e9-4402-b8b2-aa5e68e8456a', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1839bf09-17e9-4402-b8b2-aa5e68e8456a', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('1839bf09-17e9-4402-b8b2-aa5e68e8456a', foundational, substance_use_inherently_harmful).
narrative_ontology:cs_axiom_status(substance_use_inherently_harmful, holdable).
narrative_ontology:cs_axiom_grounding('1839bf09-17e9-4402-b8b2-aa5e68e8456a', substance_use_inherently_harmful, empirically_contingent).
narrative_ontology:cs_axiom('1839bf09-17e9-4402-b8b2-aa5e68e8456a', foundational, state_has_moral_duty_to_prohibit).
narrative_ontology:cs_axiom_status(state_has_moral_duty_to_prohibit, holdable).
narrative_ontology:cs_axiom_grounding('1839bf09-17e9-4402-b8b2-aa5e68e8456a', state_has_moral_duty_to_prohibit, deontological).
narrative_ontology:cs_reference_frame('1839bf09-17e9-4402-b8b2-aa5e68e8456a', moral_purity_and_public_order).
narrative_ontology:cs_drift_state('1839bf09-17e9-4402-b8b2-aa5e68e8456a', contemporary_public_health_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1839bf09-17e9-4402-b8b2-aa5e68e8456a', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, state_carceral_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, black_market_actors).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces prohibition laws, leading to arrests, incarceration, and asset seizures. Benefits from funding, expanded powers, and institutional growth tied to maintaining the prohibition. Justifies its actions as preventing harm and upholding moral order.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, state_carceral_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Face criminal penalties, social stigma, and health risks due to the illegality of substances. Their options are limited by addiction, lack of legal alternatives, and the threat of incarceration. They bear the direct costs of criminalization.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by enforcement, leading to higher rates of arrest, incarceration, and family separation. The criminalization framework exacerbates existing social and economic inequalities, creating a cycle of disadvantage.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Profit from the inflated prices and risk premiums created by prohibition. They operate outside legal frameworks, often engaging in violence and corruption, but their existence is a direct consequence of the criminalization model.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_actors, beneficiary,
    powerful, biographical, arbitrage, regional).

% Bear the financial costs of enforcement, incarceration, and related social services. While some support prohibition, others question the economic efficiency and social impact of the current approach.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Advocate for public health-centered approaches to substance use, emphasizing treatment, prevention, and harm reduction over criminalization. Their voices are often marginalized in policy debates dominated by law enforcement and moral arguments.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_advocates, excluded,
    organized, generational, analytical, national).

% Monitor and challenge the human rights implications of prohibition, including issues of privacy, due process, and disproportionate sentencing. They analyze the constraint's impact on fundamental freedoms.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, state_carceral_apparatus).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate public safety and moral order by deterring substance use and maintaining societal norms against perceived harmful behaviors.
% TRANSFER_FUNCTION: Transfers resources (fines, seized assets, labor) from substance users and taxpayers to the state's carceral apparatus; transfers risk premiums and profits to black market actors.
% ABSENT_VOICES: Harm reduction advocates, civil liberties groups, and those directly impacted by criminalization (substance users, their families, marginalized communities) are often marginalized or excluded from policy-making, despite bearing the highest costs.
% DISAPPEARANCE_RATIONALE: If prohibition and its enforcement vanished overnight, the entire criminal justice system's approach to substances would collapse, black markets would be severely disrupted, and public health systems would face an immediate need to adapt to a new reality of regulated or decriminalized substance use. Social norms and economic structures would reorganize significantly.
% FOUNDING_PROBLEM: The perceived moral decay and societal harm (crime, public disorder, health issues) associated with substance use, leading to a belief in the state's moral duty to intervene through criminalization.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement agencies and some moral/religious groups attest that the founding problem of societal harm from substance use is still live and requires criminalization. However, public health experts, economists, and civil liberties advocates often dispute this, citing evidence of prohibition's ineffectiveness, its role in creating black markets, and its negative social externalities. Legislative hearing testimony and independent research from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) due to the severe penalties, fines, and asset seizures imposed on users and associated actors, as well as the economic burden on taxpayers. Suppression is very high (0.92) because the state actively enforces prohibition through policing, legal frameworks, and incarceration, with virtually no legal alternatives for users. Theater ratio is moderate (0.4) as some enforcement is genuinely aimed at public safety (e.g., preventing impaired driving), but a significant portion is dedicated to maintaining the prohibition itself, often with diminishing returns on public health outcomes. Accessibility collapse is high (0.7) for legal alternatives, but black markets provide illicit access, preventing a complete collapse. Resistance is high (0.75) from users, advocacy groups, and the inherent challenges of controlling a widespread social behavior.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state carceral apparatus and its proponents, the prohibition regime is a necessary moral and public safety measure. From the perspective of substance users, marginalized communities, and public health advocates, it is a highly extractive and suppressive system that causes more harm than it prevents. The engine's classification as a snare, despite the claimed moral justification, highlights this fundamental perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state carceral apparatus is a primary beneficiary, gaining power, funding, and institutional legitimacy from its role in enforcement. Black market actors also paradoxically benefit from the prohibition, which inflates prices and creates a lucrative, albeit dangerous, market. Substance users and marginalized communities are the primary targets, bearing the brunt of criminalization, incarceration, and social stigma. Taxpayers are also targets, funding the expensive enforcement machinery. Public health and civil liberties advocates are excluded or observe, experiencing the constraint as a barrier to alternative, more effective approaches.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_harm_vs_prohibition_induced_harm,
    'To what extent is the ''harm'' associated with substance use inherent to the substances themselves, versus being a consequence of the prohibition regime (e.g., black market violence, adulterated products, lack of safe consumption sites)?',
    'Comparative studies of jurisdictions with different regulatory models (prohibition vs. harm reduction vs. legalization) on public health, crime rates, and social outcomes.',
    'If harm is largely prohibition-induced, the justification for criminalization weakens, shifting the constraint''s perceived legitimacy and potentially reclassifying it as a more purely extractive snare. If harm is largely inherent, the prohibition reading gains stronger empirical grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_harm_vs_prohibition_induced_harm, empirical, 'Distinguishing between intrinsic and policy-induced harms of substance use.').

omega_variable(
    moral_duty_vs_pragmatic_effectiveness,
    'Does the state''s ''moral duty to prevent harm'' through criminalization outweigh the demonstrated negative externalities and ineffectiveness of prohibition in achieving its stated goals?',
    'A societal re-evaluation of foundational ethical principles regarding state intervention, individual autonomy, and the definition of ''harm,'' informed by empirical evidence on policy outcomes.',
    'If pragmatic effectiveness is prioritized, the deontological grounding of prohibition weakens, potentially leading to a reclassification towards a less legitimate type. If moral duty is held as paramount regardless of outcomes, the constraint''s claimed legitimacy remains strong within its own framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_duty_vs_pragmatic_effectiveness, conceptual, 'Balancing moral imperatives against practical policy outcomes in substance control.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal penalties, incarceration) or internalized (stigma, fear, self-blame among users)?',
    'Post-decriminalization/legalization studies: if suppression persists (e.g., through social stigma or self-limiting behaviors) after legal barriers are removed, it indicates a significant internalized component.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests, as users carry the suppression with them even if legal enforcement lessens. This would make exit harder and amplify effective extraction.',
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
narrative_ontology:measurement(subs_tr_t30, substance_control_legitimacy__prohibition_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(subs_tr_t50, substance_control_legitimacy__prohibition_reading, theater_ratio, 50, 0.4).

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
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__prohibition_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(subs_su_t30, substance_control_legitimacy__prohibition_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__prohibition_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(subs_su_t50, substance_control_legitimacy__prohibition_reading, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, black_market_dynamics).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, public_health_resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
