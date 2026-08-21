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
 *   human_readable: Substance Use Prohibition as Moral Transgression
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'prohibition_reading' of the
 *   'substance_control_kernel'. It frames substance use as a moral
 *   transgression requiring state punishment to protect social order. This
 *   reading leads to high extraction from users and marginalized communities,
 *   significant suppression of alternatives, and the emergence of black
 *   markets. The claimed coordination function is social order, but the
 *   actual operation is heavily extractive and coercive.
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
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Substance Use Prohibition as Moral Transgression").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'b5fc845a-4cce-458b-9391-1e0eef67bb9e').
narrative_ontology:cs_kernel_codification('b5fc845a-4cce-458b-9391-1e0eef67bb9e', formalized).
narrative_ontology:cs_authority_grounding('b5fc845a-4cce-458b-9391-1e0eef67bb9e', extraction).
narrative_ontology:cs_interpretation_layer_present('b5fc845a-4cce-458b-9391-1e0eef67bb9e').
narrative_ontology:cs_reading_relation('b5fc845a-4cce-458b-9391-1e0eef67bb9e', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5fc845a-4cce-458b-9391-1e0eef67bb9e', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('b5fc845a-4cce-458b-9391-1e0eef67bb9e', foundational, substance_use_is_moral_failing).
narrative_ontology:cs_axiom_status(substance_use_is_moral_failing, holdable).
narrative_ontology:cs_axiom_grounding('b5fc845a-4cce-458b-9391-1e0eef67bb9e', substance_use_is_moral_failing, deontological).
narrative_ontology:cs_axiom('b5fc845a-4cce-458b-9391-1e0eef67bb9e', foundational, state_punishment_protects_social_order).
narrative_ontology:cs_axiom_status(state_punishment_protects_social_order, holdable).
narrative_ontology:cs_axiom_grounding('b5fc845a-4cce-458b-9391-1e0eef67bb9e', state_punishment_protects_social_order, empirically_contingent).
narrative_ontology:cs_reference_frame('b5fc845a-4cce-458b-9391-1e0eef67bb9e', punitive_social_order_framework).
narrative_ontology:cs_drift_state('b5fc845a-4cce-458b-9391-1e0eef67bb9e', contemporary_public_health_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5fc845a-4cce-458b-9391-1e0eef67bb9e', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, state_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, black_market_actors).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, social_conservatives).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, marginalized_communities).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, public_health_advocates).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce drug prohibition laws, receiving significant funding and expanded powers. They justify their role as protecting public safety and social order, and benefit from the perpetuation of the 'war on drugs' framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, state_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Profits directly from increased incarceration rates driven by drug offenses, lobbying for stricter laws and longer sentences. Their business model is directly tied to the punitive approach.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_industry, beneficiary,
    powerful, biographical, arbitrage, national).

% Benefit from the illegality of substances, which creates high profit margins due to risk premiums and lack of legal competition. They thrive on the very prohibition designed to suppress substance use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_actors, beneficiary,
    organized, immediate, arbitrage, regional).

% Advocate for prohibition based on moral and religious convictions, viewing substance use as a societal ill. They benefit from policies that align with their moral framework and reinforce traditional social norms.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, social_conservatives, beneficiary,
    organized, generational, mobile, national).

% Face criminalization, incarceration, social stigma, and health risks due to the illegal status of substances. They bear the direct punitive costs and often lack access to health-centered support.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, substance_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by enforcement, leading to higher rates of arrest, incarceration, and family disruption. The punitive approach exacerbates existing social and economic inequalities.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, marginalized_communities, payer,
    powerless, biographical, identity_locked, local).

% Argue for evidence-based, health-centered approaches to substance use, but their recommendations are often sidelined or actively opposed by the prohibition framework. They are excluded from primary policy-making roles.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_advocates, excluded,
    organized, biographical, constrained, national).

% Fund the extensive enforcement and carceral infrastructure of prohibition, often without seeing commensurate reductions in substance use or improvements in social order. They bear the financial costs of an inefficient system.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Monitor and challenge the erosion of civil rights and liberties resulting from aggressive drug enforcement tactics, including disproportionate policing and mandatory minimum sentences.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, state_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate social behavior by deterring substance use, thereby protecting public health, safety, and moral order through state punishment.
% TRANSFER_FUNCTION: Transfers vast financial resources from taxpayers to state enforcement agencies and the private prison industry. It also transfers wealth to black market actors by creating artificial scarcity and risk premiums. It transfers social costs (incarceration, stigma, health crises) onto substance users and marginalized communities.
% ABSENT_VOICES: Public health experts, civil liberties advocates, and directly impacted individuals and communities are often marginalized or excluded from policy discussions, despite offering alternative, evidence-based approaches.
% DISAPPEARANCE_RATIONALE: If prohibition vanished overnight, the entire criminal justice apparatus built around drug enforcement would collapse. Black markets would be severely disrupted, and public health systems would face an immediate, massive need to scale up harm reduction and treatment services. Social norms around substance use would undergo rapid shifts.
% FOUNDING_PROBLEM: Perceived social decay, moral decline, and public disorder attributed to substance use, often linked to specific racial or immigrant groups, leading to calls for punitive state intervention.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is primarily attested by social conservative groups and some law enforcement bodies, who maintain that substance use inherently destabilizes society. However, public health experts, economists, and civil liberties organizations, from outside the benefiting parties, largely dispute the efficacy of punishment in solving these problems and highlight the negative externalities created by prohibition itself.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.85) due to the severe penalties, fines, and social costs imposed on substance users and communities. Suppression is also very high (0.90) as the state actively criminalizes and punishes, effectively collapsing legal alternatives and exit options. Theater ratio is moderate-high (0.60) because while the stated goal is social order and public safety, the actual outcomes often include increased violence (from black markets), public health crises (overdoses, disease transmission), and mass incarceration, suggesting a significant gap between stated function and actual effect. Accessibility collapse is high (0.75) as legal and safe alternatives for substance acquisition or management are almost entirely removed. Resistance is high (0.70) from affected communities, civil liberties groups, and public health advocates.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading (e.g., state enforcement, social conservatives) perceive the constraint as a necessary 'rope' for social order, with any extraction being a legitimate cost of deterrence. However, from the perspective of substance users, marginalized communities, and public health advocates, it operates as a 'snare' or 'tangled rope' that extracts heavily, suppresses alternatives, and often exacerbates the very problems it claims to solve.
 *
 * DIRECTIONALITY LOGIC:
 *   State enforcement agencies, the private prison industry, and black market actors are clear beneficiaries, profiting from the punitive framework. Social conservatives also benefit by seeing their moral framework codified into law. Substance users, marginalized communities, and taxpayers are the primary victims, bearing the direct and indirect costs of enforcement and incarceration. Public health advocates are excluded from policy-making, despite their expertise.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope, despite the high extraction, acknowledges the *claimed* coordination function of protecting social order. However, the high extractiveness, suppression, and theater ratio, coupled with the 'contested' status of the founding problem, suggest significant mandatrophy. The engine's computation will likely classify this as a Snare for many seats, indicating that the coordination story is largely cover for extraction, or that the original mandate has been severely degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_punishment,
    'To what extent does state punishment effectively deter substance use or genuinely protect social order, versus displacing problems or creating new harms?',
    'Longitudinal studies comparing outcomes in jurisdictions with punitive vs. health-centered approaches, and comprehensive cost-benefit analyses of prohibition.',
    'If punishment is found to be ineffective or counterproductive, the ''social order'' coordination function would be undermined, pushing the classification closer to a pure Snare. If it shows genuine efficacy, the Tangled Rope classification would be more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_of_punishment, empirical, 'Empirical effectiveness of punitive measures in achieving stated goals.').

omega_variable(
    moral_vs_health_framing,
    'Is substance use fundamentally a moral failing requiring punitive intervention, or a health condition requiring medical and social support?',
    'This is a conceptual/preference question, not empirically resolvable. Resolution depends on societal values and ethical frameworks, potentially shifting with public discourse and scientific understanding of addiction.',
    'If framed as a health condition, the justification for state punishment collapses, leading to a reclassification away from any coordination function and towards pure extraction (Snare). If framed as a moral failing, the current structure retains its internal coherence for its proponents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_vs_health_framing, conceptual, 'Underlying conceptual framing of substance use.').

omega_variable(
    black_market_externality,
    'Is the violence and instability associated with black markets an unavoidable externality of prohibition, or a separate problem that could be mitigated within a prohibition framework?',
    'Analysis of historical and international examples where changes in prohibition policy (e.g., alcohol prohibition repeal) impacted organized crime and violence levels.',
    'If black market violence is an inherent consequence of prohibition, it further undermines the ''social order'' coordination claim, increasing the effective extraction and pushing towards a Snare. If it''s separable, the coordination claim might retain more validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_externality, empirical, 'Relationship between prohibition and black market violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_kernel__prohibition_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(subs_tr_t1980, substance_control_kernel__prohibition_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(subs_tr_t1990, substance_control_kernel__prohibition_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__prohibition_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(subs_tr_t2010, substance_control_kernel__prohibition_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(subs_tr_t2025, substance_control_kernel__prohibition_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_kernel__prohibition_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(subs_be_t1980, substance_control_kernel__prohibition_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(subs_be_t1990, substance_control_kernel__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__prohibition_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(subs_be_t2010, substance_control_kernel__prohibition_reading, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement(subs_be_t2025, substance_control_kernel__prohibition_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_kernel__prohibition_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(subs_su_t1980, substance_control_kernel__prohibition_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(subs_su_t1990, substance_control_kernel__prohibition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__prohibition_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(subs_su_t2010, substance_control_kernel__prohibition_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(subs_su_t2025, substance_control_kernel__prohibition_reading, suppression_requirement, 2025, 0.9).


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
