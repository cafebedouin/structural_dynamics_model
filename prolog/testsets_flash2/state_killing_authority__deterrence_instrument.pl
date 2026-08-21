% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: State Killing Authority: Deterrence Instrument Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence instrument' reading of state
 *   killing authority: capital punishment is justified solely by its efficacy
 *   in preventing future murders at an acceptable cost. It is a contested
 *   claim, with empirical evidence often failing to support a significant
 *   deterrent effect. The constraint operates as a Tangled Rope, as it
 *   purports to coordinate public safety while extracting the lives of
 *   condemned individuals, requiring active enforcement and suppressing
 *   alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.65).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.9).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "State Killing Authority: Deterrence Instrument Reading").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'd2279a17-071d-48a4-95aa-c08e600dc49d').
narrative_ontology:cs_kernel_codification('d2279a17-071d-48a4-95aa-c08e600dc49d', formalized).
narrative_ontology:cs_authority_grounding('d2279a17-071d-48a4-95aa-c08e600dc49d', extraction).
narrative_ontology:cs_interpretation_layer_present('d2279a17-071d-48a4-95aa-c08e600dc49d').
narrative_ontology:cs_reading_relation('d2279a17-071d-48a4-95aa-c08e600dc49d', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('d2279a17-071d-48a4-95aa-c08e600dc49d', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('d2279a17-071d-48a4-95aa-c08e600dc49d', foundational, state_has_right_to_protect_citizens_by_any_means).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_by_any_means, holdable).
narrative_ontology:cs_axiom_grounding('d2279a17-071d-48a4-95aa-c08e600dc49d', state_has_right_to_protect_citizens_by_any_means, instrumental).
narrative_ontology:cs_axiom('d2279a17-071d-48a4-95aa-c08e600dc49d', foundational, deterrence_is_primary_purpose_of_punishment).
narrative_ontology:cs_axiom_status(deterrence_is_primary_purpose_of_punishment, holdable).
narrative_ontology:cs_axiom_grounding('d2279a17-071d-48a4-95aa-c08e600dc49d', deterrence_is_primary_purpose_of_punishment, empirically_contingent).
narrative_ontology:cs_reference_frame('d2279a17-071d-48a4-95aa-c08e600dc49d', utilitarian_crime_prevention_framework).
narrative_ontology:cs_drift_state('d2279a17-071d-48a4-95aa-c08e600dc49d', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d2279a17-071d-48a4-95aa-c08e600dc49d', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, state_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, law_and_order_politicians).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_individuals).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_condemned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement capital punishment, framing it as a necessary tool for public safety and crime prevention. Their careers and public image are often tied to securing convictions and severe sentences.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from public perception of toughness on crime, using capital punishment as a policy plank to gain electoral support. They are not directly involved in the execution but benefit from its perceived deterrent effect.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, law_and_order_politicians, beneficiary,
    powerful, immediate, mobile, national).

% The theoretical beneficiaries whose lives are purportedly saved by the deterrent effect of capital punishment. This group is abstract and cannot act, but their protection is the primary justification for the constraint.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(state_killing_authority__deterrence_instrument, potential_future_victims).

% Bear the ultimate cost of the constraint, losing their lives. They are entirely subject to the state's authority and have no exit options once a death sentence is final.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_individuals, payer,
    powerless, immediate, trapped, local).

% Bear significant emotional and financial costs associated with the legal process and the loss of a family member. Their resistance is often through legal appeals and advocacy, but their direct influence on the constraint is minimal.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_condemned, payer,
    powerless, biographical, constrained, local).

% Actively campaign against capital punishment, arguing it is morally wrong and ineffective as a deterrent. While they influence public opinion and legal discourse, they are structurally excluded from the direct decision-making process of the state.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Conduct empirical studies on the deterrent effect of capital punishment. Their findings often challenge the deterrence claim, but their role is primarily analytical, not executive.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, social_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to deter violent crime by imposing the ultimate penalty, aiming to reduce the overall murder rate and enhance public safety.
% TRANSFER_FUNCTION: Transfers the lives of convicted murderers from themselves to the state, justified by the claim of preventing future murders and protecting potential victims.
% ABSENT_VOICES: The condemned individuals themselves, whose voices are silenced by the execution. Also, the broader international human rights community, which largely opposes capital punishment but has limited direct influence on national legal systems.
% DISAPPEARANCE_RATIONALE: If capital punishment vanished overnight, the criminal justice system would need to re-evaluate sentencing guidelines, public safety strategies, and the role of life imprisonment. The political discourse around crime would shift, and the moral landscape of state power would be fundamentally altered.
% FOUNDING_PROBLEM: The problem of deterring heinous crimes and ensuring public safety in the face of murder.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state prosecutors, some politicians) argue the problem is live and capital punishment is a necessary deterrent. Opponents (social scientists, abolitionist advocates) argue the deterrence effect is unproven or non-existent, making the founding problem effectively 'dead' as a justification for this specific solution. Empirical studies from independent academic institutions often contradict the deterrence claim.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the cost (a human life) is absolute, and the benefit (deterrence) is empirically uncertain. Suppression is very high (0.90) because the state's power to execute is nearly absolute once legal avenues are exhausted, and the condemned have no exit. Theater ratio is moderate (0.40) because while the state genuinely seeks to deter crime, a significant portion of the justification and maintenance of capital punishment is performative, aimed at public perception of justice rather than proven efficacy. Accessibility collapse is high (0.80) as the finality of execution leaves no alternative for the condemned. Resistance is also high (0.70) due to ongoing legal challenges and abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authorities and some politicians, this constraint is a necessary tool for public safety. From the perspective of the condemned and abolitionists, it is an unjust and ineffective act of state violence. The engine's classification will highlight this divergence, showing a claimed 'deterrence' function operating with high extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   State prosecutors and politicians are beneficiaries, as they gain political capital and perceived public safety. Potential future victims are theoretical beneficiaries. Condemned individuals and their families are clear victims, bearing the ultimate cost. Social scientists and abolitionist advocates act as observers or excluded parties, challenging the constraint's justification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deterrence) is highly contested. If empirical evidence definitively proves no deterrent effect, the constraint would shift from a Tangled Rope (with a coordination claim) to a Snare (pure extraction), as its coordination function would be revealed as cover for state-sanctioned killing. The persistence of the practice despite weak evidence suggests a degree of mandatrophy, where the original justification has atrophied but the practice continues due to inertia and other benefits (e.g., political capital).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrent_effect_empirical_uncertainty,
    'Does capital punishment actually prevent future murders at a rate that justifies its cost?',
    'Longitudinal, cross-jurisdictional empirical studies comparing murder rates in states with and without capital punishment, controlling for other variables. Meta-analyses of existing research.',
    'If a significant deterrent effect is proven, the extractiveness might be re-evaluated as a necessary cost for a genuine coordination function. If no effect is found, the constraint''s coordination claim collapses, reclassifying it closer to a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_effect_empirical_uncertainty, empirical, 'Uncertainty regarding the empirical validity of the deterrence claim.').

omega_variable(
    acceptable_cost_definition,
    'What constitutes an ''acceptable cost'' for preventing future murders, particularly when that cost is a human life?',
    'Societal consensus through deliberative democracy, legislative action, or judicial interpretation that explicitly defines the moral and economic thresholds for ''acceptable cost''.',
    'A high threshold for ''acceptable cost'' would make capital punishment harder to justify, potentially leading to its abolition. A low threshold would reinforce its use. This is a preference-based question that shapes the moral legitimacy of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_definition, preference, 'Ambiguity in the definition of ''acceptable cost'' for capital punishment.').

omega_variable(
    reading_structural_divergence,
    'How do the structural elements (beneficiaries, victims, authority grounding) of this ''deterrence instrument'' reading differ from the ''retributive desert'' and ''categorical abolition'' readings?',
    'Comparative analysis of legal texts, judicial opinions, and philosophical arguments grounding each reading. Identification of specific clauses or principles that define each reading''s unique structural configuration.',
    'The ''deterrence instrument'' reading places potential victims as beneficiaries and grounds authority in efficacy. The ''retributive desert'' reading would place the state as an agent of justice and the victim''s family as beneficiaries of retribution. The ''categorical abolition'' reading would have no beneficiaries of state killing and would identify the condemned as victims of an inherently unjust act. This omega documents the distinct structural implications of each reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_divergence, conceptual, 'Documents the structural differences between this reading and its siblings within the ''state_killing_authority'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__deterrence_instrument, theater_ratio, 10, 0.35).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__deterrence_instrument, theater_ratio, 20, 0.4).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__deterrence_instrument, theater_ratio, 30, 0.4).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.4).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__deterrence_instrument, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__deterrence_instrument, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__deterrence_instrument, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__deterrence_instrument, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__deterrence_instrument, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__deterrence_instrument, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__deterrence_instrument, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__deterrence_instrument, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__deterrence_instrument, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel, focusing on deterrence. It is structurally distinct from the 'retributive_desert' and 'categorical_abolition' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
