% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox (Credibility Paradox Reading)
 *   domain: Strategic Studies / International Relations / Nuclear Deterrence Theory
 *
 * SUMMARY:
 *   This constraint describes the 'credibility paradox' reading of nuclear
 *   deterrence, where the threat of nuclear use, necessary for deterrence, is
 *   inherently incredible due to the guaranteed mutual destruction it would
 *   entail. This reading emphasizes the instability of deterrence and the
 *   constant, often performative, efforts by great powers to make the
 *   incredible threat credible, leading to the pursuit of 'usable' nuclear
 *   options (e.g., counterforce, limited war scenarios). The 'unthinkability'
 *   of nuclear war is seen as rhetorical rather than a structural reality,
 *   with war remaining reachable via escalation ladders. This is one reading
 *   of the 'nuclear_impossibility_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.85).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.9).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox (Credibility Paradox Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "Strategic Studies / International Relations / Nuclear Deterrence Theory").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, 'd885f6b2-3044-4f62-bf4c-963986f968f1').
narrative_ontology:cs_kernel_codification('d885f6b2-3044-4f62-bf4c-963986f968f1', formalized).
narrative_ontology:cs_authority_grounding('d885f6b2-3044-4f62-bf4c-963986f968f1', lineage).
narrative_ontology:cs_interpretation_layer_present('d885f6b2-3044-4f62-bf4c-963986f968f1').
narrative_ontology:cs_reading_relation('d885f6b2-3044-4f62-bf4c-963986f968f1', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('d885f6b2-3044-4f62-bf4c-963986f968f1', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('d885f6b2-3044-4f62-bf4c-963986f968f1', foundational, nuclear_use_is_suicidal).
narrative_ontology:cs_axiom_status(nuclear_use_is_suicidal, holdable).
narrative_ontology:cs_axiom_grounding('d885f6b2-3044-4f62-bf4c-963986f968f1', nuclear_use_is_suicidal, empirically_contingent).
narrative_ontology:cs_axiom('d885f6b2-3044-4f62-bf4c-963986f968f1', foundational, deterrence_requires_credible_threat).
narrative_ontology:cs_axiom_status(deterrence_requires_credible_threat, holdable).
narrative_ontology:cs_axiom_grounding('d885f6b2-3044-4f62-bf4c-963986f968f1', deterrence_requires_credible_threat, conventional).
narrative_ontology:cs_reference_frame('d885f6b2-3044-4f62-bf4c-963986f968f1', cold_war_mutually_assured_destruction).
narrative_ontology:cs_drift_state('d885f6b2-3044-4f62-bf4c-963986f968f1', post_cold_war_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d885f6b2-3044-4f62-bf4c-963986f968f1', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industries).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, global_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and strategic doctrines, claiming deterrence as a benefit. They bear the immense cost of maintaining these systems but also derive security from the threat of retaliation. They are trapped by the paradox, constantly seeking to make an incredible threat credible.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states, beneficiary).

% Live under the shadow of nuclear threat without the means to deter directly. They bear the risk of nuclear conflict and are often pressured into conventional arms races or alliances that further entrench the nuclear order. They have no direct say in nuclear policy.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, payer,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, excluded).

% Bear the ultimate existential risk of nuclear war, with no agency in the strategic decisions that govern their fate. They also indirectly fund the nuclear enterprise through taxes.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, global_populations, payer,
    powerless, immediate, trapped, universal).

% Profit immensely from the continuous development, maintenance, and modernization of nuclear arsenals and related strategic technologies. They have a vested interest in the persistence of the deterrence paradigm and the associated arms race.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industries, beneficiary,
    organized, biographical, arbitrage, national).

% Fund the vast expenditures required for nuclear weapons programs, often at the expense of other public services. Their ability to influence these spending decisions is limited.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, taxpayers, payer,
    powerless, immediate, constrained, national).

% Analyze and interpret the dynamics of nuclear deterrence, often contributing to the doctrines and rhetoric that shape the constraint. They are aware of the paradox and its implications, but their role is primarily descriptive and advisory.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% Actively resist the nuclear deterrence paradigm, arguing for its abolition due to the inherent risks and moral implications. They are largely excluded from the core decision-making processes of nuclear states.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, disarmament_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent large-scale conventional warfare between nuclear-armed great powers by making the costs of such conflict prohibitively high, thereby maintaining a fragile global strategic stability.
% TRANSFER_FUNCTION: Transfers immense financial and human capital resources from national economies to the development and maintenance of nuclear arsenals and associated strategic infrastructure. It also transfers existential risk to global populations and limits the sovereignty of non-nuclear states.
% ABSENT_VOICES: Future generations, who will inherit the risks and costs of nuclear deterrence, and the populations of non-nuclear states, who bear the risk without agency. Disarmament advocates are also largely excluded from the core policy discussions.
% DISAPPEARANCE_RATIONALE: If the credibility paradox vanished (e.g., nuclear weapons became genuinely unusable or were abolished), the entire global security architecture would undergo a profound and rapid reorganization. Conventional military power would regain primacy, alliances would shift, and new forms of conflict or cooperation would emerge, fundamentally altering international relations.
% FOUNDING_PROBLEM: To prevent a third world war between great powers, particularly after the devastation of World War II, by creating a deterrent so powerful that direct military confrontation became unthinkable.
% FOUNDING_PROBLEM_CORROBORATION: The initial intent is corroborated by historical documents, memoirs of Cold War strategists, and early deterrence theory texts. However, its current status is contested by independent international relations scholars, peace researchers, and former policymakers who point to the ongoing risks of accidental war, proliferation, and the development of 'usable' nuclear weapons.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it performs a genuine coordination function (preventing great power war) but does so through highly extractive and coercive means. Extractiveness is high (0.85) due to the immense resources diverted to nuclear arsenals and the existential risk imposed on global populations. Suppression is very high (0.90) as the system actively suppresses alternatives to nuclear deterrence (e.g., disarmament) and forces states into a specific, high-stakes strategic posture. Theater ratio is moderate-high (0.60) because much of the strategic posturing and doctrine development serves to maintain the illusion of credibility for an inherently incredible threat. Accessibility collapse is high (0.88) as the nuclear reality severely limits the viable alternatives for great power security and global conflict resolution. Resistance is moderate (0.45) from disarmament movements, but states themselves are largely locked into the paradigm.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states perceive the system as a necessary evil for stability, a 'rope' that prevents worse outcomes. However, from the perspective of non-nuclear states and global populations, it operates as a 'snare' or 'tangled rope,' extracting resources and imposing existential risk through a coercive, unstable mechanism. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are the primary agenda-setters and beneficiaries, as they wield the deterrent threat, but they also bear significant costs and risks. Non-nuclear states and global populations are victims, bearing the risks and indirect costs without direct agency. Defense industries are clear beneficiaries, profiting from the arms race. Taxpayers are payers, funding the system. Disarmament advocates are excluded voices, resisting the paradigm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_perception_vs_reality,
    'Is the perceived credibility of nuclear threats sufficient for deterrence, even if the actual use is irrational, or does the inherent irrationality undermine deterrence over time?',
    'Empirical analysis of crisis stability, escalation dynamics, and the historical record of near-misses; psychological studies of decision-making under extreme risk.',
    'If perception is sufficient, the constraint might be more stable than this reading suggests. If inherent irrationality erodes deterrence, the system is more unstable and extractive, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_perception_vs_reality, empirical, 'Whether perceived or actual credibility drives deterrence.').

omega_variable(
    nuclear_impossibility_kernel_reading_ambiguity,
    'Is this constraint best understood through the ''credibility paradox'' reading, or do sibling readings (e.g., ''structural contraction'' or ''rational dropout'') offer a more accurate structural account?',
    'Further analysis of state behavior, strategic doctrine evolution, and the outcomes of nuclear crises, weighing evidence for inherent irrationality vs. cost-benefit calculations or structural impossibility.',
    'If a sibling reading is more accurate, the constraint''s classification, beneficiaries, and victims might shift. For example, the ''structural contraction'' reading might classify it closer to a Mountain (physical impossibility), while ''rational dropout'' might emphasize different forms of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_impossibility_kernel_reading_ambiguity, conceptual, 'Ambiguity in the most fitting reading of the nuclear impossibility kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., lack of viable alternatives to nuclear deterrence for great powers) or internalized (e.g., the ''nuclear taboo'' or self-imposed constraints on escalation)?',
    'Analysis of policy debates, military exercises, and historical instances of de-escalation: if de-escalation occurs even when structural options for escalation exist, internalized suppression is stronger.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the purely structural measure suggests, as actors carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in nuclear deterrence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1950, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(nucl_tr_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1965, 0.45).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(nucl_tr_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1950, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(nucl_be_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(nucl_be_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1950, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(nucl_su_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(nucl_su_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1995, 0.82).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_proliferation_constraint).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, conventional_arms_race).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear_impossibility_kernel', focusing on the inherent incredibility of nuclear threats. It is linked to its sibling readings as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
