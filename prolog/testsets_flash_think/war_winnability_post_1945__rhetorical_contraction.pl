% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear War Winnability: Rhetorical Contraction
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint describes the post-1945 phenomenon where the public
 *   discourse around nuclear war winnability contracted into a rhetorical
 *   taboo (it became 'unsayable'), while classified strategic planning
 *   continued to treat winnability as a constrained but operationally
 *   reachable goal. This creates a dual-layer reality: a public narrative of
 *   unthinkable catastrophe coexisting with a secret operational reality of
 *   strategic options. The constraint is claimed as a Tangled Rope because it
 *   serves a genuine coordination function (deterrence stability, public
 *   reassurance) but also involves significant asymmetric extraction (of
 *   democratic oversight and public accountability) maintained through active
 *   enforcement of the taboo.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.85).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear War Winnability: Rhetorical Contraction").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, 'faf76e31-3427-464c-be26-6dc0084e10b8').
narrative_ontology:cs_kernel_codification('faf76e31-3427-464c-be26-6dc0084e10b8', implicit).
narrative_ontology:cs_authority_grounding('faf76e31-3427-464c-be26-6dc0084e10b8', extraction).
narrative_ontology:cs_interpretation_layer_present('faf76e31-3427-464c-be26-6dc0084e10b8').
narrative_ontology:cs_reading_relation('faf76e31-3427-464c-be26-6dc0084e10b8', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('faf76e31-3427-464c-be26-6dc0084e10b8', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_axiom('faf76e31-3427-464c-be26-6dc0084e10b8', foundational, public_discourse_nuclear_war_unwinnable).
narrative_ontology:cs_axiom_status(public_discourse_nuclear_war_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('faf76e31-3427-464c-be26-6dc0084e10b8', public_discourse_nuclear_war_unwinnable, conventional).
narrative_ontology:cs_axiom('faf76e31-3427-464c-be26-6dc0084e10b8', foundational, operational_planning_retains_winnability_options).
narrative_ontology:cs_axiom_status(operational_planning_retains_winnability_options, holdable).
narrative_ontology:cs_axiom_grounding('faf76e31-3427-464c-be26-6dc0084e10b8', operational_planning_retains_winnability_options, instrumental).
narrative_ontology:cs_reference_frame('faf76e31-3427-464c-be26-6dc0084e10b8', post_hiroshima_nuclear_taboo).
narrative_ontology:cs_drift_state('faf76e31-3427-464c-be26-6dc0084e10b8', contemporary_strategic_environment, gap(stable, minor, false)).
narrative_ontology:cs_created_at('faf76e31-3427-464c-be26-6dc0084e10b8', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_powers_political_leadership).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_bodies).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for developing and maintaining nuclear war plans, which inherently include scenarios for 'winning' or achieving strategic objectives. They benefit from the public taboo on winnability, as it grants them operational flexibility without intense public scrutiny or accountability for the implications of their plans.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, identity_locked, global).

% Maintains the public rhetoric that nuclear war is unwinnable to reinforce deterrence and prevent panic. They benefit from the stability this taboo provides, while also relying on strategic planners to maintain operational options in secret. They face a constrained exit from this dual narrative due to political and strategic imperatives.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_powers_political_leadership, agenda_setter,
    institutional, biographical, constrained, global).

% Tasked with scrutinizing government policy and spending, but are largely excluded from the details of classified nuclear war planning. They bear the cost of reduced transparency and accountability, as the rhetorical taboo makes it difficult to publicly question the operational reality of winnability.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_bodies, payer,
    organized, biographical, constrained, national).

% Largely accepts the rhetorical taboo that nuclear war is unwinnable, which provides a sense of stability and reduces anxiety. However, they bear the ultimate cost of being uninformed about the actual operational planning for such a conflict, lacking the ability to provide informed consent or democratic input on existential strategic choices.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, public, payer,
    powerless, immediate, trapped, global).

% Analyze the theoretical and practical aspects of nuclear deterrence, often identifying the gap between public rhetoric and operational reality. They can articulate the constraint but have limited direct power to alter it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, academic_experts_deterrence_theory, observer,
    moderate, generational, analytical, global).

% Advocate for nuclear disarmament and highlight the catastrophic consequences of nuclear war. Their arguments are often marginalized or dismissed by the rhetorical taboo, which frames their concerns as alarmist or naive, thus limiting their influence on policy and public discourse.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, anti_nuclear_movements, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public perception of nuclear war as an unthinkable, unwinnable catastrophe to reinforce deterrence and prevent widespread panic, while simultaneously allowing strategic planners to maintain operational flexibility and contingency plans for conflict scenarios.
% TRANSFER_FUNCTION: Transfers public accountability and transparency regarding nuclear war planning from strategic decision-makers to the realm of classified operations, in exchange for perceived strategic stability and public reassurance.
% ABSENT_VOICES: Transparency advocates, segments of the public demanding full accountability for nuclear planning, and anti-nuclear movements whose arguments are sidelined by the prevailing rhetorical taboo. They would challenge the necessity and ethics of maintaining operational winnability plans in secret.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo on nuclear war winnability vanished overnight, public discourse would be radically altered, demanding full transparency and accountability for nuclear planning. Strategic planners would lose their operational flexibility, facing intense scrutiny, and the entire framework of nuclear deterrence would be forced into a new, more transparent, and potentially more volatile, equilibrium.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, the problem was how to manage the existential threat of nuclear weapons: prevent public panic, reinforce deterrence by emphasizing catastrophic consequences, and simultaneously retain strategic options for national security in a world where nuclear war remained a possibility.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and political leadership attest that the core problem of managing nuclear risk and maintaining deterrence remains live, necessitating the current approach. Academic experts and democratic oversight bodies attest that while deterrence is still critical, the 'founding problem' has evolved, and the current arrangement primarily serves to extract accountability rather than genuinely solve the original problem, citing historical evidence of shifting strategic doctrines and public discourse.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the cost to democratic oversight and public transparency, as critical strategic decisions are made in secret under the cover of a public taboo. Suppression (0.85) is very high due to the powerful rhetorical and institutional mechanisms that enforce the 'unwinnable' narrative, marginalizing dissenting voices and preventing public access to operational realities. The theater ratio (0.60) indicates that a significant portion of the public discourse is performative, designed to maintain the taboo rather than genuinely reflect the full spectrum of strategic thought. The rising trends in these metrics over time reflect the hardening of this dual-layer reality throughout the Cold War and beyond.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of strategic planners, this arrangement is a necessary evil for national security and deterrence, allowing them to maintain credible options. From the perspective of democratic oversight and the public, it represents a profound deficit in accountability and transparency, where existential decisions are made behind a veil of rhetorical consensus. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and political leadership are beneficiaries (low d) as they gain operational flexibility and political stability without full public accountability. Democratic oversight bodies and the public are targets (high d) as they bear the costs of opacity and lack of input on existential issues. Academic experts and anti-nuclear movements, while analytical or organized, are constrained or excluded, reflecting their limited ability to penetrate or alter the core dynamic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_vs_operational_reality_gap,
    'What is the true extent of the gap between the public rhetorical taboo on nuclear war winnability and the actual operational planning for achieving strategic objectives in a nuclear conflict?',
    'Declassification of historical nuclear war plans, testimony from former strategic planners, and independent analysis of strategic doctrines across nuclear powers.',
    'A wider gap would increase the measured extractiveness and theater ratio, potentially reclassifying the constraint closer to a Snare. A narrower gap would support a stronger coordination function, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_vs_operational_reality_gap, empirical, 'Measures the divergence between public rhetoric and classified strategy.').

omega_variable(
    necessity_of_taboo_for_deterrence,
    'Is the rhetorical taboo on nuclear war winnability genuinely necessary for maintaining deterrence stability and preventing public panic, or does it primarily serve to shield strategic planners from accountability?',
    'Comparative analysis of deterrence stability in states with varying levels of transparency regarding nuclear planning, or counterfactual modeling of public reactions to greater transparency.',
    'If the taboo is found to be primarily extractive, the constraint''s classification would shift towards Snare. If genuinely necessary for stability, its coordination function would be emphasized, reinforcing a Tangled Rope or even Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_taboo_for_deterrence, conceptual, 'Assesses the functional justification versus extractive utility of the taboo.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where is the core disagreement located between the ''rhetorical_contraction'' reading and its siblings (''deterrence_unthinkable'', ''countervailing_thinkable'')?',
    'Analysis of the foundational axioms and reference frames of each reading, identifying the specific points of logical or empirical contradiction.',
    'Clarifies the structural relationships between these competing interpretations of nuclear deterrence, informing how policy debates are framed and which arguments are considered legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Identifies the structural locus of disagreement within the ''war_winnability_post_1945'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1960, 0.45).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.55).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(war__tr_t2023, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2023, 0.6).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1960, 0.65).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(war__be_t2023, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1960, 0.75).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.82).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(war__su_t2023, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_winnability_post_1945' kernel, focusing on the rhetorical contraction of winnability in public discourse versus its persistence in operational planning. It is linked to sibling readings that emphasize either the categorical unwinnability of nuclear war or the continued possibility of limited victory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
