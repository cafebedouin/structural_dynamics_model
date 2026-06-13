% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Principle of Westphalian Sovereignty
 *   domain: international_relations/political_theory/law
 *
 * SUMMARY:
 *   This constraint represents the 'absolute non-intervention' reading of
 *   Westphalian sovereignty, where external interference in a state's
 *   domestic affairs is considered illegitimate regardless of internal
 *   conduct. It is a foundational principle of the modern international
 *   system, often invoked by states to shield themselves from accountability
 *   for human rights abuses. The constraint is claimed as a Rope by its
 *   proponents (promoting stability) but operates as a Tangled Rope due to
 *   its significant extractive and suppressive effects on populations under
 *   authoritarian rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.6).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.7).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Principle of Westphalian Sovereignty").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_relations/political_theory/law").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, 'fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9').
narrative_ontology:cs_kernel_codification('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', formalized).
narrative_ontology:cs_authority_grounding('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', lineage).
narrative_ontology:cs_interpretation_layer_present('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9').
narrative_ontology:cs_reading_relation('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', foundational, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', post_westphalian_state_system).
narrative_ontology:cs_drift_state('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('fb4e9c32-82a3-4a8b-a1de-a58ca53f65d9', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, international_human_rights_advocates).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, state_autonomy_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, non_interference_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites benefit directly from the principle, as it grants them unchecked authority within their borders, protecting them from external accountability for domestic conduct. They actively champion and enforce this reading of sovereignty.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly, agenda_setter,
    institutional, generational, arbitrage, global).

% These regimes rely on the absolute non-intervention principle to legitimize their internal control and suppress dissent without fear of external reprisal, even in cases of severe human rights abuses. They are strong proponents of this reading.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, global).

% These populations bear the direct costs of the absolute non-intervention principle, as it denies them any external recourse or protection when their own state commits atrocities or systematically violates their rights. Their suffering is deemed an 'internal affair'.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, biographical, trapped, national).

% These advocates work to protect human rights globally but are consistently hampered by the absolute non-intervention principle, which limits their ability to intervene or hold states accountable for internal abuses. They face a high barrier to action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_human_rights_advocates, payer,
    organized, generational, constrained, global).

% The UNSC is nominally responsible for international peace and security, but its actions are often constrained by the absolute non-intervention principle, particularly when permanent members invoke it to protect allies or their own interests. It acts as an enforcer of the principle through veto power.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, united_nations_security_council, agenda_setter,
    institutional, generational, constrained, global).

% While often upholding the principle in their own foreign policy, liberal democracies frequently face internal and external pressure to intervene in cases of mass atrocities, creating a tension with the absolute non-intervention reading. They observe and sometimes challenge the constraint.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, liberal_democracies, observer,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally recognized boundary for state authority, preventing constant external meddling and promoting a stable international order based on mutual recognition of territorial integrity.
% TRANSFER_FUNCTION: Transfers absolute authority over domestic affairs to state governments, removing external accountability for internal conduct, from populations and international human rights norms to state elites.
% ABSENT_VOICES: Populations suffering under repressive regimes are effectively silenced by this principle, as their pleas for external assistance are deemed illegitimate interference. Victims of mass atrocities would object most vehemently.
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention principle vanished, the international system would undergo a profound rearrangement. States would face constant scrutiny and potential intervention based on internal conduct, leading to a highly unstable and potentially chaotic global environment, or a new, more robust system of international accountability.
% FOUNDING_PROBLEM: The principle was established to end the religious wars and constant interference in European states' internal affairs, creating a system where states recognized each other's exclusive authority within their borders to achieve peace and stability.
% FOUNDING_PROBLEM_CORROBORATION: State elites and authoritarian regimes attest the problem of external interference is still live and the principle is vital for stability. International human rights organizations and many liberal democracies argue the original problem is largely solved, but the principle now serves to shield human rights abusers, as evidenced by numerous UN reports and NGO investigations.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the unchecked power it grants state elites, allowing them to extract resources and obedience from their populations without external accountability. Suppression (0.7) is high because it actively legitimizes the suppression of internal dissent by denying external support to victims. The theater ratio (0.2) is relatively low, as the principle is genuinely invoked and enforced, though its stated coordination function (international stability) increasingly serves as a cover for extraction. The historical measurements reflect a rise in extractiveness and suppression after WWII with the rise of human rights norms, as the principle became more actively contested and defended by those who benefit from it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state elites and authoritarian regimes, this principle is a necessary Rope for international order and stability. From the perspective of oppressed populations and human rights advocates, it is a Snare that enables atrocities and extraction. The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both its coordination function for state-to-state relations and its asymmetric extraction from vulnerable populations.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes are clear beneficiaries (d near 0.0), as the principle directly subsidizes their power and protects them from external checks. Populations under authoritarian control and human rights advocates are targets (d near 1.0), bearing the costs of non-intervention. The UNSC acts as an agenda-setter, often enforcing the principle through vetoes, even if individual members might internally contest it. Liberal democracies are observers, caught between their own values and the established international legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the principle of absolute non-intervention a natural law of international relations, or a constructed norm that benefits identifiable state actors?',
    'Historical analysis of its emergence and evolution, and counterfactual analysis of alternative international systems. If it can be shown to have clear historical origins and alternative viable systems exist, it is a constructed norm.',
    'If a natural law, its extractiveness is an unavoidable cost of international order. If a constructed norm, its extractiveness is a policy choice that can be challenged and altered.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Ambiguity between inherent international order and state-centric power projection.').

omega_variable(
    legitimacy_vs_effectiveness,
    'Does the absolute non-intervention principle enhance overall international stability and peace, or does it undermine it by enabling internal conflicts and mass atrocities that eventually spill over borders?',
    'Empirical study of conflict patterns, refugee flows, and regional destabilization in contexts where the principle is strictly applied versus where it is challenged or overridden.',
    'If it undermines stability, its coordination function is illusory, and its classification shifts closer to a Snare. If it genuinely enhances stability, its Rope-like qualities are stronger, even with extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_effectiveness, empirical, 'The tension between formal legitimacy and practical outcomes for global stability.').

omega_variable(
    victim_set_definition,
    'Should ''populations under authoritarian control'' be considered a ''victim'' group of this constraint, or are their grievances solely against their own state, with the non-intervention principle merely a neutral background condition?',
    'Analysis of international legal frameworks that recognize individual human rights as transcending state sovereignty, and the concept of ''responsibility to protect'' (R2P). If R2P is accepted, the principle directly victimizes these populations.',
    'If they are victims, the constraint''s extractiveness and suppression are directly attributable to its operation. If not, the constraint is less extractive from an international perspective, though still enabling domestic extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_definition, conceptual, 'Defining the scope of victimhood in international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(west_tr_t1800, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(west_tr_t1900, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.4).
narrative_ontology:measurement(west_be_t1800, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement(west_be_t1900, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.65).
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.5).
narrative_ontology:measurement(west_su_t1800, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(west_su_t1900, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalia_sovereignty' kernel. It emphasizes categorical non-intervention, contrasting with 'conditional_responsibility' and 'graded_sovereignty' readings that allow for intervention under certain conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
