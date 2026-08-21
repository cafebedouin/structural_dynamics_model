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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty: Absolute Non-Intervention
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This constraint represents the 'absolute non-intervention' reading of
 *   Westphalian sovereignty, where external interference in a state's
 *   domestic affairs is deemed illegitimate regardless of internal conduct.
 *   This reading prioritizes state stability and territorial integrity above
 *   all else, even at the cost of human rights. It is a contested
 *   interpretation, particularly in the post-Cold War era, but remains a
 *   powerful norm in international relations, especially for states seeking
 *   to avoid external scrutiny.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.65).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.78).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty: Absolute Non-Intervention").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '601c15b1-30c9-4e9b-ba37-9961f3794069').
narrative_ontology:cs_kernel_codification('601c15b1-30c9-4e9b-ba37-9961f3794069', formalized).
narrative_ontology:cs_authority_grounding('601c15b1-30c9-4e9b-ba37-9961f3794069', lineage).
narrative_ontology:cs_interpretation_layer_present('601c15b1-30c9-4e9b-ba37-9961f3794069').
narrative_ontology:cs_reading_relation('601c15b1-30c9-4e9b-ba37-9961f3794069', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('601c15b1-30c9-4e9b-ba37-9961f3794069', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('601c15b1-30c9-4e9b-ba37-9961f3794069', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('601c15b1-30c9-4e9b-ba37-9961f3794069', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('601c15b1-30c9-4e9b-ba37-9961f3794069', foundational, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('601c15b1-30c9-4e9b-ba37-9961f3794069', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('601c15b1-30c9-4e9b-ba37-9961f3794069', classical_westphalian_order).
narrative_ontology:cs_drift_state('601c15b1-30c9-4e9b-ba37-9961f3794069', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('601c15b1-30c9-4e9b-ba37-9961f3794069', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, international_human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites benefit from the absolute non-intervention principle, as it grants them unchallenged authority within their borders, regardless of their internal conduct. They actively defend this interpretation in international forums, using it to deflect criticism and prevent external accountability.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly, agenda_setter,
    institutional, generational, arbitrage, global).

% Authoritarian regimes are direct beneficiaries, as the principle shields them from external intervention, allowing them to maintain power and suppress dissent without fear of international reprisal. Their legitimacy is often tied to this claim of absolute sovereignty.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, national).

% These populations bear the primary cost of this constraint, as it denies them any external recourse or protection against state-sponsored violence, human rights abuses, or mass atrocities. Their suffering is deemed an 'internal affair'.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, biographical, trapped, national).

% These advocates work to protect human rights globally but are consistently frustrated by the absolute non-intervention principle, which limits their ability to mobilize international action against states committing atrocities. They pay in terms of moral and political capital expended against a deeply entrenched norm.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_human_rights_advocates, payer,
    organized, generational, constrained, global).

% The UNSC is theoretically empowered to authorize interventions but is often paralyzed by the absolute non-intervention principle, particularly when permanent members invoke it to protect allies or their own interests. Its actions are constrained by this reading, even as it seeks to uphold international peace and security.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, united_nations_security_council, agenda_setter,
    institutional, generational, constrained, global).

% These states often articulate a tension between the non-intervention principle and the responsibility to protect. While they generally uphold state sovereignty, they face moral and political pressure to intervene in cases of mass atrocities, leading to a contested and often inconsistent foreign policy.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for stable interstate relations by defining clear territorial boundaries and mutual respect for internal governance, preventing constant warfare over internal affairs.
% TRANSFER_FUNCTION: Transfers absolute authority over internal populations and resources to state elites, shielding them from external accountability, while transferring the burden of internal suffering to the populations themselves.
% ABSENT_VOICES: Victims of state-sponsored violence and human rights abuses within sovereign states are structurally excluded from the international conversation about intervention, as their pleas are deemed 'domestic matters'. Their voices are suppressed by the very principle meant to ensure order.
% DISAPPEARANCE_RATIONALE: If the principle of absolute non-intervention vanished overnight, the international system would undergo a profound rearrangement. States would face immediate pressure to justify their internal conduct, humanitarian interventions might become more frequent, and the concept of state sovereignty itself would be fundamentally re-evaluated, leading to a more fluid and potentially chaotic global order.
% FOUNDING_PROBLEM: The principle was established to end the religious wars and constant interventions that plagued Europe, creating a system of sovereign states that could coexist peacefully by respecting each other's internal jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: State elites and authoritarian regimes attest the problem of external interference is still live, citing historical precedents and the need for stability. Human rights organizations and liberal internationalists argue that while the original problem of religious wars is largely dead, the principle now enables new forms of oppression, and its status is contested by the ongoing reality of mass atrocities.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (interstate stability) but also enables significant asymmetric extraction (state elites maintaining power at the expense of their populations). Extractiveness is high (0.65) because it allows state elites to act with impunity internally. Suppression is also high (0.78) as it actively suppresses any legitimate grounds for external intervention, effectively trapping populations within abusive states. The theater ratio is low (0.20) because the principle is still genuinely invoked and enforced, though its justification is increasingly performative in the face of mass atrocities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state elites, this is a foundational Rope ensuring global order. From the perspective of victim populations, it is a Snare that enables their oppression. The engine's classification as Tangled Rope reflects this dual nature, acknowledging both the coordination function and the severe extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes are clear beneficiaries, as the constraint directly protects their power and autonomy (low directionality). Populations under authoritarian control and human rights advocates are victims, bearing the costs of non-intervention (high directionality). The UNSC and liberal democracies occupy more complex positions, often caught between upholding the principle and responding to humanitarian crises.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent interstate warfare by establishing clear boundaries of authority. While this function remains, the 'absolute' interpretation has allowed it to be co-opted to shield regimes from accountability for internal abuses. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring its historical coordination role), highlighting the tension between its original purpose and its current function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_internal_conduct,
    'Is the legitimacy of a state''s internal conduct a valid basis for external intervention, or is it strictly a domestic affair?',
    'Evolution of international customary law and treaty obligations, particularly the ''Responsibility to Protect'' (R2P) doctrine''s acceptance and implementation by states.',
    'If internal conduct becomes a legitimate basis for intervention, the extractiveness of this constraint would decrease significantly, and its classification would shift towards a more conditional form of sovereignty (e.g., conditional_responsibility reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_internal_conduct, conceptual, 'Whether state sovereignty is absolute or conditional on internal conduct.').

omega_variable(
    state_stability_vs_human_security,
    'Which value should take precedence in international law: state stability and non-intervention, or human security and the protection of populations from mass atrocities?',
    'A shift in the normative consensus among UN member states, reflected in Security Council resolutions, General Assembly declarations, and the consistent practice of states.',
    'Prioritizing human security would fundamentally undermine the absolute non-intervention principle, leading to a re-evaluation of sovereignty and potentially more frequent humanitarian interventions. Prioritizing state stability reinforces the current constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_stability_vs_human_security, preference, 'The normative trade-off between state stability and human security.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of intervention structural (legal barriers, veto power) or internalized (normative reluctance, fear of precedent)?',
    'Analysis of state behavior in cases of mass atrocities: if states consistently refrain from intervention despite legal avenues, internalized suppression is higher. If legal/political barriers are consistently invoked, structural suppression dominates.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — states carry the suppression with them even when legal barriers are weak. If structural, removing legal barriers would more directly enable intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.1).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 15, 0.12).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.15).
narrative_ontology:measurement(west_tr_t45, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 45, 0.18).
narrative_ontology:measurement(west_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.19).
narrative_ontology:measurement(west_tr_t75, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 75, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(west_be_t45, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 45, 0.63).
narrative_ontology:measurement(west_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(west_be_t75, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 75, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(west_su_t45, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 45, 0.77).
narrative_ontology:measurement(west_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(west_su_t75, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalia_sovereignty' kernel. Its absolute non-intervention stance directly influences the operationalization of other international norms like R2P and competes with alternative readings of sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
