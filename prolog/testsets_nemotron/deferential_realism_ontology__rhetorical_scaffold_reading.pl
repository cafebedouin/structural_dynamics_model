% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Typology as Rhetorical Scaffold (Reading)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the rhetorical_scaffold_reading of the
 *   deferential_realism_ontology kernel. Under this reading, the six-category
 *   typology (mountain, rope, tangled_rope, snare, scaffold, piton) is not an
 *   observational instrument that discovers constraint types in the world,
 *   but a normative vocabulary for policy critique. The label 'snare' is
 *   declared — not discovered — when a mechanism serves beneficiaries deemed
 *   illegitimate by the analyst; epsilon values are constructed through
 *   normative judgment about who counts as a legitimate beneficiary rather
 *   than measured from mechanism operation. The framework's value lies in its
 *   persuasive power: it equips critics with a rhetoric that makes extraction
 *   visible and actionable. This reading treats the typology as a
 *   transitional scaffold — useful for the critique it enables, not as a
 *   final description of reality. It carries a sunset clause: when the
 *   critique succeeds and the illegitimate mechanism is reformed, the
 *   vocabulary has served its purpose and can be discarded.
 *
 * KEY AGENTS:
 *   - policy_analysts: Primary beneficiaries (organized/biographical/constrained) — use the typology to structure critique and advocacy
 *   - reform_advocates: Primary beneficiaries (organized/biographical/constrained) — deploy the vocabulary in public and institutional campaigns
 *   - institutional_critics: Primary beneficiaries (organized/biographical/constrained) — wield the framework as analytical leverage
 *   - immutable_diagnostic_practitioners: Excluded (institutional/generational/analytical) — hold the rival reading that the typology discovers fixed referents; would object to the claim that epsilon is constructed
 *   - hybrid_pragmatic_scholars: Excluded (institutional/generational/analytical) — hold the intermediate reading; would object to the binary opposition between discovery and declaration
 *   - analytical_observer: Observer (analytical/civilizational/analytical) — sees the full kernel contest and the structural relations among readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.15).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Typology as Rhetorical Scaffold (Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '4f20a692-91ed-4445-bd4d-7905401285e2').
narrative_ontology:cs_kernel_codification('4f20a692-91ed-4445-bd4d-7905401285e2', distributed).
narrative_ontology:cs_authority_grounding('4f20a692-91ed-4445-bd4d-7905401285e2', distributed).
narrative_ontology:cs_reading_relation('4f20a692-91ed-4445-bd4d-7905401285e2', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f20a692-91ed-4445-bd4d-7905401285e2', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('4f20a692-91ed-4445-bd4d-7905401285e2', foundational, epsilon_constructed_through_normative_judgment).
narrative_ontology:cs_axiom_status(epsilon_constructed_through_normative_judgment, holdable).
narrative_ontology:cs_axiom_grounding('4f20a692-91ed-4445-bd4d-7905401285e2', epsilon_constructed_through_normative_judgment, deontological).
narrative_ontology:cs_axiom('4f20a692-91ed-4445-bd4d-7905401285e2', foundational, framework_value_is_persuasive_power_not_observational_fidelity).
narrative_ontology:cs_axiom_status(framework_value_is_persuasive_power_not_observational_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('4f20a692-91ed-4445-bd4d-7905401285e2', framework_value_is_persuasive_power_not_observational_fidelity, instrumental).
narrative_ontology:cs_reference_frame('4f20a692-91ed-4445-bd4d-7905401285e2', rhetorical_scaffold_community).
narrative_ontology:cs_drift_state('4f20a692-91ed-4445-bd4d-7905401285e2', contemporary_policy_contest, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f20a692-91ed-4445-bd4d-7905401285e2', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_analysts).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_critics).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, constraint_classification_as_persuasive_tool).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_vocabulary_enables_critique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the deferential realism typology to structure policy analysis and critique. The vocabulary gives them a ready-made rhetoric for identifying extraction and coordinating reform proposals. They can switch to alternative frameworks (e.g., public choice, institutional economics) but lose the specific rhetorical coherence and community uptake of this typology.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_analysts, beneficiary,
    organized, biographical, constrained, global).

% Deploy the 'snare' label and associated categories in campaigns, testimony, and litigation. The framework's persuasive power amplifies their claims. Exit to other rhetorics is possible but costly — the typology has built a community of practice and a shared vocabulary that would be abandoned.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates, beneficiary,
    organized, biographical, constrained, global).

% Wield the typology as analytical leverage in academic and institutional debates. The categories (especially snare, tangled_rope) function as conceptual weapons. They benefit from the framework's uptake but are constrained by its internal logic — they cannot easily use it to critique mechanisms the framework classifies as rope or mountain.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_critics, beneficiary,
    organized, biographical, constrained, global).

% Hold the rival reading that the typology discovers fixed referents (mountains as physical invariants, snares as measurable extraction). They would object to the claim that epsilon is constructed and that 'snare' is a declaration. They are not suppressed — their reading remains live in discourse — but they are excluded from the rhetorical community this reading constitutes.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_practitioners, excluded,
    institutional, generational, analytical, global).

% Hold the intermediate reading: the typology has a fixed core (mountains, ropes grounded in physical/coordination constraints) but a contested periphery (tangled_ropes, snares where classification depends on normative judgments). They would object to both the pure discovery claim and the pure declaration claim. They are excluded from this reading's community but not suppressed.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, hybrid_pragmatic_scholars, excluded,
    institutional, generational, analytical, global).

% Observes the full kernel contest and the structural relations among the three readings. Neither collects nor pays; sees the typology as a site of epistemic contest where the same label ('snare') indexes different structural commitments depending on the reading.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, diffuse).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a community of critics around a shared vocabulary for identifying and challenging extraction mechanisms — the typology solves the coordination problem of 'how do we name and oppose illegitimate arrangements?' by providing a ready-made, mutually intelligible rhetoric.
% TRANSFER_FUNCTION: Moves analytical leverage and discursive authority from the community of critics (who build and maintain the vocabulary) to the targets of critique (mechanisms labeled 'snare' or 'tangled_rope') — the transfer is rhetorical, not material: the framework makes extraction visible and actionable, shifting the burden of justification onto the challenged arrangement.
% ABSENT_VOICES: Mechanisms and arrangements that would be classified as 'snare' under this reading — they have no voice in the vocabulary's construction because the vocabulary is built by critics, not by the arrangements it critiques. Also absent: practitioners of the immutable_diagnostic_reading who would insist the typology discovers rather than declares, but who are not structurally excluded from discourse.
% DISAPPEARANCE_RATIONALE: If the rhetorical scaffold vanished overnight, critics would lose a shared vocabulary that makes extraction visible and coordinates reform campaigns. They would revert to less coherent, less mutually intelligible rhetorics (public choice, Marxist critique, institutional economics, ad hoc moral language). The coordination function would collapse and reform efforts would fragment. The world of policy critique would rearrange.
% FOUNDING_PROBLEM: Critics of extraction mechanisms lacked a shared, mutually intelligible vocabulary that could make illegitimate arrangements visible and actionable across policy domains. Existing rhetorics (public choice, critical theory, legal doctrine) were either too domain-specific, too academically opaque, or too politically polarized to coordinate broad reform coalitions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the uptake of the typology in reform campaigns across domains (tech platform regulation, labor policy, housing, healthcare) — practitioners outside the original authoring community have adopted it because existing vocabularies failed to coordinate cross-domain critique. No single benefiting party controls this attestation; it is visible in the framework's diffusion.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint does not extract material resources from subjects — it is a vocabulary. The modest extractiveness that exists comes from the opportunity cost of adopting this framing over alternatives, and from the rhetorical pressure it places on targets of 'snare' declarations. Suppression is very low (0.10) because alternative framings (immutable_diagnostic, hybrid_pragmatic) remain live in discourse and are not formally excluded — the contest is open. Theater ratio is moderate (0.45) because part of the framework's operation is performative: the classification ritual enacts the critique it purports to describe. Accessibility collapse is low (0.20) because the typology's categories are contestable and alternatives remain accessible — one can reject the vocabulary without losing the ability to analyze constraints. Resistance is moderate (0.35) from practitioners of rival readings who contest the normative foundations.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries are policy_analysts, reform_advocates, and institutional_critics — they gain analytical leverage and persuasive power from the vocabulary. They have constrained exit (can adopt rival vocabularies but lose the rhetorical coherence of this framework). No victims are declared because the constraint does not impose costs on a target population — its 'targets' are the mechanisms classified as snares, which are not agents but arrangements. The excluded seats (immutable_diagnostic_practitioners, hybrid_pragmatic_scholars) bear no extraction from this reading; they simply lose the discursive field if this reading becomes hegemonic. The analytical_observer sits at the analytical seat with zero extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The typology as rhetorical scaffold has a live founding problem: the need for a persuasive vocabulary that makes illegitimate extraction visible and actionable in policy contests. This problem remains live (founding_problem_status: live) because extraction mechanisms persist and critics need rhetoric to challenge them. The scaffold is not mandatrophic — it serves a live coordination function (equipping critique) and carries an explicit sunset (discard when the critique succeeds). The hybrid_pragmatic_reading treats the periphery as contested but the core as fixed; the immutable_diagnostic_reading treats the whole as fixed. This reading treats the whole as normative — no fixed referents, only persuasive utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the deferential realism typology a descriptive instrument with discoverable referents, or a normative vocabulary for policy critique?',
    'Cross-reading audit: if classification outcomes change when the analyst''s normative commitments change (holding the observed mechanism fixed), the typology operates as rhetorical scaffold rather than diagnostic instrument.',
    'If rhetorical, epsilon values are advocacy-constructed; the typology''s value is persuasive power, not observational fidelity. This reading would be validated; immutable_diagnostic_reading would be falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the typology discovers or declares constraint types').

omega_variable(
    suppression_of_alternative_framings,
    'Does the rhetorical scaffold reading actively suppress alternative framings of the typology, or does it coexist with them in open contest?',
    'Trace citation and uptake patterns: if practitioners of this reading dismiss or exclude the immutable_diagnostic_reading from discourse rather than engaging its claims, suppression is present despite low formal coercion.',
    'Active suppression would contradict this reading''s claim of low suppression and push classification toward snare; coexistence supports scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_framings, empirical, 'Whether the advocacy-driven reading tolerates or suppresses rival readings').

omega_variable(
    persuasive_power_measurement,
    'Can the framework''s claimed value — its persuasive power — be measured independently of the analyst''s prior normative commitments?',
    'Controlled persuasion experiments: present the same mechanism classified as ''snare'' vs. ''tangled_rope'' to audiences with varying priors; measure classification uptake.',
    'If persuasive power tracks prior commitments, the framework is a mirror, not a lever; if it shifts judgments across priors, the rhetorical scaffold has independent causal force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persuasive_power_measurement, empirical, 'Whether the framework''s persuasive power is measurable and independent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(defe_tr_t3, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(defe_tr_t6, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 6, 0.39).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(defe_be_t3, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 3, 0.08).
narrative_ontology:measurement(defe_be_t6, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(defe_su_t3, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 3, 0.07).
narrative_ontology:measurement(defe_su_t6, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 6, 0.09).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.08).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family decomposing the deferential_realism_ontology kernel. The rhetorical_scaffold_reading treats the entire typology as normative vocabulary (epsilon constructed, low suppression, scaffold type). The immutable_diagnostic_reading treats it as observational instrument (epsilon discovered, mountain-like core). The hybrid_pragmatic_reading splits the difference (fixed core, normative periphery). They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
