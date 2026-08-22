% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Incumbent Extraction
 *   domain: economic/technological/history
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists not because it is technically optimal
 *   but because incumbent manufacturers (Remington, Union Typewriter Trust)
 *   and allied typing schools actively maintained it as a standard to protect
 *   sunk investments in training curricula, manufacturing tooling, and market
 *   dominance. This reading treats the persistence mechanism as a constraint
 *   with identifiable beneficiaries who capture returns through artificial
 *   switching costs and active suppression of alternative layouts (e.g.,
 *   Dvorak), distinguishing it from naturalization readings (fair competition
 *   adequacy) and lock-in readings (passive coordination failure). It is the
 *   beneficiary_extraction_reading of the contested kernel
 *   qwerty_persistence_mechanism.
 *
 * KEY AGENTS:
 *   - incumbent_typewriter_manufacturers: Agenda-setter (institutional/mobile) â controls standards, patents, and hardware production
 *   - typing_school_incumbents: Beneficiary (organized/constrained) â curriculum lock-in reinforces the standard
 *   - keyboard_users: Payer (powerless/constrained) â bears ergonomic and efficiency costs with no exit
 *   - alternative_layout_innovators: Excluded (moderate/trapped) â blocked from market entry by incumbent-controlled pipelines
 *   - economic_historians: Observer (analytical) â documents the extraction mechanism from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.7).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Incumbent Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic/technological/history").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'eb35b7bc-d5a5-471c-9978-6f13472baf1e').
narrative_ontology:cs_kernel_codification('eb35b7bc-d5a5-471c-9978-6f13472baf1e', fixed_text).
narrative_ontology:cs_authority_grounding('eb35b7bc-d5a5-471c-9978-6f13472baf1e', extraction).
narrative_ontology:cs_interpretation_layer_present('eb35b7bc-d5a5-471c-9978-6f13472baf1e').
narrative_ontology:cs_reading_relation('eb35b7bc-d5a5-471c-9978-6f13472baf1e', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('eb35b7bc-d5a5-471c-9978-6f13472baf1e', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_axiom('eb35b7bc-d5a5-471c-9978-6f13472baf1e', foundational, incumbent_suppression_as_cause).
narrative_ontology:cs_axiom_status(incumbent_suppression_as_cause, holdable).
narrative_ontology:cs_axiom_grounding('eb35b7bc-d5a5-471c-9978-6f13472baf1e', incumbent_suppression_as_cause, empirically_contingent).
narrative_ontology:cs_axiom('eb35b7bc-d5a5-471c-9978-6f13472baf1e', foundational, artificial_switching_costs).
narrative_ontology:cs_axiom_status(artificial_switching_costs, holdable).
narrative_ontology:cs_axiom_grounding('eb35b7bc-d5a5-471c-9978-6f13472baf1e', artificial_switching_costs, empirically_contingent).
narrative_ontology:cs_reference_frame('eb35b7bc-d5a5-471c-9978-6f13472baf1e', incumbent_controlled_standard).
narrative_ontology:cs_drift_state('eb35b7bc-d5a5-471c-9978-6f13472baf1e', digital_remap_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb35b7bc-d5a5-471c-9978-6f13472baf1e', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_school_incumbents).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, keyboard_users).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controlled typewriter patents, tooling, and distribution. Promoted QWERTY as the industry standard and used compatibility requirements and market power to prevent alternative layouts from gaining hardware traction. Collected revenue from the resulting lock-in to their production ecosystem.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typewriter_manufacturers, beneficiary).

% Built curricula, certification exams, and instructor training around QWERTY proficiency. Benefited from predictable demand for standardized typing education. Switching to a new layout would require rewriting materials and retraining staff, creating curriculum lock-in.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typing_school_incumbents, beneficiary,
    organized, biographical, constrained, national).

% Learned QWERTY because it was the only available standard on commercial equipment. Bear suboptimal typing efficiency and higher ergonomic strain compared to alternative layouts. Individual users lack leverage to change hardware standards or workplace requirements.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, keyboard_users, payer,
    powerless, biographical, constrained, global).

% Designed demonstrably more efficient keyboard layouts but faced blocked access to hardware production contracts, typing school adoption, and institutional procurement. Their innovations were rendered economically invisible by incumbent-controlled standards.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators, excluded,
    moderate, biographical, trapped, national).

% Research and document the historical mechanisms of QWERTY standardization, evaluating claims of natural superiority against archival evidence of incumbent suppression and market manipulation.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardization of keyboard layouts allows interoperability of equipment, transferability of trained labor, and economies of scale in manufacturing and instruction.
% TRANSFER_FUNCTION: Moves surplus from keyboard users and alternative-layout innovators to incumbent manufacturers and typing schools through artificial switching costs and suppressed competition.
% ABSENT_VOICES: Alternative layout inventors and ergonomic researchers were structurally excluded from early standard-setting; users experiencing repetitive strain were never consulted in layout design decisions.
% DISAPPEARANCE_RATIONALE: If the active maintenance mechanism vanished and incumbents stopped suppressing alternatives, alternative layouts would gain market share, training curricula would diversify, hardware procurement would shift, and the input-device economy would reorganize around open layout competition.
% FOUNDING_PROBLEM: The original need for a standardized typewriter keyboard to allow interchangeable operator training and equipment compatibility in the emerging typing profession.
% FOUNDING_PROBLEM_CORROBORATION: Business historians outside the benefiting parties document that the interoperability problem was solved decades ago; incumbent archives reveal active suppression campaigns against Dvorak and other alternatives, corroborated by independent economic historians and competition-policy researchers.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because incumbents leveraged standardization to create artificial switching costs rather than merely solving a coordination problem. Suppression (0.70) reflects active campaigns against alternatives and capture of standard-setting institutions. Theater ratio (0.38) captures the growing gap between the claimed interoperability rationale and the actual function of protecting incumbent position after the founding problem was solved. Accessibility collapse (0.70) reflects how alternative layouts became economically invisible after decades of incumbent control. Resistance (0.40) comes from efficiency advocates and marginalized innovators who lacked institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The manufacturer seat experiences the constraint as necessary industrial order and legitimate return on standardization investment; the user and innovator seats experience it as a locked-in, suboptimal extraction mechanism. The typing school seat experiences curriculum stability as a benefit while simultaneously being constrained by market demand. The engine computes this divergence from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers are structural beneficiaries with institutional power and mobile exit (they could have switched standards but chose to enforce QWERTY), giving them low directionality. Users are targets with constrained exit and diffuse costs, yielding high directionality. Typing schools sit as secondary beneficiaries with constrained exit (curriculum dependence). Alternative innovators are excluded targets with trapped exit, receiving the highest directionality. The engine will compute effective extraction as amplified for users and innovators and damped for manufacturers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâinteroperability in early typingâwas genuine and has been solved for over a century. The constraint's persistence beyond that point, maintained by actors who profit from its stability, indicates mandatrophy. The Tangled Rope classification captures that genuine coordination (standardization) was the origin, but the current operation is asymmetric extraction through suppression of alternatives. If the founding problem were still live, the constraint might read as Rope or Scaffold; its death confirms the extraction layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_natural_lapse,
    'Did alternative keyboard layouts fail because incumbents actively suppressed them, or because they were uncompetitive and naturally lapsed?',
    'Historical archival analysis of manufacturer marketing budgets, patent enforcement actions against alternatives, typing school procurement contracts, and standard-setting body composition.',
    'If suppression is proven, extraction and suppression scores are validated and the reading remains extraction-oriented; if alternatives lapsed naturally, the constraint shifts toward naturalization or rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_vs_natural_lapse, empirical, 'Core empirical ambiguity between active extraction and natural competition').

omega_variable(
    switching_cost_origin,
    'Are the switching costs binding users to QWERTY artificial constructs maintained by incumbents, or emergent network effects?',
    'Economic analysis of hardware retooling costs, training curriculum reform costs, and software remapping feasibility over time.',
    'Artificial costs validate the tangled_rope classification; purely emergent network effects support the lock_in_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_origin, conceptual, 'Whether switching costs are endogenous to incumbent strategy or emergent coordination properties').

omega_variable(
    founding_problem_current_relevance,
    'Does the original interoperability problem that justified QWERTY standardization still require the same constraint today?',
    'Analysis of digital input methods, software configurability, and contemporary hardware standardization requirements.',
    'If the founding problem is dead, mandatrophy is confirmed; if still live, the constraint retains more rope character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_current_relevance, empirical, 'Whether the constraint persists beyond its founding necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_ben_ext_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwerty_ben_ext_tr_t16, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(qwerty_ben_ext_tr_t32, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(qwerty_ben_ext_tr_t48, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 48, 0.3).
narrative_ontology:measurement(qwerty_ben_ext_tr_t64, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 64, 0.35).
narrative_ontology:measurement(qwerty_ben_ext_tr_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 80, 0.38).

% Extraction over time
narrative_ontology:measurement(qwerty_ben_ext_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwerty_ben_ext_be_t16, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(qwerty_ben_ext_be_t32, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(qwerty_ben_ext_be_t48, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(qwerty_ben_ext_be_t64, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 64, 0.7).
narrative_ontology:measurement(qwerty_ben_ext_be_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_ben_ext_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwerty_ben_ext_su_t16, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(qwerty_ben_ext_su_t32, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(qwerty_ben_ext_su_t48, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(qwerty_ben_ext_su_t64, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 64, 0.67).
narrative_ontology:measurement(qwerty_ben_ext_su_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 80, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
