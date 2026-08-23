% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: QWERTY Persistence via Incumbent Extraction
 *   domain: economic/historical/technological
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, originally designed to reduce mechanical key
 *   jams in 1870s typewriters, persisted long after the technical rationale
 *   vanished. This reading argues that persistence was not a neutral lock-in
 *   but an actively maintained extraction mechanism: Remington/Union
 *   Typewriter and the typing-school ecosystem they seeded used procurement
 *   contracts, certification requirements, and lobbying to suppress superior
 *   alternatives (e.g., Dvorak), thereby converting a coordination standard
 *   into a rent-extraction device. The constraint is claimed as a tangled
 *   rope because it retains a residual coordination function
 *   (interoperability) while layering asymmetric extraction via artificial
 *   switching costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.75).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "QWERTY Persistence via Incumbent Extraction").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic/historical/technological").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '7df95a2f-d7c5-4014-b38d-07b46572222f').
narrative_ontology:cs_kernel_codification('7df95a2f-d7c5-4014-b38d-07b46572222f', implicit).
narrative_ontology:cs_authority_grounding('7df95a2f-d7c5-4014-b38d-07b46572222f', extraction).
narrative_ontology:cs_reading_relation('7df95a2f-d7c5-4014-b38d-07b46572222f', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_reading_relation('7df95a2f-d7c5-4014-b38d-07b46572222f', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_axiom('7df95a2f-d7c5-4014-b38d-07b46572222f', foundational, qwerty_persists_by_incumbent_extraction).
narrative_ontology:cs_axiom_status(qwerty_persists_by_incumbent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('7df95a2f-d7c5-4014-b38d-07b46572222f', qwerty_persists_by_incumbent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('7df95a2f-d7c5-4014-b38d-07b46572222f', foundational, switching_costs_are_artificially_maintained).
narrative_ontology:cs_axiom_status(switching_costs_are_artificially_maintained, holdable).
narrative_ontology:cs_axiom_grounding('7df95a2f-d7c5-4014-b38d-07b46572222f', switching_costs_are_artificially_maintained, empirically_contingent).
narrative_ontology:cs_reference_frame('7df95a2f-d7c5-4014-b38d-07b46572222f', mechanical_typewriter_era_standard).
narrative_ontology:cs_drift_state('7df95a2f-d7c5-4014-b38d-07b46572222f', digital_keyboard_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7df95a2f-d7c5-4014-b38d-07b46572222f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_makers).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, path_dependence_can_be_engineered).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held patents and production capacity for QWERTY typewriters; actively lobbied for QWERTY adoption in government and business contracts, and used market power to exclude competing layouts (e.g., by bundling QWERTY machines with training contracts). Beneficiaries of the artificial switching costs they helped create.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, remington_union_typewriter, agenda_setter,
    powerful, generational, arbitrage, global).

% Built curricula, certification programs, and teacher pipelines around QWERTY; profit from the recurring need for typing instruction. Switching layouts would require massive reinvestment in materials and retraining, so they resist change.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Learned QWERTY as a professional skill; switching entails high cognitive retraining costs and temporary productivity loss. Their professional identity (speed, accuracy certification) is fused with QWERTY, making exit psychologically and economically costly.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Developed technically superior layouts (e.g., Dvorak, Colemak) but cannot overcome the installed base, typing-school pipeline, and procurement standards that enforce QWERTY. Their exclusion is maintained by the same network effects and active suppression that benefit incumbents.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_keyboard_makers, excluded,
    moderate, biographical, trapped, global).

% Study the QWERTY case as a canonical example of path dependence; debate whether persistence reflects efficiency, lock-in, or engineered extraction. Their analysis shapes policy discourse on standards and competition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, historians_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: QWERTY provided a common keyboard layout that enabled mass production of typewriters, interchangeable parts, and a transferable typing skill across firms and generations — a genuine coordination solution for the mechanical era.
% TRANSFER_FUNCTION: The entrenched standard transfers wealth from typists (who bear lifelong switching costs and suboptimal ergonomics) and alternative keyboard makers (who are blocked from market entry) to incumbent manufacturers and typing schools (who collect rents from the installed base and training pipeline).
% ABSENT_VOICES: Alternative keyboard inventors (e.g., August Dvorak) and typists who would prefer a more efficient layout were structurally excluded; they could not organize a countervailing coalition because the standard was locked in before they could gain traction, and incumbents controlled the procurement and training channels.
% DISAPPEARANCE_RATIONALE: If the QWERTY mandate vanished overnight, keyboard layouts would become a competitive market; typing education would diversify; manufacturers would innovate on ergonomics; the typing workforce would undergo a one-time retraining shock but then settle on more efficient layouts.
% FOUNDING_PROBLEM: The founding problem was the need for a standard keyboard layout to prevent mechanical key jams in early typewriters and to create a transferable typing skill for the emerging clerical workforce in the 1870s.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (David 1985; Liebowitz & Margolis 1990) document that the anti-jamming rationale was specific to mechanical linkages and became irrelevant with electric and electronic keyboards; the persistence of QWERTY after the 1930s cannot be explained by the founding problem.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness rises from 0.2 to 0.72 as the mechanical justification fades and the rent-seeking layer thickens. Suppression requirement climbs from 0.3 to 0.75 because maintaining the standard against superior alternatives required escalating institutional enforcement (government procurement specs, typing-test mandates, OEM bundling). Theater ratio grows from 0.1 to 0.38 as the coordination cover story ("standardization") becomes increasingly performative relative to the extraction core. All metrics share a single time grid (0,30,60,90,120,150) representing decades since 1870.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the constraint looks like a coordination success they built and maintain; from the payer seats it looks like an enforced monopoly. The engine will compute this divergence — the claimed type (tangled_rope) reflects the author's structural judgment that both functions are real and fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Remington/Union and typing schools are structural beneficiaries (d ≈ 0.1–0.2): they collect rents and control the standard. Typists are full targets (d ≈ 0.9): identity-locked into QWERTY, bearing switching costs with no exit. Alternative keyboard makers are trapped (d ≈ 0.95): excluded by the very constraint they would disrupt. Historians are analytical (d = 0.5). The engine derives these from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jamming) died in the 1930s; the arrangement persisted because incumbents captured the coordination function and converted it into extraction. This is a textbook mandatrophy case: a scaffold that never sunset, hardened into a tangled rope. The founding_problem_status = dead + disappearance_verdict = world_rearranges mismatch flags the zombie structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the QWERTY persistence kernel admit a single structural classification, or do the three readings (beneficiary_extraction, lock_in, naturalization) identify genuinely distinct constraints with different ε values?',
    'Decompose the kernel into three constraint stories (this file plus two siblings) and compare their metric profiles; if ε differs substantially across readings, the kernel is a family, not a single constraint.',
    'If the readings decompose into distinct constraints, the engine will classify them separately and link them via network.affects_constraints; if they collapse to one ε, the kernel is a false multiplex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel qwerty_persistence_mechanism is one constraint or a family of three.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of alternative layouts primarily structural (procurement rules, bundling) or internalized (typists believing QWERTY is optimal)?',
    'Examine historical records of Dvorak adoption attempts: if typists resisted Dvorak even when offered, internalized suppression is significant; if they were blocked by institutional barriers, structural suppression dominates.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the QWERTY case.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(qwer_tr_t90, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 90, 0.3).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(qwer_tr_t150, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 150, 0.38).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(qwer_be_t90, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 90, 0.62).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement(qwer_be_t150, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 150, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(qwer_su_t90, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(qwer_su_t120, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 120, 0.73).
narrative_ontology:measurement(qwer_su_t150, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 150, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.02).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% This reading, lock_in_reading, and naturalization_reading form a constraint family decomposing the QWERTY persistence kernel. Each has distinct ε, beneficiaries/victims, and claimed type. The extraction reading influences the lock-in reading by showing that active suppression — not just passive network effects — maintains the standard.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, organized, 0.15).
constraint_indexing:directionality_override(qwerty_persistence_mechanism__beneficiary_extraction_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
