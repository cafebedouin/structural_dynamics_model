% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__copenhagen_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quantum_formalism__copenhagen_reading
 *   human_readable: Copenhagen Reading of Wavefunction Collapse as Physical Measurement Boundary
 *   domain: philosophy_of_physics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the quantum_formalism kernel: the
 *   Copenhagen reading, which treats wavefunction collapse as a physical
 *   process constituting an absolute epistemic boundary, with measurement
 *   entering the theory as a primitive, non-eliminable ontological category
 *   and determinism genuinely abandoned at measurement events. This is not a
 *   story about 'quantum mechanics' broadly or about which interpretation is
 *   correct — it is a story about the specific institutional and epistemic
 *   constraint created by treating THIS reading as physics-department
 *   default. The sibling readings (many_worlds_reading: deterministic
 *   universal wavefunction with branching; pilot_wave_reading: deterministic
 *   hidden-variable particles guided by a physical pilot wave) are separate
 *   constraint stories with their own epsilon values, not alternative
 *   measurements of this one. All three readings are empirically
 *   indistinguishable by current experiment; they differ in what ontological
 *   commitments and institutional defaults they license.
 *
 * KEY AGENTS:
 *   - copenhagen_aligned_physics_departments: institutional beneficiary — pedagogical default status
 *   - operationalist_textbook_publishers: organized beneficiary — curriculum lock-in
 *   - foundational_orthodoxy_incumbents: institutional beneficiary/agenda_setter — gatekeeping via peer review and hiring
 *   - foundations_of_physics_researchers: moderate-power payer — career friction for holding minority readings
 *   - physics_students: powerless payer/beneficiary — receive workable calculational recipe with unexamined ontological commitment
 *   - many_worlds_and_pilot_wave_theorists: excluded — structurally minority voice on the same formalism
 *   - philosophers_of_physics: analytical observer — document the structure of the interpretive dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.28).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.22).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading of Wavefunction Collapse as Physical Measurement Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'a31196df-670e-44fc-8f76-fb71f0159051').
narrative_ontology:cs_kernel_codification('a31196df-670e-44fc-8f76-fb71f0159051', distributed).
narrative_ontology:cs_authority_grounding('a31196df-670e-44fc-8f76-fb71f0159051', practice).
narrative_ontology:cs_interpretation_layer_present('a31196df-670e-44fc-8f76-fb71f0159051').
narrative_ontology:cs_reading_relation('a31196df-670e-44fc-8f76-fb71f0159051', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('a31196df-670e-44fc-8f76-fb71f0159051', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('a31196df-670e-44fc-8f76-fb71f0159051', foundational, measurement_is_ontologically_primitive).
narrative_ontology:cs_axiom_status(measurement_is_ontologically_primitive, holdable).
narrative_ontology:cs_axiom_grounding('a31196df-670e-44fc-8f76-fb71f0159051', measurement_is_ontologically_primitive, conventional).
narrative_ontology:cs_axiom('a31196df-670e-44fc-8f76-fb71f0159051', foundational, collapse_is_irreducible_indeterminism).
narrative_ontology:cs_axiom_status(collapse_is_irreducible_indeterminism, holdable).
narrative_ontology:cs_axiom_grounding('a31196df-670e-44fc-8f76-fb71f0159051', collapse_is_irreducible_indeterminism, empirically_contingent).
narrative_ontology:cs_reference_frame('a31196df-670e-44fc-8f76-fb71f0159051', copenhagen_founding_consensus_1927).
narrative_ontology:cs_drift_state('a31196df-670e-44fc-8f76-fb71f0159051', post_decoherence_theory_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a31196df-670e-44fc-8f76-fb71f0159051', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_aligned_physics_departments).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operationalist_textbook_publishers).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, foundational_orthodoxy_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, physics_students).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, foundations_of_physics_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, physics_students).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_as_primitive_ontological_category).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, irreducible_indeterminism_at_collapse).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_non_eliminable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and publish within the Copenhagen framework as the default textbook interpretation. Curricula, qualifying exams, and standard textbooks (Sakurai, Griffiths introductory treatment) present collapse-on-measurement as settled pedagogy, which reduces the burden of teaching alternative ontologies and stabilizes what counts as a complete answer on an exam.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_aligned_physics_departments, beneficiary,
    institutional, generational, arbitrage, global).

% Produce standard curricula built around the measurement postulate as a clean, teachable rule (Born rule plus projection). Switching to a many-worlds or pilot-wave framing would require substantial rewriting and re-certification against established syllabi; the existing textbook base benefits from Copenhagen's status as the path of least resistance.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operationalist_textbook_publishers, beneficiary,
    organized, generational, mobile, global).

% Senior figures whose careers and citation records were built defending or elaborating the measurement-as-primitive framework (decoherence-tolerant Copenhagen, QBism-adjacent variants). They referee journals, sit on hiring committees, and shape which foundational disputes are treated as live physics versus philosophy.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundational_orthodoxy_incumbents, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, foundational_orthodoxy_incumbents, agenda_setter).

% Researchers working on many-worlds, pilot-wave, or objective-collapse programs must argue against an entrenched default, face funding and publication headwinds in mainstream physics venues, and are sometimes filed under 'philosophy' rather than 'physics' for tenure and grant purposes. They pay in career friction for holding a minority reading of the same formalism.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, foundations_of_physics_researchers, payer,
    moderate, biographical, constrained, global).

% Learn the measurement postulate as the operational rule needed to compute Born-rule probabilities and pass exams, often without exposure to the interpretive contest. They benefit from a workable calculational recipe but pay in an unexamined ontological commitment presented as settled fact rather than one reading among several.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, physics_students, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, physics_students, beneficiary).

% Hold that determinism is preserved (via branching or hidden variables) and that collapse is not a physical event but an artifact of incomplete description or decoherence. They are structurally present in the same journals and conferences but are a minority voice against a default that shapes what 'quantum mechanics' means in most training pipelines.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, many_worlds_and_pilot_wave_theorists, excluded,
    moderate, biographical, constrained, global).

% Analyze the interpretive dispute itself, cataloguing which empirical predictions (if any) distinguish the readings and which commitments are metaphysical additions with no operational difference. They do not adjudicate the physics but document the structure of the disagreement.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophers_of_physics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single operational rule (Born rule + projection postulate) that lets physicists compute measurement outcome probabilities without resolving deeper ontological questions — a genuinely useful calculational convention adopted as a default teaching and research framework.
% TRANSFER_FUNCTION: Moves pedagogical default status, textbook adoption, and 'mainstream physics' legitimacy toward departments and researchers whose work presupposes measurement-as-primitive, and away from researchers whose programs (many-worlds, pilot-wave, objective collapse) require rebutting that default before their own work can be evaluated on its merits.
% ABSENT_VOICES: Many-worlds and pilot-wave theorists are present in the field but structurally minority; physics students rarely encounter the interpretive contest as a live dispute during standard coursework, so the population most affected by the ontological framing is the population least equipped to evaluate it.
% DISAPPEARANCE_RATIONALE: If Copenhagen's default status vanished, the empirical predictions of quantum mechanics (Born-rule statistics) would be entirely unchanged — no experiment distinguishes the leading interpretations. What would rearrange is pedagogical default, funding patterns for foundations research, and which papers get filed as 'physics' versus 'philosophy.' Whether that counts as the world rearranging or staying the same is itself part of the interpretive dispute — Copenhagen-aligned physicists would say nothing of physical substance changes, and foundations researchers on excluded programs would say a great deal of institutional substance changes.
% FOUNDING_PROBLEM: Early quantum mechanics needed an operational rule connecting the abstract formalism (wavefunctions, operators) to actual laboratory outcomes — something practicing physicists could use to compute and verify predictions without waiting for a complete ontological theory of measurement.
% FOUNDING_PROBLEM_CORROBORATION: Working physicists across all interpretive camps attest that the calculational need (Born rule predictions matching experiment) is still live and universally used. But many-worlds and pilot-wave theorists, along with philosophers of physics writing from outside the Copenhagen-aligned institutional base, attest that the ORIGINAL problem — a placeholder pending a complete theory — has been transformed into a claimed metaphysical conclusion (collapse as a real physical event, observer as ontologically primitive) that no experiment has corroborated and that competing programs answer without abandoning determinism.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__copenhagen_reading_tests).
:- end_tests(quantum_formalism__copenhagen_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.28) and suppression (0.22) are both moderate-low: the constraint's cost is not economic rent but epistemic and career-institutional — the redirection of legitimacy, funding, and 'settled physics' status toward one reading over empirically equivalent siblings. Accessibility collapse is moderate (0.45), not high, because the alternative readings remain fully articulated and available in the literature; nothing physically prevents a student or researcher from adopting many-worlds or pilot-wave. Resistance is comparatively high (0.55) because active, sophisticated resistance to Copenhagen's default status exists throughout the foundations-of-physics community and has for decades (Bohm 1952, Everett 1957, and continuously since). This is a Rope claim (a genuinely useful operational convention: the Born rule as a working recipe) whose metrics show a real but modest extractive residue in institutional legitimacy allocation — the claim and the metrics are authored independently, and their partial divergence is the reading's own signature: a good operational tool has accreted a metaphysical default status beyond what the operational content requires.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a Copenhagen-aligned department, this is simply 'quantum mechanics' — the standard, complete, physically motivated account. From the seat of a many-worlds or pilot-wave researcher, the identical formalism is a case of one empirically-underdetermined reading having captured default institutional status through path dependence and gatekeeping rather than evidential superiority. The engine computes these as structurally different seat experiences from the same authored data; this story does not adjudicate which seat is 'right' about the physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are institutional actors whose careers, curricula, and gatekeeping authority are built around Copenhagen as default (departments, publishers, orthodoxy incumbents) — low d, they collect legitimacy and pedagogical convenience from the default status. Foundations researchers on minority programs are payers — higher d, they bear career and publication friction. Physics students sit near symmetric: they benefit from a workable calculational tool but pay in an unexamined ontological commitment they are rarely told is contested. No group is a 'victim' in the extraction sense used for snare/tangled_rope — this is a rope-with-residue, not an extraction mechanism with identifiable victims, which is why victims is left empty and no tangled_rope gate is triggered.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a working rule connecting formalism to lab outcomes) remains genuinely live and universally needed — that function has not atrophied. What is contested is whether the METAPHYSICAL gloss added onto that operational rule (collapse as physically real, observer as ontologically primitive) still serves the original problem or has become a separate, unearned institutional default. The disappearance_verdict is marked contested precisely because Copenhagen-aligned physicists would say the operational content is all that matters and nothing changes if the metaphysical gloss vanished, while excluded programs would say a great deal of institutional allocation would change. This divergence is exactly what the six-questions genealogy is designed to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_primitive_vs_derived,
    'Is ''measurement'' a genuinely primitive physical category requiring a separate collapse postulate, or is it a derived, approximate description of ordinary unitary interaction between system and environment (as decoherence theory and the sibling readings hold)?',
    'No current experiment distinguishes the readings; resolution would require either a future empirical signature that differentiates collapse dynamics from unitary-plus-decoherence dynamics (e.g., objective-collapse-model signatures at macroscopic scale), or a decisive theoretical argument establishing that observer-relative description is eliminable in principle.',
    'If measurement is shown to be reducible to unitary dynamics plus decoherence, the Copenhagen reading''s core ontological claim collapses into a special case of the many-worlds reading, and its institutional default status would lose its physical justification (though its operational Born-rule content would remain valid under any reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_primitive_vs_derived, conceptual, 'Whether measurement is ontologically primitive or a derived approximation — the central undecided question this reading commits to answering one way.').

omega_variable(
    beneficiary_capture_vs_genuine_default,
    'Does Copenhagen''s status as default pedagogy and research orthodoxy reflect genuine epistemic superiority (simplicity, historical priority, operational adequacy) or institutional path-dependence and gatekeeping that has outlasted its justification?',
    'Comparative history-of-science analysis of how interpretive defaults are set and maintained in physics curricula, cross-checked against surveys of working physicists'' actual interpretive commitments versus what is taught (e.g., the ''shut up and calculate'' phenomenon, where practitioners are agnostic but curricula are not).',
    'If path-dependence dominates, the beneficiary declarations here are structurally load-bearing (a textbook case of institutional default capture); if epistemic superiority dominates, the beneficiary structure is incidental to a genuinely warranted convention and the FSM-adjacent reading (naturalized default masking contestedness) is weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_default, empirical, 'Whether the beneficiary structure reflects genuine warrant or institutional capture of a contested question.').

omega_variable(
    cs_framing_kernel_vs_reading_level,
    'Should the commitment-system framing be applied at the kernel level (the entire quantum formalism as one contested commitment system with three readings as competing authority claims) or at the reading level (each reading as its own separate, self-contained authority structure)?',
    'This story adopts the reading-level framing per the ε-invariance and kernel/reading rules: each reading gets its own axioms and cs_structure fields, with reading_relations carrying the kernel-level connective tissue. A kernel-level alternative framing would instead treat ''quantum foundations'' as one CS object with the interpretive dispute as internal contestation, which would produce a single constraint story rather than three linked ones.',
    'The reading-level framing (adopted here) allows each reading''s distinct beneficiary structure and institutional consequences to be measured independently; a kernel-level framing would average over these differences and obscure exactly the divergence in institutional consequence that motivates the decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_reading_level, conceptual, 'Framing choice between kernel-level and reading-level commitment-system analysis, and why reading-level was selected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.05).
narrative_ontology:measurement(quan_tr_t1957, quantum_formalism__copenhagen_reading, theater_ratio, 1957, 0.08).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__copenhagen_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(quan_tr_t2000, quantum_formalism__copenhagen_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(quan_tr_t2015, quantum_formalism__copenhagen_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.15).
narrative_ontology:measurement(quan_be_t1957, quantum_formalism__copenhagen_reading, base_extractiveness, 1957, 0.2).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__copenhagen_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(quan_be_t2000, quantum_formalism__copenhagen_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(quan_be_t2015, quantum_formalism__copenhagen_reading, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__copenhagen_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint stories decomposing the 'quantum formalism interpretation' natural-language label per the ε-invariance principle. quantum_formalism__copenhagen_reading (this file) treats collapse as physically real and measurement as ontologically primitive. quantum_formalism__many_worlds_reading treats evolution as universally unitary/deterministic with apparent branching. quantum_formalism__pilot_wave_reading treats particles as having definite deterministic trajectories guided by a physical pilot wave. All three share the same empirical formalism and predictions but diverge sharply in ontological commitment, beneficiary structure (which reading enjoys institutional default status), and epistemic consequence. Each carries its own claimed_type and metrics; they are linked via affects_constraints rather than merged, per Rule 1 of the kernel/reading authoring discipline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
