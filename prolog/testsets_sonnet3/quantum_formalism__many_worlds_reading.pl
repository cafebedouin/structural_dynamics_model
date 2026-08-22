% ============================================================================
% CONSTRAINT STORY: quantum_formalism__many_worlds_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__many_worlds_reading, []).

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
 *   constraint_id: quantum_formalism__many_worlds_reading
 *   human_readable: Many-Worlds Reading of the Quantum Formalism Kernel
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story authors the many-worlds reading of the quantum formalism
 *   kernel: the universal wavefunction evolves strictly unitarily and without
 *   exception; what appears to observers as measurement outcomes and collapse
 *   is decoherence-induced branching, with every outcome realized in a
 *   distinct, causally isolated branch. The reading eliminates the observer
 *   as a fundamental ingredient of the theory and treats determinism as
 *   global — nothing in nature is stochastic, only self-locating uncertainty
 *   within a branch is apparent. The extraction this story measures is not
 *   physical-world extraction but institutional and epistemic: legitimacy,
 *   publication access, and framing authority flow to researchers whose
 *   programs are congenial to strict unitarity, while experimentalists
 *   seeking discriminating tests and early-career researchers choosing a
 *   specialization bear real costs from a kernel dispute that current physics
 *   cannot adjudicate empirically. Sibling readings (Copenhagen, pilot-wave)
 *   are NOT part of this story; they are separate constraints linked via
 *   network.affects_constraints, each with its own epsilon.
 *
 * KEY AGENTS:
 *   - many_worlds_theorists: agenda_setter/beneficiary (institutional/arbitrage) — sets the internal research program and collects institutional legitimacy
 *   - decision_theoretic_probability_researchers: beneficiary (organized/mobile) — builds careers on the internal Born-rule derivation problem
 *   - experimentalists_seeking_falsification_targets: payer (moderate/constrained) — cannot secure funding for discriminating tests the reading renders currently inaccessible
 *   - graduate_students_choosing_interpretive_specialization: payer (powerless/trapped) — commits early to a specialization with high switching costs
 *   - copenhagen_and_pilot_wave_theorists: excluded (organized/constrained) — hold rival readings of the same kernel, structurally outside this reading's internal agenda
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__many_worlds_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__many_worlds_reading, 0.18).
domain_priors:theater_ratio(quantum_formalism__many_worlds_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__many_worlds_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__many_worlds_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__many_worlds_reading, "Many-Worlds Reading of the Quantum Formalism Kernel").
narrative_ontology:topic_domain(quantum_formalism__many_worlds_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__many_worlds_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__many_worlds_reading, '591d5c2f-aa1a-42e7-aca3-4bfefdf12172').
narrative_ontology:cs_kernel_codification('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', distributed).
narrative_ontology:cs_authority_grounding('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', expertise).
narrative_ontology:cs_interpretation_layer_present('591d5c2f-aa1a-42e7-aca3-4bfefdf12172').
narrative_ontology:cs_reading_relation('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', quantum_formalism__pilot_wave_reading, coexists_with).
narrative_ontology:cs_axiom('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', foundational, wavefunction_is_the_complete_physical_state).
narrative_ontology:cs_axiom_status(wavefunction_is_the_complete_physical_state, holdable).
narrative_ontology:cs_axiom_grounding('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', wavefunction_is_the_complete_physical_state, conventional).
narrative_ontology:cs_axiom('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', foundational, measurement_is_eliminable_as_a_fundamental_primitive).
narrative_ontology:cs_axiom_status(measurement_is_eliminable_as_a_fundamental_primitive, holdable).
narrative_ontology:cs_axiom_grounding('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', measurement_is_eliminable_as_a_fundamental_primitive, instrumental).
narrative_ontology:cs_reference_frame('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', unitary_formalism_minimalism).
narrative_ontology:cs_drift_state('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', post_decoherence_program_maturation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('591d5c2f-aa1a-42e7-aca3-4bfefdf12172', '').
narrative_ontology:cs_kernel_id(quantum_formalism__many_worlds_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, many_worlds_theorists).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, decision_theoretic_probability_researchers).
narrative_ontology:constraint_beneficiary(quantum_formalism__many_worlds_reading, quantum_information_unitarity_programs).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_targets).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, graduate_students_choosing_interpretive_specialization).
narrative_ontology:constraint_victim(quantum_formalism__many_worlds_reading, philosophy_of_science_public_communicators).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, wavefunction_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, global_unitarity).
narrative_ontology:constraint_vindicates(quantum_formalism__many_worlds_reading, eliminativism_about_observers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and defend the Everettian program, publish decision-theoretic derivations of the Born rule, and set the research agenda for what counts as a live problem within the reading (e.g. the preferred-basis problem, probability derivation). They hold tenured positions and grant access built partly on the program's continued vitality, and can move between formal derivation projects if one approach stalls.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, many_worlds_theorists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__many_worlds_reading, many_worlds_theorists, beneficiary).

% Build careers on solving the Born-rule derivation problem internal to many-worlds (Deutsch-Wallace decision theory and its critics). Their research program's legitimacy depends on many-worlds being treated as the live default rather than one contested reading among several; they can pivot to adjacent foundations work if the program loses standing, but current output and citations are heavily invested here.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, decision_theoretic_probability_researchers, beneficiary,
    organized, biographical, mobile, global).

% Quantum computing and quantum gravity research often assumes strict unitarity and no physical collapse as a working default, which is congenial to and sometimes explicitly justified by the many-worlds reading. They benefit from a philosophical vindication of an assumption their engineering and theoretical work already needs, even though the assumption is separable from full commitment to many-worlds ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, quantum_information_unitarity_programs, beneficiary,
    organized, generational, mobile, global).

% Design experiments intended to discriminate among interpretations, but many-worlds is constructed so that no accessible experiment distinguishes it from rivals at current technological reach — all interpretations predict identical Born-rule statistics for any single-world observer. They pay in the currency of unfunded or unpublishable proposals, since grant panels and journals increasingly treat interpretation choice as settled by theoretical elegance rather than pending confirmation, and cannot exit the felt obligation to address many-worlds when framing foundational work.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, experimentalists_seeking_falsification_targets, payer,
    moderate, biographical, constrained, global).

% Must choose a specialization advisor and program early in training, often before they have the technical maturity to independently evaluate the metaphysical costs of ontological profligacy versus the technical costs of collapse dynamics or hidden-variable nonlocality. Committing to many-worlds shapes which conferences, journals, and hiring committees will recognize their work as mainstream; switching later costs years of retraining and social capital they may not have.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, graduate_students_choosing_interpretive_specialization, payer,
    powerless, biographical, trapped, national).

% Explain quantum foundations to the public and to policy-adjacent audiences and must represent the interpretive contest honestly. They bear reputational cost either way: presenting many-worlds as consensus science overstates its status, while presenting it as fringe understates its institutional weight within specific subfields, and the felt need to hedge every statement is a direct cost of the kernel's unresolved status being routed through this one reading's confident public voice.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, philosophy_of_science_public_communicators, payer,
    moderate, biographical, constrained, global).

% Hold rival readings of the same kernel and would object that many-worlds' claim to parsimony (no collapse postulate) trades one kind of extravagance (infinite branching worlds) for another and is not obviously the more economical theory. They are present in the broader field but structurally external to this specific reading's internal research agenda and citation network.
narrative_ontology:constraint_stakeholder(quantum_formalism__many_worlds_reading, copenhagen_and_pilot_wave_theorists, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, mathematically minimal extension of the unmodified Schrodinger equation (no collapse postulate, no hidden variables) that lets researchers across quantum computing, cosmology, and quantum gravity share a common formal starting point without invoking an observer-dependent collapse mechanism.
% TRANSFER_FUNCTION: Moves institutional legitimacy, publication access, and framing authority toward researchers whose programs are congenial to strict unitarity and away from researchers pursuing collapse models, hidden-variable programs, or experimentalists seeking discriminating tests — while moving explanatory burden (accounting for the appearance of a single outcome, deriving probability from a deterministic multiverse) onto whoever adopts the reading.
% ABSENT_VOICES: Copenhagen and pilot-wave theorists are present in the broader field but not inside this reading's internal research agenda; they would argue the ontological cost of infinite unobservable worlds is at least as extravagant as a collapse postulate or hidden variables, and that treating decoherence as solving the preferred-basis and probability problems overstates settled progress.
% DISAPPEARANCE_RATIONALE: If the many-worlds reading vanished as an institutionally live option, the underlying mathematical formalism (Schrodinger equation, decoherence theory) would persist unchanged, since decoherence is uncontested physics; what would rearrange is the sociology of foundations departments, the framing of quantum-computing popularizations, and specific decision-theoretic research programs that have no object without the ontological commitment. Whether that counts as 'the world rearranging' or 'a contested interpretive layer evaporating while physics continues' is itself part of the kernel dispute.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics needs an account of why measurements yield single definite outcomes when the formalism (unitarily evolved) predicts superpositions persisting indefinitely. Many-worlds was proposed to solve this without adding a physical collapse postulate that seemed ad hoc and in tension with relativistic locality.
% FOUNDING_PROBLEM_CORROBORATION: Decoherence theorists broadly agree decoherence explains why superpositions become practically unobservable and branches stop interfering (this part is corroborated by working physicists outside the interpretive debate, e.g. quantum-computing engineers who use decoherence calculations without adopting many-worlds ontology). Whether decoherence alone solves the preferred-basis problem and derives objective probabilities — the specifically many-worlds claims — is corroborated mainly by many-worlds theorists themselves; Copenhagen and pilot-wave theorists, and philosophers of probability such as those pressing the incoherence-of-self-locating-belief objections, dispute that the founding problem is actually solved rather than relabeled.
narrative_ontology:disappearance_verdict(quantum_formalism__many_worlds_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__many_worlds_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__many_worlds_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__many_worlds_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__many_worlds_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__many_worlds_reading_tests).
:- end_tests(quantum_formalism__many_worlds_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate rather than low: it does not extract material resources but does redirect scarce institutional goods (positions, grants, publication slots, framing authority in popularization) toward a specific metaphysical commitment that has not been and by its own internal logic largely cannot be empirically discriminated from its rivals at accessible energies. Suppression is low (0.18) because no one is coerced into believing many-worlds; the mechanism is soft — selection effects in hiring, funding panel composition, and citation networks, not censorship. Accessibility collapse is moderate (0.35): the mathematical formalism and decoherence theory remain shared ground; what collapses is mainly the felt legitimacy of pursuing rival ontological commitments within specific subfields, not experimental access to the underlying physics. Resistance is real (0.55): a substantial community of collapse-model and hidden-variable theorists actively contests the reading's claim to have solved the measurement problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Many-worlds theorists and the decision-theoretic research program are the structural beneficiaries: they collect career capital, publication access, and the standing to declare a problem (Born-rule derivation, preferred basis) solved-in-principle by their own program's internal resources. Quantum-information programs benefit incidentally by having an existing formal assumption they need (strict unitarity) philosophically dignified. Experimentalists, early-career researchers, and public communicators are the targets: they bear an unresolved metaphysical dispute as a career and communicative cost without having chosen the terms of the dispute. Copenhagen and pilot-wave theorists are excluded rather than coordinated — they hold a genuinely rival reading of the same kernel and are external to this reading's internal agenda, not stakeholders coordinated by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accounting for definite measurement outcomes without an ad hoc collapse postulate) is genuinely live at the level of decoherence explaining branch non-interference — that part of the program has not become mandatrophic. What is contested is whether the further claims (objective probability derivation, preferred-basis selection, elimination of the observer) have actually resolved the founding problem or merely relocated it into decision theory and self-locating credence puzzles that remain internally disputed even among many-worlds proponents (e.g. the Born-rule derivation debates between Wallace, Deutsch, and their critics). The tangled_rope classification is chosen because there is a genuine coordination function (a shared, minimal formal starting point across quantum computing, cosmology, and foundations) coexisting with asymmetric extraction (career and framing capital flowing to the reading's internal community at cost to those outside it) requiring active maintenance (citation gatekeeping, curriculum design, funding panel composition) — not because the physics itself is settled either way.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    many_worlds_kernel_reading_status,
    'Is the many-worlds reading a genuine resolution of the measurement problem that the other kernel readings have failed to match, or is it one contested metaphysical package among several equally unresolved packages, distinguished mainly by which formal cost (branching ontology vs. collapse postulate vs. nonlocal hidden variables) its proponents find least objectionable?',
    'No purely empirical resolution mechanism currently exists at accessible energies, since all three kernel readings reproduce identical Born-rule statistics for single-world observers; resolution (if any) would require either a future empirical discriminator (e.g. testable deviations from unitarity, or evidence bearing on decoherence-based probability derivations) or philosophical consensus on parsimony criteria that does not currently exist.',
    'If many-worlds is a genuine resolution, the institutional legitimacy it currently commands is warranted and the extraction measured here is better read as ordinary paradigm-building cost. If it is one contested package among several, the same institutional legitimacy functions partly as extraction from rival programs and from researchers who cannot yet be shown to be wrong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(many_worlds_kernel_reading_status, conceptual, 'Whether many-worlds'' claimed solution to the measurement problem is structurally superior to its sibling readings or merely differently costly.').

omega_variable(
    sibling_reading_structural_delta,
    'What specifically changes structurally if a reader adopts the copenhagen_reading or pilot_wave_reading instead of this one, and where exactly is the disagreement located?',
    'Compare the three sibling constraint stories directly: copenhagen_reading treats measurement as a real, irreducible physical event and an epistemic boundary (observer is NOT eliminable, determinism is NOT global); pilot_wave_reading treats the wavefunction as a real physical guiding field with definite particle positions (hidden variables restore classical ontology, no branching). The disagreement is located precisely in (a) whether collapse is physical or apparent, (b) whether the observer/measurement act is fundamental or derivative, (c) whether the theory needs one world or many, and (d) how each reading accounts for the empirically observed Born-rule statistics without adding an untestable auxiliary structure.',
    'Adopting a sibling reading changes which agents are structurally beneficiaries versus payers: under copenhagen_reading, the measurement-as-physical-event framing benefits researchers building interpretive frameworks around observer/apparatus distinctions and burdens many-worlds-style decision theorists; under pilot_wave_reading, hidden-variable researchers become the beneficiary class and both many-worlds and Copenhagen theorists become the excluded rival community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Locating exactly where the three kernel readings structurally diverge, for cross-reference against the sibling constraint files.').

omega_variable(
    probability_derivation_internal_dispute,
    'Within the many-worlds reading itself, is the Deutsch-Wallace decision-theoretic derivation of the Born rule a successful internal resolution, or does it merely relocate the probability problem into unresolved puzzles about self-locating credence in a deterministic multiverse?',
    'Ongoing philosophical and technical literature (e.g. objections from Adrian Kent, David Albert, Simon Saunders'' responses) constitutes the resolution mechanism; no external empirical test currently distinguishes the positions.',
    'If unresolved, part of the extraction measured here (career capital flowing to decision-theoretic probability researchers) rests on a foundational internal dispute rather than a settled internal success, sharpening the tangled_rope reading over a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_derivation_internal_dispute, empirical, 'Whether the Born-rule derivation problem is settled within many-worlds itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__many_worlds_reading, 1957, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1957, quantum_formalism__many_worlds_reading, theater_ratio, 1957, 0.1).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__many_worlds_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(quan_tr_t1995, quantum_formalism__many_worlds_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(quan_tr_t2005, quantum_formalism__many_worlds_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(quan_tr_t2015, quantum_formalism__many_worlds_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(quan_tr_t2024, quantum_formalism__many_worlds_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(quan_be_t1957, quantum_formalism__many_worlds_reading, base_extractiveness, 1957, 0.15).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__many_worlds_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(quan_be_t1995, quantum_formalism__many_worlds_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(quan_be_t2005, quantum_formalism__many_worlds_reading, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement(quan_be_t2015, quantum_formalism__many_worlds_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(quan_be_t2024, quantum_formalism__many_worlds_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1957, quantum_formalism__many_worlds_reading, suppression_requirement, 1957, 0.08).
narrative_ontology:measurement(quan_su_t1980, quantum_formalism__many_worlds_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(quan_su_t1995, quantum_formalism__many_worlds_reading, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement(quan_su_t2005, quantum_formalism__many_worlds_reading, suppression_requirement, 2005, 0.15).
narrative_ontology:measurement(quan_su_t2015, quantum_formalism__many_worlds_reading, suppression_requirement, 2015, 0.17).
narrative_ontology:measurement(quan_su_t2024, quantum_formalism__many_worlds_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__many_worlds_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__many_worlds_reading, 0.1).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__many_worlds_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quantum_formalism kernel (copenhagen_reading, many_worlds_reading, pilot_wave_reading). Each reading is authored as its own constraint with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle: the underlying formal claim (the Schrodinger equation, decoherence theory) is uncontested across all three and would itself be a separate near-mountain constraint if authored; what differs across these three stories is the institutional and epistemic packaging built on top of that shared uncontested core. Network edges here register that a shift in one reading's institutional standing (e.g. a resolved probability-derivation dispute) would structurally affect resource allocation and legitimacy in the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
