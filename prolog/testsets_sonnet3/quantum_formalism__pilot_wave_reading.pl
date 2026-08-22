% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantum_formalism__pilot_wave_reading, []).

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
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot-Wave (de Broglie–Bohm) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story instantiates the pilot-wave (de Broglie–Bohm) reading of the
 *   quantum formalism kernel: particles possess definite positions and
 *   trajectories at all times, guided nonlocally by a physically real
 *   wavefunction/pilot-wave field. Determinism is restored at the level of
 *   the guiding equation (though guidance is nonlocal), and the observer is
 *   eliminable as a fundamental ingredient — measurement merely reveals a
 *   pre-existing position rather than causing collapse. The reading is
 *   empirically equivalent to Copenhagen and many-worlds readings for all
 *   currently testable predictions; the coordination function (a workable,
 *   teachable, mathematically explicit alternative to collapse postulates) is
 *   real, but the reading also functions as an extraction structure on the
 *   career prospects of researchers who commit to it in an institutional
 *   landscape still organized around Copenhagen-flavored pedagogy and hiring
 *   norms.
 *
 * KEY AGENTS:
 *   - bohmian_mechanics_research_programs: Primary beneficiary/agenda_setter (organized/constrained) — sustains and administers the reading's research community
 *   - realist_philosophy_of_physics_departments: Secondary beneficiary (moderate/mobile) — draws on the reading without bearing its career costs
 *   - pilot_wave_graduate_students: Primary payer (powerless/trapped) — bears career risk from committing to a minority interpretation
 *   - non_standard_interpretation_researchers: Payer (moderate/constrained) — bears funding and publication friction
 *   - mainstream_quantum_foundations_community: Excluded from serious engagement (institutional/arbitrage) — treats interpretation as instrumentally moot
 *   - philosophy_of_science_observers: Analytical observer (analytical/analytical) — documents empirical equivalence across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.55).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave (de Broglie–Bohm) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '086a7152-f100-4222-a9ae-ff608a02a8f4').
narrative_ontology:cs_kernel_codification('086a7152-f100-4222-a9ae-ff608a02a8f4', formalized).
narrative_ontology:cs_authority_grounding('086a7152-f100-4222-a9ae-ff608a02a8f4', expertise).
narrative_ontology:cs_interpretation_layer_present('086a7152-f100-4222-a9ae-ff608a02a8f4').
narrative_ontology:cs_reading_relation('086a7152-f100-4222-a9ae-ff608a02a8f4', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('086a7152-f100-4222-a9ae-ff608a02a8f4', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('086a7152-f100-4222-a9ae-ff608a02a8f4', foundational, particles_have_definite_positions_always).
narrative_ontology:cs_axiom_status(particles_have_definite_positions_always, holdable).
narrative_ontology:cs_axiom_grounding('086a7152-f100-4222-a9ae-ff608a02a8f4', particles_have_definite_positions_always, empirically_contingent).
narrative_ontology:cs_axiom('086a7152-f100-4222-a9ae-ff608a02a8f4', foundational, observer_is_eliminable_primitive).
narrative_ontology:cs_axiom_status(observer_is_eliminable_primitive, holdable).
narrative_ontology:cs_axiom_grounding('086a7152-f100-4222-a9ae-ff608a02a8f4', observer_is_eliminable_primitive, conventional).
narrative_ontology:cs_axiom('086a7152-f100-4222-a9ae-ff608a02a8f4', secondary, wavefunction_is_physically_real_field).
narrative_ontology:cs_axiom_status(wavefunction_is_physically_real_field, holdable).
narrative_ontology:cs_axiom_grounding('086a7152-f100-4222-a9ae-ff608a02a8f4', wavefunction_is_physically_real_field, empirically_contingent).
narrative_ontology:cs_reference_frame('086a7152-f100-4222-a9ae-ff608a02a8f4', de_broglie_1927_pilot_wave_proposal).
narrative_ontology:cs_drift_state('086a7152-f100-4222-a9ae-ff608a02a8f4', post_bell_inequality_experiments, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('086a7152-f100-4222-a9ae-ff608a02a8f4', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_programs).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, realist_philosophy_of_physics_departments).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, pilot_wave_graduate_students).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, non_standard_interpretation_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, local_beable_ontology).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, determinism_is_recoverable).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, observer_eliminability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Small but durable clusters (following Bohm, Bell, Dürr, Goldstein, Zanghì) that develop and defend the pilot-wave formalism, publish in dedicated venues, and organize conferences and textbooks around it. They gain a coherent realist ontology to work within and a research identity as the alternative to orthodoxy, but they administer the case for the reading and set its research agenda within their own community.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_programs, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_programs, agenda_setter).

% Philosophers who find pilot-wave theory's restoration of definite trajectories and eliminated observer philosophically satisfying use it as a case study against instrumentalism. They gain teaching material and a strong metaphysical position without bearing the experimental or funding costs Bohmian physicists face.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, realist_philosophy_of_physics_departments, beneficiary,
    moderate, generational, mobile, global).

% Students who commit dissertation years to Bohmian mechanics find hiring committees dominated by Copenhagen- and decoherence-trained faculty skeptical of the ontology's payoff, given its identical empirical predictions to standard quantum mechanics. Exit mid-career means abandoning years of specialized technical investment; staying means competing for a narrow set of positions that value the interpretation.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, pilot_wave_graduate_students, payer,
    powerless, biographical, trapped, national).

% Physicists working on pilot-wave extensions to field theory and relativistic generalizations bear a persistent burden of justifying the reading's value given its observational equivalence to competing interpretations; grant panels and journal referees often treat interpretation work as lower-priority than 'real physics,' making publication and funding harder to secure regardless of technical merit.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, non_standard_interpretation_researchers, payer,
    moderate, biographical, constrained, global).

% Physicists who treat interpretation as instrumentally irrelevant or who hold Copenhagen/decoherence views as institutional default rarely engage seriously with pilot-wave arguments in mainstream venues; their objections (naturalness of nonlocal guidance equations, field-theoretic scaling difficulties) are acknowledged in specialist literature but rarely surface in general physics training, where the reading is often omitted or dismissed in a paragraph.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, mainstream_quantum_foundations_community, excluded,
    institutional, generational, arbitrage, global).

% Analysts of the interpretive dispute itself, tracking which ontological commitments each reading requires and what predictive content (if any) distinguishes them; they document that pilot-wave, Copenhagen, and many-worlds are empirically equivalent at present and assess the dispute as metaphysical rather than experimental.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fully deterministic, realist ontology in which particles always have definite positions and trajectories, guided nonlocally by a physically real wavefunction/pilot wave — restoring classical-style objectivity and eliminating the need for an observer-triggered collapse postulate.
% TRANSFER_FUNCTION: Moves career and institutional legitimacy: the reading transfers prestige, funding access, and hiring viability toward proponents when it gains acceptance in a subfield, and away from students and researchers who invest in it when mainstream institutions treat it as fringe, despite its empirical equivalence to competing readings.
% ABSENT_VOICES: Working experimentalists whose funding decisions are shaped by which interpretation gets textbook and grant-committee legitimacy rarely participate directly in the interpretive debate; they inherit the consequences of the community's interpretive consensus without a seat in adjudicating it.
% DISAPPEARANCE_RATIONALE: If the pilot-wave reading vanished as a live research program, Bohmian researchers and their institutional niches would dissolve and their students would need to relocate professionally — a real rearrangement for that community. But experimental quantum mechanics and its technological applications would proceed unchanged, since all three readings of the kernel share identical empirical predictions; whether 'the world rearranges' depends on which layer (professional community vs. physical practice) is being asked about.
% FOUNDING_PROBLEM: Standard (Copenhagen) quantum mechanics left measurement outcomes and particle position as fundamentally indeterminate until observation, treating 'observation' as an unanalyzed primitive and offering no account of definite outcomes in individual experimental runs — a problem de Broglie and Bohm sought to resolve by supplying explicit trajectories guided by a real physical field.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians and philosophers of physics (e.g. work surveying the measurement problem literature) attest that the underdetermination between interpretations remains empirically live and unresolved by any known experiment to date; they are outside the Bohmian research community and do not depend on the reading's acceptance for their own standing, but they also do not corroborate that the founding problem is uniquely solved BY pilot-wave theory rather than by its rivals.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantum_formalism__pilot_wave_reading_tests).
:- end_tests(quantum_formalism__pilot_wave_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate, not low, because the reading imposes real professional costs on committed researchers (via institutional skepticism toward interpretation-heavy careers) while producing no distinguishing experimental predictions that would justify those costs on purely epistemic grounds; it rose slowly across the interval as the reading solidified into an entrenched but still-marginal subfield rather than either dying out or achieving mainstream parity. Suppression (0.55) reflects real but softening institutional resistance — pilot-wave theory is now a recognized minority position rather than a fringe one, hence the mild downward drift in suppression_requirement over the interval even as extraction ticks upward (a maturing but still-constrained research niche). Theater ratio (0.3) captures that a meaningful share of activity is genuine technical work (relativistic and field-theoretic extensions) rather than purely defensive posturing, though defensive framing (justifying the program's continued existence) is a real and growing component.
 *
 * DIRECTIONALITY LOGIC:
 *   Bohmian mechanics research programs and realist philosophy departments are declared beneficiaries: they collect intellectual and institutional capital from the reading's coherence without bearing its worst career costs (the departments especially, who can teach the material as a case study without professional risk). Graduate students and non-standard-interpretation researchers are declared victims: they are structurally locked in (trapped/constrained exit) by sunk technical investment while institutional hiring and funding norms remain oriented around competing readings, so the reading's persistence transfers real career risk onto them. The mainstream community is excluded rather than victimized — they simply do not engage, which is itself a structural fact about the field's absent voices.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabelings. First, treating the pilot-wave reading purely as Rope (harmless minority coordination mechanism with no cost structure) would erase the real professional risk borne by students and junior researchers who commit to it in a hiring environment that does not reward it — Tangled Rope requires naming that cost. Second, treating it purely as Snare (pure extraction dressed as physics) would erase its genuine coordination function: it is a technically serious, internally consistent, empirically adequate alternative ontology that solves a real conceptual problem (the measurement problem) that Copenhagen leaves unresolved. Both the coordination value and the asymmetric cost are real and coexist — hence Tangled Rope, not Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_equivalence_persistence,
    'Will pilot-wave theory, Copenhagen, and many-worlds remain permanently empirically indistinguishable, or could a future experiment (e.g. probing trajectory-dependent effects, or quantum gravity regimes) break the equivalence?',
    'Proposed experiments probing weak-measurement trajectory reconstruction, or theoretical work on quantum-gravitational regimes where interpretations may diverge in testable predictions.',
    'If a genuine empirical divergence is found, the classification shifts from a purely interpretive/institutional dispute (Tangled Rope, coordination + career-cost extraction) toward a genuinely evidential contest, potentially collapsing toward Mountain (settled empirical fact) for whichever reading wins and toward Snare for readings sustained past disconfirmation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_equivalence_persistence, empirical, 'Whether the three kernel readings are permanently or only currently empirically equivalent.').

omega_variable(
    field_theoretic_extension_viability,
    'Can the pilot-wave framework be extended to a fully relativistic, Lorentz-invariant quantum field theory without ad hoc constructions (e.g., preferred foliations), or does this remain a persistent technical obstruction unique to this reading?',
    'Track the technical literature on relativistic Bohmian field theories and assess whether proposed extensions gain acceptance as principled versus patchwork.',
    'Persistent technical obstruction would support treating continued investment in the reading as increasingly extractive relative to its coordination payoff (lower ε would need revision upward); a principled resolution would strengthen the coordination-function side of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(field_theoretic_extension_viability, empirical, 'Whether relativistic generalization is a solvable technical problem or a structural limitation of the reading.').

omega_variable(
    kernel_framing_alternative,
    'Is ''the quantum formalism'' the correct kernel-level object, or should the kernel be split further — e.g., separating ''what the wavefunction is'' (ontology) from ''what happens at measurement'' (dynamics) as two distinct contested commitments, each with its own reading set?',
    'Compare classification stability under the current single-kernel framing versus a two-kernel decomposition (ontology kernel + dynamics kernel), checking whether ε or stakeholder structure changes materially under the finer-grained split.',
    'If the finer decomposition produces materially different ε or stakeholder profiles for the ontology question versus the dynamics question, the current three-reading kernel framing under-determines the analysis and should be split per the ε-invariance principle, similar to the BGS spectral/eigenvector decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the single quantum_formalism kernel is the right level of granularity or should itself decompose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 1952, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1952, quantum_formalism__pilot_wave_reading, theater_ratio, 1952, 0.1).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__pilot_wave_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__pilot_wave_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(quan_tr_t2005, quantum_formalism__pilot_wave_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(quan_tr_t2015, quantum_formalism__pilot_wave_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(quan_tr_t2026, quantum_formalism__pilot_wave_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(quan_be_t1952, quantum_formalism__pilot_wave_reading, base_extractiveness, 1952, 0.2).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__pilot_wave_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__pilot_wave_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(quan_be_t2005, quantum_formalism__pilot_wave_reading, base_extractiveness, 2005, 0.37).
narrative_ontology:measurement(quan_be_t2015, quantum_formalism__pilot_wave_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(quan_be_t2026, quantum_formalism__pilot_wave_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1952, quantum_formalism__pilot_wave_reading, suppression_requirement, 1952, 0.65).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__pilot_wave_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(quan_su_t1990, quantum_formalism__pilot_wave_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(quan_su_t2005, quantum_formalism__pilot_wave_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(quan_su_t2015, quantum_formalism__pilot_wave_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(quan_su_t2026, quantum_formalism__pilot_wave_reading, suppression_requirement, 2026, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.1).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the quantum_formalism kernel, each a separate constraint per the ε-invariance principle: quantum_formalism__copenhagen_reading (collapse as physical/epistemic boundary), quantum_formalism__many_worlds_reading (deterministic universal wavefunction, branching), and this one (pilot_wave_reading: deterministic hidden-variable trajectories, real guiding field). All three currently share identical empirical predictions, so their ε values differ not on observational grounds but on the differing career/institutional cost structures each reading's research community bears within the broader physics establishment. Network edges register that shifts in institutional legitimacy for one reading (e.g. increased textbook coverage of Bohmian mechanics) structurally affect resource competition for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
