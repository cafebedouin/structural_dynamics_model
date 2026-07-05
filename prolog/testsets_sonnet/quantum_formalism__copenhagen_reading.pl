% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Copenhagen Reading of Quantum Measurement: Collapse as Physical Process / Observer as Primitive
 *   domain: philosophy_of_physics
 *
 * SUMMARY:
 *   The Copenhagen reading treats wavefunction collapse as a genuine physical
 *   process and measurement as a primitive category that cannot be further
 *   reduced or explained within the formalism — the observer role is
 *   non-eliminable, and indeterminism at measurement is irreducible rather
 *   than epistemic ignorance of hidden variables. This is one reading of the
 *   quantum_formalism kernel; the many_worlds_reading and pilot_wave_reading
 *   readings are separate constraint stories with different ε profiles,
 *   different beneficiary/victim structures, and different classifications,
 *   linked here via network.affects_constraints. The label 'quantum
 *   mechanics' colloquially covers all three; this story is deliberately
 *   narrowed to the Copenhagen instantiation only, per the ε-invariance
 *   decomposition rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.28).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.32).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading of Quantum Measurement: Collapse as Physical Process / Observer as Primitive").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'db38a997-0890-4d24-9654-40ebeb0a4100').
narrative_ontology:cs_kernel_codification('db38a997-0890-4d24-9654-40ebeb0a4100', distributed).
narrative_ontology:cs_authority_grounding('db38a997-0890-4d24-9654-40ebeb0a4100', practice).
narrative_ontology:cs_interpretation_layer_present('db38a997-0890-4d24-9654-40ebeb0a4100').
narrative_ontology:cs_reading_relation('db38a997-0890-4d24-9654-40ebeb0a4100', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('db38a997-0890-4d24-9654-40ebeb0a4100', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('db38a997-0890-4d24-9654-40ebeb0a4100', foundational, measurement_is_ontologically_primitive).
narrative_ontology:cs_axiom_status(measurement_is_ontologically_primitive, holdable).
narrative_ontology:cs_axiom_grounding('db38a997-0890-4d24-9654-40ebeb0a4100', measurement_is_ontologically_primitive, conventional).
narrative_ontology:cs_axiom('db38a997-0890-4d24-9654-40ebeb0a4100', foundational, indeterminism_at_measurement_is_irreducible).
narrative_ontology:cs_axiom_status(indeterminism_at_measurement_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('db38a997-0890-4d24-9654-40ebeb0a4100', indeterminism_at_measurement_is_irreducible, empirically_contingent).
narrative_ontology:cs_reference_frame('db38a997-0890-4d24-9654-40ebeb0a4100', bohr_heisenberg_operational_consensus).
narrative_ontology:cs_drift_state('db38a997-0890-4d24-9654-40ebeb0a4100', post_decoherence_theory_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db38a997-0890-4d24-9654-40ebeb0a4100', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, copenhagen_textbook_authors).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operationalist_physics_curricula).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, instrumentalist_philosophy_departments).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, realist_foundations_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, graduate_students_taught_shut_up_and_calculate).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_as_ontological_primitive).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, irreducible_indeterminism_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and revise the standard quantum mechanics curriculum that presents collapse-on-measurement as settled physics rather than as one interpretation among several. Their pedagogical authority is reinforced each time a new cohort is taught the postulate without exposure to competing formalisms; they benefit from the reading's institutional default status without needing to defend it against alternatives in most classrooms.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, copenhagen_textbook_authors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__copenhagen_reading, copenhagen_textbook_authors, beneficiary).

% Departments and licensing exams that adopt the operationalist framing gain a clean, computationally tractable teaching path: predict measurement outcomes, skip ontological commitment. This reading's abandonment of determinism at measurement is exactly what licenses treating 'what happens between measurements' as outside the syllabus.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operationalist_physics_curricula, beneficiary,
    institutional, generational, arbitrage, global).

% Philosophy of science programs built around Copenhagen-style instrumentalism (measurement problem as pseudo-problem, or as boundary rather than gap to be filled) have a research and publishing niche that depends on treating the observer/measurement primitive as a legitimate stopping point rather than an unresolved ontological debt.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, instrumentalist_philosophy_departments, beneficiary,
    organized, generational, mobile, global).

% Physicists and philosophers pursuing many-worlds, pilot-wave, or objective-collapse programs face grant panels, hiring committees, and journal referees steeped in the Copenhagen default, who treat demands for a mechanism behind collapse as a category error rather than an open question. Exit means working in a marginalized subfield with thinner funding and fewer permanent positions.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_foundations_researchers, payer,
    moderate, biographical, constrained, global).

% Students learn the postulate of collapse as physical fact, absorb the discouragement of ontological questions ('shut up and calculate'), and carry that operational deflation into their own careers, often without ever encountering many-worlds or pilot-wave as live technical options. Their curriculum is set by others; they have no practical channel to contest the framing before their formation is complete.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, graduate_students_taught_shut_up_and_calculate, payer,
    powerless, biographical, trapped, national).

% A sibling reading of the same formal kernel, holding that the universal wavefunction evolves deterministically and collapse is only apparent branching. It is not a party inside this constraint's institutional operation but is structurally excluded from the Copenhagen classroom default; it is named here only for completeness, not as an agent of this constraint.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, many_worlds_reading, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quantum_formalism__copenhagen_reading, many_worlds_reading).

% Researchers who study the interpretive landscape itself — historians and philosophers of physics who track which reading dominates which institutions and why — without personally being beneficiaries or victims of any single reading's institutional entrenchment.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, quantum_foundations_community, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working computational recipe (Born rule + projection postulate) that lets physicists predict measurement statistics without resolving what happens to the quantum state between measurements — a genuinely useful operational shortcut for applied and experimental physics.
% TRANSFER_FUNCTION: Moves pedagogical authority, curriculum design power, and funding preference toward institutions and individuals who treat measurement-induced collapse as physically settled, and away from researchers pursuing alternative ontologies who must justify departing from the default.
% ABSENT_VOICES: Many-worlds and pilot-wave theorists are structurally absent from most standard curricula; their formalisms are typically presented, if at all, as a late 'interpretations' unit rather than as live competitors from the outset. Students who might have found determinism-preserving accounts more satisfying are never given the chance to choose.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading's institutional default status vanished overnight, the mathematics of quantum mechanics (Hilbert spaces, unitary evolution, Born rule statistics) would be completely unaffected — the world of prediction and experiment rearranges not at all. But the sociology of physics education, hiring, and funding would rearrange substantially: alternative programs would compete on equal institutional footing rather than as marginal challengers. Whether that counts as 'the world changing' depends on whether you are asking about physics or about the profession of physics.
% FOUNDING_PROBLEM: Early quantum mechanics needed a working rule to connect the abstract formalism (wavefunctions, operators) to definite experimental outcomes, since the theory as written gives superpositions but experiments give single results. The Copenhagen reading solved this by declaring measurement a primitive event that collapses the state, sidestepping the question of mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Historians of physics (outside any interpretive camp) corroborate that the founding problem — connecting formalism to definite outcomes — remains genuinely unresolved at the mechanism level; this is acknowledged even by many Copenhagen-sympathetic physicists in technical literature (e.g. decoherence theorists). But whether Copenhagen's specific answer (collapse as an ontologically primitive, non-mechanistic event) is the correct resolution or merely an institutionally convenient deferral is exactly what the sibling readings dispute; no external adjudicator has settled it.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
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
 *   Extraction is moderate (0.28) and has crept upward slowly since the 1920s as the reading calcified into default textbook orthodoxy rather than remaining one live option among several — the 'shut up and calculate' pedagogical culture is the extraction mechanism, redirecting career and funding capital toward those who accept the primitive-measurement postulate without further inquiry. Suppression (0.32) is moderate and has actually softened somewhat since the 1970s Bell-inequality and decoherence-theory renaissance reopened foundational questions institutionally, though it ticked back up slightly as quantum computing pedagogy re-entrenched operational shortcuts. Theater ratio (0.22) is low-moderate: the coordination function (a working predictive recipe) is genuinely real and does the actual computational work; the theatrical component is the residual practice of presenting collapse as settled physics rather than as an interpretive choice.
 *
 * PERSPECTIVAL GAP:
 *   From the textbook-author seat, Copenhagen is simply 'the physics' — an unremarkable transmission of settled science. From the realist-foundations-researcher seat, the same transmission is an act of institutional gatekeeping that treats a contested metaphysical choice as beyond debate. The engine should compute these as structurally different experiences of the same constraint, not adjudicate which seat is 'right' about the physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Textbook authors and operationalist curricula sit near the beneficiary end: they set the default, collect the pedagogical and institutional payoff of not having to resolve deeper ontological questions, and have mobile/arbitrage exit (they can always fall back on 'it works for predictions' if challenged). Realist foundations researchers and graduate students sit near the target end: they bear the cost of institutional marginalization or of absorbing a deflationary attitude toward their own curiosity, with constrained or trapped exit respectively — a graduate student cannot simply opt out of the curriculum that trains them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (connecting formalism to definite outcomes) was never fully solved by Copenhagen — it was named a primitive and declared out of scope. That the problem persists (contested status) while the reading's institutional dominance persists unchanged is exactly the seat of possible mandatrophy: the coordination function (a working prediction recipe) remains genuinely live, but the ontological deferral that was originally provisional pedagogy has hardened into presented-as-fact orthodoxy. Classifying this as tangled_rope rather than mountain or rope prevents mislabeling: it is not a natural law (the sibling readings are live technical competitors), and it is not pure coordination without cost (there are real, identifiable seats who pay for the entrenchment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_ontological_vs_epistemic,
    'Is wavefunction collapse a genuine physical process (Copenhagen''s claim) or merely an artifact of incomplete description that a deeper deterministic theory (many-worlds, pilot-wave) would dissolve?',
    'No decisive experimental test currently distinguishes the readings at the level of empirical predictions (by construction, all three reproduce identical Born-rule statistics); resolution would require either a novel experimental signature that discriminates between the interpretations (some proposed macroscopic-superposition tests aim at this) or a purely theoretical/philosophical argument establishing which ontological commitment is more parsimonious or explanatorily adequate.',
    'If collapse is shown to be a genuine irreducible physical process, the Copenhagen reading''s claim to institutional default status is vindicated on the merits rather than by historical inertia. If a deterministic completion is empirically or theoretically favored, the Copenhagen reading''s continued institutional dominance would look increasingly like entrenched convention rather than the best available account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_ontological_vs_epistemic, conceptual, 'Whether collapse is ontologically primitive or an artifact of an incomplete theory — the central undecided question the kernel contest turns on.').

omega_variable(
    copenhagen_pedagogical_lockin,
    'Is Copenhagen''s continued dominance in physics curricula the result of it being the best available account, or of first-mover institutional lock-in (it was formalized first, by physicists with strong personal authority — Bohr, Heisenberg — and has been reproduced by curricular inertia since)?',
    'Comparative study of curricula in institutions that have adopted many-worlds or decoherence-first pedagogical approaches from the outset (a small but growing minority), tracking whether student outcomes, research productivity, or foundational literacy differ systematically.',
    'If lock-in dominates, the tangled_rope classification (genuine coordination function riding alongside identifiable institutional extraction) is well-supported. If merit dominates, the reading''s institutional weight is closer to earned than extracted, which would push the classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copenhagen_pedagogical_lockin, empirical, 'Whether Copenhagen''s institutional dominance reflects genuine merit or first-mover lock-in effects in physics pedagogy.').

omega_variable(
    beneficiary_framing_under_determination,
    'Could the declared beneficiaries (textbook authors, operationalist curricula) be reframed instead as mere transmitters of a genuinely correct account, making the extraction reading itself a contested framing rather than a settled structural fact?',
    'Cross-check against the parallel decomposition principle applied to the sibling readings: if many_worlds_reading and pilot_wave_reading, when authored as separate stories, show comparably-structured beneficiary sets (their own advocate communities, journals, and funding niches), that symmetry would support treating all three as competing institutional programs rather than treating only Copenhagen as extractive.',
    'If the extraction framing turns out to be symmetric across all three readings (each has its own advocate beneficiaries), that supports classifying all three siblings similarly (tangled_rope-at-the-institutional-level, mountain-at-the-mathematical-formalism-level) rather than singling out Copenhagen as uniquely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_framing_under_determination, conceptual, 'Whether the beneficiary/victim asymmetry authored here is a fact about Copenhagen specifically or an artifact of which reading happens to hold the incumbent institutional position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.1).
narrative_ontology:measurement(quan_tr_t1950, quantum_formalism__copenhagen_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.17).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__copenhagen_reading, theater_ratio, 1990, 0.19).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__copenhagen_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.15).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__copenhagen_reading, base_extractiveness, 1990, 0.26).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__copenhagen_reading, base_extractiveness, 2010, 0.27).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.4).
narrative_ontology:measurement(quan_su_t1950, quantum_formalism__copenhagen_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__copenhagen_reading, suppression_requirement, 1970, 0.38).
narrative_ontology:measurement(quan_su_t1990, quantum_formalism__copenhagen_reading, suppression_requirement, 1990, 0.34).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__copenhagen_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__copenhagen_reading, suppression_requirement, 2025, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint families instantiating the quantum_formalism kernel. many_worlds_reading and pilot_wave_reading are separate files with their own ε, beneficiary/victim structure, and classification. All three share identical empirical predictions (Born-rule statistics) but diverge in ontological commitment about measurement and determinism. The underlying mathematical formalism itself (Hilbert space structure, unitary evolution) would be authored as a fourth, upstream mountain-type story if decomposed further, analogous to the BGS spectral-universality / eigenvector-thermalization split.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
