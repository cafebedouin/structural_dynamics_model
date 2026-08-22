% ============================================================================
% CONSTRAINT STORY: quantum_formalism__copenhagen_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Copenhagen Reading of Quantum Measurement — Collapse as Ontological Boundary
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This story instantiates the Copenhagen reading of the quantum-formalism
 *   kernel: the claim that wavefunction collapse is a physical process
 *   marking an absolute epistemic boundary, that measurement is ontologically
 *   primitive and the observer role non-eliminable, and that determinism is
 *   genuinely abandoned at measurement events (not merely apparent, as
 *   many-worlds holds, and not restored by hidden variables, as pilot-wave
 *   holds). The underlying mathematical formalism — unitary evolution between
 *   measurements, Born-rule probabilities at measurement — is common ground
 *   across all three readings. What is specific to THIS reading is the
 *   additional ontological commitment that collapse is real, physical, and
 *   irreducible, and that this commitment has become the institutionally
 *   default teaching position without being flagged as one interpretation
 *   among several live, empirically indistinguishable alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__copenhagen_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__copenhagen_reading, 0.38).
domain_priors:theater_ratio(quantum_formalism__copenhagen_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quantum_formalism__copenhagen_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__copenhagen_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__copenhagen_reading, "Copenhagen Reading of Quantum Measurement — Collapse as Ontological Boundary").
narrative_ontology:topic_domain(quantum_formalism__copenhagen_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__copenhagen_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__copenhagen_reading, 'fb7594a3-91b9-4c19-b357-57d41bcd2ead').
narrative_ontology:cs_kernel_codification('fb7594a3-91b9-4c19-b357-57d41bcd2ead', distributed).
narrative_ontology:cs_authority_grounding('fb7594a3-91b9-4c19-b357-57d41bcd2ead', practice).
narrative_ontology:cs_interpretation_layer_present('fb7594a3-91b9-4c19-b357-57d41bcd2ead').
narrative_ontology:cs_reading_relation('fb7594a3-91b9-4c19-b357-57d41bcd2ead', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_reading_relation('fb7594a3-91b9-4c19-b357-57d41bcd2ead', quantum_formalism__pilot_wave_reading, forecloses).
narrative_ontology:cs_axiom('fb7594a3-91b9-4c19-b357-57d41bcd2ead', foundational, collapse_is_physically_real).
narrative_ontology:cs_axiom_status(collapse_is_physically_real, holdable).
narrative_ontology:cs_axiom_grounding('fb7594a3-91b9-4c19-b357-57d41bcd2ead', collapse_is_physically_real, empirically_contingent).
narrative_ontology:cs_axiom('fb7594a3-91b9-4c19-b357-57d41bcd2ead', foundational, observer_measurement_role_non_eliminable).
narrative_ontology:cs_axiom_status(observer_measurement_role_non_eliminable, holdable).
narrative_ontology:cs_axiom_grounding('fb7594a3-91b9-4c19-b357-57d41bcd2ead', observer_measurement_role_non_eliminable, conventional).
narrative_ontology:cs_reference_frame('fb7594a3-91b9-4c19-b357-57d41bcd2ead', copenhagen_solvay_consensus_1927).
narrative_ontology:cs_drift_state('fb7594a3-91b9-4c19-b357-57d41bcd2ead', post_decoherence_theory_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb7594a3-91b9-4c19-b357-57d41bcd2ead', '').
narrative_ontology:cs_kernel_id(quantum_formalism__copenhagen_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, textbook_publishers_and_curriculum_authors).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, operational_physics_pedagogy_establishment).
narrative_ontology:constraint_beneficiary(quantum_formalism__copenhagen_reading, instrumentalist_working_physicists).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, realist_foundations_researchers).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, graduate_students_seeking_ontological_clarity).
narrative_ontology:constraint_victim(quantum_formalism__copenhagen_reading, alternative_interpretation_research_programs).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, measurement_as_primitive_category).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, observer_role_non_eliminable).
narrative_ontology:constraint_vindicates(quantum_formalism__copenhagen_reading, irreducible_indeterminism_at_collapse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and revise the standard quantum mechanics curriculum, in which the Copenhagen reading (or an unlabeled hybrid presented as 'the' interpretation) is taught as the default account, often without flagging it as one interpretation among several. Benefits from the pedagogical simplicity of treating collapse as settled physics rather than an open foundational question, since alternative accounts (many-worlds, pilot-wave) require substantially more setup to teach.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, textbook_publishers_and_curriculum_authors, beneficiary,
    institutional, generational, arbitrage, global).

% Sets qualifying-exam content, journal referee norms in mainstream (non-foundations) physics venues, and hiring emphasis toward 'shut up and calculate' competence. Administers which interpretive commitments are treated as settled background versus live research questions. Can revise this at will but bears none of the cost of graduate confusion or foreclosed research programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, operational_physics_pedagogy_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Use quantum mechanics as a predictive calculus without needing to resolve what collapse 'really is.' The Copenhagen framing — treating collapse as a physical/epistemic boundary rather than an artifact of incomplete theory — lets them bracket the ontological question entirely and get on with calculation. They can exit into any interpretation or none without professional cost.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, instrumentalist_working_physicists, beneficiary,
    organized, biographical, mobile, global).

% Work on realist alternatives (pilot-wave, objective collapse models, or their own reconstructions) that treat the wavefunction as physically real without an ontologically primitive 'measurement' event. They must fight an uphill battle for journal space, funding, and hiring committees that treat the Copenhagen boundary-claim as either settled or a closed non-question; exit into mainstream instrumentalist physics is available but abandons their research program.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, realist_foundations_researchers, payer,
    moderate, biographical, constrained, global).

% Are taught collapse as a physical postulate with an irreducible observer role, often without being told this is one of several live, mutually incompatible research programs. Cannot easily interrogate the claim without risking their advisor relationship or standing in a department that has settled on the standard presentation; largely trapped within whatever their program's default framing is until much later in their career, if ever.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, graduate_students_seeking_ontological_clarity, payer,
    powerless, biographical, trapped, national).

% Many-worlds and pilot-wave research communities exist but are structurally minority positions in hiring, funding, and default curriculum; they are not absent from the conversation but are treated as the burden-bearing challengers to a presumptively settled boundary claim rather than as co-equal live readings of the same formalism.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, alternative_interpretation_research_programs, excluded,
    moderate, generational, constrained, global).

% Analyze the interpretive landscape from outside any single research program's stakes, documenting that the formalism itself (unitary evolution + Born rule) is common ground and that 'collapse as physical process' is an additional interpretive commitment, not a theorem.
narrative_ontology:constraint_stakeholder(quantum_formalism__copenhagen_reading, philosophers_of_physics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__copenhagen_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__copenhagen_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides working physicists a stable, teachable, calculation-ready story that lets prediction and experiment proceed without resolving the measurement problem — a genuine coordination function for a discipline that must train thousands of practitioners who will never need to resolve foundational disputes.
% TRANSFER_FUNCTION: Moves interpretive authority and curricular real estate from realist/alternative research programs to the instrumentalist mainstream; moves epistemic confidence from graduate students (who receive an under-labeled default) to the institutions that benefit from not having to teach interpretive pluralism as a first-class topic.
% ABSENT_VOICES: Many-worlds and pilot-wave researchers are present in the literature but structurally treated as challengers to a presumed settled boundary rather than as holding an equally well-supported reading of the identical formalism; graduate students are the most absent voice, since they are rarely told the boundary claim is contested at all.
% DISAPPEARANCE_RATIONALE: If the Copenhagen reading's institutional dominance vanished overnight, the underlying mathematical formalism (Hilbert space, unitary evolution, Born rule) would be completely unchanged — nothing about experimental predictions rearranges. What would rearrange is curriculum design, hiring emphasis in foundations research, and public science communication, which currently treat 'collapse is real and observers are special' as background fact rather than interpretive choice. Instrumentalist physicists dispute that anything of substance would change; foundations researchers argue the entire research funding landscape would reorganize.
% FOUNDING_PROBLEM: In the 1920s-30s, physicists needed a working operational account of measurement that let them use the new quantum formalism to make predictions without waiting for a resolution to deep ontological puzzles (EPR, Schrödinger's cat) that showed no sign of near-term resolution.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science (outside both the Copenhagen-aligned and rival interpretive communities) corroborate that the original founding problem — providing a usable calculational recipe amid unresolved foundational disputes — was largely solved by the 1950s-60s once quantum electrodynamics and later quantum field theory demonstrated the formalism's predictive power independent of interpretation. Foundations researchers and philosophers of physics (e.g. in peer-reviewed foundations-of-physics literature) attest that the ontological boundary claim itself was never independently vindicated and instead calcified into default pedagogy; instrumentalist physicists dispute that this matters for physics practice.
narrative_ontology:disappearance_verdict(quantum_formalism__copenhagen_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__copenhagen_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__copenhagen_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__copenhagen_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__copenhagen_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42) and rises slowly over the interval: the coordination function (a usable, teachable calculational recipe) is genuine and largely stable, but the extractive component — treating a contested ontological commitment as settled background in curriculum and hiring — has hardened gradually as post-WWII physics pedagogy scaled and calcified around the standard presentation. Suppression (0.38) tracks the institutional cost of dissent: not coercive in the classic sense, but real in terms of funding, hiring, and curricular exposure for alternative-interpretation researchers and under-informed students. Theater ratio (0.22) is moderate-low: most of the activity is genuine predictive physics; a growing minority is pedagogical and rhetorical defense of the boundary claim as though it were empirically established rather than interpretively chosen.
 *
 * PERSPECTIVAL GAP:
 *   From the instrumentalist/pedagogical seat, the Copenhagen reading is a rope: a genuine, low-cost coordination solution to a real problem (how do you teach and use quantum mechanics without waiting out an unresolved metaphysical dispute). From the realist-foundations seat, the same institutional arrangement is a tangled rope at minimum: the coordination function is real, but it rides on an unacknowledged extraction of curricular space, funding priority, and the presumption of settledness from equally viable rival readings. The engine should compute these divergently from the same structural data — the claim (tangled_rope) reflects the authoring seat's judgment that the coordination story is genuine but not exhaustive of what the constraint does.
 *
 * DIRECTIONALITY LOGIC:
 *   Instrumentalist physicists and the pedagogy establishment are structural beneficiaries: the boundary claim lets them avoid costly foundational disputes while retaining full predictive power, at essentially zero cost to their own research programs. Realist foundations researchers and graduate students are structural targets: the former pay in career and funding terms for pursuing an alternative reading of the identical formalism; the latter pay in epistemic terms, inheriting an under-labeled default they must later un-learn if they enter foundations work at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a usable calculational recipe under conditions of foundational uncertainty) was substantially solved by the mid-20th century once QED and quantum field theory demonstrated the formalism's predictive autonomy from interpretation. What persists past that resolution is the specific additional claim that collapse is physically real and the observer is ontologically special — a claim that outlived the narrow problem it was recruited to solve (giving physicists something to say about measurement) and became naturalized as 'the' interpretation rather than remaining visibly one candidate among several.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_ontological_status_ambiguity,
    'Is wavefunction collapse a genuine physical process marking an irreducible epistemic/ontological boundary, or is the ''collapse as real event'' claim an interpretive addition to the formalism that has been mistaken for an experimentally established feature of nature?',
    'No current experiment distinguishes Copenhagen collapse from many-worlds branching or pilot-wave deterministic trajectories at the level of observable statistics; resolution would require either a future experimental discriminator (e.g., macroscopic interference tests probing decoherence timescales) or philosophical argument establishing which ontological commitment is preferable on grounds other than empirical adequacy.',
    'If collapse is genuinely physical and irreducible, the Copenhagen reading''s institutional dominance reflects correct physics and the constraint is closer to a mountain (a real feature of nature, not a constructed extraction). If it is an unwarranted interpretive addition mistaken for settled fact, the constraint is a tangled rope or worse: a coordination convenience that has hardened into an unearned ontological monopoly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collapse_ontological_status_ambiguity, conceptual, 'Whether collapse is a discovered physical boundary or an interpretive commitment mistaken for one.').

omega_variable(
    kernel_reading_committer_structure,
    'Given that the quantum formalism kernel admits at least three empirically adequate, mutually incompatible readings (Copenhagen, many-worlds, pilot-wave), where exactly is disagreement located, and what would each sibling reading change structurally?',
    'Documented here rather than folded into this constraint''s classification, per the ε-invariance and kernel-reading discipline: the many_worlds_reading sibling would remove the beneficiary/victim asymmetry around ''observer specialness'' entirely (no primitive measurement category, so no institutional gatekeeping around what counts as a measurement) but introduces its own extraction structure around ontological profligacy and the preferred-basis problem. The pilot_wave_reading sibling would restore determinism and eliminate the primitive-observer claim, but shifts the contested terrain to nonlocality and relativistic compatibility. Each is a separate constraint file with its own ε.',
    'Confirms this story should NOT average or hedge across readings; the Copenhagen-specific extractiveness (0.42) and beneficiary/victim structure are particular to this reading''s specific ontological commitments and would not transfer to the sibling constraint files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Where the three kernel readings structurally diverge and why they are separate constraints, not one hedged constraint.').

omega_variable(
    pedagogical_necessity_vs_extraction_boundary,
    'How much of the Copenhagen reading''s institutional dominance is explained by genuine pedagogical necessity (you have to teach something, and Copenhagen is historically first and simplest to state) versus path-dependent institutional capture that now actively disadvantages equally valid alternatives?',
    'Comparative study of physics curricula that DO teach interpretive pluralism as a first-class topic (a growing minority) versus those that present Copenhagen as default, tracking downstream research program choice and self-reported epistemic clarity among graduates of each.',
    'If pluralistic curricula produce equally competent instrumentalist physicists with better-calibrated beliefs about what is settled versus contested, this would support the tangled-rope reading (the current default is unnecessary extraction, not necessary coordination cost). If pluralistic curricula are less effective at producing working physicists, this would support a rope reading closer to genuine unavoidable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_necessity_vs_extraction_boundary, empirical, 'Whether Copenhagen''s curricular dominance is necessary pedagogical coordination or avoidable extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__copenhagen_reading, 1927, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1927, quantum_formalism__copenhagen_reading, theater_ratio, 1927, 0.05).
narrative_ontology:measurement(quan_tr_t1950, quantum_formalism__copenhagen_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(quan_tr_t1970, quantum_formalism__copenhagen_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(quan_tr_t1990, quantum_formalism__copenhagen_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__copenhagen_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__copenhagen_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(quan_be_t1927, quantum_formalism__copenhagen_reading, base_extractiveness, 1927, 0.2).
narrative_ontology:measurement(quan_be_t1950, quantum_formalism__copenhagen_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(quan_be_t1970, quantum_formalism__copenhagen_reading, base_extractiveness, 1970, 0.33).
narrative_ontology:measurement(quan_be_t1990, quantum_formalism__copenhagen_reading, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__copenhagen_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__copenhagen_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1927, quantum_formalism__copenhagen_reading, suppression_requirement, 1927, 0.15).
narrative_ontology:measurement(quan_su_t1950, quantum_formalism__copenhagen_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(quan_su_t1970, quantum_formalism__copenhagen_reading, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(quan_su_t1990, quantum_formalism__copenhagen_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__copenhagen_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__copenhagen_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__copenhagen_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__copenhagen_reading, 0.08).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__many_worlds_reading).
narrative_ontology:affects_constraint(quantum_formalism__copenhagen_reading, quantum_formalism__pilot_wave_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quantum_formalism kernel. The kernel itself (Hilbert-space formalism, unitary evolution, Born rule) is common ground and is not separately instantiated as a constraint — each reading adds a distinct, mutually incompatible ontological commitment on top of the shared formalism. copenhagen_reading adds real/primitive collapse and observer-nonelimination; many_worlds_reading removes collapse entirely in favor of deterministic branching; pilot_wave_reading adds deterministic hidden-variable trajectories and removes the primitive-measurement category. Each reading's ε, beneficiary/victim structure, and classification are authored independently and must not be averaged or hedged across files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
