% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Pilot Wave (de Broglie-Bohm) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This constraint isolates the pilot-wave (de Broglie-Bohm) reading of the
 *   quantum formalism kernel: particles possess definite positions at all
 *   times, guided nonlocally by a physically real wavefunction field, and
 *   measurement outcomes reveal pre-existing values rather than manifesting
 *   an ontologically special collapse or branching event. This is a distinct
 *   constraint from the Copenhagen reading (which asserts collapse as a
 *   physical process and an epistemic boundary) and from the many-worlds
 *   reading (which asserts universal deterministic evolution with branching)
 *   — the three share zero ε and cannot be averaged or merged; each is
 *   authored as its own story per the ε-invariance principle. The pilot-wave
 *   reading is empirically equivalent to both siblings for all known
 *   experiments (this is a structural fact about the formalism, not a
 *   contested empirical claim), so its distinctiveness is entirely
 *   interpretive/ontological, which is precisely what makes the tangled-rope
 *   classification interesting: genuine technical coordination (a complete,
 *   self-consistent alternative formalism) bundled with real institutional
 *   cost imposed on those who commit to the minority reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.28).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.58).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot Wave (de Broglie-Bohm) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, 'e64bb4d8-4927-446e-972e-b30aa20b35e8').
narrative_ontology:cs_kernel_codification('e64bb4d8-4927-446e-972e-b30aa20b35e8', formalized).
narrative_ontology:cs_authority_grounding('e64bb4d8-4927-446e-972e-b30aa20b35e8', expertise).
narrative_ontology:cs_interpretation_layer_present('e64bb4d8-4927-446e-972e-b30aa20b35e8').
narrative_ontology:cs_reading_relation('e64bb4d8-4927-446e-972e-b30aa20b35e8', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('e64bb4d8-4927-446e-972e-b30aa20b35e8', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_axiom('e64bb4d8-4927-446e-972e-b30aa20b35e8', foundational, particles_possess_definite_positions_always).
narrative_ontology:cs_axiom_status(particles_possess_definite_positions_always, holdable).
narrative_ontology:cs_axiom_grounding('e64bb4d8-4927-446e-972e-b30aa20b35e8', particles_possess_definite_positions_always, empirically_contingent).
narrative_ontology:cs_axiom('e64bb4d8-4927-446e-972e-b30aa20b35e8', foundational, wavefunction_is_physically_real_field).
narrative_ontology:cs_axiom_status(wavefunction_is_physically_real_field, holdable).
narrative_ontology:cs_axiom_grounding('e64bb4d8-4927-446e-972e-b30aa20b35e8', wavefunction_is_physically_real_field, empirically_contingent).
narrative_ontology:cs_axiom('e64bb4d8-4927-446e-972e-b30aa20b35e8', secondary, observer_has_no_distinguished_physical_role).
narrative_ontology:cs_axiom_status(observer_has_no_distinguished_physical_role, holdable).
narrative_ontology:cs_axiom_grounding('e64bb4d8-4927-446e-972e-b30aa20b35e8', observer_has_no_distinguished_physical_role, instrumental).
narrative_ontology:cs_reference_frame('e64bb4d8-4927-446e-972e-b30aa20b35e8', de_broglie_1927_pilot_wave_proposal).
narrative_ontology:cs_drift_state('e64bb4d8-4927-446e-972e-b30aa20b35e8', post_bell_theorem_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e64bb4d8-4927-446e-972e-b30aa20b35e8', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_program).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, realist_philosophers_of_physics).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, graduate_students_in_orthodox_programs).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, bohmian_specialty_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_specialty_researchers).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, local_beables_ontology).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, determinism_is_recoverable).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, measurement_problem_is_dissolvable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small but durable community of physicists and philosophers (following Bohm, Bell, Durr, Goldstein, Zanghi) maintains and extends the pilot-wave formalism, publishes journals and monographs, runs occasional conferences, and trains the next generation of specialists. They administer the reading's technical apparatus and set its research agenda, arguing it resolves the measurement problem by restoring particle trajectories and eliminating the observer as a primitive.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_program, agenda_setter,
    organized, generational, constrained, global).

% Philosophers seeking a scientifically respectable realist ontology for quantum mechanics find in pilot-wave theory a worked example that definite particle trajectories and objective dynamics are formally consistent with all known predictions. They cite it as proof-of-concept against claims that quantum mechanics forces indeterminism or anti-realism, without needing to do the underlying physics themselves.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, realist_philosophers_of_physics, beneficiary,
    moderate, biographical, mobile, global).

% Physicists who commit their careers to Bohmian mechanics gain a coherent research program but pay a structural cost: mainstream hiring committees, grant panels, and top journals treat pilot-wave work as a minority interpretive position rather than mainstream physics, narrowing publication venues, funding lines, and job market legibility. Leaving the specialty means abandoning years of technical investment; staying means accepting reduced institutional standing.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_specialty_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_specialty_researchers, beneficiary).

% Students trained in Copenhagen-flavored or operationalist curricula are rarely taught pilot-wave mechanics as a live alternative; when they do encounter it, it is often through advisors who treat it as a curiosity or a distraction from tractable, fundable research problems. They bear the cost of a narrowed interpretive education without having chosen it, and raising the topic seriously can read as a career-risking move within their own programs.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, graduate_students_in_orthodox_programs, payer,
    powerless, immediate, trapped, national).

% Researchers who treat the wavefunction operationally (as a computational tool for predicting measurement statistics, agnostic about hidden variables) are not part of the pilot-wave community's internal disputes and see the ontological debate as largely irrelevant to their empirical and technological work. Their instrumentalist framing is rarely engaged directly by pilot-wave advocates, who target the collapse postulate and the many-worlds branching structure instead.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, orthodox_quantum_information_theorists, excluded,
    powerful, biographical, arbitrage, global).

% Departments and journals adjudicating interpretive disputes host the argument between readings, publish comparative analyses, and can shift the reading's institutional legitimacy through peer review, hiring, and curriculum decisions without being party to the underlying physics stakes.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophy_of_science_departments, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mathematically complete, empirically equivalent alternative formalism in which measurement outcomes are explained by pre-existing particle positions guided by a real physical wavefunction, coordinating a research community around a shared technical toolkit (guiding equation, quantum potential, Bohmian trajectories) that recovers all of standard quantum mechanics' statistical predictions without postulating collapse or branching.
% TRANSFER_FUNCTION: Moves institutional credibility, funding legibility, and career capital away from researchers and students who adopt or take seriously the minority ontological reading, and toward the orthodox (Copenhagen/instrumentalist and many-worlds) mainstream that controls hiring committees, textbook selection, and grant panels — while moving philosophical credibility toward realist philosophers who can point to a working deterministic model.
% ABSENT_VOICES: Graduate students choosing interpretive positions rarely have a seat in curriculum design; experimentalists whose day-to-day work is interpretation-agnostic are not consulted on whether the ontological dispute matters for their funding or hiring, yet dispute outcomes shape the perceived seriousness of interpretive physics as a subfield they may be adjacent to.
% DISAPPEARANCE_RATIONALE: If the pilot-wave reading vanished from the literature entirely, empirical quantum mechanics would be completely unaffected (all predictions are shared across readings) — in that sense the world is unchanged. But the small community of Bohmian researchers, the journals and conferences built around it, and the philosophical arguments against orthodox indeterminism that cite it as a counterexample would lose their anchor; realist philosophy of physics would need a different existence proof. Whether this counts as 'the world rearranges' depends on whether one is asking about physics or about the sociology of interpretation.
% FOUNDING_PROBLEM: The measurement problem: standard quantum mechanics as originally formulated gives no principled account of when or how a superposition becomes a definite outcome, and treats 'observation' as a primitive with no clear physical definition. De Broglie and Bohm sought a formulation that dissolved this by giving particles definite trajectories at all times, making measurement an ordinary physical interaction rather than a special postulated collapse.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of physics outside the Bohmian community (e.g., in general philosophy of science venues) largely agree the measurement problem is a live, unresolved question in quantum foundations — this is not disputed even by orthodox physicists, who differ instead on whether it requires a realist solution at all or whether it is a pseudo-problem generated by demanding classical-style ontology from a non-classical theory. That latter deflationary view is held by working physicists (particularly in quantum information and QBism-adjacent circles) who are not beneficiaries of the pilot-wave program and have no stake in its survival.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-low (0.28) and has crept upward slowly since the 1950s as the Bohmian program formalized into a distinct research community with its own journals, funding competitions, and hiring niches — the coordination function (a complete deterministic reformulation of QM) is real and has not degraded, but institutional gatekeeping costs on adherents have modestly increased as orthodoxy consolidated around Copenhagen-flavored and instrumentalist pedagogy in the postwar decades. Suppression is high but declining (0.75 in 1952 falling to 0.58 by 2024): early Bohmian mechanics faced near-total exclusion from mainstream physics curricula and journals (reinforced by pre-Bell-era arguments, since discredited, that hidden-variable theories were provably impossible); post-Bell and post-decoherence-theory acceptance of the technical program's consistency has softened but not eliminated this exclusion. Theater ratio is low (0.22) — the community's technical output (trajectory calculations, quantum potential analyses, extensions to field theory) is substantive research, not performative maintenance, though a slowly rising share of activity is now devoted to defending the program's legitimacy in philosophy-of-physics venues rather than extending its physics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bohmian research program itself sets the interpretive agenda for its own reading and is coded as agenda_setter rather than a pure beneficiary — it administers the technical apparatus and community structures. Realist philosophers who cite pilot-wave theory as an existence proof are beneficiaries who did not build the apparatus but draw philosophical capital from it. Specialty researchers who commit careers to the program are simultaneously beneficiaries (of a coherent research program) and payers (of reduced institutional standing) — hence the dual role. Graduate students in orthodox programs are pure payers: they bear a narrowed interpretive education they did not choose and have essentially trapped exit options within their own training pipelines. Orthodox quantum information theorists are excluded rather than opposed — their instrumentalist stance sidesteps the ontological dispute entirely, giving them high exit/arbitrage relative to the debate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the measurement problem) remains genuinely live and contested — this is not a mandate that has quietly outlived its function while machinery persists on inertia. What could mislead an observer into calling this pure extraction is the visible institutional cost imposed on adherents; what would mislead an observer into calling it pure coordination is ignoring that cost entirely. Tangled rope classification holds both: the technical coordination is real (a genuine alternative deterministic ontology consistent with all experiments) and the extraction is real (career and legitimacy costs concentrated on those who choose the minority reading), sustained by active enforcement in the form of curriculum design, hiring norms, and journal gatekeeping that could in principle be relaxed without abandoning either reading's technical content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_status_of_wavefunction,
    'Is the wavefunction a genuine physical field with causal powers (as this reading requires), or is it an epistemic/informational object with no independent physical existence — and is this question even empirically decidable given the reading''s predictive equivalence to its siblings?',
    'PBR-type theorems and their extensions attempt to constrain psi-epistemic models via measurement statistics; ongoing work in quantum foundations continues to probe whether any experimentally accessible signature could distinguish ontological commitments. No consensus resolution currently exists.',
    'If the wavefunction''s physical reality is established, the pilot-wave reading''s central ontological claim is vindicated at the expense of psi-epistemic alternatives; if shown definitionally undecidable, the entire ontological debate (and the institutional apparatus built on top of it) may be revealed as resting on a metaphysical distinction without an operational difference, which would undercut this constraint''s claimed coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_status_of_wavefunction, empirical, 'Whether wavefunction realism is empirically decidable or a purely interpretive commitment.').

omega_variable(
    kernel_reading_sibling_delta,
    'Given that this reading, copenhagen_reading, and many_worlds_reading are strictly empirically equivalent (identical predictions for all performed and currently conceivable experiments), is the institutional cost differential across readings (this reading''s higher marginal career risk) a rational response to genuine theoretical deficiencies of pilot-wave mechanics (e.g., its awkward extension to relativistic quantum field theory) or an accident of sociological path-dependence from mid-20th-century gatekeeping?',
    'Comparative history-of-science analysis of how each reading''s institutional position evolved relative to its own technical progress (e.g., tracking whether Bohmian field-theoretic extensions closed technical gaps without corresponding institutional rehabilitation) would help separate technical merit from path-dependent stigma.',
    'If sociological path-dependence dominates, the extraction measured here is closer to pure institutional inertia riding on a technically sound program (closer to a piton-flavored tangled rope); if technical deficiency dominates, the institutional cost differential is a defensible response to real theoretical shortcomings rather than gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Whether institutional cost to pilot-wave adherents tracks technical merit or historical gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 1952, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1952, quantum_formalism__pilot_wave_reading, theater_ratio, 1952, 0.1).
narrative_ontology:measurement(quan_tr_t1964, quantum_formalism__pilot_wave_reading, theater_ratio, 1964, 0.12).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__pilot_wave_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(quan_tr_t1996, quantum_formalism__pilot_wave_reading, theater_ratio, 1996, 0.18).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__pilot_wave_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(quan_tr_t2024, quantum_formalism__pilot_wave_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(quan_be_t1952, quantum_formalism__pilot_wave_reading, base_extractiveness, 1952, 0.15).
narrative_ontology:measurement(quan_be_t1964, quantum_formalism__pilot_wave_reading, base_extractiveness, 1964, 0.18).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__pilot_wave_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(quan_be_t1996, quantum_formalism__pilot_wave_reading, base_extractiveness, 1996, 0.23).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__pilot_wave_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement(quan_be_t2024, quantum_formalism__pilot_wave_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1952, quantum_formalism__pilot_wave_reading, suppression_requirement, 1952, 0.75).
narrative_ontology:measurement(quan_su_t1964, quantum_formalism__pilot_wave_reading, suppression_requirement, 1964, 0.7).
narrative_ontology:measurement(quan_su_t1980, quantum_formalism__pilot_wave_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(quan_su_t1996, quantum_formalism__pilot_wave_reading, suppression_requirement, 1996, 0.62).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__pilot_wave_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(quan_su_t2024, quantum_formalism__pilot_wave_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, information_standard).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.05).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, many_worlds_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quantum_formalism kernel, decomposed per the ε-invariance principle: copenhagen_reading (collapse as real indeterministic process), many_worlds_reading (deterministic universal branching), and pilot_wave_reading (this story: deterministic hidden variables with nonlocal guidance and definite particle positions). All three share identical empirical predictions and therefore cannot be distinguished by any measurement, but their institutional/sociological ε values differ substantially because each reading's adherent community has a different size, legitimacy, and career-cost profile within physics departments. copenhagen_reading, having been the historically dominant textbook position, carries the least adherent-level extraction; many_worlds_reading has grown substantially in analytic philosophy and some physics circles with moderate institutional cost; pilot_wave_reading remains the most institutionally marginal of the three, which is reflected in this story's comparatively higher suppression figure. The reading_relations in cs_structure mark this reading as foreclosing both siblings at the level of a single physicist's ontological commitment (one cannot simultaneously hold that particles have definite positions always AND that collapse is a real indeterministic process AND that all outcomes branch into separate worlds) even though all three coexist as live positions across the discipline as a whole.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
