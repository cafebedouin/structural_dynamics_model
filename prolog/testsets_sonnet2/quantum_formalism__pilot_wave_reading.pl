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
 *   constraint_id: quantum_formalism__pilot_wave_reading
 *   human_readable: Pilot-Wave (de Broglie–Bohm) Reading of Quantum Formalism
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   This constraint isolates the pilot-wave (de Broglie–Bohm) reading of the
 *   quantum formalism kernel: particles possess definite positions at all
 *   times, guided nonlocally by a physically real wavefunction field, so that
 *   apparent quantum randomness is epistemic ignorance of initial conditions
 *   rather than ontological indeterminism. This reading is empirically
 *   indistinguishable from Copenhagen and many-worlds readings for all
 *   currently performable experiments — the dispute is
 *   interpretive/philosophical, not resolvable by new data under present
 *   technology. The constraint being modeled is the professional and
 *   pedagogical arrangement built around treating one interpretation
 *   (historically Copenhagen-flavored orthodoxy) as default while pilot-wave
 *   theory occupies a minority, resource-constrained niche; ε is authored for
 *   that standing arrangement (interpretive marginalization plus in-group
 *   professional apparatus), not for a hypothetical world where pilot-wave
 *   theory was adopted as standard.
 *
 * KEY AGENTS:
 *   - bohmian_mechanics_research_community: primary beneficiary of the reading's internal apparatus (organized/constrained) — sustains the tradition, sets its internal standards
 *   - realist_philosophy_of_physics_programs: secondary beneficiary (moderate/constrained) — uses the reading as a realism case study
 *   - early_career_researchers_in_minority_interpretation: primary payer (powerless/trapped) — bears career-market cost of commitment to a minority program
 *   - students_taught_a_single_orthodox_interpretation: primary payer (powerless/trapped) — bears cost of a foreclosed interpretive landscape via pedagogy
 *   - experimentalists_testing_bell_and_contextuality: analytical observer (institutional/analytical) — produces data that underdetermines the interpretive choice
 *   - science_communicators_and_textbook_authors: excluded voice (moderate/constrained) — transmits orthodoxy without registering the contest
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
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave (de Broglie–Bohm) Reading of Quantum Formalism").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, '8f3689cd-d6ca-47f8-ade7-1a62eacaf32b').
narrative_ontology:cs_kernel_codification('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', distributed).
narrative_ontology:cs_authority_grounding('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', practice).
narrative_ontology:cs_interpretation_layer_present('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b').
narrative_ontology:cs_reading_relation('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', quantum_formalism__copenhagen_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', quantum_formalism__many_worlds_reading, coexists_with).
narrative_ontology:cs_axiom('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', foundational, particles_have_definite_positions_at_all_times).
narrative_ontology:cs_axiom_status(particles_have_definite_positions_at_all_times, holdable).
narrative_ontology:cs_axiom_grounding('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', particles_have_definite_positions_at_all_times, empirically_contingent).
narrative_ontology:cs_axiom('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', foundational, wavefunction_is_physically_real_guiding_field).
narrative_ontology:cs_axiom_status(wavefunction_is_physically_real_guiding_field, holdable).
narrative_ontology:cs_axiom_grounding('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', wavefunction_is_physically_real_guiding_field, conventional).
narrative_ontology:cs_axiom('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', secondary, quantum_randomness_is_epistemic_not_ontological).
narrative_ontology:cs_axiom_status(quantum_randomness_is_epistemic_not_ontological, holdable).
narrative_ontology:cs_axiom_grounding('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', quantum_randomness_is_epistemic_not_ontological, empirically_contingent).
narrative_ontology:cs_reference_frame('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', de_broglie_1927_pilot_wave_proposal).
narrative_ontology:cs_drift_state('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', post_bell_theorem_and_decoherence_program_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8f3689cd-d6ca-47f8-ade7-1a62eacaf32b', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_community).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, realist_philosophy_of_physics_programs).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, early_career_researchers_in_minority_interpretation).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, students_taught_a_single_orthodox_interpretation).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, scientific_realism).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, determinism_is_recoverable).
narrative_ontology:constraint_vindicates(quantum_formalism__pilot_wave_reading, measurement_problem_is_dissolvable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and extends the pilot-wave formalism, publishes in dedicated venues, organizes conferences and edited volumes defending the interpretation's coherence and empirical equivalence with orthodox quantum mechanics. Their institutional standing, grant narratives, and philosophical arguments (retaining realism and determinism) depend on the reading's continued defensibility. They administer the interpretive tradition's internal standards for what counts as a legitimate extension (e.g., relativistic generalizations, field-theoretic Bohmian mechanics).
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_community, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_mechanics_research_community, agenda_setter).

% Philosophy departments and philosophy-of-science programs that use the pilot-wave reading as their central case for defending scientific realism against instrumentalist and anti-realist readings of quantum mechanics. They gain teaching material, publishable arguments, and a functioning counter-example to claims that quantum mechanics forces indeterminism or observer-dependence.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, realist_philosophy_of_physics_programs, beneficiary,
    moderate, generational, constrained, global).

% Graduate students and postdocs who commit to Bohmian mechanics as a research program face a much narrower hiring market, fewer grant panels sympathetic to the framework, and reviewers in mainstream physics venues who treat the interpretation as fringe or philosophically interesting but professionally risky. Switching interpretive commitments mid-career carries real reputational and publication costs; many are effectively locked into a minority tradition once their dissertation is written.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, early_career_researchers_in_minority_interpretation, payer,
    powerless, biographical, trapped, national).

% Physics undergraduates and early graduate students are almost universally taught the Copenhagen (or a decoherence-flavored) reading as if it were the uncontested content of quantum mechanics, with pilot-wave theory mentioned, if at all, as a historical curiosity. They bear the cost of a foreclosed interpretive landscape: they cannot easily discover that the choice between readings is live, and by the time they might investigate alternatives their conceptual habits and career incentives are already set by the orthodox framing.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, students_taught_a_single_orthodox_interpretation, payer,
    powerless, immediate, trapped, global).

% Run Bell-inequality violation experiments, weak-measurement trajectory reconstructions, and contextuality tests. Their empirical results constrain all three kernel readings equally (all reproduce the same statistical predictions) but are frequently cited selectively by each interpretive camp as supporting evidence, even though the experiments do not adjudicate between empirically equivalent interpretations.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, experimentalists_testing_bell_and_contextuality, observer,
    institutional, generational, analytical, global).

% Write popular and textbook accounts of quantum mechanics but are rarely specialists in foundations; they default to whichever reading their own training emphasized (usually Copenhagen), reproducing the orthodox framing to a mass audience without registering that they are transmitting one contested reading among several as settled fact.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, science_communicators_and_textbook_authors, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__pilot_wave_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mathematically precise, fully deterministic completion of quantum mechanics that recovers a classical-style ontology (particles with definite trajectories at all times), resolving the measurement problem by denying that measurement is special: outcomes are revealed, not created. This coordinates a research and pedagogical community around a shared realist ontology and a shared technical apparatus (the guidance equation).
% TRANSFER_FUNCTION: Moves professional legitimacy, hiring consideration, grant funding, and pedagogical airtime toward researchers, institutions, and textbooks that treat quantum interpretation as settled in the orthodox (typically Copenhagen-flavored) direction, and away from those committed to the pilot-wave program — while the pilot-wave community's own internal apparatus moves similar goods toward its adherents at smaller scale within its own niche.
% ABSENT_VOICES: Physics students are never told, in most curricula, that interpretation is a live philosophical dispute rather than settled physics; they would object to being taught one reading as fact if they knew the alternatives were empirically equivalent. Early-career Bohmians would object to the professional penalty attached to their research program if surveyed candidly, but rarely do so publicly for fear of compounding the career cost.
% DISAPPEARANCE_RATIONALE: If the pilot-wave reading vanished as a live research program, the empirical content of quantum mechanics would be entirely unchanged (predictions are identical across readings) — in that sense the world is unchanged. But the small community organized around it, the philosophy-of-physics teaching built on it, and the specific realist arguments it supplies would disappear, and the interpretive landscape taught to students would narrow further toward orthodoxy. Whether this counts as 'the world rearranges' depends on whether one counts the sociology of physics as part of the relevant world — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The founding problem was the measurement problem and the apparent need for an irreducible, unexplained collapse or observer-dependence in orthodox quantum mechanics; de Broglie and Bohm sought to restore a mechanistic, observer-independent, deterministic ontology in which quantum randomness is epistemic (arising from ignorance of initial particle positions) rather than ontological.
% FOUNDING_PROBLEM_CORROBORATION: Working Bohmians and sympathetic philosophers of physics (e.g., in the foundations-of-physics literature outside the immediate Bohmian community, such as historians of the EPR/Bell debates) attest the measurement problem remains genuinely unresolved by orthodox accounts and that pilot-wave theory is a serious, live solution, not merely a historical curiosity — this corroboration comes from philosophers and physicists who are not themselves committed pilot-wave researchers. Mainstream physicists outside foundations largely regard the problem as practically dissolved by decoherence for laboratory purposes, regardless of interpretation, which is itself an outside-the-community judgment cutting the other way.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, contested).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) and rises slowly across the interval: the pilot-wave tradition itself is a fairly small, low-extraction community (it mostly just sustains its own journals and conferences), but the surrounding professional apparatus — hiring committees, grant panels, textbook selection — increasingly treats the empirically-equivalent interpretive choice as settled, which imposes real if diffuse costs on researchers and students who might otherwise engage the alternative. Suppression is moderate-high (0.55) and reflects both formal barriers (few faculty lines explicitly for foundations-of-physics work, journal gatekeeping) and internalized ones (students never learn the choice is live, so they do not resist a constraint they cannot see). Theater ratio (0.3) captures that some defense of pilot-wave theory's coherence is genuine technical work (relativistic extensions, quantum field theoretic generalizations) while some is more performative signaling within a small community defending its own legitimacy. Accessibility collapse is moderate (0.4) rather than mountain-level: interpretive alternatives are not physically inaccessible, just professionally and pedagogically hard to reach. Resistance is fairly high (0.7) because philosophers of physics and a persistent minority of physicists actively contest the orthodox framing in print, conferences, and curriculum reform efforts — this is a live, actively fought dispute, not a settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bohmian research community and realist philosophy-of-physics programs are declared beneficiaries: the reading's continued defensibility is the good that sustains their research programs, teaching material, and professional identity, so they sit toward the beneficiary end of directionality despite being a minority relative to orthodox physics as a whole (this is a beneficiary relationship internal to the reading, not a claim that Bohmians dominate physics globally). Early-career minority-interpretation researchers and orthodoxy-taught students are declared victims: the former pay a direct career-market cost for choosing the minority reading, the latter pay a diffuse epistemic cost (a foreclosed sense of live scientific dispute) they cannot easily detect or resist because they are never shown the alternative. Experimentalists are observers: their data equally underdetermines all three kernel readings, so they do not accrue directional benefit or cost from this specific interpretive contest, only from the broader empirical program.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (restoring determinism and a mechanistic, observer-independent ontology against Copenhagen's unexplained collapse) remains philosophically contested rather than either clearly live or clearly dead — mainstream working physicists largely treat the measurement problem as practically dissolved by decoherence for calculational purposes, while foundations specialists (including non-Bohmian ones) regard it as a genuine open problem. This is why founding_problem_status is 'contested' rather than declared dead: declaring it dead would be a beneficiary-friendly move that dismisses the tradition's own live rationale, while declaring it unambiguously live without corroboration outside the Bohmian community would over-credit self-interested testimony. Classifying this as tangled_rope rather than pure snare or pure rope reflects that genuine coordination exists (a coherent, technically serious realist research program with real explanatory ambitions) alongside a real asymmetric cost (professional marginalization borne by researchers and pedagogical foreclosure borne by students) sustained by active institutional gatekeeping (hiring, funding, curriculum) rather than by the interpretation's own empirical weakness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_underdetermination_permanence,
    'Is the empirical equivalence between pilot-wave, Copenhagen, and many-worlds readings a permanent feature of any possible quantum theory, or could future experiments (e.g., probing sub-quantum structure, weak measurement trajectories, or quantum gravity regimes) eventually distinguish them?',
    'Track proposed experimental programs (e.g., Bohmian trajectory reconstruction via weak measurement, tests of nonlocal signaling bounds, or quantum-gravitational modifications that might break the equivalence) and whether any produces a discriminating result.',
    'If the readings are permanently empirically equivalent, the dispute is irreducibly philosophical and the professional marginalization of minority readings is harder to justify on scientific grounds alone. If a discriminating experiment becomes possible, the constraint''s classification could shift sharply once one reading gains genuine empirical support over the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_underdetermination_permanence, empirical, 'Whether the three kernel readings'' empirical equivalence is permanent or could eventually be broken by future physics.').

omega_variable(
    committer_structure_interpretive_choice_location,
    'Where is the actual interpretive disagreement located: is it about the ontology of the wavefunction (real field vs. bookkeeping device), the status of measurement (special event vs. ordinary interaction), or the nature of probability (epistemic ignorance vs. ontological chance vs. self-locating uncertainty across branches)? Different framings of ''the'' disagreement would group the readings differently.',
    'A conceptual analysis distinguishing the three candidate loci of disagreement (wavefunction ontology, measurement status, probability interpretation) and mapping which pairs of readings agree or disagree on each axis independently, rather than treating ''quantum interpretation'' as a single undifferentiated dispute.',
    'If wavefunction realism is the true axis, pilot-wave and many-worlds are closer allies (both treat the wavefunction as real) against Copenhagen''s more instrumentalist collapse postulate — which would change which readings ''coexist_with'' vs. exert ''influences'' pressure on which others, and could reshape how the constraint family''s network edges should be drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_interpretive_choice_location, conceptual, 'Alternative framings of what exactly divides the three kernel readings, and how the choice of framing changes the relational structure between them.').

omega_variable(
    professional_marginalization_causal_mechanism,
    'Is the professional marginalization of pilot-wave researchers driven primarily by genuine assessment of the program''s technical fruitfulness (e.g., difficulty of relativistic generalization), by sociological path-dependence from historical dominance of the Copenhagen framing in mid-20th-century physics pedagogy, or by both in some proportion?',
    'Historical and sociological analysis of hiring, funding, and citation patterns in foundations of physics compared against independent assessments (by non-aligned philosophers of science) of the technical maturity of pilot-wave extensions to quantum field theory.',
    'If path-dependence dominates, the suppression metric should be read as largely historically contingent and potentially reducible through deliberate pedagogical reform; if technical fruitfulness dominates, the marginalization has a stronger claim to tracking genuine scientific merit rather than pure institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professional_marginalization_causal_mechanism, empirical, 'Whether pilot-wave marginalization tracks technical merit, historical path-dependence, or a mix of both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 1952, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t1952, quantum_formalism__pilot_wave_reading, theater_ratio, 1952, 0.15).
narrative_ontology:measurement(quan_tr_t1964, quantum_formalism__pilot_wave_reading, theater_ratio, 1964, 0.18).
narrative_ontology:measurement(quan_tr_t1980, quantum_formalism__pilot_wave_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(quan_tr_t1995, quantum_formalism__pilot_wave_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(quan_tr_t2010, quantum_formalism__pilot_wave_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(quan_tr_t2025, quantum_formalism__pilot_wave_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(quan_be_t1952, quantum_formalism__pilot_wave_reading, base_extractiveness, 1952, 0.2).
narrative_ontology:measurement(quan_be_t1964, quantum_formalism__pilot_wave_reading, base_extractiveness, 1964, 0.25).
narrative_ontology:measurement(quan_be_t1980, quantum_formalism__pilot_wave_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(quan_be_t1995, quantum_formalism__pilot_wave_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement(quan_be_t2010, quantum_formalism__pilot_wave_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(quan_be_t2025, quantum_formalism__pilot_wave_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t1952, quantum_formalism__pilot_wave_reading, suppression_requirement, 1952, 0.65).
narrative_ontology:measurement(quan_su_t1964, quantum_formalism__pilot_wave_reading, suppression_requirement, 1964, 0.62).
narrative_ontology:measurement(quan_su_t1980, quantum_formalism__pilot_wave_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(quan_su_t1995, quantum_formalism__pilot_wave_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(quan_su_t2010, quantum_formalism__pilot_wave_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(quan_su_t2025, quantum_formalism__pilot_wave_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(quantum_formalism__pilot_wave_reading, 0.1).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, many_worlds_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'interpretation of quantum mechanics' per the ε-invariance principle: copenhagen_reading (collapse as a real indeterministic process; the historically dominant, lowest-ε-for-marginalization but arguably most theater-heavy reading given the loose 'measurement' postulate), many_worlds_reading (universal deterministic evolution with branching; different beneficiary set — decoherence theorists, quantum information researchers who avoid retrofitting collapse), and pilot_wave_reading (this story; deterministic hidden variables, nonlocal guidance, smallest but tightly organized beneficiary community, moderate ε driven by real but diffuse professional marginalization rather than concentrated capture). All three share identical empirical predictions and thus cannot be distinguished by any single observable choice, which is exactly why they are authored as three separate constraint stories rather than one story with a measurement-basis parameter — the labels differ in ontology and professional sociology, not in ε derivable from experiment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
