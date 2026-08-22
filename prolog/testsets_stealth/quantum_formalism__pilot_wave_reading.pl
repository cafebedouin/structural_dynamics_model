% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Pilot-Wave Reading of the Quantum Formalism (de Broglie-Bohm Guided-Particle Ontology)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The pilot-wave reading holds that the quantum formalism describes
 *   statistical equilibrium of a deeper reality: particles always possess
 *   definite positions, guided by a physically real wavefunction evolving
 *   deterministically; measurement reveals pre-existing values and the
 *   observer is eliminable. This story instantiates ONE reading of the kernel
 *   quantum_formalism; the sibling readings (copenhagen_reading,
 *   many_worlds_reading) are separate constraint files, not parts of this
 *   one. The colloquial label 'interpretations of quantum mechanics'
 *   decomposes, per the epsilon-invariance principle, into three structurally
 *   distinct constraints with different epsilon values: this reading's
 *   epsilon (~0.30) prices nonlocality acceptance, a configuration-space
 *   guiding field, the quantum-equilibrium postulate, and an incomplete
 *   relativistic extension; the Copenhagen reading's prices indeterminism and
 *   observer privilege; the Many-Worlds reading's prices
 *   world-multiplication. The reading is sometimes rhetorically naturalized
 *   ('this is simply what the formalism says'), but it is a constructed,
 *   optional stance — hence not a mountain. KEY AGENTS (by structural
 *   relationship): - bohmian_research_community: Primary beneficiary
 *   (organized/identity_locked) — collects ontological coherence and a solved
 *   measurement problem; bears the reading's theoretical costs -
 *   bohmian_program_leadership: Agenda-setter (organized/identity_locked) —
 *   administers orthodoxy, adjudicates internal disputes, sets the research
 *   agenda - physics_graduate_students_in_program: Payer with incidental
 *   benefit (moderate/constrained) — transfer training years and career risk
 *   into the program - mainstream_physics_establishment: Excluded resister
 *   (institutional/mobile) — controls textbooks, grants, and hiring; declines
 *   engagement on ontological terms - quantum_experimentalists: Structurally
 *   excluded (institutional/mobile) — empirical equivalence deprives them of
 *   any arbitrating experiment - philosophers_of_physics: Analytical observer
 *   (moderate/analytical) — maps the trade-off space from outside.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.3).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.18).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave Reading of the Quantum Formalism (de Broglie-Bohm Guided-Particle Ontology)").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, 'ff44ec62-c704-4e45-bee7-534b38e71cef').
narrative_ontology:cs_kernel_codification('ff44ec62-c704-4e45-bee7-534b38e71cef', formalized).
narrative_ontology:cs_authority_grounding('ff44ec62-c704-4e45-bee7-534b38e71cef', expertise).
narrative_ontology:cs_interpretation_layer_present('ff44ec62-c704-4e45-bee7-534b38e71cef').
narrative_ontology:cs_reading_relation('ff44ec62-c704-4e45-bee7-534b38e71cef', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('ff44ec62-c704-4e45-bee7-534b38e71cef', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_axiom('ff44ec62-c704-4e45-bee7-534b38e71cef', foundational, particles_have_definite_positions_at_all_times).
narrative_ontology:cs_axiom_status(particles_have_definite_positions_at_all_times, holdable).
narrative_ontology:cs_axiom_grounding('ff44ec62-c704-4e45-bee7-534b38e71cef', particles_have_definite_positions_at_all_times, empirically_contingent).
narrative_ontology:cs_axiom('ff44ec62-c704-4e45-bee7-534b38e71cef', foundational, wavefunction_is_physical_guiding_field).
narrative_ontology:cs_axiom_status(wavefunction_is_physical_guiding_field, holdable).
narrative_ontology:cs_axiom_grounding('ff44ec62-c704-4e45-bee7-534b38e71cef', wavefunction_is_physical_guiding_field, instrumental).
narrative_ontology:cs_axiom('ff44ec62-c704-4e45-bee7-534b38e71cef', secondary, measurement_reveals_preexisting_values).
narrative_ontology:cs_axiom_status(measurement_reveals_preexisting_values, holdable).
narrative_ontology:cs_axiom_grounding('ff44ec62-c704-4e45-bee7-534b38e71cef', measurement_reveals_preexisting_values, empirically_contingent).
narrative_ontology:cs_reference_frame('ff44ec62-c704-4e45-bee7-534b38e71cef', deterministic_guided_particle_ontology).
narrative_ontology:cs_drift_state('ff44ec62-c704-4e45-bee7-534b38e71cef', contemporary_post_valentini_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ff44ec62-c704-4e45-bee7-534b38e71cef', '2026-08-06T09:14:00Z').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_research_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_program_leadership).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, physics_graduate_students_in_program).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, bohmian_research_community).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, physics_graduate_students_in_program).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An international network of physicists, centered on a few research groups, who work within the de Broglie-Bohm ontology: particles always have definite positions, guided by a physically real wavefunction. They receive what they joined for — a deterministic, observer-free account on which measurement reveals pre-existing values — and they pay the package price: nonlocality accepted as real, a wavefunction living in configuration space, an unexplained quantum-equilibrium regularity, and an incomplete relativistic extension. Exit is possible and occasionally taken, but leaving means abandoning the research identity, collaboration network, and problem portfolio built around the ontology.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_research_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_research_community, payer).

% Senior figures and group heads who set what counts as orthodox Bohmianism: which formulations are canonical, which internal disputes (such as the status of quantum equilibrium) are central versus peripheral, and which problems the program's younger members should work. They adjudicate boundary questions through seminars, edited volumes, and invitation networks, and their own scholarly standing is bound to the program's continued viability.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_program_leadership, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, bohmian_program_leadership, beneficiary).

% Doctoral students and postdocs who invest training years mastering the program's formalism and literature. They receive ontological clarity and a well-defined problem set, but they bear concentrated career risk: few faculty positions worldwide reward this specialization, and switching fields later means writing off accumulated capital. Their costs are front-loaded; the certainty they were promised arrives only if the program thrives.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, physics_graduate_students_in_program, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, physics_graduate_students_in_program, beneficiary).

% Textbook authors, grant panels, hiring committees, and prominent theorists who control curriculum, funding, and prestige. They treat the measurement problem as administratively moot and decline to engage the pilot-wave ontology on its own terms; the reading appears in their courses, if at all, as a historical footnote. Their self-exclusion from the ontological question is what keeps the reading marginal, and it obliges them to defend the silence whenever the reading's advocates press the point.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, mainstream_physics_establishment, excluded,
    institutional, generational, mobile, global).

% Physicists running precision tests of quantum mechanics. In the reading's equilibrium regime the pilot-wave account predicts exactly what orthodox quantum mechanics predicts, so no experiment they can currently run arbitrates between the readings; they are structurally absent from the interpretive contest. They would gain a decisive seat if non-equilibrium signatures were ever detected.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_experimentalists, excluded,
    institutional, biographical, mobile, global).

% Philosophers of science who analyze the reading's structure — its ontological costs, its empirical equivalence, its embrace of nonlocality — from outside the program. Neither collecting nor paying, they map the trade-off space the reading opens and closes, and their assessments circulate back into physics as legitimacy signals.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, philosophers_of_physics, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, diffuse).
narrative_ontology:fixing_cost_class(quantum_formalism__pilot_wave_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, for its participants, the measurement problem and the ontology vacuum of orthodox quantum mechanics: fixes what exists (particles with definite positions), what guides them (a physically real wavefunction evolving unitarily), and what measurement is (revelation of pre-existing values) — supplying a shared foundation on which a research community accumulates results without per-member metaphysical improvisation.
% TRANSFER_FUNCTION: Moves career-years, training effort, and reputational risk from young physicists into the program's collective agenda; moves ontological certainty, determinism, and a solved measurement problem back to members; and moves legitimacy claims outward toward a skeptical mainstream that controls jobs, grants, and curriculum.
% ABSENT_VOICES: Quantum experimentalists — the reading predicts nothing distinguishable in its equilibrium regime, so the people who could in principle arbitrate have no seat. Textbook authors and curriculum committees never present the option, so generations of students choose without knowing a choice exists. Physicists outside the realist camp bear the discourse-fragmentation costs of a three-way interpretive contest without participating in it.
% DISAPPEARANCE_RATIONALE: Mainstream physics would proceed unchanged — predictions, experiments, and technology untouched. But the arrangements organized around the reading would rearrange: the Bohmian research community would dissolve into other programs, the realist wing of philosophy of physics would lose its flagship worked example of realism surviving quantum mechanics, and the interpretive contest would collapse to a two-way race. The dependence is real but narrow.
% FOUNDING_PROBLEM: Orthodox quantum mechanics as consolidated by the late 1920s left its ontology unset: the wavefunction evolves deterministically between measurements yet is said to collapse at them; outcomes appear without values existing beforehand; the observer seemed constitutive of reality. De Broglie's 1927 pilot-wave proposal and Bohm's 1952 reconstruction were built to remove these embarrassments — definite particle positions at all times, deterministic guidance, measurement as mere revelation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the measurement problem remains an acknowledged open problem in mainstream venues — textbook treatments by non-Bohmian physicists call it unresolved, general physics meetings continue to run interpretation sessions, and philosophers of physics across all camps (Copenhagen-sympathetic, Everettian, and otherwise) agree the bare formalism does not specify measurement. No major non-Bohmian figure attests that the problem is solved.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-low (0.30 at interval end) because the reading's costs are intrinsic prices of the ontology rather than transfers: nonlocality is forced by Bell's theorem plus empirical adequacy, the guiding field lives in configuration space, equilibrium is an additional posit, and the relativistic extension remains incomplete. No mechanism channels any member's loss into another seat's gain. Suppression is low (0.18): the reading coerces no one; its mild internal boundary maintenance (what counts as orthodox Bohmianism) is peer pressure, not enforcement machinery, and exit is open — physicists leave the program routinely. Theater is moderate-low (0.30): most program activity is functional research (formulations, QFT models, equilibrium derivations), with a growing commemorative and polemical share (Solvay anniversaries, anti-Copenhagen polemics) as the program institutionalized. Accessibility collapse is low (0.20): understanding the reading does not close alternatives — it maps the trade-off space, leaving Copenhagen and Many-Worlds fully accessible. Resistance is high (0.68): the establishment's control of curriculum, funding, and prestige meets the reading with sustained dismissal — notably, high resistance AIMED AT the constraint coexists with low suppression GENERATED BY it. The measurement series run on one shared nine-point grid (t=0 is de Broglie 1927; t=25 Bohm's 1952 revival; t=37 Bell 1964; t=65 the modern formulation era; t=95 contemporary); extractiveness peaks around Bell, when the nonlocality price became provably unavoidable, then declines as the modern formulation resolved measurement cleanly. Values are measured at the t=95 (post-maturation) phase. Identity-lock dynamics: the community's exit_options are identity_locked through professional fusion (careers built on the program), ideological fusion (realism-plus-determinism as worldview, making exit feel like defection), and relational fusion (lineage ties to advisors); if that frame broke — say, a decisive non-equilibrium test failed — exit would normalize and the program would shrink to historians of physics. Suppression mechanism note: what suppression exists is structural-external (establishment gatekeeping against the program) plus a small internalized component (members' fused loyalty); the scalar primarily reflects the reading's own mild internal pressure.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently. From the leadership seat the reading is a hard-won rational reconstruction — the formalism finally saying something definite about reality; from the student seat it is a costly apprenticeship into a marginal program with narrow job prospects, where the promised certainty arrives only if the program thrives; from the establishment seat it is a recurring nuisance claim on attention that pragmatic silence mostly settles. Same-power divergence: the bohmian community and the establishment are both organized institutional actors in the same discipline, yet their exits differ completely — the community's exit is identity-priced defection, the establishment's is costless non-engagement — because the constraint binds only those who have joined it. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The bohmian_research_community is the declared beneficiary and sits near the beneficiary end of directionality: the arrangement subsidizes members with ontological coherence, a solved measurement problem, and a protected problem portfolio. Program leadership shares that subsidy and additionally administers the arrangement. Graduate students sit nearer the target end: they pay front-loaded training and career-risk costs for benefits that arrive late and contingently. The establishment and experimentalists sit outside the transfer loop — their costs are opportunity costs of the interpretive contest itself, not extraction flowing to anyone. No directionality overrides are needed: the derivation from beneficiary declaration plus exit options reproduces these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. A snare reading would require identifiable victims and coercive persistence — absent: no one is trapped, no one's loss funds anyone's gain, and the arrangement would survive its members' goodwill indefinitely only so long as it keeps solving the measurement problem for them. A mountain reading would require naturality — absent: the reading is a constructed stance, however forcefully its advocates naturalize it. Rope captures the structure: genuine coordination (the measurement problem solved once, centrally, for all members instead of per-theoretician improvisation) at a price participants knowingly pay. On the R5 genealogy interview, the founding problem (the measurement problem and the ontology vacuum of orthodox QM) is still LIVE, corroborated from outside the benefiting parties, so no mandate-atrophy obtains: status=live crossed with verdict=world_rearranges raises no zombie flag. Receipt surface: gain_flow is authored 'diffuse' as an affirmative finding — every named seat was checked, and none receives the extraction, because the costs are prices paid to the theory's structure rather than transfers accruing anywhere (leadership accrues status from the program's outputs, not from receiving members' costs). fixing_cost is 'cheap': the community could dissolve the program at will by stopping work on it; nothing structural prevents exit. Cheap-plus-diffuse is the transient-neglect cell, not the piton cell — consistent with a live, voluntary rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_equivalence_nonequilibrium_test,
    'Is the pilot-wave reading permanently empirically equivalent to orthodox quantum mechanics in its equilibrium regime, or does quantum non-equilibrium (Valentini''s program) yield detectable deviations that would give the reading independent evidential support?',
    'Searches for non-equilibrium signatures: primordial relic particles, early-universe matter violating the Born distribution, astrophysical surveys of hypothetical quantum-nonequilibrium systems.',
    'Detection would transform the reading from an ontological preference into an empirically advantaged theory and collapse its main epistemic cost; permanent equivalence would mean its persistence rests entirely on inference-to-best-explanation and identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_equivalence_nonequilibrium_test, empirical, 'Whether empirical equivalence with orthodox QM is permanent or breakable via non-equilibrium signatures.').

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading (pilot_wave_reading) of the kernel quantum_formalism; what structurally would the sibling readings (copenhagen_reading, many_worlds_reading) change, and where exactly is the disagreement located?',
    'Comparative analysis across the three reading-files: the disagreement is located in the completion of the shared formalism with a world-picture — physical collapse versus no collapse (Copenhagen), unique definite outcome versus all outcomes realized (Many-Worlds), definite particle positions versus position-less wavefunction monism.',
    'Because the readings are ontological completions of one calculus rather than rival applications of one text, each pair makes contradictory existence claims about the same entities; a party adopting this reading is logically committed to rejecting both siblings'' core premises (forecloses edges), unlike kernels where readings merely coexist across factions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    quantum_equilibrium_status,
    'Why does the Born-rule distribution hold for the program''s own matter — is quantum equilibrium a law-like initial condition, a dynamically attractive state, or a brute unexplained postulate?',
    'Cosmological analysis of early-universe relaxation timescales; dynamical derivations of equilibrium attraction; comparison with thermalization analogies.',
    'A dynamical derivation would remove the reading''s principal residual postulate and lower its extraction further; a brute-postulate verdict fixes a permanent unexplained component in the arrangement''s cost structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_equilibrium_status, empirical, 'Status of the quantum equilibrium hypothesis within the guided-particle ontology.').

omega_variable(
    wavefunction_ontology_kind,
    'Is the configuration-space wavefunction in this reading a physical field (however alien its arena) or a nomological entity — a law governing particle motion rather than a thing in the world?',
    'Conceptual analysis within the program: separability arguments, density-functional reformulations attempting ordinary-space fields, and the program''s own shifting rhetoric across publications.',
    'Nomological framing reduces the ontological extravagance cost (one fewer kind of entity) and lowers the reading''s extraction; physical-field framing preserves the realist payoff but sustains the configuration-space burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wavefunction_ontology_kind, conceptual, 'Whether the guiding field is ontologically physical or law-like, changing the reading''s extravagance cost.').

omega_variable(
    relativistic_extension_debt,
    'Can the reading extend to full relativistic quantum field theory without ad hoc machinery, or is it confined to the non-relativistic domain?',
    'Continued development of Bell-type QFT models (particle creation via stochastic jumps, fermion position ontologies) and assessment of their empirical and conceptual adequacy against standard QFT.',
    'Failure would confine the reading to a toy domain, converting its theoretical debt into a terminal liability and raising long-run extraction for members whose careers depend on extension; success would further reduce the cost of adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relativistic_extension_debt, empirical, 'Whether the relativistic QFT extension gap is closable or a permanent liability.').

omega_variable(
    authority_grounding_framing,
    'Is the reading''s internal authority grounded in expertise (voluntary deference to demonstrated mathematical competence, as the Durr-Goldstein-Zanghi formulation''s rigor commands assent) or in lineage (continuity with the de Broglie-Bohm-Bell transmission chain, with founder texts as touchstones)?',
    'Observe how internal disputes are actually adjudicated: by mathematical demonstration and derivation (expertise signal) versus by appeal to what Bohm or Bell meant or endorsed (lineage signal); both rhetorics are present, so the ratio decides.',
    'Expertise-framing keeps the arrangement open-inquiry and supports the rope classification; lineage-framing would weight the interpretive layer toward tradition-maintenance, raising theater_ratio and shifting the computed pattern toward identity-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS framing under-determination: expertise versus lineage as the authority ground of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pilot_wave_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pilot_wave_tr_t15, quantum_formalism__pilot_wave_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(pilot_wave_tr_t25, quantum_formalism__pilot_wave_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(pilot_wave_tr_t37, quantum_formalism__pilot_wave_reading, theater_ratio, 37, 0.18).
narrative_ontology:measurement(pilot_wave_tr_t50, quantum_formalism__pilot_wave_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(pilot_wave_tr_t65, quantum_formalism__pilot_wave_reading, theater_ratio, 65, 0.24).
narrative_ontology:measurement(pilot_wave_tr_t78, quantum_formalism__pilot_wave_reading, theater_ratio, 78, 0.27).
narrative_ontology:measurement(pilot_wave_tr_t88, quantum_formalism__pilot_wave_reading, theater_ratio, 88, 0.29).
narrative_ontology:measurement(pilot_wave_tr_t95, quantum_formalism__pilot_wave_reading, theater_ratio, 95, 0.3).

% Extraction over time
narrative_ontology:measurement(pilot_wave_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pilot_wave_be_t15, quantum_formalism__pilot_wave_reading, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(pilot_wave_be_t25, quantum_formalism__pilot_wave_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(pilot_wave_be_t37, quantum_formalism__pilot_wave_reading, base_extractiveness, 37, 0.38).
narrative_ontology:measurement(pilot_wave_be_t50, quantum_formalism__pilot_wave_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(pilot_wave_be_t65, quantum_formalism__pilot_wave_reading, base_extractiveness, 65, 0.35).
narrative_ontology:measurement(pilot_wave_be_t78, quantum_formalism__pilot_wave_reading, base_extractiveness, 78, 0.33).
narrative_ontology:measurement(pilot_wave_be_t88, quantum_formalism__pilot_wave_reading, base_extractiveness, 88, 0.32).
narrative_ontology:measurement(pilot_wave_be_t95, quantum_formalism__pilot_wave_reading, base_extractiveness, 95, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quantum_formalism__pilot_wave_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, quantum_formalism__many_worlds_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'interpretations of quantum mechanics' into three readings of the kernel quantum_formalism: copenhagen_reading (upstream — institutionally dominant; its pragmatist settlement is what the dissents define themselves against), many_worlds_reading, and this pilot_wave_reading (downstream dissents). Each file carries its own epsilon over its own standing arrangement; they are linked here because the upstream settlement's stability conditions the dissents' resource environment, and because each reading cites the others' failures as evidence for itself. This file's epsilon (~0.30) prices nonlocality acceptance, configuration-space ontology, the equilibrium postulate, and QFT-extension debt; the siblings' epsilons price different packages and are authored in their own files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
