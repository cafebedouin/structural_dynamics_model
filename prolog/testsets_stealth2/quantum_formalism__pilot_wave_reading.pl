% ============================================================================
% CONSTRAINT STORY: quantum_formalism__pilot_wave_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Pilot-Wave Reading of the Quantum Formalism (de Broglie-Bohm Interpretive Commitment)
 *   domain: philosophy_of_physics/quantum_foundations
 *
 * SUMMARY:
 *   The pilot-wave reading holds that particles possess definite positions at
 *   all times, guided by an objectively real wavefunction acting as a
 *   physical field, so that measurement reveals pre-existing values and the
 *   observer is eliminable; determinism is restored at the price of explicit
 *   nonlocal guidance. This story treats that reading as a standing
 *   interpretive arrangement operating on and within its research community
 *   from Bohm's 1952 reconstruction to the present. The epsilon referent is
 *   the arrangement under contest — the reading's actual operation on its
 *   holders and neighbors — assessed by the reading's own lights: the reading
 *   regards its costs (nonlocality conceded, field-theoretic friction, a
 *   narrow career economy) as justified prices of ontological clarity, and
 *   the authored metrics describe what its operation nonetheless transfers
 *   and gates. KEY AGENTS (by structural relationship): -
 *   bohmian_program_leadership: Agenda-setter (organized/arbitrage) — defines
 *   orthodoxy, convenes venues, collects authority and definitional control -
 *   scientific_realist_philosophers: Beneficiary (moderate/mobile) — draws
 *   worldview vindication, supplies conceptual defense -
 *   junior_foundational_physicists: Primary payer (powerless/identity_locked)
 *   — supplies technical labor, bears career risk in a closed niche -
 *   quantum_nonequilibrium_dissidents: Internal payer-beneficiary
 *   (moderate/constrained) — heterodox extension line gated by orthodoxy -
 *   copenhagen_mainstream_physics: Excluded party (institutional/mobile) —
 *   would object to ontological overclaim, disengaged by mutual choice -
 *   foundations_epistemology_observers: Analytical observer
 *   (analytical/analytical) — sees the full structure without allegiance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantum_formalism__pilot_wave_reading, 0.42).
domain_priors:suppression_score(quantum_formalism__pilot_wave_reading, 0.3).
domain_priors:theater_ratio(quantum_formalism__pilot_wave_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quantum_formalism__pilot_wave_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantum_formalism__pilot_wave_reading, tangled_rope).
narrative_ontology:human_readable(quantum_formalism__pilot_wave_reading, "Pilot-Wave Reading of the Quantum Formalism (de Broglie-Bohm Interpretive Commitment)").
narrative_ontology:topic_domain(quantum_formalism__pilot_wave_reading, "philosophy_of_physics/quantum_foundations").

domain_priors:requires_active_enforcement(quantum_formalism__pilot_wave_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantum_formalism__pilot_wave_reading, 'fe9c2ebf-49d1-47be-98f1-686389d98c87').
narrative_ontology:cs_kernel_codification('fe9c2ebf-49d1-47be-98f1-686389d98c87', formalized).
narrative_ontology:cs_authority_grounding('fe9c2ebf-49d1-47be-98f1-686389d98c87', expertise).
narrative_ontology:cs_interpretation_layer_present('fe9c2ebf-49d1-47be-98f1-686389d98c87').
narrative_ontology:cs_reading_relation('fe9c2ebf-49d1-47be-98f1-686389d98c87', quantum_formalism__copenhagen_reading, forecloses).
narrative_ontology:cs_reading_relation('fe9c2ebf-49d1-47be-98f1-686389d98c87', quantum_formalism__many_worlds_reading, forecloses).
narrative_ontology:cs_axiom('fe9c2ebf-49d1-47be-98f1-686389d98c87', foundational, definite_particle_positions_at_all_times).
narrative_ontology:cs_axiom_status(definite_particle_positions_at_all_times, holdable).
narrative_ontology:cs_axiom_grounding('fe9c2ebf-49d1-47be-98f1-686389d98c87', definite_particle_positions_at_all_times, instrumental).
narrative_ontology:cs_axiom('fe9c2ebf-49d1-47be-98f1-686389d98c87', foundational, wavefunction_is_objectively_real_field).
narrative_ontology:cs_axiom_status(wavefunction_is_objectively_real_field, holdable).
narrative_ontology:cs_axiom_grounding('fe9c2ebf-49d1-47be-98f1-686389d98c87', wavefunction_is_objectively_real_field, deontological).
narrative_ontology:cs_reference_frame('fe9c2ebf-49d1-47be-98f1-686389d98c87', deterministic_realist_completion_of_quantum_statistics).
narrative_ontology:cs_drift_state('fe9c2ebf-49d1-47be-98f1-686389d98c87', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('fe9c2ebf-49d1-47be-98f1-686389d98c87', '').
narrative_ontology:cs_kernel_id(quantum_formalism__pilot_wave_reading, quantum_formalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, bohmian_program_leadership).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, scientific_realist_philosophers).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, junior_foundational_physicists).
narrative_ontology:constraint_victim(quantum_formalism__pilot_wave_reading, quantum_nonequilibrium_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quantum_formalism__pilot_wave_reading, quantum_nonequilibrium_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior theorists who anchor the principal schools working on the guidance-equation framework. They set what counts as a legitimate extension of the particle-plus-guiding-field picture, convene the recurring workshops, edit special journal issues, and train the doctoral students who staff the program. Citation weight and definitional authority over the framework's orthodoxy concentrate here. Stepping away would forfeit decades of accumulated standing inside the niche, though their general eminence in foundations would survive a pivot to neighboring topics.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, bohmian_program_leadership, agenda_setter,
    organized, generational, arbitrage, global).

% Philosophers committed to scientific realism and determinism who treat the reading as the strongest existing demonstration that quantum phenomena admit a mind-independent, value-definite description requiring no observer act. They supply conceptual defense, historical narrative, and argumentative reinforcement in journals and monographs. Their stake is vindication of a worldview rather than program revenue, and they can redirect their attention to other foundations case studies at will.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, scientific_realist_philosophers, beneficiary,
    moderate, generational, mobile, global).

% Doctoral students and postdocs trained inside the guidance-equation framework. They produce the technical derivations, translate standard quantum results into the preferred ontology, and staff the workshops and seminar circuits. Permanent positions in the niche number in the dozens worldwide, mainstream physics departments rarely hire for this specialty, and the accumulated skills are legible mainly inside the community. Continued participation is sustained by sunk training investment and by identification with the realist-determinist cause; leaving means retraining toward adjacent but distinct specialties and abandoning a professional self-concept built around the framework.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, junior_foundational_physicists, payer,
    powerless, biographical, identity_locked, global).

% Researchers extending the framework to hypothetical departures from Born-rule statistics: relaxation toward quantum equilibrium, primordial nonequilibrium signatures in the cosmic microwave background, and proposed laboratory searches. Their work presupposes the very framework whose orthodoxy they strain, and the established schools have historically treated the extension as speculative heterodoxy, gating its access to community venues and editorial goodwill. They bear the costs of internal gatekeeping while arguing that theirs is the only line within the framework capable of producing new predictions; any empirical vindication of nonequilibrium effects would redound to their credit specifically.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, quantum_nonequilibrium_dissidents, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(quantum_formalism__pilot_wave_reading, quantum_nonequilibrium_dissidents, beneficiary).

% The dominant body of working quantum physicists, quantum-information theorists, and textbook authors. They treat operator-based quantum mechanics with its standard measurement postulates as settled tooling, rarely cite the guidance-equation literature, and decline invitations to adjudicate its ontology claims. Their collective non-engagement is what keeps the program marginal in hiring, funding, and curriculum. They would object if the reading's ontological claims were presented to students or funding bodies as established fact about nature rather than as one contested completion.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, copenhagen_mainstream_physics, excluded,
    institutional, generational, mobile, global).

% Philosophers of physics and historians of science who track the interpretive contest without allegiance to any reading. They compare the readings' costs and virtues, audit claims of empirical equivalence, and document how each interpretive community polices its own boundaries. They publish field-wide assessments and hold no stake in which reading prevails.
narrative_ontology:constraint_stakeholder(quantum_formalism__pilot_wave_reading, foundations_epistemology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quantum_formalism__pilot_wave_reading, bohmian_program_leadership).
narrative_ontology:fixing_cost_class(quantum_formalism__pilot_wave_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a dispersed community of physicists and philosophers a single consistent, value-definite ontology for quantum phenomena, so that technical work — deriving the Born rule from typicality assumptions, constructing field-theoretic versions of the particle-plus-guiding-field picture, proposing nonequilibrium tests — can accumulate without each participant re-deriving a response to the measurement problem from scratch.
% TRANSFER_FUNCTION: Moves epistemic authority, venue access, and definitional power over orthodox extensions from junior entrants and internal heterodox lines toward the senior schools; moves interpretive reassurance (determinism, realism, eliminability of the observer) outward to all holders; moves defensive labor — answering empiricist, mainstream, and internal objections — downward onto junior members.
% ABSENT_VOICES: Mainstream quantum physicists and quantum-information researchers would object that an empirically equivalent ontology with no presently accessible distinguishing prediction is being vested with the status of nature's actual constitution; instrumentalist philosophers would object that the ontology talk is idle overhead. Both groups are absent from the program's venues by mutual disengagement rather than by the program's deliberate design, which is itself part of the structure to assess.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the research program built on it loses its object: no guidance equation to extend, no typicality derivations, no nonequilibrium proposals. Realist philosophy of physics loses its flagship existence proof for deterministic completability. The interpretive debate loses its determinist pole and collapses toward a two-way contest between collapse-based and branching-based accounts. Several dozen careers, a workshop infrastructure, and a monograph literature would rearrange around the loss.
% FOUNDING_PROBLEM: Orthodox quantum mechanics appeared to deny definite values to unmeasured quantities, to introduce irreducible indeterminism into fundamental physics, and to make measurement outcomes depend on an observer act — the measurement problem. The reading was constructed (de Broglie 1927; Bohm 1952) to restore definite particle positions, deterministic evolution, and an observer-independent reality while reproducing every quantum prediction.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the mainstream foundations literature — including vocal critics of the guidance-equation program and neutral textbook treatments — widely attests that orthodox quantum mechanics lacks a consensus account of measurement, and Bell's own writings urged that the completability question be taken seriously. No party outside the realist camp, however, attests that the problem requires this particular solution rather than dissolution, instrumentalist dismissal, or a rival completion; that residual dispute is itself signal.
narrative_ontology:disappearance_verdict(quantum_formalism__pilot_wave_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantum_formalism__pilot_wave_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantum_formalism__pilot_wave_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quantum_formalism__pilot_wave_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quantum_formalism__pilot_wave_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-low (0.42 at interval end) because the arrangement's transfers are real but consensual: junior labor and career risk sustain program continuity while authority and definitional control concentrate in the senior schools, and every holder concedes nonlocality and an empirically redundant ontology in exchange for interpretive resolution. Suppression is moderate-low (0.30) and is a raw structural property, unscaled by power or scope — only extractiveness is scaled by the engine. Roughly speaking the suppression decomposes into a structural component (venue gatekeeping, a job market narrow enough that skills are illiquid outside the niche) and an internalized component (sunk-training identity fusion), weighted perhaps 60/40; the identity_lock_vs_convection omega carries the ambiguity formally. Theater ratio is low-moderate (0.25): substantial genuine technical work continues (field-theoretic constructions, typicality theorems, nonequilibrium proposals), alongside a growing share of ritual restatement of established equivalence arguments and boundary defense. Accessibility collapse is 0.45: for an individual holder, accepting definite values logically closes the collapse-based rivals, but conversion between readings remains possible and observed, so the wider option space never fully disappears. Resistance is 0.55: mainstream disengagement, empiricist critique of untestable ontology, and internal equilibrium disputes all press on the arrangement continuously. The temporal series run on one shared seven-point grid (every tracked metric authored at every point, 0-72). Base extractiveness rises monotonically as orthodoxy consolidates and the career economy narrows, plateauing recently. The suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity change: gatekeeping intensity rose through the orthodoxy-consolidation decades, peaked mid-interval, and partially relaxed as nonequilibrium pluralism gained tolerated status — a rise-and-partial-decline arc, not a static picture. Final measurement values match the base_properties scalars by construction of the grid endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leadership seat the arrangement is a hard-won intellectual achievement and a legitimate order it administers; from the junior seat the same structure operates as a narrowing market with identity-costly exit; from the dissident seat it operates as gatekeeping that taxes precisely the line most likely to vindicate the framework; from the mainstream seat it is barely perceptible background. The engine derives these per-seat classifications from the structural data — the authored claim does not adjudicate them, and divergence between seats is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership holds beneficiary-and-agenda-setter position with arbitrage-grade exit, placing it near the beneficiary end (low d, damped or inverted effective burden). Scientific-realist philosophers are pure beneficiaries with mobile exit — low d. Junior foundational physicists are declared payers with identity-locked exit, sitting near the full-target end (high d, amplified effective burden). Nonequilibrium dissidents are genuinely dual-positioned: they bear gatekeeping costs as payers while their entire research agenda presupposes and would be vindicated by the framework's success, so their true relationship is intermediate (roughly d ~ 0.6); this is carried through their secondary_role declaration rather than a directionality override, because the derivation chain reads dual roles and no single power-atom-keyed override could hit this seat without mispricing the other moderate-power agents. Mainstream physics is excluded rather than coordinated — its coupling to the arrangement runs through non-engagement, and it feeds no beneficiary or victim derivation. Observers are analytically neutral. Directionality overrides are omitted entirely: the derivation from beneficiary/victim declarations plus exit options reproduces every seat's relationship, and the one genuinely ambiguous seat is handled structurally via secondary_role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — orthodox quantum mechanics' lack of a consensus measurement account — is still live, so no mandatrophy is declared and the flag is left unset. The classification prevents two opposite mislabelings. Reading the arrangement as pure coordination (a rope) would erase the documented career-economy asymmetry: junior labor and risk flow inward while authority concentrates, maintained by active boundary enforcement. Reading it as pure extraction (a snare) would erase the genuine coordination function — a shared value-definite ontology that lets technical work accumulate — and would misread consensual, benefit-laden participation as coerced. The hybrid classification holds both halves. Forward-looking drift: if the measurement problem were ever widely regarded as dissolved (for instance by a completed decoherence account accepted across the field), the reading's mandate would die while the arrangement persisted as ritual defense of a settled question — the founding_problem_status x disappearance_verdict mismatch consumer is the tripwire for that transition, and the rising theater_ratio series is its leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the pilot_wave_reading of the quantum_formalism kernel; would instantiating the copenhagen_reading or many_worlds_reading instead change the structural classification of the interpretive arrangement?',
    'Cross-reading comparison of the three sibling stories'' stakeholder surfaces, epsilon values, and computed per-seat classifications over the same formalism referent.',
    'If sibling readings compute materially different types over the same topic, the kernel''s classification is reading-indexed rather than topic-level, and any topic-level verdict about ''the quantum interpretation debate'' must aggregate across the family rather than cite one member.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: classification may be a property of the reading, not of the topic.').

omega_variable(
    empirical_accessibility_of_distinctive_content,
    'The reading''s distinctive content (departures from Born-rule statistics under nonequilibrium conditions, the reality of the empty-wave sector) is empirically inaccessible while quantum equilibrium holds; is the commitment''s persistence rational coordination around a live hypothesis, or unfalsifiable inertia?',
    'Execution of the proposed nonequilibrium searches (primordial-statistics bounds from cosmological data, relaxation-time laboratory proposals); a detected deviation transforms the reading''s empirical status overnight.',
    'Detection would convert the reading from interpretive commitment to predictive research program, collapsing its measured extraction toward coordination cost; durable null results deepen the inertial component and push the payer seats'' burden upward relative to any offsetting payoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_accessibility_of_distinctive_content, empirical, 'Whether the framework''s distinctive claims are testable in practice or only in principle.').

omega_variable(
    qft_extension_viability,
    'Does the particle-ontology framework extend to quantum field theory — particle creation and annihilation, gauge structure — without sacrificing the classical-style clarity that constitutes its principal advertised benefit?',
    'Convergence of the community on a canonical field-beable or Bell-type jump-process formulation that the rival schools accept as fully covering relativistic quantum theory.',
    'Failure would shrink the reading''s scope claim to non-relativistic systems, raising the cost side of every holder''s ledger while the benefit side stays fixed; success would remove the most-cited technical objection and lower resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qft_extension_viability, empirical, 'Whether the framework covers relativistic quantum theory without loss of its defining virtues.').

omega_variable(
    identity_lock_vs_conviction,
    'Is junior researchers'' continued participation in the program conviction (a continuing net-benefit judgment) or identity lock (exit costs manufactured by sunk training and professional self-concept)?',
    'Post-exit trajectory analysis of researchers who left the niche: if suppression of alternatives and career anxiety persist after departure, the lock is substantially internalized; if leavers report relief and smooth transitions, participation was conviction-priced.',
    'If identity-locked, the junior seat sits nearer the full-target end of the directionality range than its declarations alone suggest, amplifying its computed burden and tilting the arrangement toward the harder end of the hybrid range; if conviction, the arrangement sits nearer pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_conviction, empirical, 'Structural versus internalized retention of the program''s junior labor force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantum_formalism__pilot_wave_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pilot_wave_reading_tr_t0, quantum_formalism__pilot_wave_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pilot_wave_reading_tr_t12, quantum_formalism__pilot_wave_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(pilot_wave_reading_tr_t24, quantum_formalism__pilot_wave_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(pilot_wave_reading_tr_t36, quantum_formalism__pilot_wave_reading, theater_ratio, 36, 0.2).
narrative_ontology:measurement(pilot_wave_reading_tr_t48, quantum_formalism__pilot_wave_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement(pilot_wave_reading_tr_t60, quantum_formalism__pilot_wave_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(pilot_wave_reading_tr_t72, quantum_formalism__pilot_wave_reading, theater_ratio, 72, 0.25).

% Extraction over time
narrative_ontology:measurement(pilot_wave_reading_be_t0, quantum_formalism__pilot_wave_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(pilot_wave_reading_be_t12, quantum_formalism__pilot_wave_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(pilot_wave_reading_be_t24, quantum_formalism__pilot_wave_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(pilot_wave_reading_be_t36, quantum_formalism__pilot_wave_reading, base_extractiveness, 36, 0.36).
narrative_ontology:measurement(pilot_wave_reading_be_t48, quantum_formalism__pilot_wave_reading, base_extractiveness, 48, 0.39).
narrative_ontology:measurement(pilot_wave_reading_be_t60, quantum_formalism__pilot_wave_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(pilot_wave_reading_be_t72, quantum_formalism__pilot_wave_reading, base_extractiveness, 72, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(pilot_wave_reading_su_t0, quantum_formalism__pilot_wave_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(pilot_wave_reading_su_t12, quantum_formalism__pilot_wave_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(pilot_wave_reading_su_t24, quantum_formalism__pilot_wave_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement(pilot_wave_reading_su_t36, quantum_formalism__pilot_wave_reading, suppression_requirement, 36, 0.33).
narrative_ontology:measurement(pilot_wave_reading_su_t48, quantum_formalism__pilot_wave_reading, suppression_requirement, 48, 0.35).
narrative_ontology:measurement(pilot_wave_reading_su_t60, quantum_formalism__pilot_wave_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement(pilot_wave_reading_su_t72, quantum_formalism__pilot_wave_reading, suppression_requirement, 72, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quantum_formalism__pilot_wave_reading, identity_coordination).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, copenhagen_reading).
narrative_ontology:affects_constraint(quantum_formalism__pilot_wave_reading, many_worlds_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the quantum formalism' covers three structurally incompatible ontological commitments, and forcing one story to span them would make epsilon observer-dependent. Each reading is authored separately with its own epsilon, stakeholders, and claimed type; all three link one another through affects_constraints. Upstream structure shared by the family: the common formalism and Bell's theorem constrain every reading alike (Bell forecloses local completions generically, which this reading absorbs as a feature rather than a defect). Downstream pressure runs from whichever reading holds institutional majority — currently the collapse-based sibling — onto this reading's resource environment, which is why this story's resistance metric is elevated relative to its extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
